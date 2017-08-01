package io.llambda.typegen

import scala.util.parsing.combinator._
import java.io.File
import scala.io.Source

class ParseErrorException(message: String) extends Exception(message)

trait CommonParsers extends RegexParsers {
  // Be very strict about identifiers for now
  // We assume all names are camelCase when we rename them for different
  // output langauges
  def identifier = """[a-zA-Z][a-zA-Z0-9]+""".r

  def valueType = positioned(arrayType | nonArrayType)

  def arrayDimensions = rep1("[" ~> """\d+""".r <~ "]") ^^ { _.map(Integer.parseInt(_)) }

  // Types are identifiers plus zero or more pointer indirections
  def nonArrayType = identifier ~ rep("*") ^^ { case typeName ~ indirections =>
    indirections.foldLeft(ParsedTypeName(typeName): ParsedType) { case (innerType, _) =>
      ParsedPointerType(innerType)
    }
  }

  def arrayType = nonArrayType ~ arrayDimensions ^^ { case elementType ~ dimensions =>
    ParsedArrayType(dimensions, elementType)
  }

  // Functions can return "void" in addition to an actual type
  def returnType = voidReturn | nonVoidReturn
  def voidReturn = "void" ^^^ None
  def nonVoidReturn = valueType ^^ (Some(_))
}

trait FieldTypeAliasParser extends CommonParsers {
  def fieldTypeAlias = positioned("fieldtype" ~> (identifier ~ typeInheritence ~ opt(fieldTypeBody)) <~ ";" ^^ {
    case typeName ~ inherits ~ cTypeNameOptOpt =>
      ParsedFieldTypeAlias(typeName, inherits, cTypeNameOptOpt getOrElse None)
  })

  def typeInheritence = ":" ~> fieldSuperType
  def fieldSuperType = anonymousFunctionPointer | valueType

  def anonymousFunctionPointer = positioned(returnType ~ "(" ~ "*" ~ ")" ~ "(" ~ repsep(valueType, ",") ~ ")" ^^ {
    case retType ~ _  ~ _ ~ _ ~ _ ~ argTypes ~ _ =>
      ParsedFunctionPointerType(retType, argTypes)
  })

  def fieldTypeBody = "{" ~> opt(cppNameDef) <~ "}"

  def cppTypeName = """([a-zA-Z_][a-zA-Z0-9_<>]*::)*[a-zA-Z_][a-zA-Z0-9_<>]*""".r
  def cppNameDef = opt("extern") ~ "cppname" ~ "=" ~ cppTypeName <~ ";" ^^ { case externOpt ~ _ ~ _ ~ name =>
    val needsDefinition = !externOpt.isDefined
    ParsedCppType(name, needsDefinition)
  }
}

trait CellDeclarationParser extends CommonParsers {
  def cellDeclaration = positioned("cell" ~ identifier ~ ";" ^^ {
    case _ ~ typeName ~ _ =>
      ParsedCellClassDeclaration(typeName)
  })
}

trait CellDefinitionParser extends CommonParsers {
  def cellDefinition = positioned(rootCellDefinition | taggedCellDefinition | variantCellDefinition)

  def rootCellDefinition = "root" ~ opt(visibilityType) ~ "cell" ~ identifier ~ "typetag" ~ identifier ~ fields ~ ";" ^^ {
    case _ ~ visibility ~ _ ~ typeName ~ _ ~  typeTagField  ~ commonFields ~ _ =>
      ParsedRootClassDefinition(typeName, typeTagField, commonFields, visibility.getOrElse(CellClass.Public))
  }

  def taggedCellDefinition = instanceType ~ opt(visibilityType) ~ ("cell" ~> identifier) ~ cellInheritence ~ fields <~ ";" ^^ {
    case instanceType ~ visibility ~ typeName ~ inherits ~ fields =>
      ParsedTaggedClassDefinition(typeName, instanceType, inherits, fields, visibility.getOrElse(CellClass.Public))
  }

  def variantCellDefinition = "variant" ~ "cell" ~> identifier ~ cellInheritence ~ fields <~ ";" ^^ { case typeName ~ inherits ~ fields =>
    ParsedVariantClassDefinition(typeName, inherits, fields)
  }

  def instanceType = abstractSpecifier | concreteSpecifier | preconstructedSpecifier

  def abstractSpecifier = "abstract"             ^^^ CellClass.Abstract
  def concreteSpecifier = "concrete"             ^^^ CellClass.Concrete
  def preconstructedSpecifier = "preconstructed" ^^^ CellClass.Preconstructed

  // Cells can only inherit from other cells, not field types
  def cellInheritence = ":" ~> identifier

  def visibilityType = internalSpecifier | runtimeOnlySpecifier

  def internalSpecifier = "internal"   ^^^ CellClass.Internal
  def runtimeOnlySpecifier = "runtime" ^^^ CellClass.RuntimeOnly

  def fields = "{" ~> rep(field) <~ "}"

  def field = positioned(arrayField | functionPointerField | valueField) <~ ";"

  def valueField = constSpecifier ~ nonArrayType ~ identifier ~ initializer ^^ {
    case isConst ~ fieldType ~ fieldName ~ fieldInitializer =>
      new ParsedCellField(fieldName, fieldType, isConst, fieldInitializer)
  }

  def constSpecifier = opt("const") ^^ { constOpt =>
    constOpt.isDefined
  }

  def initializer = opt("=" ~> """\d+""".r) ^^ { initializerOpt =>
    initializerOpt.map { initializer =>
      java.lang.Long.parseLong(initializer)
    }
  }

  def arrayField = constSpecifier ~ valueType ~ identifier ~ arrayDimensions ^^ {
    case isConst ~ elementType ~ fieldName ~ dimensions =>
      ParsedCellField(fieldName, ParsedArrayType(dimensions, elementType), isConst, None)
  }

  def functionPointerField = constSpecifier ~ returnType ~ "(" ~ "*" ~ identifier ~ ")" ~ "(" ~ repsep(valueType, ",") ~ ")" ^^ {
    case isConst ~ retType ~ _  ~ _ ~ fieldName ~ _ ~ _ ~ argTypes ~ _ =>
      ParsedCellField(fieldName, ParsedFunctionPointerType(retType, argTypes), isConst, None)
  }
}

class DefinitionParser extends CellDeclarationParser with CellDefinitionParser with FieldTypeAliasParser {
  def typeDefinitions: Parser[List[ParsedDefinition]] =
    rep1(typeDefinition)

  def typeDefinition = cellDeclaration | cellDefinition | fieldTypeAlias

  override protected val whiteSpace = """(\s|//.*\n|/\*(.|\n)*?\*/)+""".r
}

object DefinitionParser {
  def parseFile(input: File): List[ParsedDefinition] = {
    val inputString = Source.fromFile(input, "UTF-8").mkString

    parseString(inputString)
  }

  def parseString(input: String): List[ParsedDefinition] = {
    val parser = new DefinitionParser

    parser.parseAll(parser.typeDefinitions, input) match {
      case parser.Success(data, _) => data
      case err =>
        throw new ParseErrorException(err.toString)
    }
  }
}
