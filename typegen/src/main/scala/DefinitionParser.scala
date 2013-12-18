package io.llambda.typegen

import scala.util.parsing.combinator._
import java.io.File
import scala.io.Source
import collection.immutable.ListMap
import scala.util.parsing.input.Positional

class ParseErrorException(message : String) extends Exception(message)

trait CommonParsers extends RegexParsers {
  // Be very strict about identifiers for now
  // We assume all names are camelCase when we rename them for different
  // output langauges
  def identifier = """[a-zA-Z][a-zA-Z0-9]+""".r

  // Types are identifiers plus zero or more pointer indirections
  def valueType = positioned(identifier ~ rep("*") ^^ { case typeName ~ indirections =>
    indirections.foldLeft(ParsedTypeName(typeName) : ParsedType) { case (innerType, _) =>
      ParsedPointerType(innerType)
    }
  })
  
  // Functions can return "void" in addition to an actual type
  def returnType = voidReturn | nonVoidReturn
  def voidReturn = "void" ^^^ None
  def nonVoidReturn = valueType ^^ (Some(_))
}

trait FieldTypeAliasParser extends CommonParsers {
  def fieldTypeAlias = positioned("fieldtype" ~> (identifier ~ typeInheritence ~ opt(fieldTypeBody)) <~ ";" ^^ {
    case typeName ~ inherits ~ cTypeNameOptOpt =>
      new ParsedFieldTypeAlias(typeName, inherits, cTypeNameOptOpt getOrElse None)
  })

  def typeInheritence = ":" ~> fieldSuperType
  def fieldSuperType = anonymousFunctionPointer | valueType

  def anonymousFunctionPointer = positioned(returnType ~ "(" ~ "*" ~ ")" ~ "(" ~ repsep(valueType, ",") ~ ")" ^^ {
    case retType ~ _  ~ _ ~ _ ~ _ ~ argTypes ~ _ =>
      ParsedFunctionPointerType(retType, argTypes)
  })

  def fieldTypeBody = "{" ~> opt(cppNameDef) <~ "}"
  
  def cppTypeName = """([a-zA-Z_][a-zA-Z0-9_]*::)*[a-zA-Z_][a-zA-Z0-9_]*""".r
  def cppNameDef = opt("extern") ~ "cppname" ~ "=" ~ cppTypeName <~ ";" ^^ { case externOpt ~ _ ~ _ ~ name => 
    val needsDefinition = !externOpt.isDefined
    ParsedCppType(name, needsDefinition)
  }
}

trait CellDeclarationParser extends CommonParsers {
  def cellDeclaration = positioned("cell" ~ identifier ~ ";" ^^ {
    case _ ~ typeName ~ _ =>
      new ParsedCellClassDeclaration(typeName)
  })
}

trait CellDefinitionParser extends CommonParsers {
  def cellDefinition = rootCellDefinition | childCellDefinition

  def rootCellDefinition = positioned("root" ~ internal ~ "cell" ~ identifier ~ "typetag" ~ identifier ~ fields ~ ";" ^^ {
    case _ ~ internal ~ _ ~ typeName ~ _ ~  typeTagField  ~ fields ~ _ =>
      new ParsedRootClassDefinition(typeName, typeTagField, fields, internal)
  })

  def childCellDefinition = positioned(instanceType ~ internal ~ ("cell" ~> identifier) ~ cellInheritence ~ fields <~ ";" ^^ {
    case instanceType ~ internal ~ typeName ~ inherits ~ fields =>
      new ParsedChildClassDefinition(typeName, instanceType, inherits, fields, internal)
  })

  def instanceType = concreteSpecifier | abstractSpecifier | preconstructedSpecifier

  def concreteSpecifier = "concrete"             ^^^ CellClass.Concrete
  def abstractSpecifier = "abstract"             ^^^ CellClass.Abstract
  def preconstructedSpecifier = "preconstructed" ^^^ CellClass.Preconstructed

  // Cells can only inherit from other cells, not field types
  def cellInheritence = ":" ~> identifier

  def internal = opt("internal") ^^ (_.isDefined)

  def fields = "{" ~> rep(field) <~ "}"

  def field = positioned(functionPointerField | valueField)

  def valueField = (valueType ~ identifier) <~ ";" ^^ { case fieldType ~ fieldName => 
    new ParsedCellField(fieldName, fieldType)
  }
  
  def functionPointerField = returnType ~ "(" ~ "*" ~ identifier ~ ")" ~ "(" ~ repsep(valueType, ",") ~ ")" <~ ";" ^^ {
    case retType ~ _  ~ _ ~ fieldName ~ _ ~ _ ~ argTypes ~ _ =>
      new ParsedCellField(fieldName, ParsedFunctionPointerType(retType, argTypes))
  }
}

class DefinitionParser extends CellDeclarationParser with CellDefinitionParser with FieldTypeAliasParser {
  def typeDefinitions : Parser[List[ParsedDefinition]] =
      rep1(typeDefinition)

  def typeDefinition = cellDeclaration | cellDefinition | fieldTypeAlias

  override protected val whiteSpace = """(\s|//.*\n|/\*(.|\n)*?\*/)+""".r
}

object DefinitionParser {
  def parseFile(input : File) : List[ParsedDefinition] = {
    val filename = input.getAbsolutePath 
    val inputString = Source.fromFile(input, "UTF-8").mkString

    parseString(inputString)
  }

  def parseString(input : String) : List[ParsedDefinition] = {
    val parser = new DefinitionParser

    parser.parseAll(parser.typeDefinitions, input) match {
      case parser.Success(data, _) => data
      case err =>
        throw new ParseErrorException(err.toString)
    }
  }
}
