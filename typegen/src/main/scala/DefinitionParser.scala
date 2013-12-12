package io.llambda.typegen

import scala.util.parsing.combinator._
import java.io.File
import scala.io.Source
import collection.immutable.ListMap

class ParseErrorException(message : String) extends Exception(message)

trait CommonParsers extends RegexParsers {
  // Be very strict about identifiers for now
  private val identifierPattern = """[a-zA-Z][a-zA-Z0-9]+"""

  def identifier = identifierPattern.r

  // Types are identifiers plus zero or more pointer indirections
  def typeReference : Parser[String] = (identifierPattern + """\**""").r
}

trait UserDefinedFieldTypeParser extends CommonParsers {
  def fieldDefinition = positioned("fieldtype" ~> (identifier ~ typeInheritence ~ opt(fieldTypeBody)) <~ ";" ^^ {
    case typeName ~ inherits ~ cTypeNameOptOpt =>
      new ParsedUserDefinedFieldType(typeName, inherits, cTypeNameOptOpt getOrElse None)
  })

  def typeInheritence = ":" ~> fieldSuperType
  // Only allow function pointer types as the supertype of fields
  // This is because the function pointer syntax for C members is a bit nuts.
  // It's not worth implementing without a use case
  def fieldSuperType = functionPointerType | typeReference

  def functionPointerType = typeReference ~ "(*)" ~ "(" ~ repsep(typeReference, ",") ~ ")" ^^ {
    case retType ~ _  ~ _ ~ fieldTypes ~ _ =>
      // XXX: We don't do any special parsing of function pointer types yet
      val fieldTypeList = fieldTypes.mkString(", ")
      s"${retType} (*)(${fieldTypeList})"
  }

  def fieldTypeBody = "{" ~> opt(cTypeDef) <~ "}"
  def cTypeDef = opt("extern") ~ "ctype" ~ "=" ~ typeReference <~ ";" ^^ { case externOpt ~ _ ~ _ ~ name => 
    ParsedCType(name=name, externallyDefined=externOpt.isDefined)
  }
}

trait CellDefinitionParser extends CommonParsers {
  def cellDefinition = positioned(instanceType ~ internal ~ ("cell" ~> identifier) ~ opt(cellInheritence) ~ fields <~ ";" ^^ {
    case instanceType ~ internal ~ typeName ~ inheritsOpt ~ fields =>
      new ParsedCellClass(typeName, instanceType, inheritsOpt, fields, internal)
  })

  def instanceType = concreteSpecifier | abstractSpecifier | preconstructedSpecifier

  def concreteSpecifier = "concrete"             ^^^ CellClass.Concrete
  def abstractSpecifier = "abstract"             ^^^ CellClass.Abstract
  def preconstructedSpecifier = "preconstructed" ^^^ CellClass.Preconstructed

  def cellInheritence = ":" ~> identifier

  def internal = opt("internal") ^^ (_.isDefined)

  def fields = opt("{" ~> rep(field) <~ "}") ^^ { fieldsOpt =>
    fieldsOpt getOrElse Nil
  }

  def field = positioned((typeReference ~ identifier) <~ ";" ^^ { case typeName ~ fieldName => 
    new ParsedCellField(fieldName, typeName)
  })
}

class DefinitionParser extends RegexParsers  with CellDefinitionParser with UserDefinedFieldTypeParser {
  def typeDefinitions = rep1(typeDefinition)
  def typeDefinition = cellDefinition | fieldDefinition

  override protected val whiteSpace = """(\s|//.*\n)+""".r
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
