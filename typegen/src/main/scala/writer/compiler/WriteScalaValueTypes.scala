package io.llambda.typegen.writer.compiler
import io.llambda.typegen._

object WriteScalaValueTypes extends writer.OutputWriter {
  private def allTaggedTreeNodes(processedTypes : ProcessedTypes, cellClass : TaggedCellClass) : List[TaggedCellClass] = {
    val childList = processedTypes.taggedCellClassesByParent.get(cellClass) .getOrElse(Nil)
    val childTreeNodes = childList.flatMap(allTaggedTreeNodes(processedTypes, _))

    cellClass :: childTreeNodes
  }

  def apply(processedTypes : ProcessedTypes) : Map[String, String] = {
    val scalaBuilder = new ScalaBuilder

    scalaBuilder.appendRaw(writer.GeneratedClikeFileComment)
    scalaBuilder += "package io.llambda.compiler.valuetype"
    scalaBuilder += "import io.llambda.compiler"
    scalaBuilder.sep()
    scalaBuilder += "import compiler.{celltype => ct}"
    scalaBuilder.sep()
    
    processedTypes.cellClasses.values collect {
      case cellClass : TaggedCellClass if cellClass.visibility.fromScheme =>
        val allNodes = allTaggedTreeNodes(processedTypes, cellClass)
        val concreteNodes = allNodes.filter(_.typeId.isDefined)
        val valueTypeName = cellClass.names.scalaValueTypeName

        concreteNodes match {
          case singleNode :: Nil =>
            val cellTypeName = "ct." + singleNode.names.scalaCellTypeName
            scalaBuilder += s"object ${valueTypeName} extends SchemeTypeAtom(${cellTypeName})"

          case multipleNodes =>
            val memberTypeNames = multipleNodes.map { singleNode =>
              "SchemeTypeAtom(ct." + singleNode.names.scalaCellTypeName + ")"
            }

            val unionType = "UnionType(Set(" + memberTypeNames.mkString(", ") + "))"
            
            scalaBuilder += s"object ${valueTypeName} extends ${unionType}"
        }
    }

    scalaBuilder.sep()
    
    scalaBuilder += "object IntrinsicSchemeTypes"
    scalaBuilder.block {
      scalaBuilder += "def apply() : Map[String, SchemeType] = Map("
      scalaBuilder.indented {
        val mapLines = processedTypes.cellClasses.values collect {
          case cellClass : TaggedCellClass if cellClass.visibility.fromScheme =>
            s"""(ct.${cellClass.names.scalaCellTypeName}.schemeName -> ${cellClass.names.scalaValueTypeName})"""
        }

        scalaBuilder.appendLines(mapLines, ",")
      }
      scalaBuilder += ")"
    }
    
    Map("compiler/src/main/scala/valuetype/generated/SchemeTypes.scala" -> scalaBuilder.toString)
  }
}
