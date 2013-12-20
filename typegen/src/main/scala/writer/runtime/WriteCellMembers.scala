package io.llambda.typegen.writer.runtime

import io.llambda.typegen._

object WriteCellMembers extends writer.OutputWriter {
  private def cppTypePredicate(processedTypes : ProcessedTypes, cellClass : CellClass) : String = cellClass match {
    case _ : RootCellClass =>
      // We're always an instance of the root classes
      "true"

    case _ if cellClass.instanceType == CellClass.Abstract =>
      // Our type check is the union of the type checks for our child classes
      val allChildren = processedTypes.cellClasses.values.filter(_.parentOption == Some(cellClass))

      val childTypeChecks = allChildren map { childCellClass =>
        "(" + cppTypePredicate(processedTypes, childCellClass) + ")"
      }

      childTypeChecks.mkString(" || ")

    case _ =>
      val typeTagField = processedTypes.rootCellClass.typeTagField
      val typeTagEnumName = FieldTypeToCpp(typeTagField.fieldType, None)

      s"datum->${typeTagField.name}() == ${typeTagEnumName}::${cellClass.name}" 
  }


  private def writeMemberFile(processedTypes : ProcessedTypes, cellClass : CellClass) : String = {
    val cppName = cellClass.names.cppClassName
    val rootCellCppName = processedTypes.rootCellClass.names.cppClassName

    val cppBuilder = new CppBuilder

    cppBuilder.appendRaw(writer.GeneratedClikeFileComment)

    // Make accessors for each field
    if (!cellClass.fields.isEmpty) {
      cppBuilder += "public:"
      cppBuilder.indented {
        for((fieldName, field) <- cellClass.fields) {
          val cppReturnType = FieldTypeToCpp(field.fieldType, None)

          cppBuilder += s"${cppReturnType} ${fieldName}() const"
          cppBuilder.blockSep {
            cppBuilder += s"return m_${fieldName};"
          }
        }
      }
    }
    
    cppBuilder += "public:"
    cppBuilder.indented {
      // Make our type check
      cppBuilder += s"static bool isInstance(const ${rootCellCppName} *datum)"
      cppBuilder.blockSep {
        cppBuilder += "return " + cppTypePredicate(processedTypes, cellClass) + ";"
      }
  
      if (cellClass != processedTypes.rootCellClass) {
        // Make our type casters 
        for(constPrefix <- List("", "const ")) {
          cppBuilder += s"static ${constPrefix}${cppName}* fromDatum(${constPrefix}${rootCellCppName} *datum)"
          cppBuilder.blockSep {
            cppBuilder += "if (isInstance(datum))"
            cppBuilder.blockSep {
              cppBuilder += s"return static_cast<${constPrefix}${cppName}*>(datum);"
            }
          
            cppBuilder += s"return nullptr;"
          }
        }
      }
    }


    if (!cellClass.fields.isEmpty) {
      // Define each field member variable
      cppBuilder += "private:"
      cppBuilder.indented {
        for((fieldName, field) <- cellClass.fields) {
          cppBuilder += FieldTypeToCpp(field.fieldType, Some("m_" + fieldName)) + ";"
        }
      }
    }

    cppBuilder.toString
  }

  def apply(processedTypes : ProcessedTypes) : Map[String, String] = {
    (processedTypes.cellClasses.values.map { cellClass =>
      val cppName = cellClass.names.cppClassName
      val outputPath = s"runtime/binding/generated/${cppName}Members.h"

      (outputPath -> writeMemberFile(processedTypes, cellClass))
    }).toMap
  }
}
