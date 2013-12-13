package io.llambda.typegen

import io.llambda.llvmir

object PredefinedFieldTypes {
  private def intTypes() : Map[String, FieldType] =
    (List(true, false) flatMap { signed =>
      List(8, 16, 32, 64) map { bits => 
        // Base our type name off of the C++ names
        val typeName = if (signed) {
          s"int${bits}"
        }
        else {
          s"uint${bits}"
        }

        val fieldType = FieldType(
          signed=Some(signed),
          llvmType=llvmir.IntegerType(bits),
          cppTypeName=s"std::${typeName}_t",
          needsDefinition=false
        )

        (typeName -> fieldType)
      }
    }).toMap

  def apply() : Map[String, FieldType] = 
    intTypes() + 
    ("float" ->
      FieldType(
        signed=None,
        llvmType=llvmir.FloatType,
        cppTypeName="float",
        needsDefinition=false
      )
    ) +
    ("double" ->
      FieldType(
        signed=None,
        llvmType=llvmir.DoubleType,
        cppTypeName="double",
        needsDefinition=false
      )
    ) + 
    ("bool" ->
      FieldType(
        signed=Some(false),
        llvmType=llvmir.IntegerType(8),
        cppTypeName="bool",
        needsDefinition=false
      )
    ) +
    ("untypedptr" ->
      FieldType(
        signed=None,
        llvmType=llvmir.PointerType(llvmir.IntegerType(8)),
        cppTypeName="void*",
        needsDefinition=false
      )
    )
}
