package io.llambda.typegen

import io.llambda.llvmir

object PredefinedFieldTypes {
  private def intTypes() : Map[String, PrimitiveFieldType] =
    (List(true, false) flatMap { signed =>
      List(8, 16, 32, 64) map { bits => 
        // Base our type name off of the C++ names
        val typeName = if (signed) {
          s"int${bits}"
        }
        else {
          s"uint${bits}"
        }

        val fieldType = PrimitiveFieldType(
          signed=Some(signed),
          llvmType=llvmir.IntegerType(bits),
          cppTypeName=s"std::${typeName}_t"
        )

        (typeName -> fieldType)
      }
    }).toMap

  def apply() : Map[String, PrimitiveFieldType] = 
    (
      intTypes() + 
      ("float" ->
        PrimitiveFieldType(
          signed=None,
          llvmType=llvmir.FloatType,
          cppTypeName="float"
        )
      ) +
      ("double" ->
        PrimitiveFieldType(
          signed=None,
          llvmType=llvmir.DoubleType,
          cppTypeName="double"
        )
      ) + 
      ("bool" ->
        PrimitiveFieldType(
          signed=Some(false),
          llvmType=llvmir.IntegerType(8),
          cppTypeName="bool"
        )
      ) +
      ("untypedptr" ->
        PrimitiveFieldType(
          signed=None,
          llvmType=llvmir.PointerType(llvmir.IntegerType(8)),
          cppTypeName="void*"
        )
      ) +
      ("World" ->
        PrimitiveFieldType(
          signed=None,
          llvmType=llvmir.UserDefinedType("world"),
          cppTypeName="World"
        )
      )
    )
}
