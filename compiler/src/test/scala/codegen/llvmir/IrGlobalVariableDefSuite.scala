package llambda.codegen.llvmir

import org.scalatest.FunSuite

class IrGlobalVariableDefSuite extends FunSuite {
  test("trivial def") {
    val globalVarDef = IrGlobalVariableDef(
      name="twelve",
      initializer=IntegerConstant(IntegerType(32), 12)
    )

    assert(globalVarDef.variable.irType === PointerType(IntegerType(32)))
    assert(globalVarDef.toIr === "@twelve = i32 12")
  }
  
  test("constant def") {
    val globalVarDef = IrGlobalVariableDef(
      name="twelveFloat",
      initializer=SingleConstant(12.0f),
      constant=true
    )

    assert(globalVarDef.variable.irType === PointerType(SingleType))
    assert(globalVarDef.toIr === "@twelveFloat = constant float 12.0")
  }
  
  test("unnamed addr def") {
    val arrayValues = List(1, 0, 1).map { innerValue =>
      IntegerConstant(IntegerType(1), innerValue)
    }

    val globalVarDef = IrGlobalVariableDef(
      name="array",
      initializer=ArrayConstant(IntegerType(1), arrayValues),
      unnamedAddr=true
    )

    assert(globalVarDef.variable.irType === PointerType(ArrayType(3, IntegerType(1))))
    assert(globalVarDef.toIr === "@array = unnamed_addr [3 x i1] [i1 1, i1 0, i1 1]")
  }
  
  test("externally available def") {
    val globalVarDef = IrGlobalVariableDef(
      name="twelveDouble",
      initializer=DoubleConstant(12.0),
      linkage=Linkage.ExternallyAvailable
    )

    assert(globalVarDef.variable.irType === PointerType(DoubleType))
    assert(globalVarDef.toIr === "@twelveDouble = externally_available double 12.0")
  }
  
  test("hidden def") {
    val globalVarDef = IrGlobalVariableDef(
      name="twelveString",
      initializer=StringConstant("twelve"),
      visibility=Visibility.Hidden
    )

    assert(globalVarDef.variable.irType === PointerType(ArrayType(7, IntegerType(8))))
    assert(globalVarDef.toIr === "@twelveString = hidden [7 x i8] c\"twelve\\00\"")
  }
  
  test("christmas tree def") {
    val globalVarDef = IrGlobalVariableDef(
      name="twelveLong",
      initializer=IntegerConstant(IntegerType(64), 12),
      visibility=Visibility.Protected,
      linkage=Linkage.Private,
      unnamedAddr=true,
      constant=true
    )

    assert(globalVarDef.variable.irType === PointerType(IntegerType(64)))
    assert(globalVarDef.toIr === "@twelveLong = private protected unnamed_addr constant i64 12")
  }
}
