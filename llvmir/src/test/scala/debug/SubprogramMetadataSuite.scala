package io.llambda.llvmir.debug
import io.llambda

import org.scalatest.FunSuite

import llambda.llvmir._

class SubprogramMetadataSuite extends FunSuite {
  test("main function") {
    val fakeSourcePath = NumberedMetadata(1)
    val fakeContext = NumberedMetadata(5)
    val fakeTypeDescriptor = NumberedMetadata(6)

    val llvmFunction = GlobalVariable("main", PointerType(FunctionType(
      returnType=IntegerType(32),
      parameterTypes=List(IntegerType(32), PointerType(PointerType(IntegerType(8))))
    )))

    val subprogramNode = SubprogramMetadata(
      sourcePath=fakeSourcePath,
      contextDescriptor=fakeContext,
      name="main",
      displayName="mainDisplay",
      mipsLinkageName="",
      definitionLine=8,
      typeDescriptor=fakeTypeDescriptor,
      compileUnitLocal=false,
      definedInCompileUnit=true,
      virtuality=0,
      virtualIndex=0,
      baseTypeOpt=None,
      flags=256,
      optimised=true,
      llvmFunctionOpt=Some(llvmFunction),
      functionTemplateParametersOpt=None,
      functionDeclDescriptorOpt=None,
      functionVariables=Nil,
      scopeStartLine=9
    )

    assert(subprogramNode.toIr ===
      """!{i32 786478, !1, !5, !"main", !"mainDisplay", !"", i32 8, !6, i1 0, i1 1, i32 0, i32 0, null, i32 256, i1 1, i32 (i32, i8**)* @main, null, null, !{i32 0}, i32 9}"""
    )
  }

  test("completely inlined function") {
    val fakeSourcePath = NumberedMetadata(1)
    val fakeContext = NumberedMetadata(5)
    val fakeTypeDescriptor = NumberedMetadata(6)

    val subprogramNode = SubprogramMetadata(
      sourcePath=fakeSourcePath,
      contextDescriptor=fakeContext,
      name="inlineMe",
      displayName="inlineMe",
      mipsLinkageName="",
      definitionLine=3,
      typeDescriptor=fakeTypeDescriptor,
      compileUnitLocal=true,
      definedInCompileUnit=false,
      virtuality=0,
      virtualIndex=0,
      baseTypeOpt=None,
      flags=0,
      optimised=false,
      llvmFunctionOpt=None,
      functionTemplateParametersOpt=None,
      functionDeclDescriptorOpt=None,
      functionVariables=Nil,
      scopeStartLine=4
    )

    assert(subprogramNode.toIr ===
      """!{i32 786478, !1, !5, !"inlineMe", !"inlineMe", !"", i32 3, !6, i1 1, i1 0, i32 0, i32 0, null, i32 0, i1 0, null, null, null, !{i32 0}, i32 4}"""
    )
  }
}
