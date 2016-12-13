package io.llambda.typegen

import io.llambda.llvmir
import annotation.unchecked

object LlvmTypeToScalaConstructor {
  def apply(llvmType: llvmir.IrType): String = {
    // This is intentionally limited to the LLVM types we can generate
    // If you add an LLVM type you might get a MatchError here until you add
    // a case for it here
    (llvmType: @unchecked) match {
      case llvmir.IntegerType(bits) =>
        s"IntegerType(${bits})"
      case llvmir.DoubleType =>
        "DoubleType"
      case llvmir.FloatType =>
        "FloatType"
      case llvmir.PointerType(pointeeType) =>
        "PointerType(" + apply(pointeeType) + ")"
      case llvmir.VoidType =>
        "VoidType"
      case llvmir.ArrayType(elements, innerType) =>
        s"ArrayType(${elements}, " + apply(innerType) + ")"
      case llvmir.UserDefinedType(name) =>
        val quotedName = (name
          .replaceAllLiterally("""\""", """\\""")
          .replaceAllLiterally("\"", "\\\""))

        s"""UserDefinedType("${quotedName}")"""
      case llvmir.FunctionType(retType, args, hasVararg) =>
        "FunctionType(" +
          apply(retType) + ", " +
          "List(" + args.map(apply).mkString(", ") + "), " +
          (if (hasVararg) { "true" } else { "false" }) +
        ")"
    }
  }
}
