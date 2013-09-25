package llambda.codegen

import llambda.{ast,nfi}

object DatumToNfiConstant {
  def apply(datum : ast.Datum, requiredType : nfi.NativeType) : Option[llvmir.IrConstant] = (datum, requiredType) match {
    // Only true is truthy
    case (ast.TrueLiteral, nfi.Bool) =>
      Some(llvmir.IntegerConstant(llvmir.IntegerType(1), 1))

    // Everything else is falsey
    case (_, nfi.Bool) =>
      Some(llvmir.IntegerConstant(llvmir.IntegerType(1), 0))

    case (ast.IntegerLiteral(value), intType : nfi.IntType) =>
      Some(llvmir.IntegerConstant(llvmir.IntegerType(intType.bits), value))
    
    case (ast.IntegerLiteral(value), nfi.Float) =>
      Some(llvmir.SingleConstant(value.toFloat))

    case (ast.RealLiteral(value), nfi.Float) =>
      Some(llvmir.SingleConstant(value.toFloat))
    
    case (ast.IntegerLiteral(value), nfi.Double) =>
      Some(llvmir.DoubleConstant(value.toDouble))

    case (ast.RealLiteral(value), nfi.Double) =>
      Some(llvmir.DoubleConstant(value))

    case (ast.StringLiteral(string), nfi.Utf8CString) =>
      Some(llvmir.StringConstant(string))
    
    case (ast.CharLiteral(codePoint), nfi.UnicodeChar) =>
      Some(llvmir.IntegerConstant(llvmir.IntegerType(32), codePoint))

    case _ =>
      None
  }
}
