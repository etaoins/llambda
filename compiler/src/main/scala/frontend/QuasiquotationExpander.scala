package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler._

// This handles quasiquotion based on a few premises:
//
// - Only (scheme base) procedures can be used. It would be nice to have both a (lists->vector) and a version of (list)
//   that can return a constant list. However, it seems overkill to micro-optimize quasiquotion by introducing custom 
//   runtime procedures at this point
//
// - Quasiquoted lists and vectors without unquoting should be represented "for free" as literals
// 
// - The number of procedure applications should be minimized
// 
// To implement this the expander first does a pass to build "segments". A segment is either completely constant
// (ConstantSegment), composed of expressions (DynamicSegment) or a procedure returning a list (SplicedSegment, used
// to implement splicing). The only thing that can create a new segment is splicing which will terminate the previous
// segment and start building a new one.
//
// If an unquoted expression is encountered while building a ConstantSegment it will be converted to a DynamicSegment
// where the previous constants are represented with et.Literal. If a quoted datum is encountered while building a
// DynamicSegment it will be added directly as an et.Literal
//
// After building the segments they are converted to the target collection type using the abstract helper methods
// createConstantLiteral, createFromList and createFromElements. These are implemented in the concrete subclasses
// ListQuasiquotationExpander and VectorQuasiquotationExpander. 
//
// There are a number of special cases that try to minimize the number of procedure applications required to expand the
// quasiquotation. The fallback case simply converts every segment to a list, appends them to a final list, and then
// calls createFromList
//
abstract class QuasiquotationExpander(extractExpr : sst.ScopedDatum => et.Expr, schemeBase : Map[String, BoundValue]) {
  protected sealed abstract class QuasiquoteSegment
  protected case class ConstantSegment(data : List[ast.Datum]) extends QuasiquoteSegment
  protected case class DynamicSegment(exprs : List[et.Expr]) extends QuasiquoteSegment
  protected case class SplicedSegment(generator : et.Expr) extends QuasiquoteSegment
    
  protected def schemeBaseProcedure(name : String) = schemeBase(name) match {
    case storageLoc : StorageLocation => et.VarRef(storageLoc)
    case _ =>
      throw new InternalCompilerErrorException("Procedure name passed that does not correspond to a storage location") 
  }

  protected val listProc = schemeBaseProcedure("list")
  protected val listAppendProc = schemeBaseProcedure("append")
  
  // Makes a constant literal 
  protected def createConstantLiteral(elements : List[ast.Datum]) : ast.Datum

  // Makes a value from a single list expression
  // This is used to convert a splicing unquote to the right type of collection
  protected def createFromList(list : et.Expr) : et.Expr

  // Makes a value from a list of expressions
  protected def createFromElements(elements : List[et.Expr]) : et.Expr
  
  private def buildQuasiquotationSegments(data : List[sst.ScopedDatum]) : List[QuasiquoteSegment] = data match {
    case Nil => Nil

    case head :: rest =>
      val restSegments = buildQuasiquotationSegments(rest)
  
      head match {
        case sst.ScopedProperList((unquote : sst.ScopedSymbol) :: unquotedDatum :: Nil)
            if unquote.resolveOpt == Some(PrimitiveExprs.Unquote) =>

          val appendingExpr = extractExpr(unquotedDatum)

          restSegments match {
            case ConstantSegment(data) :: restTail =>
              // Switch to a dynamic segment and append us
              DynamicSegment(appendingExpr :: data.map(et.Literal(_))) :: restTail

            case DynamicSegment(exprs) :: restTail =>
              // Append us on to the dynamic segment
              DynamicSegment(appendingExpr :: exprs) :: restTail

            case other =>
              // Create a new segment
              DynamicSegment(appendingExpr :: Nil) :: other
          }
          
        case sst.ScopedProperList((unquoteSplicing : sst.ScopedSymbol) :: unquotedDatum :: Nil)
            if unquoteSplicing.resolveOpt == Some(PrimitiveExprs.UnquoteSplicing) =>
          // This is always a new segement
          SplicedSegment(extractExpr(unquotedDatum)) :: restSegments

        case quotedDatum  =>
          restSegments match {
            case ConstantSegment(data) :: restTail =>
              // Append us to the constant segment
              ConstantSegment(quotedDatum.unscope :: data) :: restTail

            case DynamicSegment(exprs) :: restTail =>
              // Append us on to the dynamic segment as an expression
              DynamicSegment(et.Literal(quotedDatum.unscope) :: exprs) :: restTail

            case other =>
              // Create a new segment
              ConstantSegment(quotedDatum.unscope :: Nil) :: other
          }
      }
  }

  def apply(data : List[sst.ScopedDatum]) : et.Expr = {
    // Plan out our segments
    val segments = buildQuasiquotationSegments(data)

    segments match {
      case Nil =>
        et.Literal(createConstantLiteral(Nil))

      case ConstantSegment(data) :: Nil =>
        // Single constant segment - build it as-is in place 
        et.Literal(createConstantLiteral(data))

      case DynamicSegment(exprs) :: Nil =>
        // Directly create from the expressions
        // This lets us use (vector) instead of (list->vector (list))
        createFromElements(exprs)

      case SplicedSegment(generator) :: Nil =>
        // Call the generator and convert to the target type
        createFromList(generator)

      case multisegments =>
        // Convert the segments to lists, append them and then convert to the
        // target collection
        createFromList(
          et.Apply(listAppendProc, multisegments.map {
            case ConstantSegment(data) => et.Literal(ast.ProperList(data))
            case DynamicSegment(exprs) => et.Apply(listProc, exprs)
            case SplicedSegment(generator) => generator
          })
        )
    }
  }
}

class ListQuasiquotationExpander(extractExpr : sst.ScopedDatum => et.Expr, schemeBase : Map[String, BoundValue]) extends QuasiquotationExpander(extractExpr, schemeBase) {
  protected def createConstantLiteral(elements : List[ast.Datum]) : ast.Datum = 
    ast.ProperList(elements)
  
  protected def createFromList(list : et.Expr) : et.Expr =
    // Return the list direct
    list
  
  protected def createFromElements(elements : List[et.Expr]) : et.Expr =
    et.Apply(listProc, elements)
}

class VectorQuasiquotationExpander(extractExpr : sst.ScopedDatum => et.Expr, schemeBase : Map[String, BoundValue]) extends QuasiquotationExpander(extractExpr, schemeBase) {
  private val listToVectorProc = schemeBaseProcedure("list->vector")
  private val vectorProc = schemeBaseProcedure("vector")

  protected def createConstantLiteral(elements : List[ast.Datum]) : ast.Datum = 
    ast.VectorLiteral(elements.toVector)
  
  protected def createFromList(list : et.Expr) : et.Expr =
    // Use (list->vector)
    et.Apply(listToVectorProc, List(list))
  
  protected def createFromElements(elements : List[et.Expr]) : et.Expr =
    et.Apply(vectorProc, elements)
}
