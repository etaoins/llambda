package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler._
import llambda.compiler.{valuetype => vt}

object ExtractPatternMatch {
  /** Scheme type for the a match clause's lambda
    *
    * This takes the value to match and returns the result of the match expression
    */
  private val clauseLambdaType = vt.ProcedureType(
    mandatoryArgTypes=List(vt.AnySchemeType),
    optionalArgTypes=Nil,
    restArgMemberTypeOpt=None,
    returnType=vt.ReturnType.Reachable(vt.AnySchemeType)
  )

  /** Loads a storage location from a Scheme library
    *
    * This will throw an exception if the name is unbound or bound to a non-variable
    *
    * @param  locName      Name of the storage location to load
    * @param  libraryName  Name of the library to load the storage location from
    */
  private def expectedVarLoc(
      locName: String,
      libraryName: Seq[String]= List("scheme", "base")
  )(implicit context: FrontendContext): StorageLocation = {
    val bindings = context.libraryLoader.load(libraryName, NoSourceLocation)(context.config)

    bindings(locName) match {
      case storageLoc: StorageLocation => storageLoc
      case _ =>
        throw new InternalCompilerErrorException(s"{${locName}) expected to be a variable")
    }
  }

  private def parseRecordUnapplication(
      located: SourceLocated,
      valueLoc: StorageLocation,
      unappliedValue: BoundValue,
      constructor: et.RecordConstructor,
      constructorArgs: List[sst.ScopedDatum],
      successExpr: (List[(sst.Symbol, StorageLocation)]) => et.Expr,
      failExpr: et.Expr,
      bindings: List[(sst.Symbol, StorageLocation)]
  )(implicit context: FrontendContext): et.Expr = {
    val recordType = constructor.recordType
    val recordFields = constructor.initializedFields

    if (recordFields.length != constructorArgs.length) {
      throw new BadSpecialFormException(located, s"Unable to unapply record constructor expecting exactly ${recordFields.length} arguments with ${constructorArgs.length} arguments")
    }

    val isRecordTypeExpr = et.Apply(et.TypePredicate(recordType), List(et.VarRef(valueLoc)))

    val checkFieldsExpr =
      recordFields.zip(constructorArgs).foldRight(successExpr) { case ((recordField, patternDatum), successExpr) =>
        val fieldAccessor = et.RecordAccessor(recordType, recordField)
        val fieldExpr = et.Apply(fieldAccessor, List(et.VarRef(valueLoc)))
        val fieldLoc = new StorageLocation("<record-match-value>")

        (bindings: List[(sst.Symbol, StorageLocation)]) =>
          et.InternalDefine(
            bindings=List(et.Binding(fieldLoc, fieldExpr)),
            body=parsePattern(fieldLoc, patternDatum, successExpr, failExpr, bindings)
          )
      }

      et.Cond(
        isRecordTypeExpr,
        checkFieldsExpr(bindings),
        failExpr
      )
  }

  private def parseUnapplication(
      located: SourceLocated,
      valueLoc: StorageLocation,
      unappliedValue: BoundValue,
      unappliedArgs: List[sst.ScopedDatum],
      successExpr: (List[(sst.Symbol, StorageLocation)]) => et.Expr,
      failExpr: et.Expr,
      bindings: List[(sst.Symbol, StorageLocation)]
  )(implicit context: FrontendContext): et.Expr = {
    val consProcLoc = expectedVarLoc("cons")
    val vectorProcLoc = expectedVarLoc("vector")
    val listProcLoc = expectedVarLoc("list")

    def deconstructPair(block: (StorageLocation, StorageLocation)  => et.Expr): et.Expr = {
      // Determine if the value is a pair
      val pairPredProcLoc = expectedVarLoc("pair?")
      val isPairExpr = et.Apply(et.VarRef(pairPredProcLoc), List(et.VarRef(valueLoc)))

      val carProcLoc = expectedVarLoc("car")
      val cdrProcLoc = expectedVarLoc("cdr")

      val carExpr = et.Apply(et.VarRef(carProcLoc), List(et.VarRef(valueLoc)))
      val cdrExpr = et.Apply(et.VarRef(cdrProcLoc), List(et.VarRef(valueLoc)))

      val carLoc = new StorageLocation("<car-match-value>")
      val cdrLoc = new StorageLocation("<cdr-match-value>")

      val testBindings = List(
        et.Binding(carLoc, carExpr),
        et.Binding(cdrLoc, cdrExpr)
      )

      et.Cond(
        isPairExpr,
        et.InternalDefine(
          testBindings,
          block(carLoc, cdrLoc)
        ),
        failExpr
      )
    }

    (unappliedValue, unappliedArgs) match {
      case ((recordCons: BoundRecordConstructor), constructorArgs) =>
        parseRecordUnapplication(
          located=located,
          valueLoc=valueLoc,
          unappliedValue=unappliedValue,
          constructor=recordCons.constructor,
          constructorArgs=unappliedArgs,
          successExpr=successExpr,
          failExpr=failExpr,
          bindings=bindings
        )

      case (Primitives.Quote, List(quotedDatum)) =>
        val equalLoc = expectedVarLoc("equal?")
        val isEqualExpr = et.Apply(et.VarRef(equalLoc), List(et.VarRef(valueLoc), et.Literal(quotedDatum.unscope)))

        et.Cond(isEqualExpr, successExpr(bindings), failExpr)

      case (`consProcLoc`, List(carPattern, cdrPattern)) =>
        deconstructPair((carLoc: StorageLocation, cdrLoc: StorageLocation) => {
          parsePattern(carLoc, carPattern, (bindings: List[(sst.Symbol, StorageLocation)]) => {
            parsePattern(cdrLoc, cdrPattern, successExpr, failExpr, bindings)
          }, failExpr, bindings)
        })

      case (`vectorProcLoc`, vectorPatternData) =>
        val equalLoc = expectedVarLoc("equal?")
        val vectorPredProcLoc = expectedVarLoc("vector?")
        val vectorLengthProcLoc = expectedVarLoc("vector-length")
        val vectorRefProcLoc = expectedVarLoc("vector-ref")

        val isVectorExpr = et.Apply(et.VarRef(vectorProcLoc), List(et.VarRef(valueLoc)))
        val vectorLengthExpr = et.Apply(et.VarRef(vectorLengthProcLoc), List(et.VarRef(valueLoc)))
        val expectedLengthExpr = et.Literal(ast.Integer(vectorPatternData.length))

        val hasExpectedLengthExpr = et.Apply(et.VarRef(equalLoc), List(vectorLengthExpr, expectedLengthExpr))

        val checkElementsExpr =
          vectorPatternData.zipWithIndex.foldRight(successExpr) { case ((patternDatum, index), successExpr) =>
            val vectorIndexExpr = et.Literal(ast.Integer(index))
            val elementExpr = et.Apply(et.VarRef(vectorRefProcLoc), List(et.VarRef(valueLoc), vectorIndexExpr))
            val elementLoc = new StorageLocation("<vector-match-value>")

            (bindings: List[(sst.Symbol, StorageLocation)]) =>
              et.InternalDefine(
                bindings=List(et.Binding(elementLoc, elementExpr)),
                body=parsePattern(elementLoc, patternDatum, successExpr, failExpr, bindings)
              )
          }

        et.Cond(
          isVectorExpr,
          et.Cond(
            hasExpectedLengthExpr,
            checkElementsExpr(bindings),
            failExpr
          ),
          failExpr
        )

      case (`listProcLoc`, Nil) =>
        val nullLoc = expectedVarLoc("null?")
        val isNullExpr = et.Apply(et.VarRef(nullLoc), List(et.VarRef(valueLoc)))

        et.Cond(
          isNullExpr,
          successExpr(bindings),
          failExpr
        )

      case (`listProcLoc`, nextPatternDatum :: restPatternData) =>
        deconstructPair((carLoc: StorageLocation, cdrLoc: StorageLocation) => {
          parsePattern(carLoc, nextPatternDatum, (bindings: List[(sst.Symbol, StorageLocation)]) => {
            parseUnapplication(located, cdrLoc, listProcLoc, restPatternData, successExpr, failExpr, bindings)
          }, failExpr, bindings)
        })

      case _ =>
        throw new BadSpecialFormException(located, s"Unsupported unapplication; (list), (cons), (vector) or record type constructor expected")
    }
  }

  private def parsePattern(
      valueLoc: StorageLocation,
      patternDatum: sst.ScopedDatum,
      successExpr: (List[(sst.Symbol, StorageLocation)]) => et.Expr,
      failExpr: et.Expr,
      bindings: List[(sst.Symbol, StorageLocation)]
  )(implicit context: FrontendContext): et.Expr = {
    def testForEquality(datum: ast.Datum): et.Expr = {
      val equalLoc = expectedVarLoc("equal?")
      val isEqualExpr = et.Apply(et.VarRef(equalLoc), List(et.VarRef(valueLoc), et.Literal(datum)))

      et.Cond(isEqualExpr, successExpr(bindings), failExpr)
    }

    patternDatum match {
      case sst.ProperList(List(
        varSymbol: sst.Symbol,
        storageLocTypeAnn: sst.Symbol,
        expectedTypeDatum: sst.ScopedDatum
      )) if storageLocTypeAnn.resolveOpt == Some(Primitives.AnnotateStorageLocType) =>
        val expectedType = ExtractType.extractSchemeType(expectedTypeDatum)
        val isTypeExpr = et.Apply(et.TypePredicate(expectedType), List(et.VarRef(valueLoc)))

        et.Cond(
          isTypeExpr,
          parsePattern(valueLoc, varSymbol, successExpr, failExpr, bindings),
          failExpr
        )

      case sst.ProperList((unappliedSym: sst.Symbol) :: unappliedArgs) =>
        val unappliedValue = unappliedSym.resolve
        parseUnapplication(unappliedSym, valueLoc, unappliedValue, unappliedArgs, successExpr, failExpr, bindings)

      case sst.NonSymbolLeaf(datum) =>
        testForEquality(datum)

      case scopedVector: sst.Vector =>
        testForEquality(scopedVector.unscope)

      case symbol: sst.Symbol =>
        if (symbol.resolveOpt == Some(Primitives.Wildcard)) {
          successExpr(bindings)
        }
        else {
          successExpr((symbol -> valueLoc) :: bindings)
        }

      case other =>
        throw new BadSpecialFormException(other, "Invalid pattern syntax")
    }
  }

  /** Returns the match failure clause
    *
    * This signals a match-error with the match value as evidence
    */
  private def matchErrorClause(implicit context: FrontendContext): et.Lambda = {
    val errorProcLoc = expectedVarLoc("raise-match-error", List("llambda", "error"))
    val evidenceLoc = new StorageLocation("<evidence>")

    val bodyExpr = et.Apply(
      et.VarRef(errorProcLoc),
      List(et.Literal(ast.String("Match error")), et.VarRef(evidenceLoc))
    )

    et.Lambda(
      polyType=clauseLambdaType.toPolymorphic,
      mandatoryArgs=List(evidenceLoc),
      optionalArgs=Nil,
      restArgOpt=None,
      body=bodyExpr
    )
  }

  /** Extracts a pattern matching expression
    *
    * The pattern matching expression is converted in to a series of nested lambdas representing the match clauses. IF
    * a clause fails it will invoke the next clause which is bound within its lambda body. If no clauses match then
    * a match-error will be signalled.
    *
    * @param  valueExpr   Value being pattern matched
    * @param  clauseData  Definition of the match clauses
    */
  def apply(
      valueExpr: et.Expr,
      clauseData: List[sst.ScopedDatum]
  )(implicit context: FrontendContext): et.Expr = {
    val firstClauseExpr = clauseData.foldRight(matchErrorClause) {
      case (sst.ProperList(patternDatum :: bodyData), nextClauseExpr) =>
        val successExpr = (bindings: List[(sst.Symbol, StorageLocation)]) =>
          ExtractBodyDefinition(bindings, bodyData)

        val innerValLoc = new StorageLocation("<match-value>")

        val nextClauseLoc = new StorageLocation("<next-clause>")
        val nextClauseBindings = List(et.Binding(nextClauseLoc, nextClauseExpr))

        val failExpr = et.Apply(
          et.VarRef(nextClauseLoc),
          List(et.VarRef(innerValLoc))
        )

        val clauseBodyExpr = et.InternalDefine(
          nextClauseBindings,
          parsePattern(innerValLoc, patternDatum, successExpr, failExpr, Nil)
        )

        et.Lambda(clauseLambdaType.toPolymorphic, List(innerValLoc), Nil, None, clauseBodyExpr)

      case (_, otherClause) =>
        throw new BadSpecialFormException(otherClause, "Invalid match clause syntax; (pattern body ...) expected")
    }

    et.Apply(firstClauseExpr, List(valueExpr))
  }
}
