package io.llambda.compiler.frontend
import io.llambda

import collection.mutable.ListBuffer
import collection.immutable.ListMap

import llambda.compiler.sst
import llambda.compiler.et
import llambda.compiler.codegen.CompactRepresentationForType
import llambda.compiler.{valuetype => vt}
import llambda.compiler.valuetype.{polymorphic => pm}
import llambda.compiler.{Primitives, BoundType, Scope, RecordTypeConstructor}
import llambda.compiler.BadSpecialFormException

private[frontend] object ParseRecordTypeDefine {
  private case class ParsedField(
    field : vt.RecordField,
    accessorSymbol : sst.ScopedSymbol,
    mutatorSymbol : Option[sst.ScopedSymbol]
  )

  private def parseFields(
      selfSymbol : sst.ScopedSymbol,
      isConstructor : Boolean,
      selfTypeVar : pm.TypeVar,
      polyTypeVars : List[(sst.ScopedSymbol, pm.TypeVar)],
      fieldData : List[sst.ScopedDatum],
      inheritedFieldNames : Set[String]
  )(implicit frontendConfig : FrontendConfig) : ListMap[String, ParsedField] = {
    val selfBindings = if (isConstructor) {
      // XXX: Support recursive polymorphic instances
      Nil
    }
    else {
      List(selfSymbol -> BoundType(selfTypeVar))
    }

    val polyBindings = polyTypeVars map { case (symbol, typeVar) =>
      symbol -> BoundType(typeVar)
    }

    val typeBindings = selfBindings ++ polyBindings
    val typeScopeMapping = Scope.mappingForBoundValues(typeBindings)

    // These are poisoned bindings used in mutable fields
    val mutablePoisonBindings = polyTypeVars map { case (symbol, typeVar) =>
      symbol -> BoundType(new pm.PoisonTypeVar("Polymorphic type variables cannot appear in mutable fields" ))
    }

    val mutableTypeBindings = List(selfSymbol -> BoundType(selfTypeVar)) ++ mutablePoisonBindings
    val mutableTypeScopeMapping = Scope.mappingForBoundValues(mutableTypeBindings)

    fieldData.foldLeft(ListMap[String, ParsedField]()) {
      case (parsedFields, fieldDatum @ sst.ScopedProperList(fieldDefDatum :: procedureData)) =>
        val isMutable = procedureData.length > 1

        // We can either be just a symbol and have no type or we can be a Scala/Racket style (symbol : <type>)
        // This is a compatible extension to R7RS
        val (fieldNameSymbol, fieldType) = fieldDefDatum match {
          case nameSymbol : sst.ScopedSymbol =>
            // Just a bare symbol - implicitly we're of type <any>
            (nameSymbol, vt.AnySchemeType)

          case sst.ScopedProperList(List(
              nameSymbol : sst.ScopedSymbol,
              sst.ResolvedSymbol(Primitives.AnnotateStorageLocType),
              fieldTypeDatum
          )) =>
            val rescopedType = if (isMutable) {
              fieldTypeDatum.rescoped(mutableTypeScopeMapping)
            }
            else {
              fieldTypeDatum.rescoped(typeScopeMapping)
            }

            // Resolve the field's Scheme type
            val schemeType = ExtractType.extractStableType(rescopedType)

            // Rewrite this to a compact native type
            val compactType = CompactRepresentationForType(schemeType)

            (nameSymbol, compactType)

          case other =>
            val message = s"Unrecognized record field name definition. Must be either identifier or [identifier : <type>]."
            throw new BadSpecialFormException(other, message)
        }

        val fieldName = fieldNameSymbol.name

        // Check for duplicate field identifiers
        if (inheritedFieldNames.contains(fieldName)) {
          throw new BadSpecialFormException(fieldNameSymbol, "Record field name duplicates inherited field name: " + fieldName)
        }

        if (parsedFields.contains(fieldName)) {
          throw new BadSpecialFormException(fieldNameSymbol, "Duplicate record field name: " + fieldName)
        }

        // Create the field
        val field = new vt.RecordField(fieldName, fieldType)

        // Determine which procedures this field defines
        val parsedField = procedureData match {
          case (accessorSymbol : sst.ScopedSymbol) :: Nil =>
            ParsedField(field, accessorSymbol, None)

          case (accessorSymbol : sst.ScopedSymbol) :: (mutatorSymbol : sst.ScopedSymbol) :: Nil =>
            ParsedField(field, accessorSymbol, Some(mutatorSymbol))

          case _ =>
            throw new BadSpecialFormException(fieldDatum, "Unrecognized record field procedure definition. One identifier for an accessor must be specified. Another may optionally provided for a mutator.")
        }

        parsedFields + (fieldName -> parsedField)

      case (_, other) =>
        throw new BadSpecialFormException(other, "Unrecognized record field definition")
    }
  }

  private def parseConstructor(
      recordType : vt.RecordType,
      constructorSymbol : sst.ScopedSymbol,
      constructorOperands : List[sst.ScopedDatum]
  ) : (sst.ScopedSymbol, et.RecordConstructor) = {
    // Get a list of all of our fields including inherited ones
    val nameToField = (recordType.fieldsWithInherited.map { field =>
      field.name -> field
    }).toMap

    val initializedFields = constructorOperands.foldLeft(List[vt.RecordField]()) {
      case (initialized, symbol @ sst.ScopedSymbol(_, fieldName)) =>
        // Find the field for this symbol
        val field = nameToField.getOrElse(symbol.name, {
          throw new BadSpecialFormException(symbol, "Unknown field name in constructor: " + fieldName)
        })

        // Make sure this hasn't already been initialized
        if (initialized.contains(field)) {
          throw new BadSpecialFormException(symbol, "Duplicate field name in constructor: " + fieldName)
        }

        initialized :+ field

      case (_, other) =>
        throw new BadSpecialFormException(other, "Record type constructor field names must be symbols")
    }

    val upperBound = recordType.upperBound

    for((fieldName, field) <- nameToField) {
      if (!initializedFields.contains(field)) {
        // Make sure this can be initialized to #!unit
        if (vt.SatisfiesType(upperBound.schemeTypeForField(field), vt.UnitType) != Some(true)) {
          throw new BadSpecialFormException(constructorSymbol, "Record field \"" + fieldName + "\" is not initialized in the constructor and its type has no default value")
        }
      }
    }

    (constructorSymbol -> et.RecordConstructor(recordType, initializedFields).assignLocationFrom(constructorSymbol))
  }

  private def parse(
      appliedSymbol : sst.ScopedSymbol,
      nameDatum : sst.ScopedDatum,
      parentDatumOpt : Option[sst.ScopedDatum],
      constructorSymbol : sst.ScopedSymbol,
      constructorOperands : List[sst.ScopedDatum],
      predicateSymbol : sst.ScopedSymbol,
      fieldData : List[sst.ScopedDatum]
  )(implicit frontendConfig : FrontendConfig) : ParsedRecordTypeDefine = {
    val parentInstanceOpt = parentDatumOpt map { parentDatum =>
      ExtractType.extractSchemeType(parentDatum) match {
        case parentInstance : vt.RecordTypeInstance =>
          parentInstance

        case _ =>
          throw new BadSpecialFormException(parentDatum, "Record types can only inherit from other record types")
      }
    }

    // Parse our fields first
    val parentFieldNames = parentInstanceOpt match {
      case Some(parentInstance) =>
        parentInstance.recordType.fields.map(_.name).toSet

      case _ =>
        Set[String]()
    }

    val (nameSymbol, polyTypeVars, isConstructor) = nameDatum match {
      case nameSymbol : sst.ScopedSymbol =>
        (nameSymbol, Nil, false)

      case sst.ScopedProperList((nameSymbol : sst.ScopedSymbol) :: typeVarData) =>
        val polyTypeVars = typeVarData map ExtractTypeVar
        (nameSymbol, polyTypeVars, true)

      case other =>
        val message = s"Unrecognized record name definition. Must be either identifier or (identifier type-vars ...)."
        throw new BadSpecialFormException(other, message)
    }

    // Introduce a type variable referencing the type being defined
    val selfTypeVar = new pm.TypeVar(nameSymbol.name)

    // Parse our fields
    val nameToParsedField = parseFields(nameSymbol, isConstructor, selfTypeVar, polyTypeVars, fieldData, parentFieldNames)

    // Build our record type
    val recordFields = nameToParsedField.values.toList.map(_.field)
    val recordType = new vt.RecordType(
      sourceName=nameSymbol.name,
      fields=recordFields,
      selfTypeVarOpt=Some(selfTypeVar),
      typeVars=polyTypeVars.map(_._2),
      parentRecordOpt=parentInstanceOpt
    )

    val upperBound = recordType.upperBound

    // Create our constructor and predicate procedures
    val constructorProcedure = parseConstructor(recordType, constructorSymbol, constructorOperands)
    val predicateProcedure = (predicateSymbol -> et.TypePredicate(upperBound).assignLocationFrom(predicateSymbol))

    // Collect all of our accessors and mutators
    val accessorProcedures = (nameToParsedField.values.map { parsedField =>
      (parsedField.accessorSymbol -> et.RecordAccessor(recordType, parsedField.field).assignLocationFrom(parsedField.accessorSymbol))
    }).toList

    val mutatorProcedures = (nameToParsedField.values.flatMap { parsedField =>
      parsedField.mutatorSymbol map { mutator =>
        (mutator -> et.RecordMutator(recordType, parsedField.field).assignLocationFrom(mutator))
      }
    }).toList

    // Check for duplicate accessor or mutator procedures
    // R7RS only specifies that accessor and mutators must be unique. We intentionally ignore the predicate and
    // constructor procedure here.
    (accessorProcedures ++ mutatorProcedures).map(_._1).foldLeft(Set[sst.ScopedSymbol]()) { (seenSymbols, procSymbol) =>
      if (seenSymbols.contains(procSymbol)) {
        throw new BadSpecialFormException(procSymbol, "Duplicate record type procedure")
      }

      seenSymbols + procSymbol
    }

    val allProcedures = constructorProcedure :: predicateProcedure :: (accessorProcedures ++ mutatorProcedures)

    val boundValue = if (isConstructor) {
      // We're a polymorphic record type constructor
      RecordTypeConstructor(recordType)
    }
    else {
      // We're a monomorphic instance
      BoundType(upperBound)
    }

    ParsedRecordTypeDefine(nameSymbol, boundValue, allProcedures.toMap)
  }

  def apply(
      appliedSymbol : sst.ScopedSymbol,
      operands : List[sst.ScopedDatum]
  )(implicit frontendConfig : FrontendConfig) : ParsedRecordTypeDefine = operands match {
    case (nameDatum : sst.ScopedDatum) ::
         sst.ScopedProperList((constructorSymbol : sst.ScopedSymbol) :: constructorOperands) ::
         (predicateSymbol : sst.ScopedSymbol) ::
         fieldData =>

      parse(appliedSymbol, nameDatum, None, constructorSymbol, constructorOperands, predicateSymbol, fieldData)

    case (nameDatum : sst.ScopedDatum) ::
         (parentDatum : sst.ScopedDatum) ::
         sst.ScopedProperList((constructorSymbol : sst.ScopedSymbol) :: constructorOperands) ::
         (predicateSymbol : sst.ScopedSymbol) ::
         fieldData =>

      parse(appliedSymbol, nameDatum, Some(parentDatum), constructorSymbol, constructorOperands, predicateSymbol, fieldData)

    case _ =>
      throw new BadSpecialFormException(appliedSymbol, "Unrecognized record type form")
  }
}
