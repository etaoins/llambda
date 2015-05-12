package io.llambda.compiler.frontend
import io.llambda

import collection.immutable.ListMap

import llambda.compiler.sst
import llambda.compiler.et
import llambda.compiler.codegen.CompactRepresentationForType
import llambda.compiler.{valuetype => vt}
import llambda.compiler.valuetype.{polymorphic => pm}
import llambda.compiler.{Primitives, BoundType, Scope, SourceLocated}
import llambda.compiler.BadSpecialFormException

private[frontend] object ParseRecordTypeDefine {
  /** Parsed record type definition
    *
    * This is the result of parsing a (define-record-type)
    *
    * @param  typeSymbol   Symbol for the newly introduced record type
    * @param  recordType   The new record type
    * @param  constructor  Constructor procedure for the record type
    * @param  procedures   The associated procedures for the record type
    */
  case class Result(
      typeSymbol : sst.ScopedSymbol,
      recordType : vt.RecordType,
      constructor : (sst.ScopedSymbol, et.RecordConstructor),
      procedures : Map[sst.ScopedSymbol, et.ArtificialProcedure]
  )

  private case class ParsedField(
    field : vt.RecordField,
    accessorSymbol : sst.ScopedSymbol,
    mutatorSymbol : Option[sst.ScopedSymbol]
  )

  private def parseFields(
      selfSymbol : sst.ScopedSymbol,
      selfTypeVar : pm.TypeVar,
      fieldData : List[sst.ScopedDatum],
      inheritedFieldNames : Set[String]
  )(implicit frontendConfig : FrontendConfig) : ListMap[String, ParsedField] = {
    val typeBindings = List(selfSymbol -> BoundType(selfTypeVar))
    val typeScopeMapping = Scope.mappingForBoundValues(typeBindings)

    fieldData.foldLeft(ListMap[String, ParsedField]()) {
      case (parsedFields, fieldDatum @ sst.ScopedProperList(fieldDefDatum :: procedureData)) =>
        // We can either be just a symbol and have no type or we can be a Scala/Racket style [symbol : <type>]
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
            // Resolve the field's Scheme type
            val schemeType = ExtractType.extractStableType(fieldTypeDatum.rescoped(typeScopeMapping))

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

        // Determine which procedures this field defines
        val (accessorSymbol, mutatorSymbolOpt) = procedureData match {
          case List((accessorSymbol : sst.ScopedSymbol)) =>
            (accessorSymbol, None)

          case List((accessorSymbol : sst.ScopedSymbol), (mutatorSymbol : sst.ScopedSymbol)) =>
            (accessorSymbol, Some(mutatorSymbol))

          case _ =>
            throw new BadSpecialFormException(fieldDatum, "Unrecognized record field procedure definition. One identifier for an accessor must be specified. Another may optionally provided for a mutator.")
        }

        // Create the field
        val field = new vt.RecordField(fieldName, fieldType, mutable=mutatorSymbolOpt.isDefined)

        // Parse the field
        val parsedField = ParsedField(field, accessorSymbol, mutatorSymbolOpt)

        parsedFields + (fieldName -> parsedField)

      case (_, other) =>
        throw new BadSpecialFormException(other, "Unrecognized record field definition")
    }
  }

  private def parseConstructor(
      recordType : vt.RecordType,
      constructorSymbol : sst.ScopedSymbol,
      constructorArgs : List[sst.ScopedDatum]
  ) : (sst.ScopedSymbol, et.RecordConstructor) = {
    // Get a list of all of our fields including inherited ones
    val nameToField = (recordType.fieldsWithInherited.map { field =>
      field.name -> field
    }).toMap

    val initializedFields = constructorArgs.foldLeft(List[vt.RecordField]()) {
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

    for((fieldName, field) <- nameToField) {
      if (!initializedFields.contains(field)) {
        // Make sure this can be initialized to #!unit
        recordType.typeForField(field) match {
          case schemeType : vt.SchemeType if vt.SatisfiesType(schemeType, vt.UnitType).get =>
            // This is okay

          case _ =>
            throw new BadSpecialFormException(constructorSymbol, "Record field \"" + fieldName + "\" is not initialized in the constructor and its type has no default value")
        }
      }
    }

    (constructorSymbol -> et.RecordConstructor(recordType, initializedFields).assignLocationFrom(constructorSymbol))
  }

  private def parse(
      nameSymbol : sst.ScopedSymbol,
      parentSymbolOpt : Option[sst.ScopedSymbol],
      constructorSymbol : sst.ScopedSymbol,
      constructorArgs : List[sst.ScopedDatum],
      predicateSymbol : sst.ScopedSymbol,
      fieldData : List[sst.ScopedDatum]
  )(implicit frontendConfig : FrontendConfig) : Result = {
    val parentRecordOpt = parentSymbolOpt map { parentSymbol =>
      ExtractType.extractSchemeType(parentSymbol) match {
        case parentRecord : vt.RecordType =>
          parentRecord

        case _ =>
          throw new BadSpecialFormException(parentSymbol, "Record types can only inherit from other record types")
      }
    }

    // Parse our fields first
    val parentFieldNames = parentRecordOpt match {
      case Some(parentRecord) =>
        parentRecord.fields.map(_.name).toSet

      case _ =>
        Set[String]()
    }

    // Introduce a type variable referencing the type being defined
    val selfTypeVar = new pm.TypeVar(nameSymbol.name)

    // Parse our fields
    val nameToParsedField = parseFields(nameSymbol, selfTypeVar, fieldData, parentFieldNames)

    // Build our record type
    val recordFields = nameToParsedField.values.toList.map(_.field)
    val recordType = new vt.RecordType(nameSymbol.name, recordFields, Some(selfTypeVar), parentRecordOpt)

    // Create our constructor and predicate procedures
    val constructorProcedure = parseConstructor(recordType, constructorSymbol, constructorArgs)
    val predicateProcedure = (predicateSymbol -> et.TypePredicate(recordType).assignLocationFrom(predicateSymbol))

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

    val allProcedures = predicateProcedure :: (accessorProcedures ++ mutatorProcedures)

    Result(nameSymbol, recordType, constructorProcedure, allProcedures.toMap)
  }

  def apply(
      located : SourceLocated,
      args : List[sst.ScopedDatum]
  )(implicit frontendConfig : FrontendConfig) : Result = args match {
    case (nameSymbol : sst.ScopedSymbol) ::
         sst.ScopedProperList((constructorSymbol : sst.ScopedSymbol) :: constructorArgs) ::
         (predicateSymbol : sst.ScopedSymbol) ::
         fieldData =>

      parse(nameSymbol, None, constructorSymbol, constructorArgs, predicateSymbol, fieldData)

    case (nameSymbol : sst.ScopedSymbol) ::
         (parentSymbol : sst.ScopedSymbol) ::
         sst.ScopedProperList((constructorSymbol : sst.ScopedSymbol) :: constructorArgs) ::
         (predicateSymbol : sst.ScopedSymbol) ::
         fieldData =>

      parse(nameSymbol, Some(parentSymbol), constructorSymbol, constructorArgs, predicateSymbol, fieldData)

    case _ =>
      throw new BadSpecialFormException(located, "Unrecognized record type form")
  }
}
