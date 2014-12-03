package io.llambda.compiler.frontend
import io.llambda

import collection.mutable.ListBuffer
import collection.immutable.ListMap

import llambda.compiler.sst
import llambda.compiler.et
import llambda.compiler.codegen.CompactRepresentationForType
import llambda.compiler.{valuetype => vt}
import llambda.compiler.Primitives
import llambda.compiler.BadSpecialFormException

private[frontend] object ParseRecordTypeDefine {
  private case class ParsedField(
    field : vt.RecordField,
    accessorSymbol : sst.ScopedSymbol,
    mutatorSymbol : Option[sst.ScopedSymbol]
  )

  private def parseFields(
      fieldData : List[sst.ScopedDatum],
      inheritedFieldNames : Set[String]
  )(implicit frontendConfig : FrontendConfig) : ListMap[String, ParsedField] =
    fieldData.foldLeft(ListMap[String, ParsedField]()) {
      case (parsedFields, fieldDatum @ sst.ScopedProperList(fieldDefDatum :: procedureData)) =>
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
            // Resolve the field's Scheme type
            val schemeType = ExtractType.extractStableType(fieldTypeDatum)

            // Rewrite this to a compact native type
            val compactType = CompactRepresentationForType(schemeType)

            (nameSymbol, compactType)

          case other =>
            val message = s"Unrecognized record field name definition. Must be either identiifer or [identifier : <type>]."
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

  private def parseConstructor(
      recordType : vt.RecordType,
      nameToField : Map[String, vt.RecordField],
      constructorSymbol : sst.ScopedSymbol,
      constructorOperands : List[sst.ScopedDatum]
  ) : (sst.ScopedSymbol, et.RecordConstructor) = {
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

    for((fieldName, field) <- nameToField) {
      if (!initializedFields.contains(field)) {
        // Make sure this can be initialized to #!unit
        field.fieldType match {
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
      appliedSymbol : sst.ScopedSymbol,
      nameSymbol : sst.ScopedSymbol,
      parentSymbolOpt : Option[sst.ScopedSymbol],
      constructorSymbol : sst.ScopedSymbol,
      constructorOperands : List[sst.ScopedDatum],
      predicateSymbol : sst.ScopedSymbol,
      fieldData : List[sst.ScopedDatum]
  )(implicit frontendConfig : FrontendConfig) : ParsedRecordTypeDefine = {
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

    val nameToParsedField = parseFields(fieldData, parentFieldNames)

    // Build our record type
    val recordFields = nameToParsedField.values.toList.map(_.field)
    val recordType = new vt.RecordType(nameSymbol.name, parentRecordOpt, recordFields)

    // Get a list of all of our fields including inherited ones
    val allFieldNamesMap = (recordType.fieldsWithInherited.map { field =>
      field.name -> field
    }).toMap

    // Create our constructor and predicate procedures
    val constructorProcedure = parseConstructor(recordType, allFieldNamesMap, constructorSymbol, constructorOperands)
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

    val allProcedures = constructorProcedure :: predicateProcedure :: (accessorProcedures ++ mutatorProcedures)

    ParsedRecordTypeDefine(nameSymbol, recordType, allProcedures.toMap)
  }

  def apply(
      appliedSymbol : sst.ScopedSymbol,
      operands : List[sst.ScopedDatum]
  )(implicit frontendConfig : FrontendConfig) : ParsedRecordTypeDefine = operands match {
    case (nameSymbol : sst.ScopedSymbol) ::
         sst.ScopedProperList((constructorSymbol : sst.ScopedSymbol) :: constructorOperands) ::
         (predicateSymbol : sst.ScopedSymbol) ::
         fieldData =>

      parse(appliedSymbol, nameSymbol, None, constructorSymbol, constructorOperands, predicateSymbol, fieldData)

    case (nameSymbol : sst.ScopedSymbol) ::
         (parentSymbol : sst.ScopedSymbol) ::
         sst.ScopedProperList((constructorSymbol : sst.ScopedSymbol) :: constructorOperands) ::
         (predicateSymbol : sst.ScopedSymbol) ::
         fieldData =>

      parse(appliedSymbol, nameSymbol, Some(parentSymbol), constructorSymbol, constructorOperands, predicateSymbol, fieldData)

    case _ =>
      throw new BadSpecialFormException(appliedSymbol, "Unrecognized record type form")
  }
}
