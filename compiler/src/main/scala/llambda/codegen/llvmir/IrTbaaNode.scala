package llambda.codegen.llvmir

case class IrTbaaNode(index : Long, identity : String, parentIndex : Option[Long] = None, constant : Boolean = false) extends Irable {
  def toIr : String = {
    val identityField = "metadata !\"" + identity + "\""

    val parentFieldOpt = parentIndex match {
      case Some(parentInt) =>
        Some(s"metadata !${parentInt}")

      case None if constant =>
        // We need to add a null so we can populate the third field
        Some("null")

      case _ =>
        None
    }

    val constantFieldOpt = if (constant) {
      Some("i64 1")
    }
    else {
      None
    }

    // Put all the fields together
    val allFields = identityField :: (parentFieldOpt.toList ++ constantFieldOpt.toList)

    s"!${index} = metadata !{ " + allFields.mkString(", ") + " }"
  }
}

