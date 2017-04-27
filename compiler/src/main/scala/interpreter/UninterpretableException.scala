package io.llambda.compiler.interpreter


/** Thrown whenever an uninterpretable step or construct is encountered */
class UninterpretableException(message: String) extends Exception(message)
