package io.llambda.compiler.interpreter
import io.llambda

/** Thrown whenever an uninterpretable step or construct is encountered */
class UninterpretableException(message : String) extends Exception(message)
