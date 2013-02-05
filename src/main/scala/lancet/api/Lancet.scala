package lancet.api

import lancet.core._
import lancet.interpreter._

object Lancet {

  def newInterpreter = { 
    val it = new BytecodeInterpreter_Exec
    it.initialize
    it
  }

/*
  def newCompilerSimple = { 
    val it = new BytecodeInterpreter_Simple
    it.initialize
    it
  }
*/

  def newCompilerOpt = { 
    val it = new BytecodeInterpreter_Opt
    it.initialize
    it
  }



  // method and field hints (unfold/macro, frozen/stable, etc..)


  // quote/unquote (staging)

  // likely, speculate, stable


  // decompile/recompile


}