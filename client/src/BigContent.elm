module BigContent exposing (..)

import Markdown
import Html exposing (..)
import Html.Attributes exposing (..)

asyncTaskDef : String
asyncTaskDef = """
package edu.colorado.plv.droidstar
package experiments.lp

import android.content.Context
import android.os.Handler.Callback

import android.os.AsyncTask


import scala.collection.JavaConverters._

class AsyncTaskLP(c: Context) extends LearningPurpose(c) {
  var task: AsyncTask[AnyRef,AnyRef,AnyRef] = null
  var counter: Int = 0

  val param = "asdf"

  val execute = "exec"
  val cancel = "cancel"
  val cancelled = "on_cancelled"
  val postexec = "on_postexec"
  val preexec = "on_preexec"
  val isCancelled = "is_cancelled"

  override def betaTimeout(): Int = 500

  @throws(classOf[Exception])
  override def giveInput(i: String, altKey: Int): Unit = i match {
    case `execute` => task.execute(param)
    case `cancel` => task.cancel(false)

    // We analyze the result of isCancelled by considering a `true` to
    // indicate a successful invocation and `false` an unsuccessful
    // one.
    case `isCancelled` => {
      if (!task.isCancelled()) {
        throw new IllegalStateException("Not in cancelled state")
      }
    }

    case _ => {
      logl("Unknown command to AsyncTask")
      throw new IllegalArgumentException("Unknown command to AsyncTask")
    }
  }

  override def inputAlts(): java.util.Map[String,java.lang.Integer] = {
    val m = super.inputAlts()
    m.put(execute,1)
    m
  }

  override def isError(o: String): Boolean = false

  override def resetActions(c: Context, b: Callback): String = {
    if (task != null) {
      task.cancel(true)
      counter += 1
    }
    task = new SimpleTask(counter)
    null
  }
  override def shortName(): String = "AsyncTask"
  override def uniqueInputSet(): java.util.List[String] =
    List(execute,cancel,isCancelled).asJava

  class SimpleTask(localCounter: Int) extends AsyncTask[AnyRef,AnyRef,AnyRef] {

    /* Using `String` instead of `AnyRef` gives AbstractMethodErrors...
     *
     * (see https://stackoverflow.com/questions/24934022/asynctask-doinbackground-abstract-method-not-implemented-error-in-android-scal)
     */
    override def doInBackground(ss: AnyRef*): AnyRef = {
      try {Thread.sleep(200)}
      catch {
        case _ : Throwable => logl("Sleep problem?")
      }
      param
    }
    override def onCancelled(s: AnyRef): Unit = {
      if (localCounter == counter) {
        respond(cancelled)
      }
    }
    override def onPostExecute(s: AnyRef): Unit = {
      respond(postexec)
    }
    override def onPreExecute(): Unit = {
      respond(preexec)
    }
  }

}
"""

inputsDoc = Markdown.toHtml [ class "docs" ] """

Begin by clicking one of the class names below.

"""

learningDoc = Markdown.toHtml [ class "docs" ] """

DroidStar learns by performing **queries**, which are sequences of
callins interspersed with pauses to listen for callbacks.

These pauses are written in the following logs as <span
class="okin">(CB?)</span> blocks.  Each pause gets a blue response
block after the ">>", which either contains a callback or a <span
class="okout">(none)</span> if none were seen.

Queries are **accepted** if running the callins in sequence does not
throw an error.  In this case they will appear green and followed by
(blue) callbacks.  If one of the callins throws an error, the query is
**rejected** and appears red in the log.

"""

resultsDoc = Markdown.toHtml [ class "docs" ] """

The result of a learning session is a **callback typestate**,
presented below as a graph, which describes a class's stateful behavior.

A class object begins in state 0, and allows only those callins which
have arrows leaving state 0 to be called.  Upon having an accepted
callin invoked, the object changes to the state indicated by the
callin's arrow.

Somes states have a callback enabled, marked by an arrow labeled with
`cb_something`.  This indicates that if you leave the object in this
state, you will observe that callback.

Callback typestates are built using query results.  Try looking back
through the query log that produced this result, and comparing queries
to the graph.  For <span class="okin">accepted</span> queries, you
will be able to create a path from the callins and callbacks in the
graph (where <span class="okout">(none)</span> makes no move).  For
<span class="errin">rejected</span> queries, you will find that one
callin in the sequence is applied in a state where it is not enabled
(there is no arrow labeled with it leaving the state).

"""

