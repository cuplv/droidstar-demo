module BigContent exposing (..)

import Markdown
import Html exposing (..)
import Html.Attributes exposing (..)

countDownTimerDef : String
countDownTimerDef = """
package edu.colorado.plv.droidstar.experiments.lp;

import java.util.List;
import java.util.ArrayList;
 
import android.os.CountDownTimer;
import android.os.Handler.Callback;
import android.content.Context;


import edu.colorado.plv.droidstar.LearningPurpose;
import static edu.colorado.plv.droidstar.Static.*;

public class CountDownTimerLP extends LearningPurpose {
    protected CountDownTimer timer;

    // INPUTS
    public static String START = "start";
    public static String CANCEL = "cancel";

    protected List<String> uniqueInputSet() {
        List<String> inputs = new ArrayList();
        inputs.add(START);
        inputs.add(CANCEL);

        return inputs;
    }

    // OUTPUTS
    public static String FINISHED = "finished";
    public static String TICK = "tick";

    public boolean isError(String output) {
        // there are no errors for this class?
        return false;
    }

    public String shortName() {
        return "CountDownTimer";
    }

    public int betaTimeout() {
        // Ticks come every second, so timeout should be more than a
        // second
        return 2000;
    }

    public List<String> singleInputs() {
        List<String> inputs = new ArrayList();
        inputs.add(START);
        return inputs;
    }

    public CountDownTimerLP(Context c) {
        super(c);
        this.timer = null;
    }

    protected String resetActions(Context context, Callback callback) {
        doReset();
        return null;
    }

    protected void doReset() {
        if (timer != null) {
            timer.cancel();
        }
        timer = new CTimer(1100);
    }

    public void giveInput(String input, int altKey) throws Exception {
        if (input.equals(START)) {
            timer.start();
        } else if (input.equals(CANCEL)) {
            timer.cancel();
        } else {
            logl("Unknown command to CountDownTimer");
            throw new IllegalArgumentException("Unknown command to CountDownTimer");
        }
    }

    public class CTimer extends CountDownTimer {
        public CTimer(long s) {
            super(s, 1000);
        }
        public void onTick(long s) {
            // respond(TICK);
        }
        public void onFinish() {
            respond(FINISHED);
        }
    }
}
"""

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
callin's <span class="okin">green</span> arrow.

Somes states have a callback enabled, marked by a <span
class="okout">blue</span> arrow.  This indicates that if you leave the
object in this state, you will observe that callback.

Callback typestates are built using query results.  Try looking back
through the query log that produced this result, and comparing queries
to the graph.  For <span class="okin">accepted</span> queries, you
will be able to create a path from the callins and callbacks in the
graph (where <span class="okout">(none)</span> makes no move).  For
<span class="errin">rejected</span> queries, you will find that one
callin in the sequence is applied in a state where it is not enabled
(there is no arrow labeled with it leaving the state).

"""

sqliteHelperDef : String
sqliteHelperDef = """
package edu.colorado.plv.droidstar
package experiments.lp

import java.io.File;
import java.util.ArrayList;
import java.util.Queue;
import java.util.ArrayDeque;

import android.os.Handler.Callback;
import android.content.Context;
import android.database.sqlite.SQLiteOpenHelper;
import android.database.sqlite.SQLiteDatabase;


import scala.collection.JavaConverters._

class SQLiteOpenHelperLP(c: Context) extends LearningPurpose(c) {

  def dbfile(c: Context): File =
    new File(c.getExternalFilesDir(null), "testDB.sqlite3")

  var helper: MyDBHelper = null
  var testDB: File = dbfile(c)

  def initdb(v: Int): Unit = {
    var db: SQLiteDatabase = SQLiteDatabase.openOrCreateDatabase(testDB, null)
    db.setVersion(v)
    db.close()
  }

  // INPUTS
  val OPENNEW = "openNew"
  val OPENHV = "openHV" // higher ver
  val OPENLV = "openLV" // lower ver
  val OPENSV = "openSV" // same ver
  val CLOSE = "close"

  val THISV: Int = 5
  val HV: Int = 6
  val LV: Int = 4

  // OUTPUTS
  val CONFIGURED = "confd"
  val CREATED = "created"
  val OPENED = "opened"
  val UPGRADED = "upgrd"

  override def betaTimeout(): Int = 500

  def openinput(v: Int): Unit = {
    initdb(v)
    helper.getWritableDatabase()
  }

  @throws(classOf[Exception])
  override def giveInput(i: String, k: Int): Unit = i match {
    case `OPENNEW` => helper.getWritableDatabase()
    case `OPENSV` => openinput(THISV)
    case `OPENHV` => openinput(HV)
    case `OPENLV` => openinput(LV)
    case `CLOSE` => helper.close()
    case _ => {
      logl("Unknown command to SQLiteOpenHelper")
      throw new IllegalArgumentException("Unknown command to AsyncTask")
    }
  }

  // All queries must start with an OPEN input, and then can't have
  // any more OPEN inputs.  This is a very limited experiment...
  override def validQuery(q: Queue[String]): Boolean = {
    var query: Queue[String] = new ArrayDeque(q)
    val first: String = query.poll()

    if (first == null) {
      return true
    }
    if (first.equals(OPENHV)
        || first.equals(OPENNEW)
        || first.equals(OPENLV)
        || first.equals(OPENSV)) {

        for (input <- query.asScala) {
            if (input.equals(OPENHV)
                || input.equals(OPENNEW)
                || input.equals(OPENLV)
                || input.equals(OPENSV)) {
                return false
            }
        }
        return super.validQuery(query)
    }
    return false
  }

  override def isError(o: String): Boolean = false

  override def resetActions(c: Context, b: Callback): String = {
    if (helper != null) {
      helper.close()
    }
    testDB.delete()
    helper = new MyDBHelper(c, testDB.getAbsolutePath(), null, 5)
    null
  }

  class MyDBHelper(
    c: Context,
    n: String,
    f: SQLiteDatabase.CursorFactory,
    v: Int
  ) extends SQLiteOpenHelper(c,n,f,v) {
    override def onConfigure(db: SQLiteDatabase): Unit =
      respond(CONFIGURED)
    override def onCreate(db: SQLiteDatabase): Unit =
      respond(CREATED)
    override def onOpen(db: SQLiteDatabase): Unit =
      respond(OPENED)
    override def onUpgrade(db: SQLiteDatabase, oldVersion: Int, newVersion: Int): Unit =
      respond(UPGRADED)
  }

  override def shortName(): String = "SQLiteOpenHelper"

  override def uniqueInputSet(): java.util.List[String] =
    List(OPENNEW, OPENHV, OPENLV, OPENSV, CLOSE).asJava

}
"""
