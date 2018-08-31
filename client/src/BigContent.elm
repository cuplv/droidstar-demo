module BigContent exposing (..)

import Markdown
import Html exposing (..)
import Html.Attributes exposing (..)

import Bootstrap.Table as Table

countDownTimerDocs : Html msg
countDownTimerDocs = div []
  [ Markdown.toHtml [ class "docs" ] """
The [`CountDownTimer`](https://developer.android.com/reference/android/os/CountDownTimer)
is a simple timer that counts down for a particular amount of time and then returns
a callback to announce it has finished.

It has the following API.
"""
  , Table.table
      { options = [Table.striped, Table.hover]
      , thead = Table.simpleThead
          [ Table.th [] [ text "Return" ]
          , Table.th [] [ text "Method" ]
          , Table.th [] [ text "Description" ]
          ]
      , tbody = Table.tbody []
          [ Table.tr []
              [ Table.td [] [text "final CountDownTimer"]
              , Table.td [] [text "start()"]
              , Table.td [] [text "Start the countdown."]
              ]
          , Table.tr []
              [ Table.td [] [text "final void"]
              , Table.td [] [text "cancel()"]
              , Table.td [] [text "Cancel the countdown."]
              ]
          , Table.tr []
              [ Table.td [] [text "abstract void"]
              , Table.td [] [text "onFinish()"]
              , Table.td [] [text "Callaback fired when the time is up."]
              ]
          , Table.tr []
              [ Table.td [] [text "abstract void"]
              , Table.td [] [text "onTick(long millisUntilFinished)"]
              , Table.td [] [text "Callback fired on regular interval."]
              ]
          ]
       }
-- - `final CountDownTimer start()`
-- - `final void cancel()`
-- - `abstract void onFinished()`
-- - `abstract void onTick()`
  , Markdown.toHtml [ class "docs" ] """
Here, `start()` and `cancel()` are callins and `onFinish()` and `onTick()` are callbacks.
The interface is pretty self-explanatory to us, but our intuition for how the callins and
callbacks interact with each other has not been machine-checked.

We may also have questions about edge-cases, such as

1. Will calling `cancel()` when the timer is not running cause an error?
2. If the tick interval lines up perfectly with the timeout interval, do we get a final tick?
"""
  ]

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


    // * State and experiment setup
    //
    // In this section, we define the state that will be tested during
    // the experiment and the way to reset the state between tests in
    // order to isolate their effects.

    protected CountDownTimer timer;

    public CountDownTimerLP(Context c) {
        super(c);
        // The timer will be initialized by resetActions()
        this.timer = null;
    }

    // The resetActions destroy and recreate the state, isolating the
    // new state from any effects (or pending callbacks) the old state
    // accumulated.
    protected String resetActions(Context context, Callback callback) {
        if (timer != null) {
            timer.cancel();
        }
        timer = new CTimer(1100);
        return null;
    }


    // * Inputs
    //
    // In this section, we declare and define the list of inputs that
    // DroidStar should investigate.  DroidStar's goal is to learn
    // exactly what stateful effects each input has on the
    // CountDownTimer object, including in what order they are allowed
    // to be invoked.

    // Convenient definitions so we don't mistype the strings later
    public static String START = "start";
    public static String CANCEL = "cancel";

    // The list of inputs DroidStar is responsible for learning about.
    // DroidStar will use this list to create the necessary tests.
    protected List<String> uniqueInputSet() {
        List<String> inputs = new ArrayList();
        inputs.add(START);
        inputs.add(CANCEL);

        return inputs;
    }

    // The "definition" of each input, meaning the actual code
    // invocations DroidStar should perform in order to run an input.
    //
    // Here, as in most cases, each input corresponds to a single
    // method call.
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


    // * Outputs
    //
    // Here we set up the mechanism by which DroidStar records
    // callbacks executed by the object.

    public static String FINISHED = "finished";
    public static String TICK = "tick";

    // Callbacks are recorded by instrumenting them with a "respond"
    // call.  Generally, output tracking is performed simply by
    // creating a subclass which adds a "respond(callback)" to each
    // callback method we care about.
    //
    // This method is an opportunity to tune our experiment; listening for
    // additional callbacks will add time to the experiment and clutter the
    // results.  Try commenting out the `respond(TICK)` line to exclude it
    // from consideration.
    public class CTimer extends CountDownTimer {
        public CTimer(long s) {
            super(s, 550);
        }
        public void onTick(long s) {
            respond(TICK);
        }
        public void onFinish() {
            respond(FINISHED);
        }
    }


    // * Settings
    //
    // Here we set various settings necessary to the experiment

    // Some classes send a callback upon error instead of throwing an
    // exception.  We use this setting to declare callbacks that
    // should be considered as errors.
    //
    // CountDownTimer does not have any of these.
    public boolean isError(String output) {
        return false;
    }

    // Experiment name for logging purposes
    public String shortName() {
        return "CountDownTimer";
    }

    // How long DroidStar should wait for a callback before assuming
    // there will be none.  This value is very dependent on the class
    // being studied.  Large values are sometimes necessary, will
    // significantly extend the learning time.  (Time is in
    // milliseconds)
    public int betaTimeout() {
        return 2000;
    }

    // DroidStar is a tool for learing *regular* behavior.  Some
    // classes, like CountDownTimer, are non-regular.  This means that
    // calling an input N times will produce N callbacks.
    //
    // In order to learn a *regular subset* of its behavior, we can
    // restrict certain inputs to only be called once in any
    // particular test.
    public List<String> singleInputs() {
        List<String> inputs = new ArrayList();
        inputs.add(START);
        return inputs;
    }
}
"""

fileObserverDocs : Html msg
fileObserverDocs = div []
  [ Markdown.toHtml [ class "docs" ] """

The
[`FileObserver`](https://developer.android.com/reference/android/os/FileObserver)
monitors files, sending callback notifications when they are altered.

It has the following API:

"""
  , mkapi
      [ [ "void"
        , "startWatching()"
        , "Start watching for events."
        ]
      , [ "void"
        , "stopWatching()"
        , "Stop watching for events."
        ]
      , [ "abstract void"
        , "onEvent(int,String)"
        , "Triggers on a file event."
        ]
      ]
  , Markdown.toHtml [ class "docs" ] """

Similarly to the `SQLiteOpenHelper` experiment, the LearningPurpose
for this class uses non-standard callins and callbacks.  We encode
file manipulations (deleting and modifying) as callins, and we split
the `onEvent(int,String)` callback into two separate output symobls
for detection of modification and deletion.

"""
  ]


fileObserverDef : String
fileObserverDef = """
package edu.colorado.plv.droidstar.experiments.lp;

import java.util.ArrayList;
import java.util.List;
import java.util.Queue;
import java.util.ArrayDeque;

import android.os.Handler.Callback;
import android.content.Context;
import android.util.Log;

import edu.colorado.plv.droidstar.LearningPurpose;
import static edu.colorado.plv.droidstar.Static.*;

import android.os.FileObserver;
import java.io.File;
import java.io.FileWriter;
import java.io.FileOutputStream;
import java.io.PrintWriter;
import java.io.FileNotFoundException;

public class FileObserverLP extends LearningPurpose {
    protected TestObserver obs;
    protected File testfile;

    public String shortName() {return "FileObserver";}

    public FileObserverLP(Context c) {
        super(c);
        obs = null;
        testfile = new File(c.getFilesDir(), "file-observer-testfile");
    }

    // INPUTS
    public static String START = "start";
    public static String STOP = "stop";
    public static String MODIFY = "modify";
    public static String DELETE = "delete";

    // OUTPUTS
    public static String MODIFIED = "modified";
    public static String DELETED = "deleted";

    protected String resetActions(Context context, Callback callback) {
        if (obs != null) obs.stopWatching();
        testfile.delete();
        try {
            testfile.createNewFile();
            Log.d("STARLING:Q", "File created");
        }
        catch (Exception e) {e.printStackTrace();}
        
        obs = new TestObserver();
        return null;
    }

    public boolean isError(String output) {return false;}

    public int betaTimeout() {return 500;}

    protected List<String> uniqueInputSet() {
        List<String> is = new ArrayList();
        is.add(START);
        is.add(STOP);
        is.add(MODIFY);
        is.add(DELETE);
        return is;
    }

    public int postResetTimeout() {return 100;}

    public int eqLength() {return 3;}

    public void giveInput(String i, int altKey) throws Exception {
        if (i.equals(DELETE)) {
            testfile.delete();
            Log.d("STARLING:Q", "File deleted");
            
        } else if (i.equals(MODIFY)) {
            writeToFile(testfile);
            Log.d("STARLING:Q", "Wrote to file");
            
        } else if (i.equals(START)) {
            obs.startWatching();
            Log.d("STARLING:Q", "Started watching");
            
        } else if (i.equals(STOP)) {
            obs.stopWatching();
            Log.d("STARLING:Q", "Stopped watching");
        }
    }

    public boolean validQuery(Queue<String> q) {
        Queue<String> query = new ArrayDeque(q);
    
        while (!query.isEmpty()) {
            String i1 = query.remove();
            String i2 = query.peek();
            if ((i1.equals(MODIFY) || i1.equals(DELETE))
                && !(i2 == null || i2.equals(DELTA))) {
                    return false;
            }
        }
        return true;
    }

    public class TestObserver extends FileObserver {
        public TestObserver() {
            super(testfile.getPath(), FileObserver.ALL_EVENTS);
        }
        public void onEvent(int event, String path) {
            Log.d("STARLING:Q", "We got an event");
            switch (event) {
            case FileObserver.DELETE_SELF:
                respond(DELETED);
                break;
            case FileObserver.MODIFY:
                respond(MODIFIED);
                break;
            default:
                break;
            }
        }
    }

    protected void writeToFile(File f) throws Exception {
        FileOutputStream fo = new FileOutputStream(f);
        PrintWriter p = new PrintWriter(fo);
        p.append("hi");
        p.flush();
        p.close();
    }
}
"""


velocityTrackerDocs : Html msg
velocityTrackerDocs = div []
  [ Markdown.toHtml [ class "docs" ] """

The
[`VelocityTracker`](https://developer.android.com/reference/android/view/VelocityTracker)
keeps track of touch and slide events.

It has the following API:

"""
  , mkapi
      [ [ "void"
        , "addMovement(MotionEvent)"
        , "Add a user's movement to the tracker."]
      , [ "void"
        , "clear()"
        , "Reset the velocity tracker back to its initial state."
        ]
      , [ "void"
        , "computeCurrentVelocity(int,float)"
        , "Compute the current velocity."
        ]
      , [ "float"
        , "getXVelocity(int)"
        , "Retrieve last computed X velocity"
        ]
      , [ "void"
        , "recycle()"
        , "Return a VelocityTracker object back to be re-used by others."
        ]
      ]
  , Markdown.toHtml [ class "docs" ] """

This is a special case for DroidStar; `VelocityTracker` has no
callbacks or asynchronous behavior, so we are in this case learning a
traditional synchronous typestate.

Synchronous behavior can still be interesting.  We are checking here
whether any methods disable other methods.  In particular it seems
that `recycle()` should put the object in a "dead" state in which the
other methods return errors, so that no app accidentally continues
using it.

So does it have the expected behavior?

"""
  ]



velocityTrackerDef : String
velocityTrackerDef = """
package edu.colorado.plv.droidstar.experiments.lp;

import java.util.List;
import java.util.ArrayList;

import java.lang.AssertionError;
import java.lang.Thread;
import java.util.concurrent.TimeUnit;

import android.os.Handler.Callback;
import android.content.Context;
import android.view.VelocityTracker;
import android.view.MotionEvent;
import android.os.SystemClock;

import edu.colorado.plv.droidstar.LearningPurpose;
import static edu.colorado.plv.droidstar.Static.*;

public class VelocityTrackerLP extends LearningPurpose {
        protected VelocityTracker vt;

    protected String resetActions(Context ctx, Callback cb) {
        this.vt = VelocityTracker.obtain();
        return null;
    }
    protected List<String> uniqueInputSet() {
        List<String>is = new ArrayList();
        is.add("move");
        is.add("clear");
        is.add("compute");
        is.add("get");
        is.add("recycle");
        return is;
    }
    public int betaTimeout() {
        return 200;
    }
    public boolean isError(String o) {return false;}
    public String shortName() {
        return "VelocityTracker";
    }
    public VelocityTrackerLP(Context c) {
        super(c);
    }
    public void giveInput(String input, int altKey) throws Exception {
        if (input.equals("move")) {
            MotionEvent e = MotionEvent.obtain(SystemClock.uptimeMillis(),
                                               SystemClock.uptimeMillis(),
                                               MotionEvent.ACTION_DOWN,
                                               0,0,0);
            vt.addMovement(e);
        } else if (input.equals("clear")) {
            vt.clear();
        } else if (input.equals("compute")) {
            vt.computeCurrentVelocity(1);
        } else if (input.equals("get")) {
            vt.getXVelocity();
        } else if (input.equals("recycle")) {
            vt.recycle();
        }
    }
}
"""

mktds ls = List.map (\s -> Table.td [] [text s]) ls

mkapi lls = Table.table
  { options = [Table.striped, Table.hover]
  , thead = Table.simpleThead
      [ Table.th [] [ text "Return" ]
      , Table.th [] [ text "Method" ]
      , Table.th [] [ text "Description" ]
      ]
  , tbody = Table.tbody []
      (List.map (\ls -> Table.tr [] (mktds ls)) lls)
  }

asyncTaskDocs : Html msg
asyncTaskDocs = div []
  [ Markdown.toHtml [ class "docs" ] """

The
[`AsyncTask`](https://developer.android.com/reference/android/os/AsyncTask)
performs some long-running task in the background, notifying its app
via callback when it has finished

It has the following API.

"""
  , mkapi
      [ [ "final AsyncTask"
        , "execute(Params...)"
        , "Executes task."
        ]
      , [ "final boolean"
        , "cancel(boolean)"
        , "Attempts to cancel execution of task."
        ]
      , [ "final boolean"
        , "isCancelled()"
        , "Returns true if this task was cancelled."
        ]
      , [ "void"
        , "onCancelled()"
        , "Runs when task is cancelled."
        ]
      , [ "void"
        , "onPostExecute(Result)"
        , "Runs when task is finished."
        ]
      , [ "void"
        , "onPreExecute()"
        , "Runs just before task begins."
        ]
      ]
  , Markdown.toHtml [ class "docs" ] """

In our provided LearningPurpose, we analyze the result of
`isCancelled()` by throwing an error if it returns `false`.  This
causes it to appear as "enabled" in the resulting callback typestate
only in places where it returns `true`.  This is an example of the
flexibility a developer has in tuning the behavior they need DroidStar
to learn.

Questions we may have about `AsyncTask`'s behavior are:

1. Can we call `execute()` after `cancel()`?  If so, does the task
   indeed execute?
2. Is it possible to receive `onPostExecute()` after calling
   `cancel()` too late?

"""
  ]


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

Begin by choosing a class interface to learn.

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

sqliteOpenHelperDocs : Html msg
sqliteOpenHelperDocs = div []
  [ Markdown.toHtml [ class "docs" ] """

The
[`SQLiteOpenHelper`](https://developer.android.com/reference/android/database/sqlite/SQLiteOpenHelper)
helps manage database creation and version management, automatically
performing loading and upgrading operations as they are required.

It has a large API; we are interested in the following subset:

"""
  , mkapi
      [ [ "SQLiteDatabase"
        , "getWritableDatabase()"
        , "Create and/or open a database that can be read and written."
        ]
      , [ "void"
        , "close()"
        , "Close any open database object."
        ]
      , [ "void"
        , "onConfigure(SQLiteDatabase)"
        , "Called when the database connection is being configured."
        ]
      , [ "abstract void"
        , "onCreate(SQLiteDatabase)"
        , "Called when the database is created the first time."
        ]
      , [ "void"
        , "onOpen(SQLiteDatabase)"
        , "Called when the database has been opened."
        ]
      , [ "void"
        , "onUpgrade(SQLiteDatabase,int,int)"
        , "Called when the database needs to be upgraded."
        ]
      ]
  , Markdown.toHtml [ class "docs" ] """

The LearningPurpose we have provided for this class defines a highly
customized experiment.

The behavior of a `SQLiteOpenHelper` object mostly depends on the
details of the database it has been given, in particular its
**version** and **whether or not it exists yet**.

In this experiment, we split the single entry callin
`getWritableDatabase()` into several forms that each get a different
database (one that exists, one that doesn't, one that is out of date,
etc.) and see which callbacks each callin produces, and in what order.

Finally, we analyze the behavior of `close()` to understand when it
can be called and what processes it can interrupt.

"""
  ]


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
