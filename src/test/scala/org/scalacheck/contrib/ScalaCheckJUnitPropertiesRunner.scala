package org.scalacheck.contrib

import org.junit.runner.Description
import org.junit.runner.notification.Failure
import org.junit.runner.notification.RunNotifier
import org.scalacheck.Properties
import org.scalacheck.Test
import org.scalacheck.util.ConsoleReporter

/**
 * This a JUnit runner that allows to run ScalaCheck properties (created into an object that implements
 * Properties) as part of a JUnit test suite. Each property will be counted as a failure or passed test
 * by JUnit.
 *
 * Properties are written in the exact same way as pure ScalaCheck; the only aifference is that the test suite class
 * needs to be annotated with @RunWith[classOf[ScalaCheckJUnitPropertiesRunner]] so that JUnit knows how to run
 * the tests
 */
class ScalaCheckJUnitPropertiesRunner(suiteClass: java.lang.Class[Properties]) extends org.junit.runner.Runner {

  private val properties = suiteClass.newInstance

  lazy val getDescription = createDescription(properties)

  /**
   * Create a description
   */
  private def createDescription(props: Properties): Description = {
    val description = Description.createSuiteDescription(props.getClass)
    props.properties.foreach(p => description.addChild(Description.createTestDescription(props.getClass, p._1)))
    description
  }

  // Our custom tes callback, used to keep JUnit's runner updated about test progress
  private[contrib] class CustomTestCallback(notifier: RunNotifier, desc: Description) extends Test.TestCallback {
    def failure(res: Test.Failed) = {
      val assertion = new AssertionError(s"Failed on arguments ${res.args.map(_.arg).mkString(",")},\n${res.labels.mkString(",\n")}")
      assertion.setStackTrace(Array(new StackTraceElement(properties.getClass.getName,"",null,-1)))
      new Failure(desc, assertion)
    }

    /** Called whenever a property has finished testing */
    override def onTestResult(name: String, res: Test.Result) = {
      res.status match {
        case Test.Passed    => {} // Test passed, nothing to do
        case Test.Proved(_) => {} // Test passed, nothing to do
        case Test.Exhausted => notifier.fireTestIgnored(desc) // exhausted tests are marked as ignored in JUnit
        case r @ Test.Failed(args, labels) =>
          notifier.fireTestFailure(failure(r)) // everything else is a failed test
        case Test.PropException(args, e, labels) => notifier.fireTestFailure(new Failure(desc, e)) 
      }
    }
  }

  // we'll use this one to report status to the console, and we'll chain it with our custom reporter
  val consoleReporter = new ConsoleReporter(1, 80)

  /**
   * Run this <code>Suite</code> of tests, reporting results to the passed <code>RunNotifier</code>.
   * This class's implementation of this method invokes <code>run</code> on an instance of the
   * <code>suiteClass</code> <code>Class</code> passed to the primary constructor, passing
   * in a <code>Reporter</code> that forwards to the  <code>RunNotifier</code> passed to this
   * method as <code>notifier</code>.
   *
   * @param notifier the JUnit <code>RunNotifier</code> to which to report the results of executing
   * this suite of tests
   */
  def run(notifier: RunNotifier) {

    properties.properties.map({ propTuple =>
      propTuple match {
        case (desc, prop) => {
          val descObj = Description.createTestDescription(properties.getClass, desc)

          notifier.fireTestStarted(descObj)
          Test.check(prop)(_.withTestCallback(consoleReporter chain (new CustomTestCallback(notifier, descObj))))

          notifier.fireTestFinished(descObj)
        }
      }
    })
  }

  /**
   * Returns the number of tests that are expected to run when this ScalaTest <code>Suite</code>
   * is run.
   *
   * @return the expected number of tests that will run when this suite is run
   */
  override def testCount() = properties.properties.size
}