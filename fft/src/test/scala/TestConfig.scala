package opera.fft

import chiseltest.{VerilatorBackendAnnotation, WriteVcdAnnotation}
import chiseltest.simulator.VerilatorFlags
import firrtl2.options.TargetDirAnnotation
import org.scalatest.{Args, Outcome, Status, TestSuite, TestSuiteMixin}

import java.io.File

object TestConfig {
  @volatile private var scalaTestOptions: Map[String, String] = Map.empty
  private val currentTestName = new ThreadLocal[String] {
    override def initialValue(): String = "unknown-test"
  }

  def configure(args: Args): Unit = {
    scalaTestOptions = Seq("verbose", "plot", "randomReadyValid").flatMap { name =>
      keys(name).view
        .flatMap(key => args.configMap.get(key).map(_.toString))
        .headOption
        .map(name -> _)
    }.toMap
  }

  private def keys(name: String): Seq[String] = Seq(name, s"fft.$name")

  private def parseBoolean(name: String, value: String): Boolean =
    value match {
      case "true"  => true
      case "false" => false
      case other   => throw new IllegalArgumentException(s"$name must be exactly 'true' or 'false', got '$other'")
    }

  private def flag(name: String): Boolean =
    scalaTestOptions
      .get(name)
      .orElse(keys(name).view.flatMap(key => sys.props.get(key)).headOption)
      .map(parseBoolean(name, _))
      .getOrElse(false)

  def verbose: Boolean = flag("verbose")
  def plot: Boolean = flag("plot")
  def randomReadyValid: Boolean = flag("randomReadyValid")
  def annotations = Seq(WriteVcdAnnotation, VerilatorBackendAnnotation, TargetDirAnnotation(testDirectory.getPath))
  def nonParallelVerilatorAnnotations = annotations :+ VerilatorFlags(Seq("--output-split", "0"))

  def withTestName[T](name: String)(body: => T): T = {
    val previous = currentTestName.get()
    currentTestName.set(sanitizeTestName(name))
    try {
      body
    } finally {
      currentTestName.set(previous)
    }
  }

  def testDirectory: File = new File(projectDir, s"test_run_dir/${currentTestName.get()}")
  def plotDirectory: File = testDirectory

  private def projectDir: File =
    if (new File("fft/src/test/scala").isDirectory) new File("fft") else new File(".")

  private def sanitizeTestName(name: String): String =
    name.replaceAll(" ", "_").replaceAll("\\W+", "")
}

trait TestConfigSupport extends TestSuiteMixin { this: TestSuite =>
  abstract override def run(testName: Option[String], args: Args): Status = {
    TestConfig.configure(args)
    super.run(testName, args)
  }

  abstract override def withFixture(test: NoArgTest): Outcome =
    TestConfig.withTestName(test.name) {
      super.withFixture(test)
    }
}
