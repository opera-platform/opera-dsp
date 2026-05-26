package opera.cfar

import chiseltest.{VerilatorBackendAnnotation, WriteVcdAnnotation}
import firrtl2.options.TargetDirAnnotation
import org.scalatest.{Args, Outcome, Status, TestSuite, TestSuiteMixin}

import java.io.File

object TestConfig {
  private[cfar] val optionNames = Seq("verbose", "plot", "randomReadyValid")
  @volatile private[cfar] var scalaTestOptions: Map[String, String] = Map.empty
  def annotations = Seq(WriteVcdAnnotation, VerilatorBackendAnnotation, TargetDirAnnotation(testDirectory.getPath))
  private val currentTestName = new ThreadLocal[String] {
    override def initialValue(): String = "unknown-test"
  }

  def configure(args: Args): Unit = {
    scalaTestOptions = optionNames.flatMap { name =>
      keys(name).view
        .flatMap(key => args.configMap.get(key).map(_.toString))
        .headOption
        .map(name -> _)
    }.toMap
  }

  private[cfar] def keys(name: String): Seq[String] = Seq(name, s"cfar.$name")

  private def parseBoolean(name: String, value: String): Boolean =
    value match {
      case "true"  => true
      case "false" => false
      case other   => throw new IllegalArgumentException(s"$name must be exactly 'true' or 'false', got '$other'")
    }

  private def option(name: String): Option[String] =
    scalaTestOptions
      .get(name)
      .orElse(keys(name).view.flatMap(key => sys.props.get(key)).headOption)

  private def flag(name: String): Boolean =
    option(name).map(parseBoolean(name, _)).getOrElse(false)

  def verbose: Boolean = flag("verbose")
  def plot: Boolean = flag("plot")
  def randomReadyValid: Boolean = flag("randomReadyValid")

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
    if (new File("cfar/src/test/scala").isDirectory) new File("cfar") else new File(".")

  private def sanitizeTestName(name: String): String = {
    val compact = name.replaceAll(" ", "_").replaceAll("\\W+", "")
    if (compact.length <= 96) compact
    else {
      val hash = java.lang.Integer.toUnsignedString(name.hashCode, 16)
      s"${compact.take(83)}_$hash"
    }
  }
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
