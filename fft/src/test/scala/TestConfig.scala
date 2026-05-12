package opera.fft

import chiseltest.{VerilatorBackendAnnotation, WriteVcdAnnotation}
import chiseltest.simulator.VerilatorFlags
import org.scalatest.{Args, Status, Suite}

import java.io.File

object TestConfig {
  @volatile private var scalaTestOptions: Map[String, String] = Map.empty

  def configure(args: Args): Unit = {
    scalaTestOptions = Seq("verbose", "plot").flatMap { name =>
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
  def annotations = Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)
  def nonParallelVerilatorAnnotations = annotations :+ VerilatorFlags(Seq("--output-split", "0"))

  def plotDirectory: File = {
    val projectDir = if (new File("fft/src/test/scala").isDirectory) new File("fft") else new File(".")
    new File(projectDir, "target/plots")
  }
}

trait TestConfigSupport extends Suite {
  abstract override def run(testName: Option[String], args: Args): Status = {
    TestConfig.configure(args)
    super.run(testName, args)
  }
}
