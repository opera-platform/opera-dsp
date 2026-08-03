package opera.windowing

import breeze.numerics.cos

import scala.io.Source
import scala.math.{Pi, abs, exp}
import scala.util.{Try, Using}

sealed trait WindowType {
  val N: Int
  val function: Option[Seq[Double]]
  val periodicity: Option[Boolean]
  override def toString: String
}

private object WindowFunctions {
  def cosine(N: Int, periodic: Boolean)(coefficient: (Int, Double) => Double): Seq[Double] = {
    if (N == 1) {
      // Special case: a single value window is just [1.0]
      Seq(1.0)
    } else {
      // Periodic form
      // Symmetric form
      val denominator = if (periodic) N.toDouble else (N - 1).toDouble
      Seq.tabulate(N)(index => coefficient(index, denominator))
    }
  }
}

/**
 * TriangularWindow case class defining a Triangular window function
 *
 * @param N          Number of points in the window (window length)
 * @param periodic   Determines if the window is periodic or symmetric
 */
case class TriangularWindow(N: Int, periodic: Boolean) extends WindowType {
  override val periodicity: Option[Boolean] = Some(periodic)
  /**
   * Calculation of the Triangular window values
   *
   * @return Sequence of window values based on the triangular window formula
   */
  override val function: Option[Seq[Double]] = Some(
    if (N == 1) {
      // Special case: a single value window is just [1.0]
      Seq(1.0)
    } else {
      // Periodic form
      // Symmetric form
      val center = if (periodic) N / 2.0 else (N - 1) / 2.0
      Seq.tabulate(N) { index =>
        1 - abs((index.toDouble - center) / center)
      }
    }
  )
  /**
   * Define string name
   */
  override def toString: String = s"Triangular_$N"
}

/**
 * HammingWindow case class defining a Hamming window function
 *
 * @param N          Number of points in the window (window length)
 * @param periodic   Determines if the window is periodic or symmetric
 */
case class HammingWindow(N: Int, periodic: Boolean) extends WindowType {
  override val periodicity: Option[Boolean] = Some(periodic)
  /**
   * Constants for the Hamming window coefficients
   */
  val a0: Double = 0.54
  val a1: Double = 0.46

  /**
   * Calculation of the Hamming window values
   *
   * @return Sequence of window values based on the Hamming window formula
   */
  override val function: Option[Seq[Double]] = Some(WindowFunctions.cosine(N, periodic) {
    case (index, denominator) => a0 - a1 * cos(2 * Pi * index.toDouble / denominator)
  })
  /**
   * Define string name
   */
  override def toString: String = s"Hamming_$N"
}

/**
 * HanningWindow case class defining a Hanning window function
 *
 * @param N          Number of points in the window (window length)
 * @param periodic   Determines if the window is periodic or symmetric
 */
case class HanningWindow(N: Int, periodic: Boolean) extends WindowType {
  override val periodicity: Option[Boolean] = Some(periodic)
  /**
   * Calculation of the Hanning window values
   *
   * @return Sequence of window values based on the Hanning window formula
   */
  override val function: Option[Seq[Double]] = Some(WindowFunctions.cosine(N, periodic) {
    case (index, denominator) => 0.5 * (1 - cos(2 * Pi * index.toDouble / denominator))
  })
  /**
   * Define string name
   */
  override def toString: String = s"Hanning_$N"
}

/**
 * BlackmanWindow case class defining a Blackman window function
 *
 * @param N          Number of points in the window (window length)
 * @param periodic   Determines if the window is periodic or symmetric
 */
case class BlackmanWindow(N: Int, periodic: Boolean) extends WindowType {
  override val periodicity: Option[Boolean] = Some(periodic)
  /**
   * Constants for the Blackman window coefficients
   */
  val a0: Double = 0.42
  val a1: Double = 0.5
  val a2: Double = 0.08

  /**
   * Calculation of the Blackman window values
   *
   * @return Sequence of window values based on the Blackman window formula
   */
  override val function: Option[Seq[Double]] = Some(WindowFunctions.cosine(N, periodic) {
    case (index, denominator) =>
      a0 - a1 * cos(2 * Pi * index.toDouble / denominator) +
        a2 * cos(4 * Pi * index.toDouble / denominator)
  })
  /**
   * Define string name
   */
  override def toString: String = s"Blackman_$N"
}

/**
 * GaussianWindow case class defining a Gaussian window function
 *
 * @param N          Number of points in the window (window length)
 * @param sigma      Standard deviation of the Gaussian curve
 * @param periodic   Determines if the window is periodic or symmetric
 */
case class GaussianWindow(N: Int, sigma: Double, periodic: Boolean) extends WindowType {
  override val periodicity: Option[Boolean] = Some(periodic)
  /**
   * Calculation of the Gaussian window values
   *
   * @return Sequence of window values based on the Gaussian window formula
   */
  override val function: Option[Seq[Double]] = Some(
    if (N == 1) {
      // Special case: a single value window is just [1.0]
      Seq(1.0)
    } else {
      val center = if (periodic) N / 2.0 else (N - 1) / 2.0
      val scale = if (periodic) sigma * N / 2.0 else sigma * (N - 1) / 2.0
      // Symmetric form
      Seq.tabulate(N) { index =>
        val x = (index.toDouble - center) / scale
        exp(-0.5 * x * x)
      }
    }
  )
  /**
   * Define string name
   */
  override def toString: String = s"Gaussian_$N"
}

/**
 * NoWindow case class when no Window is needed
 */
case class NoWindow() extends WindowType {
  override val N = 0
  override val periodicity: Option[Boolean] = None
  /**
   * NoWindow
   *
   * @return None
   */
  override val function: Option[Seq[Double]] = None
  /**
   * Define string name
   */
  override def toString: String = "NoWindow"
}

/**
 * CustomWindow case class returns window function defined by the user
 *
 * @param filePath   Path to the file containing window coefficients
 */
case class CustomWindow(filePath: String) extends WindowType {
  private val coefficients = CustomWindow.readCoefficients(filePath)

  /**
   * Read the coefficients from file
   *
   * @return Sequence of window values based on provided text file
   */
  override val function: Option[Seq[Double]] = Some(coefficients)

  override val periodicity: Option[Boolean] = None

  /**
   * Define function length
   */
  override val N: Int = coefficients.length
}

object CustomWindow {
  def readCoefficients(filePath: String): Seq[Double] = {
    require(filePath.trim.nonEmpty, "Custom window file path must not be empty")
    val file = new java.io.File(filePath)
    val source = if (file.isFile) {
      Source.fromFile(file)
    } else {
      val resourceName = filePath.stripPrefix("/")
      val stream = Option(
        Thread.currentThread().getContextClassLoader.getResourceAsStream(resourceName))
        .getOrElse(throw new IllegalArgumentException(
          s"Custom window file is not readable: $filePath"))
      Source.fromInputStream(stream)
    }
    val values = Using.resource(source) { input =>
      input.getLines().zipWithIndex.filter(_._1.trim.nonEmpty).map { case (line, index) =>
        Try(line.trim.toDouble).getOrElse(
          throw new IllegalArgumentException(
            s"Invalid custom window coefficient at $filePath:${index + 1}: '${line.trim}'"
          )
        )
      }.toVector
    }
    require(values.nonEmpty, s"Custom window file is empty: $filePath")
    values
  }
}
