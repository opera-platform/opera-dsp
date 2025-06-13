package opera.windowing

import breeze.numerics.cos

import scala.io.Source
import scala.math.{Pi, abs, exp}
import scala.util.{Try, Using}

sealed trait WindowType {
  val N: Int
  val function: Option[Seq[Double]]
  def toString: String
}

/**
 * TriangularWindow case class defining a Triangular window function
 *
 * @param N          Number of points in the window (window length)
 * @param periodic   Determines if the window is periodic or symmetric
 */
case class TriangularWindow(N: Int, periodic: Boolean) extends WindowType {
  /**
   * Calculation of the Triangular window values
   *
   * @return Sequence of window values based on the triangular window formula
   */
  val function: Option[Seq[Double]] = Some(
    if (N == 1) {
      // Special case: a single value window is just [1.0]
      Seq(1.0)
    } else if (periodic) {
      // Periodic form
      Seq.tabulate(N) { n =>
        1 - abs((n.toDouble - N / 2.0) / (N / 2.0))
      }
    } else {
      // Symmetric form
      Seq.tabulate(N) { n =>
        1 - abs((n.toDouble - (N - 1) / 2.0) / ((N - 1) / 2.0))
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
  val function: Option[Seq[Double]] = Some(
    if (N == 1) {
      // Special case: a single value window is just [1.0]
      Seq(1.0)
    } else if (periodic) {
      // Periodic form
      Seq.tabulate(N) { n =>
        a0 - a1 * cos(2 * Pi * n.toDouble / N)
      }
    } else {
      // Symmetric form
      Seq.tabulate(N) { n =>
        a0 - a1 * cos(2 * Pi * n.toDouble / (N - 1))
      }
    }
  )
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
  /**
   * Calculation of the Hanning window values
   *
   * @return Sequence of window values based on the Hanning window formula
   */
  val function: Option[Seq[Double]] = Some(
    if (N == 1) {
      // Special case: a single value window is just [1.0]
      Seq(1.0)
    } else if (periodic) {
      // Periodic form
      Seq.tabulate(N) { n =>
        0.5 * (1 - cos(2 * Pi * n.toDouble / N))
      }
    } else {
      // Symmetric form
      Seq.tabulate(N) { n =>
        0.5 * (1 - cos(2 * Pi * n.toDouble / (N - 1)))
      }
    }
  )
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
  val function: Option[Seq[Double]] = Some(
    if (N == 1) {
      // Special case: a single value window is just [1.0]
      Seq(1.0)
    } else if (periodic) {
      // Periodic form
      Seq.tabulate(N) { n =>
        a0 - a1 * cos(2 * Pi * n.toDouble / N) + a2 * cos(4 * Pi * n.toDouble / N)
      }
    } else {
      // Symmetric form
      Seq.tabulate(N) { n =>
        a0 - a1 * cos(2 * Pi * n.toDouble / (N - 1)) + a2 * cos(4 * Pi * n.toDouble / (N - 1))
      }
    }
  )
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
  /**
   * Calculation of the Gaussian window values
   *
   * @return Sequence of window values based on the Gaussian window formula
   */
  val function: Option[Seq[Double]] = Some(
    if (N == 1) {
      // Special case: a single value window is just [1.0]
      Seq(1.0)
    } else if (periodic) {
      Seq.tabulate(N) { n =>
        val x = (n.toDouble - N / 2.0) / (sigma * N / 2.0)
        exp(-0.5 * x * x)
      }
    } else {
      // Symmetric form
      Seq.tabulate(N) { n =>
        val x = (n - (N - 1) / 2.0) / (sigma * (N - 1) / 2.0)
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
  val N = 0
  /**
   * NoWindow
   *
   * @return None
   */
  val function: Option[Seq[Double]] = None
  /**
   * Define string name
   */
  override def toString: String = s"NoWindow"
}

/**
 * CustomWindow case class returns window function defined by the user
 *
 * @param filePath   Path to the file containing window coefficients
 */
case class CustomWindow(filePath: String) extends WindowType {
  /**
   * Read the coefficients from file
   *
   * @return Sequence of window values based on provided text file
   */
  val function: Option[Seq[Double]] = Some(
    Using(Source.fromFile(filePath)) { source =>
      source.getLines()
        .flatMap(line => Try(line.trim.toDouble).toOption)
        .toSeq
    }.getOrElse {
      println(s"Failed to read file: $filePath.\n")
      Seq.empty[Double]
    }
  )

  /**
   * Define function length
   */
  val N: Int = function.get.length
}