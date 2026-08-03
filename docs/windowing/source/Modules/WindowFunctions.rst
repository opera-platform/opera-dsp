Supported windowing functions
=============================

This module provides several windowing functions commonly used in signal processing,
including Triangular, Hamming, Hanning, Blackman, Gaussian, and custom-defined windows.
Built-in windows support periodic and symmetric forms. A one-point built-in window
contains the single coefficient ``1.0``.

Classes
-------

.. contents::
   :local:

WindowType (trait)
------------------

.. class:: WindowType

   Abstract base trait for all window types.

   **Attributes:**

   - **N** (*Int*): Number of points in the window (window length)
   - **function** (*Option[Seq[Double]]*): Optional window function coefficients
   - **toString** (*String*): Name of the window

TriangularWindow
----------------

.. class:: TriangularWindow(N: Int, periodic: Boolean)

   Implements the **Triangular window**.

   :param N: Number of points in the window (window length)
   :type N: Int
   :param periodic: Whether the window is periodic (True) or symmetric (False)
   :type periodic: Boolean

   **Methods:**

   .. method:: toString

      Returns a string identifier for the window.

   .. method:: function

      Returns the window values as a sequence.

      .. code-block:: scala

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

HammingWindow
-------------

.. class:: HammingWindow(N: Int, periodic: Boolean)

   Implements the **Hamming window**.

   :param N: Number of points in the window (window length)
   :type N: Int
   :param periodic: Whether the window is periodic (True) or symmetric (False)
   :type periodic: Boolean

   **Attributes:**
   - **a0** (*Double*): Coefficient a0 (default: 0.54)
   - **a1** (*Double*): Coefficient a1 (default: 0.46)

   **Methods:**

   .. method:: toString

      Returns a string identifier for the window.

   .. method:: function

      Returns the window values as a sequence.

      .. code-block:: scala

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


HanningWindow
-------------

.. class:: HanningWindow(N: Int, periodic: Boolean)

   Implements the **Hanning window**.

   :param N: Number of points in the window (window length)
   :type N: Int
   :param periodic: Whether the window is periodic (True) or symmetric (False)
   :type periodic: Boolean

   **Methods:**

   .. method:: toString

      Returns a string identifier for the window.

   .. method:: function

      Returns the window values as a sequence.

      .. code-block:: scala

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


BlackmanWindow
--------------

.. class:: BlackmanWindow(N: Int, periodic: Boolean)

   Implements the **Blackman window**.

   :param N: Number of points in the window (window length)
   :type N: Int
   :param periodic: Whether the window is periodic (True) or symmetric (False)
   :type periodic: Boolean

   **Attributes:**
   - **a0** (*Double*): Coefficient a0 (default: 0.42)
   - **a1** (*Double*): Coefficient a1 (default: 0.5)
   - **a2** (*Double*): Coefficient a2 (default: 0.08)

   **Methods:**

   .. method:: toString

      Returns a string identifier for the window.

   .. method:: function

      Returns the window values as a sequence.

      .. code-block:: scala

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



GaussianWindow
--------------

.. class:: GaussianWindow(N: Int, sigma: Double, periodic: Boolean)

   Implements the **Gaussian window**.

   :param N: Number of points in the window (window length)
   :type N: Int
   :param sigma: Standard deviation of the Gaussian curve
   :type sigma: Double
   :param periodic: Whether the window is periodic (True) or symmetric (False)
   :type periodic: Boolean

   **Methods:**

   .. method:: toString

      Returns a string identifier for the window.

   .. method:: function

      Returns the window values as a sequence.

      .. code-block:: scala

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



NoWindow
--------

.. class:: NoWindow()

   Selects value-preserving bypass operation. It must be used with
   ``constWindow=true`` because no coefficient memory is required.

   **Attributes:**

   - **N** (*Int*): Always 0
   - **function**: Always None

   **Methods:**

   .. method:: toString

      Returns a string identifier for the window.

CustomWindow
------------

.. class:: CustomWindow(filePath: String)

   Loads window coefficients from a file-system path or classpath resource.

   :param filePath: File or resource containing one real-valued coefficient per line.
   :type filePath: String

   Empty lines are ignored. The path must be readable, the file must contain at least
   one coefficient, and every non-empty line must be numeric. Coefficients used by the
   Windowing datapath must be finite and representable by ``coeffType``, with
   non-negative quantized values. They are quantized using round-half-to-even.

   **Attributes:**

   - **N** (*Int*): Length of the window (number of coefficients in the file)
   - **function** (*Option[Seq[Double]]*): Window coefficients read from file


   Custom windows do not declare periodicity and therefore cannot use symmetric ROM
   folding. When used by Windowing, the file length must match ``numPoints``.
