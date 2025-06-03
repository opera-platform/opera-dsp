windowing package
=================

This module provides several windowing functions commonly used in signal processing,
including Triangular, Hamming, Hanning, Blackman, Gaussian, and custom-defined windows.

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

   .. method:: function

      Returns the window values as a sequence.

   .. method:: toString

      Returns a string identifier for the window.

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

   .. method:: function

      Returns the window values as a sequence.

   .. method:: toString

      Returns a string identifier for the window.

HanningWindow
-------------

.. class:: HanningWindow(N: Int, periodic: Boolean)

   Implements the **Hanning window**.

   :param N: Number of points in the window (window length)
   :type N: Int
   :param periodic: Whether the window is periodic (True) or symmetric (False)
   :type periodic: Boolean

   **Methods:**

   .. method:: function

      Returns the window values as a sequence.

   .. method:: toString

      Returns a string identifier for the window.

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

   .. method:: function

      Returns the window values as a sequence.

   .. method:: toString

      Returns a string identifier for the window.

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

   .. method:: function

      Returns the window values as a sequence.

   .. method:: toString

      Returns a string identifier for the window.

NoWindow
--------

.. class:: NoWindow()

   Placeholder for when no windowing is needed.

   **Attributes:**

   - **N** (*Int*): Always 0
   - **function**: Always None

   **Methods:**

   .. method:: toString

      Returns a string identifier for the window.

CustomWindow
------------

.. class:: CustomWindow(filePath: String)

   Loads window coefficients from a user-provided text file.

   :param filePath: Path to the file containing window coefficients
   :type filePath: String

   **Attributes:**

   - **N** (*Int*): Length of the window (number of coefficients in the file)
   - **function** (*Option[Seq[Double]]*): Window coefficients read from file

   **Methods:**

   .. method:: toString

      Returns a string identifier for the window.

----

Usage Example
-------------

.. code-block:: scala

   import windowing._

   val win: WindowType = HammingWindow(N = 128, periodic = false)
   val coeffs = win.function.get

   println(s"Hamming window of length 128: $coeffs")


