Windowing Overview
==================

The `Windowing` module is part of the OPERA-DSP project. A windowing function is
typically applied before an FFT to reduce spectral leakage.

This module extends the
`DspBlock <https://chipyard.readthedocs.io/en/latest/Customization/Dsptools-Blocks.html>`_
trait for modular signal processing integration.

Supported window functions can be found here:

- :doc:`Supported window functions <../Modules/WindowFunctions>`

Simplified Block Diagram
------------------------

Below is a simplified block diagram of the Windowing module.

.. image:: ./images/windowing.png
   :alt: Block diagram of Windowing module
   :align: center
   :width: 800px

Window coefficients can be stored in a fixed ROM or in bus-writable RAM.

Window Parameters
-----------------

.. code-block:: scala

  case class WindowingParams[T <: Data](
    inputType: DspComplex[T],
    outputType: DspComplex[T],
    coeffType: T,
    numPoints: Int,
    runTime: Boolean,
    windowFunc: WindowType,
    memoryFile: String,
    constWindow: Boolean,
    trimType: TrimType,
    mulPipeRegs: Int = 0,
    roundPipeRegs: Int = 0,
    romStyle: RomStyle = Distributed,
    foldSymmetric: Boolean = false
  )

Parameter descriptions
~~~~~~~~~~~~~~~~~~~~~~

- ``inputType`` / ``outputType``:
  Complex input and output formats. The output binary point and signed integer range
  must cover the input format. The output binary point must not exceed the product
  binary point. Disabled Windowing and ``NoWindow`` preserve the numerical input value
  while converting to the output format.

- ``coeffType``:
  Coefficient format. Coefficients are quantized with round-half-to-even. The Windowing
  datapath requires finite, representable coefficients whose quantized values are
  non-negative.

- ``numPoints``:
  Positive maximum frame and coefficient-table size.

- ``runTime``:
  Use the ``chirpsize`` register instead of ``numPoints`` as the active frame size.

- ``windowFunc``:
  Select a :doc:`supported window function <../Modules/WindowFunctions>`.

- ``memoryFile``:
  Generated coefficient-file path. Values are written as width-padded, two's-complement
  hexadecimal words.

- ``constWindow``:
  Select ROM when true or bus-writable RAM when false. ``NoWindow`` requires ROM mode.

- ``trimType``:
  Product rounding mode: ``Floor``, ``Ceiling``, ``Convergent`` (half-even), or
  ``Round`` (half away from zero).

- ``mulPipeRegs`` / ``roundPipeRegs``:
  Optional elastic product and rounded-output stages. Each value is zero or one, and
  ``roundPipeRegs`` cannot exceed ``mulPipeRegs``.

- ``romStyle`` / ``foldSymmetric``:
  Select an asynchronous ``Distributed`` ROM or a ``Synchronous`` ROM. Synchronous ROM
  requires ``mulPipeRegs=1``. Folding stores the unique half of a bit-exact symmetric
  window and is limited to fixed, non-runtime, built-in synchronous ROMs.

JSON Configuration
------------------

The JSON parser requires the address and functional parameter fields shown in the
example and rejects unknown fields. ``mulPipeRegs``, ``roundPipeRegs``, ``romStyle``,
and ``foldSymmetric`` are optional and use the defaults shown above. String values use
the exact names ``Floor``, ``Ceiling``, ``Convergent``, ``Round``, ``Distributed``,
``Synchronous``, and the window class names listed in the window-function
documentation. See ``windowing/src/main/resources/parameters.json`` for a complete
example.

Register Map
------------

The following registers are exposed through the MMIO interface:

+------------------+-------------+---------+--------------------------------+------------------------+
| Register Name    | Offset      | Access  | Width                          | Description            |
+==================+=============+=========+================================+========================+
| chirpsize        | 0*beatBytes | R/W     | log2Ceil(numPoints + 1)        | Active frame size      |
+------------------+-------------+---------+--------------------------------+------------------------+
| ctrl             | 1*beatBytes | R/W     | 1                              | Enable Windowing       |
+------------------+-------------+---------+--------------------------------+------------------------+


- ``chirpsize``: Defines the active frame size when ``runTime`` is enabled. Software
  must program a value from one through ``numPoints`` and change it only while the
  stream is idle. A smaller ROM size uses the leading entries of the original table;
  a genuine M-point runtime window requires RAM mode, an M-point coefficient table,
  and ``chirpsize=M``.

- ``ctrl``: Bit zero enables coefficient multiplication. When clear, samples bypass
  multiplication while retaining the configured pipeline latency.

Streaming Behavior
------------------

The AXI4-Stream input and output use ready/valid flow control. A sample and its
``last`` and enable state advance only when accepted, and the output remains stable
under backpressure. The coefficient address resets after an accepted ``last`` or the
active frame size, so the next accepted sample uses coefficient zero. With no stalls,
all configurations accept one sample per cycle.

Distributed ROM and ``NoWindow`` have no coefficient-read delay. RAM and synchronous
ROM add one read cycle; each enabled pipeline option adds one further cycle.

SystemVerilog Generation
------------------------

You can generate SystemVerilog using either AXI4 or TL as the memory-mapped control
interface.

From the ``opera-soc`` root, load the environment and use:

.. code-block:: bash

   source ./env.sh
   sbt "project opera-windowing; runMain opera.windowing.AXI4App"
   sbt "project opera-windowing; runMain opera.windowing.TLApp"

This writes the AXI4 variant to ``./rtl/WindowingAXI4`` and the TileLink variant to
``./rtl/WindowingTL``.

You can also pass a JSON configuration file:

.. code-block:: bash

   sbt "project opera-windowing; runMain opera.windowing.AXI4App \
     generators/opera-dsp/windowing/src/main/resources/parameters.json"
   sbt "project opera-windowing; runMain opera.windowing.TLApp \
     generators/opera-dsp/windowing/src/main/resources/parameters.json"

The example JSON configuration is available
`on GitHub <https://github.com/opera-platform/opera-dsp/blob/main/windowing/src/main/resources/parameters.json>`_.

Tests
-----

From the ``opera-soc`` root, run:

.. code-block:: bash

   source ./env.sh
   sbt "project opera-windowing; test"

Waveforms are disabled by default; set ``WIN_VCD=1`` to enable them. Test artifacts are
written under ``generators/opera-dsp/windowing/test_run_dir/``.

Source Code
-----------

You can view the source code here:

`Windowing.scala on GitHub <https://github.com/opera-platform/opera-dsp/blob/main/windowing/src/main/scala/Windowing.scala>`_
