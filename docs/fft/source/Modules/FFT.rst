FFT, R2FFT, and R22FFT Modules
==============================

The top-level `FFT` module wraps one of the SDF FFT implementations selected by ``FFTParams.sdfRadix``.
``Radix2`` instantiates ``R2FFT`` and ``Radix22`` instantiates ``R22FFT``.

FFT Parameters
--------------

These blocks use the same set of parameters as the FFT overview.
The code block below shows an abbreviated ``FFTParams`` signature with the current Scala defaults. The AXI4/TL apps and example JSON file use a 1024-point ``Radix22`` runtime configuration.

**Definition:**

.. code-block:: scala

  case class FFTParams(
    fftSize         : Int = 1024,
    twiddleType     : DspComplex[FixedPoint] = DspComplex(FixedPoint(16.W, 14.BP)),
    inDataType      : DspComplex[FixedPoint] = DspComplex(FixedPoint(16.W, 14.BP)),
    decimation      : DecimationType = DIF,
    sdfRadix        : SDFRadix = Radix22,
    growEnable      : Seq[Boolean] = Seq.empty,
    runTime         : Boolean = false,
    divBy2          : Seq[Boolean] = Seq.empty,
    divBy2Reg       : Boolean = false,
    overflowReg     : Boolean = false,
    trimType        : TrimType = RoundHalfUp,
    numAddPipes     : Int = 0,
    numMulPipes     : Int = 0,
    direction       : Boolean = true,
    directionReg    : Boolean = false,
    dspMul4         : Boolean = false,
    useBitReverse   : Boolean = false,
    minSRAMdepth    : Int = 0,
    singlePortSRAM  : Boolean = false,
    stageTrimTypes  : Seq[TrimType] = Seq.empty,
    twiddleTrimTypes: Seq[TrimType] = Seq.empty
  )

For detailed parameter descriptions, see :doc:`FFT Parameters <../Overview/Overview>`.

IOs
---

The direct `FFT` wrapper and the radix cores `R2FFT` and `R22FFT` all use ``FFTIO``. The top-level wrapper optionally inserts :doc:`BitReverse <BitReverse>` around the selected radix core, but it keeps the same Decoupled complex sample streams and optional runtime control/status ports.

.. code-block:: scala

  class FFTIO(params: FFTParams) extends Bundle {
    val in : DecoupledIO[DspComplex[FixedPoint]] = Flipped(Decoupled(params.inDataType))
    val out: DecoupledIO[DspComplex[FixedPoint]] = Decoupled(params.fftOutputType)

    val i_last: Bool = Input(Bool())
    val o_last: Bool = Output(Bool())

    val i_load_cfg : Option[Bool] = if (hasRuntimeConfig) Some(Input(Bool())) else None
    val i_size : Option[UInt] = if (params.runTime) Some(Input(UInt(log2Ceil(params.fftSize).W))) else None
    val i_divBy2 : Option[Vec[Bool]] = if (params.divBy2Reg) Some(Input(Vec(log2Ceil(params.fftSize), Bool()))) else None
    val i_fft_or_ifft: Option[Bool] = if (params.directionReg) Some(Input(Bool())) else None

    val o_overflow: Option[Vec[Bool]] = if (params.overflowReg) Some(Output(Vec(log2Ceil(params.fftSize), Bool()))) else None
  }

- ``in`` and ``out`` are Decoupled complex data streams.
- ``i_last`` and ``o_last`` mark frame boundaries.
- ``i_load_cfg`` latches runtime configuration and clears wrapper-local state when any runtime control is enabled.
- ``i_size`` selects runtime FFT size when ``runTime`` is enabled.
- ``i_divBy2`` selects per-stage scaling when ``divBy2Reg`` is enabled.
- ``i_fft_or_ifft`` selects FFT/IFFT direction when ``directionReg`` is enabled.
- ``o_overflow`` is generated only when ``overflowReg`` is enabled.

`FFTAXI4` and `FFTTL` wrap the direct `FFT` module with AXI4-Stream input/output and AXI4 or TileLink memory-mapped registers through the `DspBlock <https://chipyard.readthedocs.io/en/latest/Customization/Dsptools-Blocks.html>`_ interface.

Module Operation
----------------

The `FFT` wrapper instantiates either `R2FFT` or `R22FFT` and connects the runtime control and overflow ports.
When ``useBitReverse`` is enabled, it also inserts :doc:`BitReverse <BitReverse>` after a DIF core or before a DIT core so that the external stream can be natural order.

`R2FFT` builds a chain of :doc:`SDFStage <SDFStages>` instances. Each stage uses a delay-feedback butterfly and, where needed, a twiddle multiplication.
The stage order depends on ``decimation``:

- ``DIF`` starts with the largest delay stage and moves toward smaller delay stages.
- ``DIT`` starts with the smallest delay stage and moves toward larger delay stages.

`R22FFT` builds a radix-2\ :sup:`2` SDF chain. It uses the same ``SDFStage`` module as the radix-2 core, but applies trivial rotations and shared twiddle multipliers across stage pairs.
This implementation supports only FFT sizes of the form :math:`4^N`.

If the output stream cannot accept data, the core holds its pipeline state and deasserts input ``ready`` once it can no longer accept another sample.
The ``o_last`` signal is aligned with ``out.valid`` and marks the final sample of each output frame.

Runtime Configuration
---------------------

When runtime controls are enabled, ``i_load_cfg`` latches the selected FFT size, divide-by-two controls, and direction controls.
If a configuration load occurs while output data from the previous frame is still pending, the FFT stores the pending configuration and applies it after the current output frame drains.
Static direction and divide-by-two runtime-control ports are also applied at frame-safe boundaries when their register controls are enabled.
In ``R22FFT`` runtime mode, loaded sizes must select active FFT sizes of the form :math:`4^N`.

The memory-mapped `FFTAXI4` and `FFTTL` wrappers expose the same controls through the registers described in :doc:`FFT Overview <../Overview/Overview>`.

SystemVerilog Generation
------------------------

The direct `FFT`, `R2FFT`, and `R22FFT` modules do not currently have a standalone app entry point.
Generate FFT SystemVerilog through the AXI4 or TileLink wrapper apps:

.. code-block:: bash

   sbt "fft/runMain opera.fft.AXI4App"
   sbt "fft/runMain opera.fft.TLApp"

This generates SystemVerilog code in the ``./rtl/FFTAXI4`` folder for the AXI4 variant or ``./rtl/FFTTL`` for the TileLink variant.
Both apps also accept a JSON configuration file:

.. code-block:: bash

   sbt "fft/runMain opera.fft.AXI4App fft/src/main/resources/parameters.json"
   sbt "fft/runMain opera.fft.TLApp fft/src/main/resources/parameters.json"

Tests
-----

To run non-memory-mapped FFT tests, use the following commands in the project root folder:

.. code-block:: bash

   sbt "fft/testOnly opera.fft.BitReverseSpec"
   sbt "fft/testOnly opera.fft.SDFStageSpec"
   sbt "fft/testOnly opera.fft.FFTModelSpec"
   sbt "fft/testOnly opera.fft.FFTModelvsFloatingPointSpec"
   sbt "fft/testOnly opera.fft.FFTvsModelSpec"
   sbt "fft/testOnly opera.fft.FFTvsFloatingPointSpec"
   sbt "fft/testOnly opera.fft.FFTSQNRSpec"

Memory-mapped wrapper tests are described in :doc:`FFT Overview <../Overview/Overview>`.

Random ready/valid and large-FFT Verilator options are passed after ``--``:

.. code-block:: bash

   sbt "fft/testOnly opera.fft.FFTvsFloatingPointSpec -- -Dfft.randomReadyValid=true -Dfft.nonParallel=64 -z \"size = 64\""

Output directory is ``fft/test_run_dir/`` when tests are run from the project root.

Source Code
-----------

You can view the source code here: `fft/src on GitHub <https://github.com/opera-platform/opera-dsp/blob/main/fft/src>`_
