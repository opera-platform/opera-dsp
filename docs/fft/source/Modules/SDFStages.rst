SDFStage Module
===============

``SDFStage`` is the shared internal delay-feedback stage used by :doc:`R2FFT and R22FFT <FFT>`.
Both FFT cores instantiate the same hardware module and select the radix-specific schedule with ``RadixParams.sdfRadix``.

The ``SDFStageSpec`` test suite runs the shared stage across both ``Radix2`` and ``Radix22`` schedules.

RadixParams
-----------

**Definition:**

.. code-block:: scala

  case class RadixParams (
    inDataType   : DspComplex[FixedPoint],
    outDataType  : DspComplex[FixedPoint],
    twiddleType  : DspComplex[FixedPoint],
    stageSize    : Int,
    decimation   : DecimationType,
    sdfRadix     : SDFRadix,
    overflowReg  : Boolean,
    divBy2Reg    : Boolean,
    divBy2       : Boolean,
    growEnable   : Boolean,
    latency      : Int,
    addPipeRegs  : Int,
    mulPipeRegs  : Int,
    dspMul4      : Boolean,
    delay        : Int,
    bufferAsMem  : Boolean,
    singlePortMem: Boolean,
    trimType     : TrimType,
  )

**Parameter descriptions:**

- ``inDataType``  
  Complex FixedPoint input type for the stage.

- ``outDataType``  
  Complex FixedPoint output type for the stage.

- ``twiddleType``  
  Complex FixedPoint type used for twiddle coefficients in the parent FFT core.

- ``stageSize``  
  FFT stage size. This value must be a power of two.

- ``decimation``  
  Selects ``DIF`` or ``DIT`` stage control timing.

- ``sdfRadix``  
  Selects the radix schedule. ``Radix2`` uses a zero counter init. ``Radix22`` uses a zero counter init for ``DIF`` and an offset init for ``DIT``.

- ``overflowReg``  
  Enables overflow status output.

- ``divBy2Reg``  
  Enables runtime divide-by-two control input.

- ``divBy2``  
  Static divide-by-two control used when ``divBy2Reg`` is disabled.

- ``growEnable``  
  Indicates whether this stage grows output width by one bit.

- ``latency``  
  Latency of the twiddle multiplication path in the parent FFT core.

- ``addPipeRegs``  
  Number of pipeline registers after stage add/subtract operations.

- ``mulPipeRegs``  
  Number of pipeline registers after multiplication operations in the parent FFT core.

- ``dspMul4``  
  Indicates whether the parent FFT core uses the four-real-multiplier complex multiplier implementation.

- ``delay``  
  Delay line depth. ``SDFStage`` requires ``delay = stageSize / 2``.

- ``bufferAsMem``  
  If ``true``, the delay buffer uses memory-backed storage.

- ``singlePortMem``  
  If ``true``, eligible memory-backed buffers use single-port memory.

- ``trimType``  
  Trim mode used after stage butterfly scaling.

**Requirements:**

- ``stageSize`` must be a power of two.
- ``delay`` must be a power of two.
- ``SDFStage`` requires ``delay = stageSize / 2``.

IOs
---

``SDFStage`` has the following I/O signals:

.. code-block:: scala

  class RadixIO(params: RadixParams) extends Bundle {
    val in : DecoupledIO[DspComplex[FixedPoint]] = Flipped(Decoupled(params.inDataType))
    val out: DecoupledIO[DspComplex[FixedPoint]] = Decoupled(params.outDataType)

    val o_counter: UInt = Output(UInt(log2Ceil(params.stageSize).W))

    val i_divBy2  : Option[Bool] = if (params.divBy2Reg)   Some(Input(Bool()))  else None
    val o_overflow: Option[Bool] = if (params.overflowReg) Some(Output(Bool())) else None
  }

- ``in`` and ``out`` are Decoupled complex data streams.
- ``o_counter`` exposes the stage phase counter to the parent FFT core.
- ``i_divBy2`` controls stage scaling when runtime scaling is enabled.
- ``o_overflow`` reports overflow when overflow status is enabled.

Module Operation
----------------

Each SDF stage stores half of the stage samples in a delay buffer and combines delayed data with the current input using the shared ``Butterfly`` helper.
The butterfly outputs are scaled by ``Utils.scaleButterfly`` using the configured divide-by-two and growth controls.

The stage then either feeds one butterfly result back into the delay line or forwards the other result to the next stage.
The selected output is delayed by the configured add pipeline registers.

The stage is Decoupled and stallable.
When ``out.ready`` is low, the stage holds its data, control delays, counter, and delay-buffer state.
The stage accepts input only when the output side can advance.

``SDFStage`` selects its counter init from ``sdfRadix``:

- ``Radix2`` starts at zero.
- ``Radix22`` starts at zero for ``DIF``.
- ``Radix22`` starts at ``(stageSize / 2 + 1) & (stageSize - 1)`` for ``DIT``.

``R2FFT`` and ``R22FFT`` both instantiate ``SDFStage``.
``R22FFT`` adds radix-2\ :sup:`2` control logic, trivial rotations, and shared twiddle multipliers around the same stage module.

SystemVerilog Generation
------------------------

``SDFStage`` is an internal module and does not have a standalone App object.
It is generated as part of the FFT AXI4 or TileLink top-level wrappers.

Use the following commands in the project root folder:

.. code-block:: bash

   sbt "fft/runMain opera.fft.AXI4App"
   sbt "fft/runMain opera.fft.TLApp"

Tests
-----

To run SDF stage tests, use the following commands in the project root folder:

.. code-block:: bash

   sbt "fft/testOnly opera.fft.SDFStageSpec"

This suite instantiates ``SDFStage`` with both ``Radix2`` and ``Radix22`` parameters.
It covers model matching, R22 DIT counter initialization, pipeline alignment, seeded raw vectors, fixed-point formats and scaling, runtime divide-by-two control, overflow behavior, output stalls, SRAM-backed delay storage, and reset between chirps.

Output directory is ``fft/test_run_dir/`` when tests are run from the project root.

Source Code
-----------

You can view the source code here:

`SDFStage.scala on GitHub <https://github.com/opera-platform/opera-dsp/blob/main/fft/src/main/scala/SDFStage.scala>`_

`RadixParams.scala on GitHub <https://github.com/opera-platform/opera-dsp/blob/main/fft/src/main/scala/RadixParams.scala>`_

`RadixIO.scala on GitHub <https://github.com/opera-platform/opera-dsp/blob/main/fft/src/main/scala/RadixIO.scala>`_
