Padder Module
=============

The `Padder` module is a zero-padding AXI4-Stream component that ensures each chirp in a frame contains a fixed number of samples. It supports configurable chirp sizes and automatically inserts zero samples after input data to meet the desired output size.

Padder Parameters
-----------------

**Fields:**

- `MaxChirpSize`: Int - Maximum number of expected samples per chirp  
- `MaxChirpsPerFrame`: Int - Maximum number of expected chirps per frame

IOs
---

Alongside the AXI4-Stream interfaces defined by:

.. code-block:: scala

  val streamNode: AXI4StreamIdentityNode = AXI4StreamIdentityNode()

this module includes the following additional I/O signals:

.. code-block:: scala

   class PadderIO(MaxChirpSize: Int, MaxChirpsPerFrame: Int) extends Bundle {
    val i_samples         : UInt = Input(UInt(log2Ceil(MaxChirpSize+1).W))
    val i_samples_expected: UInt = Input(UInt(log2Ceil(MaxChirpSize+1).W))
    val i_chirps          : UInt = Input(UInt(log2Ceil(MaxChirpsPerFrame+1).W))
    val i_en              : Bool = Input(Bool())
  }

**Signal Descriptions:**

- ``i_samples``: Desired number of samples per chirp (after padding).
- ``i_samples_expected``: Number of samples expected from the input stream.
- ``i_chirps``: Number of chirps per frame.
- ``i_en``: Padding enable signal. When `false`, data is passed through as is.

Module Operation
----------------

The ``Padder`` module operates in two primary states: **sPass** (pass-through mode) and **sPad** (padding mode).

Data Tracking
~~~~~~~~~~~~~

Two internal registers track transmission progress:

- ``r_sent_samples``: Counts how many samples have been sent within the current chirp.
- ``r_sent_chirps``: Counts how many chirps have been completed.

State Machine
~~~~~~~~~~~~~

1. **sPass (Pass-Through Mode)**:

   - Default operational state.
   - Data from the input stream is passed directly to the output.
   - The ``out.bits.last`` signal is asserted based on:

     - Whether the received input marks the last sample (``in.bits.last``), or
     - The count of received samples matching the expected number. Last is generated when full FMCW frame is sent (number of sent samples is equal to ``i_samples*i_chirps``)

   - If padding is enabled (``i_en``) and early termination of a chirp is detected (i.e., fewer samples than required), the state machine switches to **sPad**.

2. **sPad (Padding Mode)**:

   - The module generates ``0.U`` data values to pad the chirp up to the required ``i_samples`` count.
   - Padding ends when the sample count reaches ``i_samples - 1``.
   - Once padding is complete, the module returns to **sPass** to resume regular data processing or begin the next chirp.

This control ensures well-formed output frames, even when the input data provides fewer samples than required. The logic supports dynamic reconfiguration and is robust to varying frame structures at runtime.

SystemVerilog Generation
------------------------

You can generate SystemVerilog for the `Padder` module using the provided `PadderApp`.

**Command to generate Verilog:**

.. code-block:: bash

   sbt "project preprocessing; runMain preprocessing.PadderApp"

This will generate SystemVerilog files in the `./rtl/Padder` directory.

If you want to change padding parameters, modify the values of ``MaxChirpSize`` and ``MaxChirpsPerFrame`` in `PadderApp`.

Tests
-----

To run tests, use the following command in the project root folder:

.. code-block:: bash

   sbt "project preprocessing; testOnly preprocessing.PadderSpec"

Output directory is ``./test_run_dir/``.

Source Code
-----------

You can view the source code here:

`Padder.scala on GitHub <https://github.com/opera-platform/opera-dsp/blob/main/preprocessing/src/main/scala/Padder.scala>`_
