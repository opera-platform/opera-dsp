Swap Module
===========

The `Swap` module is an AXI4-Stream data reformatter that combines the current and previous input data words to produce a wider output word. Its behavior is controlled by a 2-bit format selector, which determines how the two samples are combined—supporting zero-padded real-only output, interleaved complex data, or raw concatenation.

This module is useful for preparing streaming data for FFT, I/Q processing, or any context where a wider output word needs to be formed from consecutive input samples.

Swap Parameters
---------------

- ``outDataWidth``: Width of the output stream in bytes.  
  The output must be exactly twice the input width. This constraint is enforced by a hardware assertion.

IOs
---

The AXI4-Stream interface is defined as:

.. code-block:: scala

   val slaveNode = AXI4StreamSlaveNode(AXI4StreamSlaveParameters())
   val masterNode = AXI4StreamMasterNode(AXI4StreamMasterParameters(name = "streamNode", n = outDataWidth))
   val streamNode = NodeHandle(slaveNode, masterNode)

The module also includes a control bundle:

.. code-block:: scala

   class SwapIO extends Bundle {
     val i_format: UInt = Input(UInt(2.W))
     val i_en    : Bool = Input(Bool())
   }

**Signal Descriptions:**

- ``i_format``: Selects how the output data is constructed. Supported formats:

  - ``0x0``: **Complex 1x** - zero-padded real input  
    - Pads the upper half of the output with zeros: ``{'0, input}``.

  - ``0x1``: **Complex 2x** - real and imaginary interleaving  
    - If ``i_en = 1``: output is ``{previous_input, current_input}``  
    - If ``i_en = 0``: output is ``{current_input, previous_input}``

  - ``0x2`` or ``0x3``: **Raw concatenation** (default)  
    - Output is always ``{current_input, previous_input}``

- ``i_en``: Controls the order of concatenation in Complex 2x mode.

Module Operation
----------------

- The module registers every valid input word into an internal register ``r_data`` when ``in.fire`` is asserted.
- It uses a 1-bit counter ``r_counter`` to control when output is valid (every second input word).

- Output format is determined by ``i_format``:
  
  - ``0x0`` **(Complex 1x)**:
    - Outputs ``{0.U, in.bits.data}``
    - Valid every cycle
  
  - ``0x1`` **(Complex 2x)**:
    - Swaps the order of ``{r_data, in.bits.data}`` or ``{in.bits.data, r_data}`` based on ``i_en``
    - Valid only when ``r_counter === 1``

  - ``0x2`` or ``0x3`` **(Raw mode)**:
    - Always outputs ``{in.bits.data, r_data}``
    - Valid only when ``r_counter === 1``

- A hardware assertion ensures the output width is double the input width.
- Standard AXI4-Stream handshake signals (``ready``, ``valid``, ``last``) are preserved and propagated.

SystemVerilog Generation
------------------------

To generate SystemVerilog for the `Swap` block, run the following from the project root:

.. code-block:: bash

   sbt "project preprocessing; runMain opera.preprocessing.SwapApp"

This will create Verilog files in the ``./rtl/Swap`` directory.

By changing the parameter ``outDataWidth``, you can change the output data width.

Tests
-----

To run tests, use the following command in the project root folder:

.. code-block:: bash

   sbt "project preprocessing; testOnly opera.preprocessing.SwapSpec"

Output directory is ``./test_run_dir/``.

Source Code
-----------

You can view the source code here:

`Swap.scala on GitHub <https://github.com/opera-platform/opera-dsp/blob/main/preprocessing/src/main/scala/Swap.scala>`_
