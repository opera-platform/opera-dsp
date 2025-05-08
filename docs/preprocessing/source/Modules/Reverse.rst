Reverse Module
==============

The `Reverse` module is a simple AXI4-Stream transformer that conditionally reverses the bits of each data word. When enabled via the ``i_en`` signal, the module applies bitwise reversal to each incoming data word before passing it to the output. When disabled, the data stream passes through unchanged.

IOs
---

Alongside the AXI4-Stream interfaces defined by:

.. code-block:: scala

  val streamNode: AXI4StreamIdentityNode = AXI4StreamIdentityNode()

this module includes the following additional I/O signals:

.. code-block:: scala

   class ReverseIO extends Bundle {
     val i_en: Bool = Input(Bool())
   }

**ReverseIO signal descriptions:**

- ``i_en``: When ``true``, bit reversal is applied to incoming data.

Module Operation
----------------

- The module continuously forwards AXI4-Stream traffic from its input to output.
- When ``i_en`` is ``true``, the data word on ``in.bits.data`` is reversed bitwise and assigned to ``out.bits.data``.
- When ``i_en`` is ``false``, the input data is passed through unchanged.
- All other AXI4-Stream signals (``valid``, ``ready``, ``last``, etc.) are forwarded transparently using ``out <> in``.

SystemVerilog Generation
------------------------

You can generate SystemVerilog for the `Reverse` block.

Use the following commands in the project root folder:

.. code-block:: bash

   sbt "project preprocessing; runMain preprocessing.ReverseApp"

This will generate SystemVerilog files in the ``./rtl/Reverse`` directory.

Tests
-----

To run tests, use the following command in the project root folder:

.. code-block:: bash

   sbt "project preprocessing; testOnly preprocessing.ReverseSpec"

Output directory is ``./test_run_dir/``.

Source Code
-----------

You can view the source code here:

`Reverse.scala on GitHub <https://github.com/opera-platform/opera-dsp/blob/main/preprocessing/src/main/scala/Reverse.scala>`_
