CRC Module
==========

This module implements a parameterizable CRC-32 (Cyclic Redundancy Check) generator in Chisel. It allows configuration of the CRC polynomial, reflection behavior, and XOR output. Input data width can be parameterized.

CRCParameters
-------------

A Scala case class that holds the CRC configuration parameters.

**Definition:**

.. code-block:: scala

   case class CRCParameters(
     dataBytes : Int,
     polynomial: Long,
     init      : Long,
     reflectIn : Boolean,
     reflectOut: Boolean,
     xorOut    : Long
   )

**Fields:**

- ``dataBytes``: Width of input data in bytes.
- ``polynomial``: Polynomial used for CRC calculation.
- ``init``: Initial value of the CRC register.
- ``reflectIn``: If ``true``, each input byte is bit-reversed before processing.
- ``reflectOut``: If ``true``, the final CRC value is bit-reversed.
- ``xorOut``: Value XORed with the final CRC before output.

IOs
---

`CRC` has the following I/O signals:

.. code-block:: scala

   class CRCIO(dataBytes: Int) extends Bundle {
     val i_data: UInt = Input(UInt((dataBytes * 8).W))
     val i_en  : Bool = Input(Bool())
     val i_done: Bool = Input(Bool())
     val o_crc : UInt = Output(UInt(32.W))
   }

**CRCIO signal descriptions:**

- ``i_data``: Input data for CRC calculation (`dataBytes * 8` bits).
- ``i_en``: When high, triggers a CRC update with the input data.
- ``i_done``: When high, resets the CRC register to the initial value.
- ``o_crc``: Output CRC result (32-bit).

Module Operation
----------------

- When ``i_en`` is high:

  - The input ``i_data`` is split into ``dataBytes`` number of 8-bit bytes.
  - Each byte is optionally bit-reversed based on ``params.reflectIn``.
  - Each byte is then processed using a shift-XOR algorithm to update the CRC register.

- When ``i_done`` is high:

  - The CRC register is reset to the initial value defined by ``params.init``.

- The final CRC value is:

  - Bit-reversed if ``params.reflectOut`` is ``true``.
  - XORed with ``params.xorOut``.
  - Presented on the output ``o_crc``.

Note:  
The CRC is updated **only** during cycles when ``i_en`` is asserted. When ``i_done`` is asserted, the module reinitializes the CRC computation regardless of the current ``i_en`` state.

SystemVerilog Generation
------------------------

You can generate SystemVerilog for the `CRC` block.

Use the following commands in the project root folder:

.. code-block:: bash

   sbt "project preprocessing; runMain preprocessing.CRCApp"

This will generate SystemVerilog files in the ``./rtl/CRC`` directory.

If you want to modify the CRC configuration (e.g., polynomial, initial value, byte width), edit the ``params`` definition in the `CRCApp` object.

Tests
-----

To run tests, use the following command in the project root folder:

.. code-block:: bash

   sbt "project preprocessing; testOnly preprocessing.CRCSpec"

Output directory is ``./test_run_dir/``.

Source Code
-----------

You can view the source code here:

`CRC.scala on GitHub <https://github.com/opera-platform/opera-dsp/blob/main/preprocessing/src/main/scala/CRC.scala>`_
