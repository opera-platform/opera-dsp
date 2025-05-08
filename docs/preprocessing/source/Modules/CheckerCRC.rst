CheckerCRC Module
=================

The `CheckerCRC` module is a wrapper around the :doc:`CRC <CRC>` module that performs runtime CRC checking on AXI4-Stream data. It counts incoming data samples, compares them against expected values, and verifies the received CRC against a calculated one. `CheckerCRC` expects 2-byte-wide input data.


CheckerCRC Parameters
---------------------

`CheckerCRC` parameters are:

- ``CRCParameters``: These define CRC width, polynomial, and reflection behavior.

  - ``dataBytes``: Width of input data in bytes. `CheckerCRC` supports only ``dataBytes = 1, 2 or 4``.
  - ``polynomial``: Polynomial used for CRC calculation.
  - ``init``: Initial value of the CRC register.
  - ``reflectIn``: If ``true``, each input byte is bit-reversed before processing.
  - ``reflectOut``: If ``true``, the final CRC value is bit-reversed.
  - ``xorOut``: Value XORed with the final CRC before output.

- ``samplesBeforeCRC``: Maximum number of samples before the CRC is sent


IOs
---

Alongside the AXI4-Stream interfaces defined by:
 
 .. code-block:: scala

  val streamNode: AXI4StreamIdentityNode = AXI4StreamIdentityNode()
  
this module includes the following additional I/O signals:

.. code-block:: scala

   class CheckerCRCIO(samplesBeforeCRC: Int) extends Bundle {
    val i_en              : Bool = Input(Bool())
    val i_crc_data        : Bool = Input(Bool())
    val i_samples_expected: UInt = Input(UInt(log2Ceil(samplesBeforeCRC + 1).W))
    val o_crc             : UInt = Output(UInt(32.W))
    val o_error           : Bool = Output(Bool())
    val o_crc_valid       : Bool = Output(Bool())
  }

**CheckerCRCIO signal descriptions:**

- ``i_en``               : When high, CRC calculation is active.
- ``i_crc_data``         : Signals that incoming data corresponds to CRC content.
- ``i_samples_expected`` : Specifies how many data samples to process before CRC data is sent via the input stream.
- ``o_crc``              : The CRC value computed by the internal module. Considered valid when ``o_crc_valid`` is high.
- ``o_error``            : Asserted when a mismatch occurs between the received and calculated CRC. Valid only when ``o_crc_valid`` is asserted.
- ``o_crc_valid``        : Indicates that a CRC check has been completed and the output values are valid.



Module operation
----------------

`CheckerCRC` wraps an internal :doc:`CRC <CRC>` module and provides the following functionality:

- CRC calculation and comparison are performed only when ``i_en`` is asserted.
- The CRC is computed over ``i_samples_expected`` data samples or until ``i_crc_data`` is asserted (two words are needed to receive CRC data). Once this phase completes:

  - The following two data words are interpreted as the received CRC (alternatively, ``i_crc_data`` must be asserted for two consecutive cycles).
  - The first word is latched into a register.
  - The second word is concatenated with the first to reconstruct the received CRC.
  - The reconstructed CRC is then compared against the internally calculated value.
  - Any mismatch between the two is flagged via ``o_error``.

The ``i_crc_data`` input can be tied to 0. In this case, `CheckerCRC` determines when the CRC is expected based on the value provided by the ``i_samples_expected`` input.


SystemVerilog Generation
------------------------

You can generate SystemVerilog for the `CheckerCRC` block.

Use the following commands in the project root folder:

.. code-block:: bash

  sbt "project preprocessing; runMain preprocessing.CheckerCRCApp"

This will produce SystemVerilog files in the ``./rtl/CheckerCRC`` directory.

To customize CRC parameters or change the number of samples before CRC validation, edit the ``params`` and ``samplesBeforeCRC`` arguments in `CheckerCRCApp`.

Tests
-----

To run tests, use the following command in the project root folder:

.. code-block:: bash

   sbt "project preprocessing; testOnly preprocessing.CheckerCRCSpec"

Output directory is ``./test_run_dir/``.

Source Code
-----------

You can view the source code here:

`CheckerCRC.scala on GitHub <https://github.com/opera-platform/opera-dsp/blob/main/preprocessing/src/main/scala/CheckerCRC.scala>`_
