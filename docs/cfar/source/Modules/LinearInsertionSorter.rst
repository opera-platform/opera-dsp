Linear Insertion Sorter Module
==============================

The Linear Insertion Sorter (LIS) is the streaming sliding-window sorter used by the ordered-statistic CFAR family. It accepts one sample per beat, keeps the active FIFO window sorted in ascending order, and exposes the sorted window for rank selection.

The maintained implementation lives in the ``opera.lis`` package under the CFAR source tree.

LIS Parameters
--------------

.. code-block:: scala

  case class LISParams[T <: Data: Real](
    dataType     : T,
    maxWindowSize: Int,
    sorterType   : String = LISType.CntBased,
    runTime      : Boolean = false
  )

**Parameter descriptions:**

- ``dataType``  
  Input and output sample type.

- ``maxWindowSize``  
  Maximum number of samples kept in the sorted sliding window.

- ``sorterType``  
  Selects ``LISType.CntBased`` or ``LISType.RegBased``.

- ``runTime``  
  If ``true``, exposes ``i_window_size`` so the active window size can be reduced at runtime.

**Requirements:**

- ``dataType`` must be a Chisel type.
- ``maxWindowSize`` must be greater than zero.
- ``sorterType`` must be one of the values in ``LISType.all``.

IOs
---

``LIS`` uses ``LISIO``:

.. code-block:: scala

  class LISIO[T <: Data: Real](params: LISParams[T]) extends Bundle {
    val i_data        = Flipped(Decoupled(params.dataType))
    val i_last        = Input(Bool())
    val o_data        = Decoupled(params.dataType)
    val o_last        = Output(Bool())
    val o_sorted_data = Output(Vec(params.maxWindowSize, params.dataType))
    val o_sorter_full = Output(Bool())
    val i_window_size = if (params.runTime) Some(Input(UInt(log2Ceil(params.maxWindowSize + 1).W))) else None
  }

``o_sorted_data(0)`` is the smallest active sample. ``o_data`` emits the FIFO-evicted sample once the window is full and drains remaining samples during frame flush. ``i_last`` starts the flush sequence, and ``o_last`` marks the final flushed beat.

Module Operation
----------------

``LIS`` is a wrapper that selects one of the available sorter implementations:

- ``CntBasedLIS`` stores samples with FIFO position counters and removes the outgoing sample with mask logic.
- ``RegBasedLIS`` stores a sorted register vector and updates it with a shift network.
- ``maxWindowSize == 1`` uses a single-lane path without a sorter network.

Both sorter architectures have the same external behavior. ``GOSCFARLinearRankProvider`` instantiates ``LIS`` with ``runTime = true`` so ``i_reference_cells`` controls the active rank-window size per frame.

SystemVerilog Generation
------------------------

The LIS block can be elaborated directly with ``ChiselStage``:

.. code-block:: scala

   import circt.stage.ChiselStage
   import fixedpoint._
   import opera.lis._

   ChiselStage.emitSystemVerilog(
     new LIS(
       LISParams(
         FixedPoint(16.W, 8.BP),
         maxWindowSize = 16,
         sorterType = LISType.CntBased
       )
     )
   )

Tests
-----

To run LIS tests, use the following commands in the project root folder:

.. code-block:: bash

   sbt "cfar/testOnly opera.lis.LISStreamingSorterSpec"
   sbt "cfar/testOnly opera.lis.LISStreamingModelSpec"
   sbt "cfar/testOnly opera.lis.CntSorterCellSpec"
   sbt "cfar/testOnly opera.lis.RegSorterCellAndNetworkSpec"

The LIS tests compare both sorter architectures against a streaming reference model across runtime and static window sizes, randomized ready/valid stalls, frame flushes, duplicate values, and numeric corner cases.

Source Code
-----------

You can view the source code here: `LIS source on GitHub <https://github.com/opera-platform/opera-dsp/tree/main/cfar/src/main/scala/lis>`_
