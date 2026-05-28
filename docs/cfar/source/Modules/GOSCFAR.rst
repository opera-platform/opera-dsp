GOS-CFAR Module
===============

The GOS-CFAR family uses ordered reference-cell values instead of averaged reference sums. The unified :doc:`CFAR <../Overview/Overview>` top level generates this family when ``CFARParams.cfarType`` is ``CFARType.OrderedStatistic``.

GOS-CFAR Parameters
-------------------

This block uses the shared :doc:`CFARParams <../Overview/Overview>` definition.

Ordered-statistic CFAR uses these runtime controls in addition to the common frame and threshold controls:

- ``lisType``: selects the linear insertion sorter implementation, ``LISType.CntBased`` or ``LISType.RegBased``
- ``i_reference_cells``: active reference cells on each side of the CUT
- ``i_guard_cells``: active guard cells on each side of the CUT
- ``i_order_rank_left``: one-based ascending rank selected from the left reference side
- ``i_order_rank_right``: one-based ascending rank selected from the right reference side
- ``i_cfar_mode``: selects GOS-CA, GOS-GO, or GOS-SO

``i_noise_div_shift`` is not generated for this family because the noise estimate is selected by rank.

IOs
---

``GOSCFAR`` uses the shared ``CFARIO`` stream and configuration interface described in :doc:`CFAR Overview <../Overview/Overview>`.

``i_order_rank_left`` and ``i_order_rank_right`` are generated only for ordered-statistic CFAR. Each rank is one-based, must be at least 1, and must be no larger than the active ``i_reference_cells`` value.

``i_data`` and ``o_data`` use ready/valid handshakes. ``i_last`` must mark the final input sample of the active frame, and ``o_last`` marks the final output result.

Module Operation
----------------

For each side of the CUT, the active reference cells are sorted in ascending order. The configured rank selects one value from each side:

:math:`\text{leftOrder} = \text{sort(leftRefs)}[\text{i\_order\_rank\_left} - 1]`

:math:`\text{rightOrder} = \text{sort(rightRefs)}[\text{i\_order\_rank\_right} - 1]`

The selected mode combines the ranked side estimates:

- GOS-CA: :math:`(\text{leftOrder} + \text{rightOrder}) / 2`
- GOS-GO: :math:`\max(\text{leftOrder}, \text{rightOrder})`
- GOS-SO: :math:`\min(\text{leftOrder}, \text{rightOrder})`

Threshold scaling, peak comparison, optional log mode, peak grouping, and edge policies match the Cell-Averaging family.

The non-wrap streaming path uses :doc:`Linear Insertion Sorter <LinearInsertionSorter>` instances for the left and right reference windows. The wrap-around path buffers and replays the frame, builds wrapped reference vectors, and selects runtime ranks from those vectors.

SystemVerilog Generation
------------------------

The direct ordered-statistic path can be elaborated through the unified ``CFAR`` top level:

.. code-block:: scala

   import circt.stage.ChiselStage
   import fixedpoint._
   import opera.cfar._
   import opera.lis.LISType

   ChiselStage.emitSystemVerilog(
     new CFAR(
       CFARParams.fixed(
         cfarType = CFARType.OrderedStatistic,
         lisType = LISType.CntBased
       )
     )
   )

For standalone AXI4 or TileLink wrappers, use the generator apps described in :ref:`memory-mapped CFAR wrapper generation <memory-mapped-cfar>`.

Tests
-----

To run GOS-CFAR tests, use the following commands in the project root folder:

.. code-block:: bash

   sbt "cfar/testOnly opera.cfar.GOSCFARSpec"
   sbt "cfar/testOnly opera.cfar.GOSCFARLinearRankProviderSpec"
   sbt "cfar/testOnly opera.lis.LISStreamingSorterSpec"

These tests compare hardware against ``CFARModel`` across sorter architectures, GOS modes, per-side ranks, edge policies, runtime edge routing, log mode, output pipelining, and randomized ready/valid traffic.

Source Code
-----------

You can view the source code here: `GOS-CFAR source on GitHub <https://github.com/opera-platform/opera-dsp/tree/main/cfar/src/main/scala>`_
