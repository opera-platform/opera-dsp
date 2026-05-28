Cell-Averaging CFAR Module
==========================

The Cell-Averaging CFAR family implements CA-CFAR, Greatest-Of Cell Averaging (GOCA), and Smallest-Of Cell Averaging (SOCA) detection. The unified :doc:`CFAR <../Overview/Overview>` top level generates this family when ``CFARParams.cfarType`` is ``CFARType.CellAveraging``.

Cell-Averaging CFAR Parameters
------------------------------

This block uses the shared :doc:`CFARParams <../Overview/Overview>` definition.

The Cell-Averaging family uses these runtime controls in addition to the common frame and threshold controls:

- ``i_reference_cells``: active reference cells on each side of the CUT
- ``i_guard_cells``: active guard cells on each side of the CUT
- ``i_noise_div_shift``: right shift applied to each side reference sum before side estimates are combined
- ``i_cfar_mode``: selects CA-CFAR, GOCA, or SOCA

For each CUT, the active window is:

.. code-block:: text

   [ left references | left guards | CUT | right guards | right references ]

``maxReferenceCells`` must be a positive power of two, ``maxGuardCells`` must be positive, and ``maxReferenceCells`` must be greater than ``maxGuardCells`` for this family. At runtime, ``i_fft_size`` must be large enough to contain both reference windows, both guard windows, and the CUT.

IOs
---

``CACFAR`` uses the shared ``CFARIO`` stream and configuration interface described in :doc:`CFAR Overview <../Overview/Overview>`.

``i_noise_div_shift`` is present only for the Cell-Averaging family. ``i_order_rank_left`` and ``i_order_rank_right`` are not generated for this family.

``i_data`` and ``o_data`` use ready/valid handshakes. ``i_last`` must mark the final input sample of the active frame, and ``o_last`` marks the final output result.

Module Operation
----------------

The streaming CA path forms rolling left and right reference sums. Each side is converted to an estimate with ``i_noise_div_shift``:

:math:`\text{leftAverage} = \text{leftSum} \gg \text{i\_noise\_div\_shift}`

:math:`\text{rightAverage} = \text{rightSum} \gg \text{i\_noise\_div\_shift}`

The selected mode combines the side estimates:

- CA-CFAR: :math:`(\text{leftAverage} + \text{rightAverage}) / 2`
- GOCA: :math:`\max(\text{leftAverage}, \text{rightAverage})`
- SOCA: :math:`\min(\text{leftAverage}, \text{rightAverage})`

In linear mode, the threshold is ``noiseEstimate * i_threshold_scale``. In log mode, the threshold is ``noiseEstimate + i_threshold_scale``.

``peak`` is true when the CUT is greater than the threshold. When peak grouping is enabled, the CUT must also be greater than its neighbouring samples.

Static ``SuppressEdges`` and ``OneSidedAverage`` policies use the streaming core. Static ``WrapAroundFrame`` uses the cyclic replay core. When ``runtimeEdgePolicy`` is enabled, both paths are generated and each frame is routed according to the loaded edge policy.

SystemVerilog Generation
------------------------

The direct Cell-Averaging path can be elaborated through the unified ``CFAR`` top level:

.. code-block:: scala

   import circt.stage.ChiselStage
   import fixedpoint._
   import opera.cfar._

   ChiselStage.emitSystemVerilog(
     new CFAR(CFARParams.fixed(cfarType = CFARType.CellAveraging))
   )

For standalone AXI4 or TileLink wrappers, use the generator apps described in :ref:`memory-mapped CFAR wrapper generation <memory-mapped-cfar>`.

Tests
-----

To run Cell-Averaging CFAR tests, use the following commands in the project root folder:

.. code-block:: bash

   sbt "cfar/testOnly opera.cfar.CFARSpec"
   sbt "cfar/testOnly opera.cfar.CFARFamilySpec"
   sbt "cfar/testOnly opera.cfar.CACFARLinearWindowProviderSpec"
   sbt "cfar/testOnly opera.cfar.CyclicWindowProviderSpec"
   sbt "cfar/testOnly opera.cfar.CFARDelayCellsSpec"

These tests compare hardware against ``CFARModel`` across CA modes, edge policies, runtime configuration, wrap-around frames, output pipelining, and randomized ready/valid traffic.

Source Code
-----------

You can view the source code here: `Cell-Averaging CFAR source on GitHub <https://github.com/opera-platform/opera-dsp/tree/main/cfar/src/main/scala>`_
