#####################################################
# RTL
#####################################################
rtl_cfar_axi4:
	sbt "project cfar; runMain opera.cfar.AXI4App"
	rm CFARAXI4.sv # Remove generated empty file
rtl_cfar_tl:
	sbt "project cfar; runMain opera.cfar.TLApp"
	rm CFARTL.sv # Remove generated empty file
rtl_cfar_all:
	sbt "project cfar; \
	runMain opera.cfar.AXI4App; \
	runMain opera.cfar.TLApp; \
	"
	rm CFARAXI4.sv CFARTL.sv # Remove generated empty files

#####################################################
# Tests
#####################################################
test_cfar:
	sbt -J-Xms2048M -J-Xmx8G "project cfar; testOnly opera.cfar.CFARSpec"
test_cfar_family:
	sbt -J-Xms2048M -J-Xmx8G "project cfar; testOnly opera.cfar.CFARFamilySpec"
test_gos_cfar:
	sbt -J-Xms2048M -J-Xmx8G "project cfar; testOnly opera.cfar.GOSCFARSpec"
test_cfar_delay_cells:
	sbt -J-Xms2048M -J-Xmx8G "project cfar; testOnly opera.cfar.CFARDelayCellsSpec"
test_cfar_axi4:
	sbt -J-Xms2048M -J-Xmx8G "project cfar; testOnly opera.cfar.MemoryMappedAXI4CFARSpec"
test_cfar_tl:
	sbt -J-Xms2048M -J-Xmx8G "project cfar; testOnly opera.cfar.MemoryMappedTLCFARSpec"
test_cacfar_linear_window_provider:
	sbt -J-Xms2048M -J-Xmx8G "project cfar; testOnly opera.cfar.CACFARLinearWindowProviderSpec"
test_cyclic_window_provider:
	sbt -J-Xms2048M -J-Xmx8G "project cfar; testOnly opera.cfar.CyclicWindowProviderSpec"
test_gos_cfar_linear_rank_provider:
	sbt -J-Xms2048M -J-Xmx8G "project cfar; testOnly opera.cfar.GOSCFARLinearRankProviderSpec"
test_cnt_sorter_cell:
	sbt -J-Xms2048M -J-Xmx8G "project cfar; testOnly opera.lis.CntSorterCellSpec"
test_lis_streaming_model:
	sbt -J-Xms2048M -J-Xmx8G "project cfar; testOnly opera.lis.LISStreamingModelSpec"
test_lis_streaming_sorter:
	sbt -J-Xms2048M -J-Xmx8G "project cfar; testOnly opera.lis.LISStreamingSorterSpec"
test_reg_sorter_cell_and_network:
	sbt -J-Xms2048M -J-Xmx8G "project cfar; testOnly opera.lis.RegSorterCellAndNetworkSpec"

test_cfar_all:
	sbt -J-Xms2048M -J-Xmx8G "project cfar; \
	testOnly opera.cfar.CFARSpec; \
	testOnly opera.cfar.CFARFamilySpec; \
	testOnly opera.cfar.GOSCFARSpec; \
	testOnly opera.cfar.CFARDelayCellsSpec; \
	testOnly opera.cfar.MemoryMappedAXI4CFARSpec; \
	testOnly opera.cfar.MemoryMappedTLCFARSpec; \
	testOnly opera.cfar.CACFARLinearWindowProviderSpec; \
	testOnly opera.cfar.CyclicWindowProviderSpec; \
	testOnly opera.cfar.GOSCFARLinearRankProviderSpec; \
	testOnly opera.lis.CntSorterCellSpec; \
	testOnly opera.lis.LISStreamingModelSpec; \
	testOnly opera.lis.LISStreamingSorterSpec; \
	testOnly opera.lis.RegSorterCellAndNetworkSpec; \
	"

#####################################################
# docs
#####################################################
docs_cfar_html:
	cd docs/cfar; make html; mkdir -p ../../website/docs/cfar; cp -R ./build/html/. ../../website/docs/cfar; cd -;
docs_cfar_pdf:
	cd docs/cfar; make latexpdf; mv ./build/latex/cfar.pdf ./cfar.pdf; cd -;
