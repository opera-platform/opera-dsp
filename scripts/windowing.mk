#####################################################
# RTL
#####################################################
rtl_windowing_axi4:
	sbt "project windowing; runMain opera.windowing.AXI4App"
	rm WindowingAXI4.sv # Remove generated empty file
rtl_windowing_tl:
	sbt "project windowing; runMain opera.windowing.TLApp"
	rm WindowingTL.sv # Remove generated empty file
rtl_windowing_all:
	sbt "project windowing; \
	runMain opera.windowing.AXI4App; \
	runMain opera.windowing.TLApp; \
	"
	rm WindowingAXI4.sv WindowingTL.sv # Remove generated empty files

#####################################################
# Tests
#####################################################
test_windowing_binPoint_axi4:
	sbt -J-Xms2048M -J-Xmx8G "project windowing; testOnly opera.windowing.WindowingBinPointAXI4Spec"
test_windowing_functions_axi4:
	sbt -J-Xms2048M -J-Xmx8G "project windowing; testOnly opera.windowing.WindowingFunctionsAXI4Spec"
test_windowing_binPoint_tl:
	sbt -J-Xms2048M -J-Xmx8G "project windowing; testOnly opera.windowing.WindowingBinPointTLSpec"
test_windowing_functions_tl:
	sbt -J-Xms2048M -J-Xmx8G "project windowing; testOnly opera.windowing.WindowingFunctionsTLSpec"
test_windowing_all:
	sbt -J-Xms2048M -J-Xmx8G "project windowing; \
	testOnly opera.windowing.WindowingBinPointAXI4Spec; \
	testOnly opera.windowing.WindowingFunctionsAXI4Spec; \
	testOnly opera.windowing.WindowingBinPointTLSpec; \
	testOnly opera.windowing.WindowingFunctionsTLSpec; \
	"

#####################################################
# docs
#####################################################
docs_windowing_html:
	cd docs/windowing; make html; mv ./build/html ../../website/docs/windowing; cd -;
docs_windowing_pdf:
	cd docs/windowing; make latexpdf; mv ./build/latex/windowing.pdf ./windowing.pdf; cd -;