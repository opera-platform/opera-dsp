#####################################################
# RTL
#####################################################
rtl_windowing_axi4:
	sbt "project windowing; runMain windowing.AXI4App"
	rm WindowingAXI4.sv # Remove generated empty file
rtl_windowing_tl:
	sbt "project windowing; runMain windowing.TLApp"
	rm WindowingTL.sv # Remove generated empty file
rtl_windowing_all:
	sbt "project windowing; \
	runMain windowing.AXI4App; \
	runMain windowing.TLApp; \
	"
	rm WindowingAXI4.sv WindowingTL.sv # Remove generated empty files

#####################################################
# Tests
#####################################################
test_windowing_axi4:
	sbt "project windowing; testOnly windowing.WindowingAXI4Spec"
test_windowing_tl:
	sbt "project windowing; testOnly windowing.WindowingTLSpec"
test_windowing_all:
	sbt "project windowing; \
	testOnly windowing.WindowingAXI4Spec; \
	testOnly windowing.WindowingTLSpec; \
	"

#####################################################
# docs
#####################################################
docs_windowing_html:
	cd docs/windowing; make html; cd -;
docs_windowing_pdf:
	cd docs/windowing; make latexpdf; mv ./build/latex/windowing.pdf ./windowing.pdf; cd -;