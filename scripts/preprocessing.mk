#####################################################
# RTL
#####################################################
rtl_preprocessing_axi4:
	sbt "project preprocessing; runMain preprocessing.AXI4App"
	rm PreProcessingAXI4.sv # Remove generated empty file
rtl_preprocessing_tl:
	sbt "project preprocessing; runMain preprocessing.TLApp"
	rm PreProcessingTL.sv # Remove generated empty file
rtl_crc:
	sbt "project preprocessing; runMain preprocessing.CRCApp"
	rm CRC.sv # Remove generated empty file
rtl_checkercrc:
	sbt "project preprocessing; runMain preprocessing.CheckerCRCApp"
	rm CheckerCRC.sv # Remove generated empty file
rtl_reverse:
	sbt "project preprocessing; runMain preprocessing.ReverseApp"
	rm Reverse.sv # Remove generated empty file
rtl_swap:
	sbt "project preprocessing; runMain preprocessing.SwapApp"
	rm Swap.sv # Remove generated empty file
rtl_padder:
	sbt "project preprocessing; runMain preprocessing.PadderApp"
	rm Padder.sv # Remove generated empty file
rtl_preprocessing_all:
	sbt "project preprocessing; \
	runMain preprocessing.CRCApp; \
	runMain preprocessing.CheckerCRCApp; \
	runMain preprocessing.ReverseApp; \
	runMain preprocessing.SwapApp; \
	runMain preprocessing.PadderApp; \
	runMain preprocessing.AXI4App; \
	runMain preprocessing.TLApp; \
	"
	rm PreProcessingAXI4.sv PreProcessingTL.sv CRC.sv CheckerCRC.sv Reverse.sv Swap.sv Padder.sv # Remove generated empty files

#####################################################
# Tests
#####################################################
test_preprocessing:
	sbt "project preprocessing; testOnly preprocessing.PreProcessingSpec"
test_crc:
	sbt "project preprocessing; testOnly preprocessing.CRCSpec"
test_checkercrc:
	sbt "project preprocessing; testOnly preprocessing.CheckerCRCSpec"
test_reverse:
	sbt "project preprocessing; testOnly preprocessing.ReverseSpec"
test_swap:
	sbt "project preprocessing; testOnly preprocessing.SwapSpec"
test_padder:
	sbt "project preprocessing; testOnly preprocessing.PadderSpec"
test_preprocessing_all:
	sbt "project preprocessing; \
	testOnly preprocessing.CRCSpec; \
	testOnly preprocessing.CheckerCRCSpec; \
	testOnly preprocessing.ReverseSpec; \
	testOnly preprocessing.SwapSpec; \
	testOnly preprocessing.PadderSpec; \
	testOnly preprocessing.PreProcessingSpec; \
	"

#####################################################
# docs
#####################################################
docs_preprocessing_html:
	cd docs/preprocessing; make html; cd -;
docs_preprocessing_pdf:
	cd docs/preprocessing; make latexpdf; mv ./build/latex/preprocessing.pdf ./preprocessing.pdf; cd -;