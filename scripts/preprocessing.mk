#####################################################
# RTL
#####################################################
rtl_preprocessing_axi4:
	sbt "project preprocessing; runMain opera.preprocessing.AXI4App"
	rm PreProcessingAXI4.sv # Remove generated empty file
rtl_preprocessing_tl:
	sbt "project preprocessing; runMain opera.preprocessing.TLApp"
	rm PreProcessingTL.sv # Remove generated empty file
rtl_crc:
	sbt "project preprocessing; runMain opera.preprocessing.CRCApp"
	rm CRC.sv # Remove generated empty file
rtl_checkercrc:
	sbt "project preprocessing; runMain opera.preprocessing.CheckerCRCApp"
	rm CheckerCRC.sv # Remove generated empty file
rtl_reverse:
	sbt "project preprocessing; runMain opera.preprocessing.ReverseApp"
	rm Reverse.sv # Remove generated empty file
rtl_swap:
	sbt "project preprocessing; runMain opera.preprocessing.SwapApp"
	rm Swap.sv # Remove generated empty file
rtl_padder:
	sbt "project preprocessing; runMain opera.preprocessing.PadderApp"
	rm Padder.sv # Remove generated empty file
rtl_preprocessing_all:
	sbt "project preprocessing; \
	runMain opera.preprocessing.CRCApp; \
	runMain opera.preprocessing.CheckerCRCApp; \
	runMain opera.preprocessing.ReverseApp; \
	runMain opera.preprocessing.SwapApp; \
	runMain opera.preprocessing.PadderApp; \
	runMain opera.preprocessing.AXI4App; \
	runMain opera.preprocessing.TLApp; \
	"
	rm PreProcessingAXI4.sv PreProcessingTL.sv CRC.sv CheckerCRC.sv Reverse.sv Swap.sv Padder.sv # Remove generated empty files

#####################################################
# Tests
#####################################################
test_preprocessing:
	sbt "project preprocessing; testOnly opera.preprocessing.PreProcessingSpec"
test_crc:
	sbt "project preprocessing; testOnly opera.preprocessing.CRCSpec"
test_checkercrc:
	sbt "project preprocessing; testOnly opera.preprocessing.CheckerCRCSpec"
test_reverse:
	sbt "project preprocessing; testOnly opera.preprocessing.ReverseSpec"
test_swap:
	sbt "project preprocessing; testOnly opera.preprocessing.SwapSpec"
test_padder:
	sbt "project preprocessing; testOnly opera.preprocessing.PadderSpec"
test_preprocessing_all:
	sbt "project preprocessing; \
	testOnly opera.preprocessing.CRCSpec; \
	testOnly opera.preprocessing.CheckerCRCSpec; \
	testOnly opera.preprocessing.ReverseSpec; \
	testOnly opera.preprocessing.SwapSpec; \
	testOnly opera.preprocessing.PadderSpec; \
	testOnly opera.preprocessing.PreProcessingSpec; \
	"

#####################################################
# docs
#####################################################
docs_preprocessing_html:
	cd docs/preprocessing; make html; mv ./build/html ../../website/docs/preprocessing; cd -;
docs_preprocessing_pdf:
	cd docs/preprocessing; make latexpdf; mv ./build/latex/preprocessing.pdf ./preprocessing.pdf; cd -;