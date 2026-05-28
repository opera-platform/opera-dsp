#####################################################
# RTL
#####################################################
rtl_fft_axi4:
	sbt "project fft; runMain opera.fft.AXI4App"
	rm FFTAXI4.sv # Remove generated empty file
rtl_fft_tl:
	sbt "project fft; runMain opera.fft.TLApp"
	rm FFTTL.sv # Remove generated empty file
rtl_bit_reverse:
	sbt "project fft; runMain opera.fft.BitReverseApp"
	rm BitReverse.sv # Remove generated empty file
rtl_fft_all:
	sbt "project fft; \
	runMain opera.fft.AXI4App; \
	runMain opera.fft.TLApp; \
	runMain opera.fft.BitReverseApp; \
	"
	rm FFTAXI4.sv FFTTL.sv BitReverse.sv # Remove generated empty files

#####################################################
# Tests
#####################################################
test_fft_axi4:
	sbt -J-Xms2048M -J-Xmx8G "project fft; testOnly opera.fft.MemoryMappedAXI4FFTSpec"
test_fft_tl:
	sbt -J-Xms2048M -J-Xmx8G "project fft; testOnly opera.fft.MemoryMappedTLFFTSpec"
test_bit_reverse:
	sbt -J-Xms2048M -J-Xmx8G "project fft; testOnly opera.fft.BitReverseSpec"
test_sdf_stage:
	sbt -J-Xms2048M -J-Xmx8G "project fft; testOnly opera.fft.SDFStageSpec"
test_fft_model:
	sbt -J-Xms2048M -J-Xmx8G "project fft; testOnly opera.fft.FFTModelSpec"
test_fft_model_vs_floating_point:
	sbt -J-Xms2048M -J-Xmx8G "project fft; testOnly opera.fft.FFTModelvsFloatingPointSpec"
test_fft_vs_model:
	sbt -J-Xms2048M -J-Xmx8G "project fft; testOnly opera.fft.FFTvsModelSpec"
test_fft_vs_floating_point:
	sbt -J-Xms2048M -J-Xmx8G "project fft; testOnly opera.fft.FFTvsFloatingPointSpec"
test_fft_sqnr:
	sbt -J-Xms2048M -J-Xmx8G "project fft; testOnly opera.fft.FFTSQNRSpec"

test_fft_all:
	sbt -J-Xms2048M -J-Xmx8G "project fft; \
	testOnly opera.fft.MemoryMappedAXI4FFTSpec; \
	testOnly opera.fft.MemoryMappedTLFFTSpec; \
	testOnly opera.fft.BitReverseSpec; \
	testOnly opera.fft.SDFStageSpec; \
	testOnly opera.fft.FFTModelSpec; \
	testOnly opera.fft.FFTModelvsFloatingPointSpec; \
	testOnly opera.fft.FFTvsModelSpec; \
	testOnly opera.fft.FFTvsFloatingPointSpec; \
	testOnly opera.fft.FFTSQNRSpec; \
	"

#####################################################
# docs
#####################################################
docs_fft_html:
	cd docs/fft; make html; mkdir -p ../../website/docs/fft; cp -R ./build/html/. ../../website/docs/fft; cd -;
docs_fft_pdf:
	cd docs/fft; make latexpdf; mv ./build/latex/fft.pdf ./fft.pdf; cd -;
