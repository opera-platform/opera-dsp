#####################################################
# RTL
#####################################################
rtl_magnitude_axi4:
	sbt "project log-magnitude; runMain opera.logmagnitude.MagnitudeAXI4App"
	rm MagnitudeAXI4.sv # Remove generated empty file
rtl_magnitude_tl:
	sbt "project log-magnitude; runMain opera.logmagnitude.MagnitudeTLApp"
	rm MagnitudeTL.sv # Remove generated empty file
rtl_magnitude_jpl:
	sbt "project log-magnitude; runMain opera.logmagnitude.MagnitudeJPLApp"
	rm MagnitudeJPL.sv # Remove generated empty file
rtl_magnitude_squared:
	sbt "project log-magnitude; runMain opera.logmagnitude.MagnitudeSquaredApp"
	rm MagnitudeSquared.sv # Remove generated empty file
rtl_magnitude_log:
	sbt "project log-magnitude; runMain opera.logmagnitude.MagnitudeLogApp"
	rm MagnitudeLog.sv # Remove generated empty file
rtl_magnitude_muxed:
	sbt "project log-magnitude; runMain opera.logmagnitude.MagnitudeMuxedApp"
	rm MagnitudeMuxed.sv # Remove generated empty file
rtl_magnitude_all:
	sbt "project log-magnitude; \
	runMain opera.logmagnitude.MagnitudeJPLApp; \
	runMain opera.logmagnitude.MagnitudeSquaredApp; \
	runMain opera.logmagnitude.MagnitudeLogApp; \
	runMain opera.logmagnitude.MagnitudeMuxedApp; \
	runMain opera.logmagnitude.MagnitudeAXI4App; \
	runMain opera.logmagnitude.MagnitudeTLApp; \
	"
	rm MagnitudeAXI4.sv MagnitudeTL.sv MagnitudeJPL.sv MagnitudeSquared.sv MagnitudeLog.sv MagnitudeMuxed.sv # Remove generated empty files

#####################################################
# Tests
#####################################################
test_magnitude_axi4:
	sbt -J-Xms2048M -J-Xmx8G "project log-magnitude; testOnly opera.logmagnitude.MagnitudeAXI4Spec"
test_magnitude_tl:
	sbt -J-Xms2048M -J-Xmx8G "project log-magnitude; testOnly opera.logmagnitude.MagnitudeTLSpec"
test_magnitude_jpl:
	sbt -J-Xms2048M -J-Xmx8G "project log-magnitude; testOnly opera.logmagnitude.MagnitudeJPLSpec"
test_magnitude_squared:
	sbt -J-Xms2048M -J-Xmx8G "project log-magnitude; testOnly opera.logmagnitude.MagnitudeSquaredSpec"
test_magnitude_log:
	sbt -J-Xms2048M -J-Xmx8G "project log-magnitude; testOnly opera.logmagnitude.MagnitudeLogSpec"
test_magnitude_muxed:
	sbt -J-Xms2048M -J-Xmx8G "project log-magnitude; testOnly opera.logmagnitude.MagnitudeMuxedSpec"

test_magnitude_all:
	sbt -J-Xms2048M -J-Xmx8G "project log-magnitude; \
	testOnly opera.logmagnitude.MagnitudeAXI4Spec; \
	testOnly opera.logmagnitude.MagnitudeTLSpec; \
	testOnly opera.logmagnitude.MagnitudeJPLSpec; \
	testOnly opera.logmagnitude.MagnitudeSquaredSpec; \
	testOnly opera.logmagnitude.MagnitudeLogSpec; \
	testOnly opera.logmagnitude.MagnitudeMuxedSpec; \
	"

#####################################################
# docs
#####################################################
docs_magnitude_html:
	cd docs/log-magnitude; make html; mv ./build/html ../../website/docs/log-magnitude; cd -;
docs_magnitude_pdf:
	cd docs/log-magnitude; make latexpdf; mv ./build/latex/magnitude.pdf ./magnitude.pdf; cd -;