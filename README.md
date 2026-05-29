<picture>
  <source media="(prefers-color-scheme: dark)" srcset="website/images/logo-dark.svg">
  <source media="(prefers-color-scheme: light)" srcset="website/images/logo-light.svg">
  <img alt="OPERA-DSP" src="website/images/logo-light.svg" width="400">
</picture>

# 
The OPERA-DSP project goal is to develop an open-source FMCW radar DSP hardware library, making radar signal processing more accessible to researchers and developers. It (will) provide essential IP cores for FMCW radar signal processing, including windowing functions, Fast Fourier Transform (FFT), magnitude computation, and Constant False Alarm Rate (CFAR) detection. 

## Project website

The project website is available at [opera-platform.github.io/opera-dsp](https://opera-platform.github.io/opera-dsp/). It includes the project overview, news, and generated block documentation.

## Dependencies

### OpenJDK and SBT

OPERA-DSP is based on [Chisel](https://www.chisel-lang.org/). To run OPERA-DSP, you will need SBT and OpenJDK installed.

To install OpenJDK on Debian-based systems (Ubuntu, etc.), use the following command:

```bash
$ apt install openjdk-17-jdk
```

If root access is required, use `sudo` before `apt`:
```bash
$ sudo apt install openjdk-17-jdk
```

For SBT installation instructions, follow the guide [HERE](https://www.scala-sbt.org/1.x/docs/Installing-sbt-on-Linux.html).

### GIT

If Git is not already installed, you can install it using:

```bash
$ apt install git
```

### OSS CAD Suite

To execute OPERA-DSP tests, several open-source tools (such as Verilator and SMD solvers) are required. The easiest way to install these tools is through the OSS CAD Suite from YosysHQ. You can download the OSS CAD Suite [HERE](https://github.com/YosysHQ/oss-cad-suite-build). 

Download the appropriate release for your system and ensure it is added to your system PATH.

### Sphinx

Sphinx is used to generate project documentation. To install Sphinx and the Sphinx theme, use the following commands:

```bash
$ pip install sphinx
$ pip install sphinx_rtd_theme
```

To generate PDF documentation, install the necessary TeX Live packages:

```bash
$ apt install texlive-base texlive-latex-recommended texlive-latex-extra texlive-fonts-recommended
```

## OPERA-DSP as a standalone project

If you are using OPERA-DSP as a standalone project, you will need to clone the required submodules. These dependencies include:

- [rocket-chip](https://github.com/chipsalliance/rocket-chip.git)
- [dsptools](https://github.com/ucb-bar/dsptools.git)
- [rocket-dsp-utils](https://github.com/ucb-bar/rocket-dsp-utils.git)

To clone these submodules, navigate to the project root directory and run:

```bash
$ make clone_dependencies
```

This command will clone the submodules into the `./dependencies` folder.

## PreProcessing block

The PreProcessing block is responsible for formatting raw ADC data from the FMCW radar sensor, ensuring compatibility with the subsequent DSP blocks.

### Documentation

For detailed information about the PreProcessing block, including instructions for RTL generation and test execution, refer to the documentation in the [/docs/preprocessing](/docs/preprocessing) folder.

To generate the documentation:

```bash
# For HTML (output in ./website/docs/preprocessing)
$ make docs_preprocessing_html
# For PDF (output in ./docs/preprocessing/)
$ make docs_preprocessing_pdf
```

### RTL generation

To generate AXI4 or TileLink variants of the PreProcessing block, use the following commands in the project root directory:

```bash
# AXI4
$ make rtl_preprocessing_axi4
# TileLink
$ make rtl_preprocessing_tl
```

The generated SystemVerilog code will be located in the `./rtl` folder.

### Tests

To run the PreProcessing tests, use the following command in the project root directory:

```bash
$ make test_preprocessing
```

Test results will be stored in the `./test_run_dir` folder. 

By default 4,884 tests will be run, and that will take around 2 hours. The tests can be found in folder [/preprocessing/src/test/scala](preprocessing/src/test/scala). To reduce the number of tests, modify the [PreProcessingSpec](preprocessing/src/test/scala/PreProcessingSpec.scala) parameters.

## Windowing block

A windowing function is typically used in digital signal
processing before performing an FFT to reduce spectral leakage that appears in the frequency spectrum.

### Documentation

For detailed information about the Windowing block, including instructions for RTL generation and test execution, refer to the documentation in the [/docs/windowing](/docs/windowing) folder.

To generate the documentation:

```bash
# For HTML (output in ./website/docs/windowing)
$ make docs_windowing_html
# For PDF (output in ./docs/windowing/)
$ make docs_windowing_pdf
```

### RTL generation

To generate AXI4 or TileLink variants of the Windowing block, use the following commands in the project root directory:

```bash
# AXI4
$ make rtl_windowing_axi4
# TileLink
$ make rtl_windowing_tl
```

The generated SystemVerilog code will be located in the `./rtl` folder.

### Tests

To run the Windowing tests, use the following command in the project root directory:

```bash
# AXI4
$ make test_windowing_axi4
# TileLink
$ make test_windowing_tl
# For both TileLink and AXI4 tests
$ make test_windowing_all
```

Test results will be stored in the `./test_run_dir` folder. 

By default 672 tests will be run for both AXI4 and TileLink variants of the Windowing. For each (TileLink or AXI4), tests will take around 30 minutes. The tests can be found in folder [/windowing/src/test/scala/](windowing/src/test/scala). To reduce the number of tests, modify the [WindowingAXI4Spec](windowing/src/test/scala/WindowingAXI4Spec.scala) and/or [WindowingTLSpec](windowing/src/test/scala/WindowingTLSpec.scala) parameters.

## Magnitude block

This module is used to calculate (or to approximate) magnitude of a complex signal. Module supports Squared magnitude and Jet Propulsion Laboratory magnitude approximation (refer to this [document](https://ipnpr.jpl.nasa.gov/progress_report/42-40/40L.PDF) for more information). This module also includes block for calculating log2 value of the real input signal.

### Documentation

For detailed information about the Magnitude block, including instructions for RTL generation and test execution, refer to the documentation in the [/docs/log-magnitude](/docs/log-magnitude) folder.

To generate the documentation:

```bash
# For HTML (output in ./website/docs/log-magnitude)
$ make docs_magnitude_html
# For PDF (output in ./docs/log-magnitude/)
$ make docs_magnitude_pdf
```

### RTL generation

To generate AXI4 or TileLink variants of the Magnitude block, use the following commands in the project root directory:

```bash
# AXI4
$ make rtl_magnitude_axi4
# TileLink
$ make rtl_magnitude_tl
```

The generated SystemVerilog code will be located in the `./rtl` folder.

### Tests

To run the Magnitude tests, use the following command in the project root directory:

```bash
# AXI4
$ make test_magnitude_axi4
# TileLink
$ make test_magnitude_tl
# For all tests, this will take a lot of time
$ make test_magnitude_all
```

Test results will be stored in the `./log-magnitude/test_run_dir` folder. 

By default 1368 tests will be run for both AXI4 and TileLink variants of the Magnitude. For each (TileLink or AXI4), tests will take around 60 minutes. Tests can be found in folder [./log-magnitude/src/test/scala/](log-magnitude/src/test/scala). To reduce the number of tests, modify the [*Spec.scala](log-magnitude/src/test/scala) file parameters.

## FFT block

The FFT block implements a streaming Single-path Delay-Feedback (SDF) Fast Fourier Transform for FMCW radar DSP chains. It supports Radix-2 and Radix-2^2 cores, DIT/DIF decimation modes, optional bit reversal, runtime FFT size and direction control, per-stage scaling and overflow status, SRAM-backed delay lines, and AXI4 or TileLink memory-mapped wrappers.

### Documentation

For detailed information about the FFT block, including instructions for RTL generation and test execution, refer to the documentation in the [/docs/fft](/docs/fft) folder.

To generate the documentation:

```bash
# For HTML (output in ./website/docs/fft)
$ make docs_fft_html
# For PDF (output in ./docs/fft/)
$ make docs_fft_pdf
```

### RTL generation

To generate AXI4, TileLink, or BitReverse variants of the FFT block, use the following commands in the project root directory:

```bash
# AXI4 Version
$ make rtl_fft_axi4
# TileLink Version
$ make rtl_fft_tl
```

The generated SystemVerilog code will be located in the `./rtl` folder.

To pass a JSON configuration file directly to the AXI4 or TileLink generator:

```bash
# AXI4 Version
$ sbt "project fft; runMain opera.fft.AXI4App fft/src/main/resources/parameters.json"
# TileLink Version
$ sbt "project fft; runMain opera.fft.TLApp fft/src/main/resources/parameters.json"
```

### Tests

To run the most common FFT tests, use the following commands in the project root directory:

```bash
# AXI4 memory-mapped wrapper
$ make test_fft_axi4
# TileLink memory-mapped wrapper
$ make test_fft_tl
# For all FFT tests
$ make test_fft_all
```

Test results will be stored in the `./fft/test_run_dir` folder. The tests can be found in the [/fft/src/test/scala](fft/src/test/scala) folder.

FFT tests accept optional ScalaTest configuration flags after `--`. Useful options include `-Dfft.verbose=true`, `-Dfft.plot=true`, `-Dfft.randomReadyValid=true`, and `-Dfft.nonParallel=N`. The `fft.nonParallel` option makes tests with FFT sizes greater than or equal to `N` use non-parallel Verilator output; it defaults to `256`.

For example:

```bash
$ sbt -J-Xms2048M -J-Xmx8G "project fft; testOnly opera.fft.FFTvsFloatingPointSpec -- -Dfft.randomReadyValid=true -Dfft.nonParallel=64 -z \"size = 64\""
```

## CFAR block

The CFAR block implements 1D Constant False Alarm Rate target detection for range spectra. For each Cell Under Test (CUT), it estimates local noise from neighboring reference cells, scales the estimate into an adaptive threshold, and reports a peak when the CUT crosses that threshold.

The block supports Cell-Averaging CFAR modes (CA-CFAR, GOCA, and SOCA), Generalized Ordered-Statistic CFAR modes (GOS-CA, GOS-GO, and GOS-SO), runtime FFT size and window configuration, optional log-domain thresholding, edge policies, peak grouping, and AXI4 or TileLink memory-mapped wrappers.

### Documentation

For detailed information about the CFAR block, including instructions for RTL generation and test execution, refer to the documentation in the [/docs/cfar](/docs/cfar) folder.

To generate the documentation:

```bash
# For HTML (output in ./website/docs/cfar)
$ make docs_cfar_html
# For PDF (output in ./docs/cfar/)
$ make docs_cfar_pdf
```

### RTL generation

To generate AXI4 or TileLink variants of the CFAR block, use the following commands in the project root directory:

```bash
# AXI4 Version
$ make rtl_cfar_axi4
# TileLink Version
$ make rtl_cfar_tl
# For both AXI4 and TileLink variants
$ make rtl_cfar_all
```

The generated SystemVerilog code will be located in the `./rtl` folder.

To pass a JSON configuration file directly to the AXI4 or TileLink generator:

```bash
# AXI4 Version
$ sbt "project cfar; runMain opera.cfar.AXI4App cfar/src/main/resources/parameters.json"
# TileLink Version
$ sbt "project cfar; runMain opera.cfar.TLApp cfar/src/main/resources/parameters.json"
```

### Tests

To run the most common CFAR tests, use the following commands in the project root directory:

```bash
# AXI4 memory-mapped wrapper
$ make test_cfar_axi4
# TileLink memory-mapped wrapper
$ make test_cfar_tl
# For all CFAR tests
$ make test_cfar_all
```

Test results will be stored in the `./cfar/test_run_dir` folder. The tests can be found in the [/cfar/src/test/scala](cfar/src/test/scala) folder.

CFAR tests accept optional ScalaTest configuration flags after `--`. Useful options include `-Dcfar.verbose=true`, `-Dcfar.plot=true`, and `-Dcfar.randomReadyValid=true`.

For example:

```bash
$ sbt -J-Xms2048M -J-Xmx8G "project cfar; testOnly opera.cfar.MemoryMappedAXI4CFARSpec -- -Dcfar.verbose=true"
```
