![OPERA-DSP](docs/images/logo.png)
# OPERA-DSP

The OPERA-DSP project goal is to develop an open-source FMCW radar DSP hardware library, making radar signal processing more accessible to researchers and developers. It (will) provide essential IP cores for FMCW radar signal processing, including windowing functions, Fast Fourier Transform (FFT), magnitude computation, and Constant False Alarm Rate (CFAR) detection. 

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
# For HTML (output in ./docs/preprocessing/build/html)
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
# For HTML (output in ./docs/windowing/build/html)
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

By default 672 tests will be run for both AXI4 and TileLink variants of the Windowing. For each (TileLink or AXI4), tests will take around 30 minutes. The tests can be found in folder [./windowing/src/test/scala/](windowing/src/test/scala). To reduce the number of tests, modify the [WindowingAXI4Spec](windowing/src/test/scala/WindowingAXI4Spec.scala) and/or [WindowingTLSpec](windowing/src/test/scala/WindowingTLSpec.scala) parameters.

## Log2Magnitude block

Work in Progress

## FFT block

Work in Progress

## CFAR block

Work in Progress