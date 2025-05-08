# Commit tags
COMMIT_ROCKET = c9a3244cf51ba424efcbb7d9fa50b3b3ad9ebbfb
COMMIT_DSPTOOLS = fb3b3c6523c3d807316effa1531b7f623d8d9316
COMMIT_ROCKET_UTILS = 6ee2309f80a54b404795f227793d2a54e1dfadf8

# Clone dependencies
clone_dependencies:
# Rocket-chip
	git clone --no-checkout --recurse-submodules --shallow-submodules https://github.com/chipsalliance/rocket-chip.git dependencies/rocket-chip \
		&& cd dependencies/rocket-chip \
		&& git remote set-branches origin dev \
		&& git fetch --depth 1 origin ${COMMIT_ROCKET} \
		&& git checkout ${COMMIT_ROCKET} \
		&& git submodule update --depth 1 --recursive \
		&& cd ../..
# dsptools
	git clone --no-checkout --recurse-submodules --shallow-submodules https://github.com/ucb-bar/dsptools.git dependencies/dsptools \
		&& cd dependencies/dsptools \
		&& git fetch --depth 1 origin ${COMMIT_DSPTOOLS} \
		&& git checkout ${COMMIT_DSPTOOLS} \
		&& git submodule update --depth 1 --recursive \
		&& cd ../..
# rocket-dsp-utils
	git clone --no-checkout https://github.com/ucb-bar/rocket-dsp-utils.git dependencies/rocket-dsp-utils \
		&& cd dependencies/rocket-dsp-utils \
		&& git fetch --depth 1 origin ${COMMIT_ROCKET_UTILS} \
		&& git checkout ${COMMIT_ROCKET_UTILS} \
		&& cd ../..

