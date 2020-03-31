SOURCES=src/Main.hs src/FSADeterminize.hs src/FSATypes.hs src/FSAParser.hs
EXECUTABLE=rka-2-dka
BUILD_DIR=build
TEST_DIR=test
TEST_OUT=${TEST_DIR}/dir_out

.PHONY: test build pack clean

all: build

build: ${SOURCES}
	ghc --make ${SOURCES} -o ${EXECUTABLE} -odir ${BUILD_DIR} -hidir ${BUILD_DIR}

test:
	$(MAKE) build
	${TEST_DIR}/test.sh -e "./${EXECUTABLE} -t" -d ${TEST_DIR}

pack:
	zip -r flp-fun-xvenge00.zip ${SOURCES} Makefile README.md ${TEST_DIR}

clean:
	${TEST_DIR}/test.sh -c -d ${TEST_DIR}
	rm -rf ${BUILD_DIR} ${EXECUTABLE} ${TEST_OUT}