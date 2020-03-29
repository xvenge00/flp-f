SOURCES=main.hs FSADeterminize.hs FSATypes.hs FSAParser.hs
EXECUTABLE=rka-2-dka
BUILD_DIR=build
TEST_OUT=dir_out

all: build

${EXECUTABLE}: ${SOURCES}
	ghc --make ${SOURCES} -o ${EXECUTABLE} -odir ${BUILD_DIR} -hidir ${BUILD_DIR}

build: ${EXECUTABLE}

test: ${EXECUTABLE}
	./test.sh

pack:
	zip xvenge00.zip ${SOURCES} Makefile

clean:
	rm -rf ${BUILD_DIR} ${EXECUTABLE} ${TEST_OUT}