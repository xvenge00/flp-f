#!/bin/bash

# binary executable
EXECUTABLE="./rka-2-dka"
if [ $# -ge 1 ]; then
	EXECUTABLE=$1
fi;

# dir containing subdirectories with test files
TEST_DIR="test"
if [ $# -ge 2 ]; then
	TEST_DIR=$2
fi;

# TODO better argument parsing
# TODO list of test cases


FILE_IN="in"
FILE_REF="ref"
FILE_OUT="out"
# TODO FILE_ERR="err"
# TODO FILE_CONTROL="ctrl" # file with control options 

DEFAULT_CMD="${EXECUTABLE} -t"

SEPARATOR="-------------------------------------------"

RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

checkOK() {
    DIFF=$2
    MESSAGE=$3

    if [ -z "${DIFF}" ] ; then
		echo -e "${GREEN}OK${NC} (${MESSAGE})"
	else
		echo -e "${RED}ERR: (${MESSAGE})${NC}"
        echo "${DIFF}"
	fi
}

expectOK() {
    CURR_TEST="${TEST_DIR}/$1"

    # print test case
    echo "Test: ${CURR_TEST}"

    # print command
    echo "${DEFAULT_CMD}"

    # run command
    ${DEFAULT_CMD} "${CURR_TEST}/${FILE_IN}" > "${CURR_TEST}/${FILE_OUT}"
    
    ERR_CODE="${?}"
    DIFF="$(diff "${CURR_TEST}"/${FILE_REF} "${CURR_TEST}"/${FILE_OUT})"

    checkOK "${ERR_CODE}" "${DIFF}" "message"
}

# TODO expectErr function
# pozri ci existuje subor err, ked ano precitaj

expectOK "001-basic"


