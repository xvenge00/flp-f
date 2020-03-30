#!/bin/bash

FILE_IN="in"
FILE_REF="ref"
FILE_OUT="out"
FILE_STDERR="stderr"
# FILE_ERR="err"
# FILE_CONTROL="ctrl" # file with control options 

COLOR_FAIL='\033[0;31m' # red
COLOR_OK='\033[0;32m'   # green
NC='\033[0m'

# TODO horible argument parsing!!!!!!!!!!!!!!!!!!!!!!!!!!

# dir containing subdirectories with test files
TEST_DIR="test"
if [ $# -ge 2 ]; then
	TEST_DIR=$2
fi;

# binary executable
EXECUTABLE="./rka-2-dka"
if [ $# -ge 1 ]; then
	EXECUTABLE=$1
fi;

clean() {
    for d in "${TEST_DIR}"/*/ ; do
        rm -f "$d/${FILE_OUT}" "$d/${FILE_STDERR}"
    done
}

print_ok_color() {
    echo -e "${COLOR_OK}${1}${NC}"
}

print_fail_color() {
    echo -e "${COLOR_FAIL}${1}${NC}"
}

expect_ok() {
    CURR_TEST="${TEST_DIR}/$1"
    CMD="$2"

    # run command
    ${CMD} <"${CURR_TEST}/${FILE_IN}" >"${CURR_TEST}/${FILE_OUT}" 2>"${CURR_TEST}/${FILE_STDERR}"
    
    ERR_CODE="${?}"
    DIFF="$(diff "${CURR_TEST}"/${FILE_REF} "${CURR_TEST}"/${FILE_OUT})"

    if [ -z "${DIFF}" ] && [ "${ERR_CODE}" -eq 0 ]; then
        print_ok_color "[PASSED] ${CURR_TEST}"
	else
		print_fail_color "[FAILED] ${CURR_TEST}"

        # print command
        echo "${CMD} ${CURR_TEST}/${FILE_IN}"

        # print diff
        # TODO diff -U0 --label="" --label="" test/002-fail/ref test/002-fail/out
        #   show only differences
        echo "${DIFF}"
	fi
}

expect_err() {
    CURR_TEST="${TEST_DIR}/$1"
    CMD="$2"
    ERR_EXPECT="$3"

    # run command
    ${CMD} <"${CURR_TEST}/${FILE_IN}" >"${CURR_TEST}/${FILE_OUT}" 2>"${CURR_TEST}/${FILE_STDERR}"
    
    ERR_CODE="${?}"

    if [ "${ERR_CODE}" -eq "${ERR_EXPECT}" ]; then
        print_ok_color "[PASSED] ${CURR_TEST}"
	else
		print_fail_color "[FAILED] ${CURR_TEST}"

        echo "expected err: ${ERR_EXPECT}"
        echo "got: ${ERR_CODE}"

        # print command
        echo "${CMD} ${CURR_TEST}/${FILE_IN}"

        cat "${CURR_TEST}/${FILE_OUT}"
	fi
}

# TODO total score - pass/fail

# clean if first argument is clean
if [ $# -eq 1 ] && [ "$1" = "clean" ]; then
    clean
    exit;
fi

# TODO pozri ci existuje subor err, ked ano precitaj
DEFAULT_CMD="${EXECUTABLE} -t"

# TODO automatic discover directories
expect_ok "001-basic" "${DEFAULT_CMD}"
expect_ok "002-basic" "${DEFAULT_CMD}"
expect_ok "003-basic" "${DEFAULT_CMD}"
expect_ok "004-basic" "${DEFAULT_CMD}"

expect_err "101-invalid-opt" "${EXECUTABLE} -o" "1"



