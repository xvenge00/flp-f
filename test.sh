#!/bin/bash

# simplify-bkg
# Peter Tisovčík (xtisov00)
# Klára Nečasová (xnecas24)

HS_BIN="./rka-2-dka"

# directory containing input files
DIR_IN="dir_in"

# directory containing output files
DIR_OUT="dir_out"
mkdir -p $DIR_OUT

# directory containing reference output files
DIR_REF="dir_ref"

SHOW_OUTPUT=false

ERROR=0
ERR_CODE=0
SEPARATOR="-------------------------------------------"

echoTest() {
	echo -n -e "\e[36m"
	echo "$1"
	echo -n -e "\e[39m"
}

check () {
	ERR=$?
	ERR_CODE=$ERR
	OK=0

	#check if result is OK or ERROR
	if [ $ERR -eq 0 ] ; then
		OK=1
	fi

	#correct exit code is in green
	#incorrect exit code is in red
	if [ "$1" == "OK"  ] && [  $OK -eq 1 ] ; then
		echo -ne "\e[92m"
	elif  [ "$1" == "ERR"  ] && [  $OK -eq 0 ] ; then
		echo -ne "\e[92m"
	else
		echo -ne "\e[31m"
		ERROR=$((ERROR + 1)) #number of errors
	fi

	#print result
	if [ $ERR -eq 0 ] ; then
		echo "OK (${2})"
	else
		echo "ERR: ${ERR} (${2})"
	fi

	#reset of colour
	echo -n -e "\e[39m"
}

testArg() {
    echoTest "${1}"
    echo "run: " ${HS_BIN} ${2}
    ${HS_BIN} ${2}
    check "${3}" "run program with arguments"
    echo "$SEPARATOR"
}

expectOK() {
    echoTest "${1}"
    echo "run: " ${HS_BIN} ${2} "${DIR_IN}/${3}"
    ${HS_BIN} ${2} "${DIR_IN}/${3}" > "${DIR_OUT}/${3%%.*}.out"
    # check "${4}" "run program with arguments"

    if [ "${SHOW_OUTPUT}" == "true" ]; then
        cat "${DIR_OUT}/${3%%.*}${2}.out"
    fi

    #remove extension of filename
    diff "${DIR_REF}/${3%%.*}.out" "${DIR_OUT}/${3%%.*}.out"
    check "OK" "compare input and reference files"

    echo "$SEPARATOR"
}

expectOK "01 - ref" "-t" "test01.in"
expectOK "02 - opora" "-t" "test02.in"
expectOK "03 - simple" "-t" "test03.in"
expectOK "04 - simple" "-t" "test04.in"

################################################
# Tests for argument check
################################################

testArg "00 - invalid option \"-o\"" "-o" "ERR"
testArg "01 - duplicate option \"-i\" without file" "-i -i" "ERR"
testArg "02 - duplicate option \"-i\" with file" "-i -i filename" "ERR"
testArg "03 - no option" "" "ERR"
testArg "04 - file does not exist" "-i notexisting" "ERR"

