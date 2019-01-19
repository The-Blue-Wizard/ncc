#!/bin/sh

BASE_ADDRESS=0x100000

CPP=../ncpp/ncpp
CC1=../ncc1/ncc1

fail()
{
    echo FAILED: $1
    exit 1
}

check_files()
{
    local prefix=$1
    local number=$2
    local suffix=$3

    # ignore zero-length generated files

    if [ ! -s $prefix.test.$suffix ]
    then
        rm -f $prefix.test.$suffix
    fi

    # either both files exist or don't.
    # if they do, their contents must match.

    if [ -e $prefix.$number.$suffix ] 
    then
        if [ ! -e $prefix.test.$suffix ]
        then
            fail "produces $suffix unexpectedly"
        fi

        cmp -s $prefix.test.$suffix $prefix.$number.$suffix
        if [ "$?" != "0" ]
        then
            fail "mismatched $suffix"
        fi
    else
        if [ -e $prefix.test.$suffix ] 
        then
            fail "should produce $suffix"
        fi
    fi
}

cpp_test()
{
    local number=$1

    rm -f cpp.test.out
    rm -f cpp.test.err

    echo CPP test: $number
    $CPP cpp.$number.in cpp.test.out 2>cpp.test.err

    check_files cpp $number err
    check_files cpp $number out

    rm -f cpp.test.out
    rm -f cpp.test.err
}

run_tests()
{
    local prefix=$1
    local tester=$2
    local number
    local i

    for i in $prefix.*.in
    do
        number=`echo $i | cut -f 2 -d .`
        $tester $number
    done
}

run_tests cpp cpp_test

exit 0
