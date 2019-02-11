#!/bin/sh

BASE_ADDRESS=0x100000

CPP=../ncpp/ncpp
CC1=../ncc1/ncc1
AS=../nas/nas
LD=../nld
EXEC=../nexec

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
            fail "should produce $suffix"
        fi

        cmp -s $prefix.test.$suffix $prefix.$number.$suffix
        if [ "$?" != "0" ]
        then
            fail "mismatched $suffix"
        fi
    else
        if [ -e $prefix.test.$suffix ] 
        then
            fail "unexpected $suffix produced"
        fi
    fi
}

error_free()
{
    $* 2>/dev/null

    if [ ! "$?" -eq 0 ]
    then
        fail "$1 exited with error"
        exit 1
    fi
}

cpp_test()
{
    local number=$1

    rm -f cpp.test.out
    rm -f cpp.test.err

    echo -n CPP $number ...
    $CPP cpp.$number.in cpp.test.out 2>cpp.test.err

    check_files cpp $number err
    check_files cpp $number out

    rm -f cpp.test.out
    rm -f cpp.test.err

    echo OK
}

cc1_test()
{
    local number=$1
    local flags=$2

    echo -n CC1 $number \($flags\) $number ...

    rm -f cc1.test.tmp
    rm -f cc1.test.s
    rm -f cc1.test.o
    rm -f cc1.test.aout
    rm -f cc1.test.out

    error_free $CPP cc1.$number.in cc1.test.tmp

    $CC1 $flags cc1.test.tmp cc1.test.s 2>cc1.test.err
    check_files cc1 $number err

    if [ ! -e cc1.$number.err ] 
    then
        error_free $AS -o cc1.test.o cc1.test.s 
        error_free $LD -b $BASE_ADDRESS -e _main -o cc1.test.aout cc1.test.o support.o
        error_free $EXEC -b $BASE_ADDRESS cc1.test.aout >cc1.test.out

        check_files cc1 $number out
    fi

    rm -f cc1.test.tmp
    rm -f cc1.test.s
    rm -f cc1.test.o
    rm -f cc1.test.aout
    rm -f cc1.test.out
    echo OK
}

run_tests()
{
    local prefix=$1
    local tester=$2
    local flags=$3
    local number
    local i

    for i in $prefix.*.in
    do
        number=`echo $i | cut -f 2 -d .`
        $tester $number $flags
    done
}

run_tests cpp cpp_test

error_free $CPP support_c.c support_c.i
error_free $CC1 support_c.i support_c.s
error_free $AS -o support.o support_asm.s support_c.s

run_tests cc1 cc1_test
run_tests cc1 cc1_test -O

exit 0
