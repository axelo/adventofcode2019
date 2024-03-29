#!/usr/bin/env bash

SESSION="$1"
OUTPUTELM="$2"
DAYS=7

if [ "$SESSION" == "" ] || [ "${OUTPUTELM}" == "" ]; then
    echo "Must be called with a session and output value"
    exit 1
fi

echo -e "-- Autogenerated\n\n\nmodule Inputs exposing (..)" > ${OUTPUTELM}

echo -e "\n\ndays : List String" >> ${OUTPUTELM}
echo "days =" >> ${OUTPUTELM}
echo "    [ day1" >> ${OUTPUTELM}

for i in $(seq 2 $DAYS); do
    echo "    , day$i" >> ${OUTPUTELM}
done

echo "    ]" >> ${OUTPUTELM}


for i in $(seq 1 $DAYS); do
    # INPUT=`curl --silent --show-error --fail -b "session=$SESSION" "https://adventofcode.com/2019/day/$i/input" | jq -R -s`
    INPUT=`curl --silent --show-error --fail -b "session=$SESSION" "https://adventofcode.com/2019/day/$i/input"`

    if [ $? == 0 ]; then
        echo "Got input for day $i"
        echo -e "\n\nday$i : String" >> ${OUTPUTELM}
        echo "day$i = " >> ${OUTPUTELM}
        echo -n "    \"\"\"" >> ${OUTPUTELM}
        echo -ne "${INPUT}" >> ${OUTPUTELM}
        echo "\"\"\"" >> ${OUTPUTELM}

    else
        echo "Failed to download puzzle input for day $i"
        exit 2
    fi

    sleep 1
done

