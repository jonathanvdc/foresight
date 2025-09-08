#!/bin/bash

EGGLOG_RELEASE="/Users/aziz/dev/lib/egglog/target/release/egglog"  # Points to the egglog bin
EGGLOG_EXP_RELEASE="/Users/aziz/dev/lib/egglog-experimental/target/release/egglog-experimental"  # Points to the egglog exp bin

echo "Running benchmark on poly.egg"
$EGGLOG_RELEASE poly.egg 2> /dev/null
echo "-----------------------------------"

FILES=(
    "3mm"
    "5mm"
    "10mm"
    "20mm"
    "40mm"
    "80mm"
)

# warmup
$EGGLOG_EXP_RELEASE "$FILE".egg > /dev/null 2> /dev/null

for FILE in "${FILES[@]}"; do
    echo "Running benchmark on $FILE.egg"
    $EGGLOG_EXP_RELEASE "$FILE".egg 2> /dev/null
    echo "-----------------------------------"
done