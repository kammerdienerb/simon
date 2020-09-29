#!/usr/bin/env bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

cd ${DIR}

C_FLAGS="-DSIMON_DO_ASSERTIONS -O0 -Wall -Werror"
LD_FLAGS="-lpthread -lm"

echo "Building Simon compiler.."
rm -rf build
mkdir -p build
mkdir -p build/obj
mkdir -p build/bin

pids=()

for f in src/*.c; do
    echo "  $f"
    gcc -c ${C_FLAGS} $f -o build/obj/$(basename $f .c).o &
    pids+=($!)
done

for p in ${pids[@]}; do
    wait $p || exit 1
done

gcc build/obj/*.o ${LD_FLAGS} -o build/bin/simon || exit 1

echo "Done."
