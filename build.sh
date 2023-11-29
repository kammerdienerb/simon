#!/usr/bin/env bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

cd ${DIR}



DEBUG=yes

# SHOULD_USE_LIBC_MALLOC="-DUSE_LIBC_MALLOC"

WHICH_STRING_INTERN_STRUCTURE="-DSTRING_INTERN_STRUCTURE=STRING_HASH_TABLE"
# WHICH_STRING_INTERN_STRUCTURE="-DSTRING_INTERN_STRUCTURE=STRING_RB_TREE"

# WHICH_TLS_METHOD="-DTLS_METHOD=TLS_PER_HW_THREAD"
WHICH_TLS_METHOD="-DTLS_METHOD=TLS_PER_OS_THREAD"

TLS_MODEL="-ftls-model=local-exec"

if [ $(uname) = "Darwin" ]; then
    if uname -a | grep "arm64" >/dev/null 2>&1; then
        APPLE_ARM="yes"
    fi
fi

if [[ ${DEBUG} == "yes" ]]; then
    SHOULD_DO_ASSERTIONS="-DSIMON_DO_ASSERTIONS"
    OPT="-O0"
    DEBUG_SYMBOLS="-g"
else
    OPT="-O3"
    LTO="-flto"
    if [[ "${APPLE_ARM}" == "yes" ]]; then
        MARCH="-mcpu=native"
    else
        MARCH="-march=native -mtune=native"
    fi
fi

C_FLAGS+="${SHOULD_DO_ASSERTIONS} ${SHOULD_USE_LIBC_MALLOC}   \
         ${WHICH_STRING_INTERN_STRUCTURE} ${WHICH_TLS_METHOD} \
         ${MARCH} ${TLS_MODEL} ${OPT} ${LTO} ${DEBUG_SYMBOLS} \
         -Wall -Werror"

LD_FLAGS="${MARCH} ${TLS_MODEL} ${OPT} ${LTO} ${DEBUG_SYMBOLS} -lpthread -lm"

echo "Building Simon compiler.."
rm -rf build
mkdir -p build
mkdir -p build/pp
mkdir -p build/obj
mkdir -p build/bin

pids=()

for f in src/*.c; do
    echo "  $f"
    gcc -E ${C_FLAGS} $f -o build/pp/$(basename $f .c).c &
    gcc -c ${C_FLAGS} $f -o build/obj/$(basename $f .c).o &
    pids+=($!)
done

for p in ${pids[@]}; do
    wait $p || exit 1
done

gcc build/obj/*.o ${LD_FLAGS} -o build/bin/simon || exit 1

echo "Done."
