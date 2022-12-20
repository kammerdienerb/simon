#!/usr/bin/env bash

# build/bin/simon -v --dump-symbols syntax.si
# build/bin/simon -v syntax.si

# build/bin/simon -v poly.si
build/bin/simon -v --threads=1 test.si basic.si
# build/bin/simon -v x.si
