#!/usr/bin/env bash
# SPIKE: regenerate every rendered traceback in out/.
set -u
cd "$(dirname "$0")"
(cd rue-debug-spike && cargo build -q) || exit 1
B=./rue-debug-spike/target/debug/rue-debug-spike
BURN=$(printf burn | shasum -a 256 | cut -d' ' -f1)
PK=0x$(printf 'a%.0s' {1..96}); TG=0x$(printf 'b%.0s' {1..64})
rm -rf out && mkdir out
run() { name=$1; shift; "$B" "$@" > "out/$name.txt" 2>&1; echo "$name: $(grep -c . "out/$name.txt") lines"; }
run nested_assert cases/nested_assert.rue "(10)"
run nested_assert_release cases/nested_assert.rue "(10)" release
SPIKE_LET_SCOPES=1 run nested_assert_let_scopes cases/nested_assert.rue "(10)"
run inline_fail cases/inline_fail.rue "(5)"
run closure_capture cases/closure_capture.rue "(10 (1 2 30 4))"
run closure_no_params cases/closure_no_params.rue "(3)"
run struct_destructure cases/struct_param.rue "((0x$BURN 100) (3 . 5))"
run struct_tuple cases/struct_param.rue "((0x$BURN 100) (5 . 3))"
run typed_param cases/typed_param.rue "($PK $TG 100 \"hello world\" 1)"
run typed_param_values cases/typed_param.rue "($PK $TG 1751477356 \"hi\" 0)"
run recursion cases/recursion.rue "(3)"
run recursion_release cases/recursion.rue "(3)" release
run recursion_tabs cases/recursion_tabs.rue "(1)"
run duplicate_bodies_release cases/duplicate_bodies.rue "(-1 5)" release
run duplicate_subexpr_release cases/duplicate_subexpr.rue "(-1 5)" release
rm -f cases/*.sym.clvm
