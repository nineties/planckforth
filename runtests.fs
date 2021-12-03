\ planckforth -
\ Copyright (C) 2021 nineties

\ test/tester.fs and test codes are base on
\ https://github.com/gerryjackson/forth2012-test-suite

." Running PlanckForth test programs" cr

include lib/tester.fs

include test/core.fs
include test/utilities.fs
include test/coreexttest.fs

include test/export.fs
include lib/array.fs
include lib/bitscan.fs
include lib/table.fs

report-and-exit
