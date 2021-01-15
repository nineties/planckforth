\ planckforth -
\ Copyright (C) 2021 nineties

\ test/tester.fs and test codes are base on
\ https://github.com/gerryjackson/forth2012-test-suite

." Running PlanckForth test programs" cr

include test/tester.fs
include test/core.fs
include test/errorreport.fs
include test/coreexttest.fs

report-errors
report-and-exit
