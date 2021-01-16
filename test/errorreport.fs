\ planckforth -
\ Copyright (C) 2021 nineties

\ test/tester.fs and test codes are base on
\ https://github.com/gerryjackson/forth2012-test-suite

decimal

variable total-errors

: error-count  ( "name" n1 -- n2 )  \ n2 = n1 + 1cell
   create  dup , cell+
   does>  ( -- offset ) @     \ offset in address units
;

0     \ Offset into errors[] array
error-count core-errors          error-count core-ext-errors
error-count double-errors        error-count exception-errors
error-count facility-errors      error-count file-errors
error-count locals-errors        error-count memory-errors
error-count searchorder-errors   error-count string-errors
error-count tools-errors         error-count block-errors
create errors[] dup allot constant #error-counts

\ set-error-count called at the end of each test file with its own offset into
\ the errors[] array. #error is in files tester.fr and ttester.fs

: set-error-count  ( offset -- )
   #error @ swap errors[] + !
   #error @ total-errors +!
   0 #error !
;

: init-errors  ( -- )
   errors[] #error-counts over + swap do -1 i ! 1 cells +loop
   0 total-errors !
   core-errors set-error-count
;

init-errors

\ Report summary of errors

25 constant margin

: show-error-line  ( n caddr -- )
   cr dup strlen swap type margin - abs >r
   dup -1 = if drop r> 1- spaces ." -" else
   r> .r then
;

: show-error-count  ( caddr offset -- )
   errors[] + @ swap show-error-line
;

: hline  ( -- )  cr ." ---------------------------"  ;

: report-errors
   hline
   cr 8 spaces ." Error Report"
   cr ." Word Set" 13 spaces ." Errors"
   hline
   s" Core" core-errors show-error-count
   s" Core extension" core-ext-errors show-error-count
   s" Block" block-errors show-error-count
   s" Double number" double-errors show-error-count
   s" Exception" exception-errors show-error-count
   s" Facility" facility-errors show-error-count
   s" File-access" file-errors show-error-count
   s" Locals"    locals-errors show-error-count
   s" Memory-allocation" memory-errors show-error-count
   s" Programming-tools" tools-errors show-error-count
   s" Search-order" searchorder-errors show-error-count
   s" String" string-errors show-error-count
   hline
   total-errors @ s" Total" show-error-line
   hline cr cr
;
