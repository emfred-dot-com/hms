# Revision history for hms

## 0.1.0.0 -- 2025-02-25

* First version. Basic functionality with adding and subtracting durations.

## 0.1.1.0 -- 2025-02-26

* Greatly improved Eval error messages due to rewritten `eval` function which
  handles cases more thoroughly.

## 0.1.1.1 -- 2025-02-26

* Fix a subtle bug in `/lib/Duration.hs:carryMins` where a remainder was being
  handled improperly.

## 0.1.2.0 -- 2025-02-27

* Add a bunch of Parser tests to the test suite, which involved making the Term
  and Operator types derive the Eq type-class to enable equality checks in the
  testing code.

## 0.1.3.0 -- 2025-02-28

* Fix a bug where subtractions were not properly underflowing, e.g. "1:00 -
  00:1" would yield "1:-1".
  
* Add a bunch of math test cases to the test suite which verify that underflow,
  as well as other arithmetic behavior, works correctly.

* Split /test/test.hs into multiple modules, each holding an individual test
  tree, and add these modules to the cabal file.

* Rename /test/test.hs to /test/Test.hs, so that it's consistent with the name
  styling of the other modules in the project.
