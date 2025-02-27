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
