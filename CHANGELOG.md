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

## 0.1.4.0 -- 2025-02-28

* Add the multiplication ("*") operator.

* Add a bunch of test cases to /test/MathTest.hs to verify that multiplication
  works properly / overflows properly.

## 0.1.4.1 -- 2025-02-28

* Vastly improved "Evaluation error:" error messages that involve parenthesized
  expressions: these messages now show the parenthesized expressions with proper
  parentheses and syntax. The error messages reflect the nesting depth of the
  expression at which the eval error occurred.

* ^ this involved changing the `Show` instance for the `Paren` constructor in
  /lib/Expr.hs:`Term`.

## 0.2.0.0 -- 2025-03-03

* Remove the `MS` and `S` constructors from /lib/Duration:`Duration`, leaving
  only the `HMS` constructor. This improves the code significantly, as the
  removed constructors were useless outside of parsing and created awkward
  patterns in the code like needing `hmsIfy` in case statements. `hmsIfy` is now
  also gone, as it was used only to deal with the problem of the `MS` and `S`
  constructors.

## 1.0.0.0 -- 2025-08-11

* Add support for fractional numbers of seconds at arbitrary precision, using
  the Data.Scientific library. All of the following notations are supported:

  ```
  > hms "10.24 + 20.35"
  # 00:00:30.59

  > hms "0.5:0:0"
  # 00:30:00.0

  > hms "00:1/4:00"
  # 00:00:15.0
  ```

  This new feature makes hms easily compatible with tools like `soxi`:

  ```
  soxi -d *.mp3 | xargs echo | tr ' ' '+' | xargs hms
  soxi -D *.mp3 | xargs echo | tr ' ' '+' | xargs hms
  ```

  or `ffprobe`:

  ```
  for audio_file in *.mp3; do
    ffprobe -i $audio_file -show_entries format=duration -v quiet -of csv="p=0"
  done | xargs echo | tr ' ' '+' | xargs hms
  ```
