#lang racket/base

(require json/pretty
         racket/port
         rackunit)

(define-check (check-pretty s expected)
  (define res
    (call-with-output-string
     (lambda (out)
       (pretty-print-json s out))))
  (unless (equal? res expected)
    (with-check-info
      (['result res]
       ['expected expected])
      (fail-check))))

(define pretty-suite
  (test-suite
   "pretty"

   (test-case "empty"
     (check-pretty "" ""))

   (test-case "open-close"
     (check-pretty "{}" "{}")
     (check-pretty "[]" "[]")
     (check-pretty "[{}]" "[\n  {}\n]"))

   (test-case "kitchen sink"
     (check-pretty
      #<<JSON
[1, 2, "hello", "hello, \"world\"", "\u03bbambda", [], {}, [{}], {"a": 1, "b": [2, 3.14E+0, 10e20]}, [true, false, null]]
JSON
      #<<EXPECTED
[
  1,
  2,
  "hello",
  "hello, \"world\"",
  "\u03bbambda",
  [],
  {},
  [
    {}
  ],
  {
    "a": 1,
    "b": [
      2,
      3.14E+0,
      10e20
    ]
  },
  [
    true,
    false,
    null
  ]
]
EXPECTED
      ))

   (test-case "sequence"
     (check-pretty
      "1 2 1.5 \"hello\" true     false [1, 2, 3]"
      #<<EXPECTED
1 2 1.5 "hello" true false [
  1,
  2,
  3
]
EXPECTED
      ))))

(module+ test
  (require rackunit/text-ui)
  (run-tests pretty-suite))
