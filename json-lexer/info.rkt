#lang info

(define license 'BSD-3-Clause)
(define collection "json")
(define deps '("base"
               "json-lexer-lib"))
(define build-deps '("racket-doc"
                     "scribble-lib"))
(define implies '("json-lexer-lib"))
(define scribblings '(("json-lexer-manual.scrbl")))
