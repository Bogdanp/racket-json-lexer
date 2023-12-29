#lang racket/base

(require racket/contract/base
         "lexer.rkt")

(provide
 (contract-out
  [pretty-print-json (->* ((or/c string? input-port?))
                          (output-port? #:width exact-positive-integer?)
                          void?)]))

(define (pretty-print-json in [out (current-output-port)]
                           #:width [width 2])
  (define (indent depth)
    (when (positive? depth)
      (write-string (make-string (* width depth) #\space) out)))
  (define l
    (make-lexer
     (if (string? in)
         (open-input-string in)
         in)))
  (let loop ([depth 0] [indent? #f] [space? #f])
    (define t
      (lexer-take l))
    (case (token-type t)
      [(eof) (void)]
      [(whitespace)
       (loop depth indent? space?)]
      [(lcubrace lsqbrace)
       (when indent? (indent depth))
       (when space? (write-char #\space out))
       (case (token-type (lexer-peek l))
         [(rcubrace rsqbrace)
          (write-token t out)
          (write-token (lexer-take l) out)
          (loop depth #t #t)]
         [else
          (write-token t out)
          (newline out)
          (loop (add1 depth) #t #f)])]
      [(rcubrace rsqbrace)
       (newline out)
       (indent (sub1 depth))
       (write-token t out)
       (loop (sub1 depth) #t #t)]
      [(colon)
       (write-token t out)
       (loop depth #f #t)]
      [(comma)
       (write-token t out)
       (newline out)
       (loop depth #t #f)]
      [else
       (when indent? (indent depth))
       (when space? (write-char #\space out))
       (write-token t out)
       (loop depth #f #t)])))

(define (write-token t out)
  (write-string (token-str t) out))
