#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/match
         racket/port)

(provide
 (struct-out exn:fail:lexer)
 (struct-out token)

 make-lexer
 lexer-peek
 lexer-take)

(struct exn:fail:lexer exn:fail (line col pos)
  #:transparent)

(define (raise-lexer-error message line col pos)
  (raise (exn:fail:lexer message (current-continuation-marks) line col pos)))

(struct token (type str val line col pos)
  #:prefab)

(struct lexer
  (in partial-strings? [pending #:mutable])
  #:transparent)

(define (make-lexer in #:partial-strings? [partial-strings? #f])
  (lexer in partial-strings? #f))

(define (lexer-peek l)
  (cond
    [(lexer-pending l) => values]
    [else
     (define pending
       (lexer-read-token l))
     (begin0 pending
       (set-lexer-pending! l pending))]))

(define (lexer-take l)
  (cond
    [(lexer-pending l)
     => (lambda (pending)
          (begin0 pending
            (set-lexer-pending! l #f)))]

    [else
     (lexer-read-token l)]))

(define (lexer-read-token l)
  (read-token
   (lexer-in l)
   (lexer-partial-strings? l)))


;; readers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (read-token in [partial-strings? #f])
  (define-values (line col pos)
    (port-next-location in))

  (define (make-token type str [val str])
    (token type str val line col pos))

  (match (peek-char in)
    [(? eof-object?) (make-token 'eof        (read-string 1 in))]
    [(? whitespace?) (make-token 'whitespace (read-whitespace in))]

    [#\{ (make-token 'lcubrace (read-string 1 in))]
    [#\} (make-token 'rcubrace (read-string 1 in))]
    [#\[ (make-token 'lsqbrace (read-string 1 in))]
    [#\] (make-token 'rsqbrace (read-string 1 in))]
    [#\: (make-token 'colon    (read-string 1 in))]
    [#\, (make-token 'comma    (read-string 1 in))]

    [#\"
     (define-values (s v)
       (json:read-string in partial-strings?))
     (make-token 'string s v)]

    [(? signed-number-start?)
     (define-values (s v)
       (json:read-number in))
     (make-token 'number s v)]

    [#\f
     (unless (equal? (read-string 5 in) "false")
       (raise-lexer-error "expected false"))
     (make-token 'false "false" #t)]

    [#\t
     (unless (equal? (read-string 4 in) "true")
       (raise-lexer-error "expected true"))
     (make-token 'true "true" #t)]

    [#\n
     (unless (equal? (read-string 4 in) "null")
       (raise-lexer-error "expected null"))
     (make-token 'null "null" null)]

    [c
     (raise-lexer-error (format "unexpected character: ~a" c) line col pos)]))


;; helpers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (read-whitespace in)
  (take-while in whitespace?))

(define (take-while in p)
  (define-values (line col pos)
    (port-next-location in))

  (with-output-to-string
    (lambda ()
      (let loop ([p p]
                 [c (peek-char in)]
                 [span 0])
        (define next-p
          (with-handlers ([exn:fail? (λ (e) (raise-lexer-error (exn-message e) line col pos))])
            (p c)))

        (when next-p
          (display (read-char in))
          (loop next-p (peek-char in) (add1 span)))))))


;; matchers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (λcase stx)
  (syntax-parse stx
    #:literals (else)
    [(_ {~optional {~seq #:char-id char-id}}
        [(lit ...) e ...] ...
        {~optional [else else-e ...]})
     #:with c #'{~? char-id next-c}
     #'(λ (c)
         (case c
           [(lit ...) e ...] ...
           {~? [else else-e ...]
               [else #f]}))]))

(define-syntax (define-λcase stx)
  (syntax-parse stx
    [(_ name:id . rest)
     #'(define name (λcase . rest))]))

(define-λcase whitespace?
  [(#\u00A0 #\space #\tab #\newline #\return) whitespace?])

(define-λcase signed-number-start?
  #:char-id c
  [(#\-) number-start?]
  [else (number-start? c)])

(define-λcase number-start?
  [(#\.) (number-more? #\.)]
  [(#\0) (λcase
          [(#\.) decimal-digit-or-exponent?]
          [else #f]) ]
  [(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) number-more?])

(define-λcase number-more?
  #:char-id c
  [(#\e #\E) decimal-digit-or-sign?]
  [(#\.) (λ (next-c)
           (or (decimal-digit-or-exponent? next-c)
               (error "expected a digit")))]
  [else (and (decimal-digit? c) number-more?)])

(define-λcase decimal-digit-or-sign?
  #:char-id c
  [(#\+ #\-) decimal-digit?]
  [else (and (decimal-digit? c) decimal-digit?)])

(define-λcase decimal-digit-or-exponent?
  #:char-id c
  [(#\e #\E) decimal-digit-or-sign?]
  [else (and (decimal-digit? c) decimal-digit-or-exponent?)])

(define-λcase decimal-digit?
  [(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) decimal-digit?])

(define-λcase hex-digit?
  #:char-id c
  [(#\a #\b #\c #\d #\e #\f) hex-digit?]
  [(#\A #\B #\C #\D #\E #\F) hex-digit?]
  [else (and (decimal-digit? c) hex-digit?)])


;; readers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ((make-reader p f) in)
  (define s (take-while in p))
  (values s (f s)))

(define json:read-number
  (make-reader signed-number-start? string->number))

(define (json:read-string in [partial? #f])
  (define quote-char (read-char in))
  (define lit-str (open-output-string))
  (define actual-bs (open-output-bytes))
  (write-char quote-char lit-str)
  (write-char quote-char actual-bs)
  (define has-end-quote?
    (let loop ([escaped? #f])
      (define char
        (read-char in))
      (cond
        [(eof-object? char)
         (cond
           [partial? #f]
           [else (error 'json:read-string "unexpected EOF while reading string")])]
        [escaped?
         (define-values (escape-seq escape-char)
           (json:string-escape char in))
         (write-string escape-seq lit-str)
         (write-char escape-char actual-bs)
         (loop #f)]
        [(eqv? char #\\)
         (loop #t)]
        [else
         (write-char char lit-str)
         (write-char char actual-bs)
         (cond
           [(eqv? char quote-char) #t]
           [else  (loop #f)])])))
  (define bs
    (let ([bs (get-output-bytes actual-bs)])
      (subbytes bs 1 ((if has-end-quote? sub1 values) (bytes-length bs)))))
  (values (get-output-string lit-str) bs))

(define (json:string-escape chr in)
  (case chr
    [(#\\) (values "\\\\" #\\)]
    [(#\b) (values "\\b"  #\backspace)]
    [(#\f) (values "\\f"  #\page)]
    [(#\n) (values "\\n"  #\newline)]
    [(#\r) (values "\\r"  #\return)]
    [(#\t) (values "\\t"  #\tab)]
    [(#\u)
     (define-values (seq code)
       (read-char-code (make-limited-input-port in 4 #f)))
     (define lit-str
       (string-append (string #\\ #\u) seq))
     (unless code
       (error 'json:string-escape "unexpected EOF while reading unicode escape char"))
     (values lit-str (integer->char code))]
    [else
     (values (string #\\ chr) chr)]))

(define read-char-code
  (make-reader hex-digit? (λ (digits)
                            (string->number digits 16))))
