#lang scribble/manual

@(require scribble/example
          (for-label json/lexer
                     json/pretty
                     racket/base
                     racket/contract/base))

@title{JSON Lexer}
@author[(author+email "Bogdan Popa" "bogdan@defn.io")]

This package provides an implementation of a JSON lexer and pretty
printer, suitable for implementing text highlighting and formatting
functionality.

@section{Lexer}
@defmodule[json/lexer]

@defstruct[token ([type symbol?]
                  [str string?]
                  [val any/c]
                  [line (or/c #f exact-positive-integer?)]
                  [col (or/c #f exact-nonnegative-integer?)]
                  [pos exact-nonnegative-integer?])
                 #:prefab ]{
  The structure that represents tokens read from the input.
}

@defproc[(lexer? [v any/c]) boolean?]{
  Returns @racket[#t] when @racket[v] is a lexer.
}

@defproc[(make-lexer [in input-port?]
                     [#:partial-strings? partial-strings? boolean? #f]) lexer?]{

  Returns a lexer that reads data from @racket[in].  When
  @racket[partial-strings?] is @racket[#t], an incomplete string at
  the end of the input is not considered an error.

  @examples[
    (require json/lexer)
    (define inp (open-input-string "[1, 2, true, {}]"))
    (port-count-lines! inp)
    (define l (make-lexer inp))
    (let loop ()
      (define t (lexer-take l))
      (unless (eq? (token-type t) 'eof)
       (println t)
       (loop)))
  ]
}

@defproc[(lexer-peek [l lexer?]) token?]{
  Returns the next token in the input stream without consuming it.
}

@defproc[(lexer-take [l lexer?]) token?]{
  Reads the next token from the input stream.
}


@section{Pretty Printer}
@defmodule[json/pretty]

@defproc[(pretty-print-json [in (or/c string? input-port?)]
                            [out output-port? (current-output-port)]
                            [#:width width exact-positive-integer? 2]) void?]{

  Streams JSON tokens from @racket[in] and writes indented output to
  @racket[out].  Minimal validation is performed at the token level,
  but not higher, which means that some malformed inputs are accepted.

  The @racket[width] parameter controls the number of spaces used per
  indentation depth.

  @examples[
    (require json/pretty)
    (pretty-print-json "[1, 2, {}, {\"a\": 1.5e+10, \"b\": []}]")
    (pretty-print-json "1 2 3 [] [\"\\u03bbambda\"]")
  ]
}
