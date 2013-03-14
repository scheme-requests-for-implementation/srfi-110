; kernel.scm
; Implementation of the sweet-expressions project by readable mailinglist.
;
; Copyright (C) 2005-2013 by David A. Wheeler and Alan Manuel K. Gloria
;
; This software is released as open source software under the "MIT" license:
;
; Permission is hereby granted, free of charge, to any person obtaining a
; copy of this software and associated documentation files (the "Software"),
; to deal in the Software without restriction, including without limitation
; the rights to use, copy, modify, merge, publish, distribute, sublicense,
; and/or sell copies of the Software, and to permit persons to whom the
; Software is furnished to do so, subject to the following conditions:
;
; The above copyright notice and this permission notice shall be included
; in all copies or substantial portions of the Software.
;
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
; OTHER DEALINGS IN THE SOFTWARE.

; Warning: For portability use eqv?/memv (not eq?/memq) to compare characters.
; A "case" is okay since it uses "eqv?".

; -----------------------------------------------------------------------------
; Compatibility Layer
; -----------------------------------------------------------------------------
; The compatibility layer is composed of:
;
;   (readable-kernel-module-contents (exports ...) body ...)
;   - a macro that should package the given body as a module, or whatever your
;     scheme calls it (chicken eggs?), preferably with one of the following
;     names, in order of preference, depending on your Scheme's package naming
;     conventions/support
;       (readable kernel)
;       readable/kernel
;       readable-kernel
;       sweetimpl
;   - The first element after the module-contents name is a list of exported
;     procedures.  This module shall never export a macro or syntax, not even
;     in the future.
;   - If your Scheme requires module contents to be defined inside a top-level
;     module declaration (unlike Guile where module contents are declared as
;     top-level entities after the module declaration) then the other
;     procedures below should be defined inside the module context in order
;     to reduce user namespace pollution.
;
;   (my-peek-char port)
;   (my-read-char port)
;   - Performs I/O on a "port" object.
;   - The algorithm assumes that port objects have the following abilities:
;     * The port automatically keeps track of source location
;       information.  On R5RS there is no source location
;       information that can be attached to objects, so as a
;       fallback you can just ignore source location, which
;       will make debugging using sweet-expressions more
;       difficult.
;   - "port" or fake port objects are created by the make-read procedure
;     below.
;   
;   (make-read procedure)
;   - The given procedure accepts exactly 1 argument, a "fake port" that can
;     be passed to my-peek-char et al.
;   - make-read creates a new procedure that supports your Scheme's reader
;     interface.  Usually, this means making a new procedure that accepts
;     either 0 or 1 parameters, defaulting to (current-input-port).
;   - If your Scheme doesn't support unlimited lookahead, you should make
;     the fake port that supports 2-char lookahead at this point.
;   - If your Scheme doesn't keep track of source location information
;     automatically with the ports, you may again need to wrap it here.
;   - If your Scheme needs a particularly magical incantation to attach
;     source information to objects, then you might need to use a weak-key
;     table in the attach-sourceinfo procedure below and then use that
;     weak-key table to perform the magical incantation.
;
;   (invoke-read read port)
;   - Accepts a read procedure, which is a (most likely built-in) procedure
;     that requires a *real* port, not a fake one.
;   - Should unwrap the fake port to a real port, then invoke the given
;     read procedure on the actual real port.
;
;   (get-sourceinfo port)
;   - Given a fake port, constructs some object (which the algorithm treats
;     as opaque) to represent the source information at the point that the
;     port is currently in.
;
;   (attach-sourceinfo pos obj)
;   - Attaches the source information pos, as constructed by get-sourceinfo,
;     to the given obj.
;   - obj can be any valid Scheme object.  If your Scheme can only track
;     source location for a subset of Scheme object types, then this procedure
;     should handle it gracefully.
;   - Returns an object with the source information attached - this can be
;     the same object, or a different object that should look-and-feel the
;     same as the passed-in object.
;   - If source information cannot be attached anyway (your Scheme doesn't
;     support attaching source information to objects), just return the
;     given object.
;
;   (replace-read-with f)
;   - Replaces your Scheme's current reader.
;   - Replace 'read and 'get-datum at the minimum.  If your Scheme
;     needs any kind of involved magic to handle load and loading
;     modules correctly, do it here.
;
;   (parse-hash no-indent-read char fake-port)
;   - a procedure that is invoked when an unrecognized, non-R5RS hash
;     character combination is encountered in the input port.
;   - this procedure is passed a "fake port", as wrapped by the
;     make-read procedure above.  You should probably use my-read-char
;     and my-peek-char in it, or at least unwrap the port (since
;     make-read does the wrapping, and you wrote make-read, we assume
;     you know how to unwrap the port).
;   - if your procedure needs to parse a datum, invoke
;     (no-indent-read fake-port).  Do NOT use any other read procedure.  The
;     no-indent-read procedure accepts exactly one parameter - the fake port
;     this procedure was passed in.
;     - no-indent-read is either a version of curly-infix-read, or a version
;       of neoteric-read; this specal version accepts only a fake port.
;       It is never a version of sweet-read.  You don't normally want to
;       call sweet-read, because sweet-read presumes that it's starting
;       at the beginning of the line, with indentation processing still
;       active.  There's no reason either must be true when processing "#".
;   - At the start of this procedure, both the # and the character
;     after it have been read in.
;   - The procedure returns one of the following:
;       #f  - the hash-character combination is invalid/not supported.
;       ()  - the hash-character combination introduced a comment;
;             at the return of this procedure with this value, the
;             comment has been removed from the input port.
;       (a) - the datum read in is the value a
;
;   hash-pipe-comment-nests?
;   - a Boolean value that specifies whether #|...|# comments
;     should nest.
;
;   my-string-foldcase
;   - a procedure to perform case-folding to lowercase, as mandated
;     by Unicode.  If your implementation doesn't have Unicode, define
;     this to be string-downcase.  Some implementations may also
;     interpret "string-downcase" as foldcase anyway.


; On Guile 2.0, the define-module part needs to occur separately from
; the rest of the compatibility checks, unfortunately.  Sigh.
(cond-expand
  (guile
    ; define the module
    ; this ensures that the user's module does not get contaminated with
    ; our compatibility procedures/macros
    (define-module (readable kernel))))
(cond-expand
; -----------------------------------------------------------------------------
; Guile Compatibility
; -----------------------------------------------------------------------------
  (guile

    ; properly get bindings
    (use-modules (guile))

    ; On Guile 1.x defmacro is the only thing supported out-of-the-box.
    ; This form still exists in Guile 2.x, fortunately.
    (defmacro readable-kernel-module-contents (exports . body)
      `(begin (export ,@exports)
              ,@body))

    ; Guile was the original development environment, so the algorithm
    ; practically acts as if it is in Guile.
    ; Needs to be lambdas because otherwise Guile 2.0 acts strangely,
    ; getting confused on the distinction between compile-time,
    ; load-time and run-time (apparently, peek-char is not bound
    ; during load-time).
    (define (my-peek-char fake-port)
      (if (eof-object? (car fake-port))
          (car fake-port)
          (let* ((port (car fake-port))
                 (char (peek-char port)))
            (if (eof-object? char)
              (set-car! fake-port char))
            char)))
    (define (my-read-char fake-port)
      (if (eof-object? (car fake-port))
          (car fake-port)
          (let* ((port (car fake-port))
                 (char (read-char port)))
            (if (eof-object? char)
              (set-car! fake-port char))
            char)))

    (define (make-read f)
      (lambda args
        (let ((port (if (null? args) (current-input-port) (car args))))
          (f (list port)))))

    (define (invoke-read read fake-port)
      (if (eof-object? (car fake-port))
        (car fake-port)
        (read (car fake-port))))

    ; create a list with the source information
    (define (get-sourceinfo fake-port)
      (if (eof-object? (car fake-port))
          #f
          (let ((port (car fake-port)))
            (list (port-filename port)
                  (port-line port)
                  (port-column port)))))
    ; destruct the list and attach, but only to cons cells, since
    ; only that is reliably supported across Guile versions.
    (define (attach-sourceinfo pos obj)
      (cond
        ((not pos)
          obj)
        ((pair? obj)
          (set-source-property! obj 'filename (list-ref pos 0))
          (set-source-property! obj 'line     (list-ref pos 1))
          (set-source-property! obj 'column   (list-ref pos 2))
          obj)
        (#t
          obj)))

    ; To properly hack into 'load and in particular 'use-modules,
    ; we need to hack into 'primitive-load.  On 1.8 and 2.0 there
    ; is supposed to be a current-reader fluid that primitive-load
    ; hooks into, but it seems (unverified) that each use-modules
    ; creates a new fluid environment, so that this only sticks
    ; on a per-module basis.  But if the project is primarily in
    ; sweet-expressions, we would prefer to have that hook in
    ; *all* 'use-modules calls.  So our primitive-load uses the
    ; 'read global variable if current-reader isn't set.

    (define %sugar-current-load-port #f)
    ; replace primitive-load
    (define primitive-load-replaced #f)
    (define (setup-primitive-load)
      (cond
        (primitive-load-replaced
           (values))
        (#t
          (module-set! (resolve-module '(guile)) 'primitive-load
            (lambda (filename)
              (let ((hook (cond
                            ((not %load-hook)
                              #f)
                            ((not (procedure? %load-hook))
                              (error "value of %load-hook is neither procedure nor #f"))
                            (#t
                              %load-hook))))
                (cond
                  (hook
                    (hook filename)))
                (let* ((port      (open-input-file filename))
                       (save-port port))
                  (define (load-loop)
                    (let* ((the-read
                             (or
                                 ; current-reader doesn't exist on 1.6
                                 (if (string=? "1.6" (effective-version))
                                     #f
                                     (fluid-ref current-reader))
                                 read))
                           (form (the-read port)))
                      (cond
                        ((not (eof-object? form))
                          ; in Guile only
                          (primitive-eval form)
                          (load-loop)))))
                  (define (swap-ports)
                    (let ((tmp %sugar-current-load-port))
                      (set! %sugar-current-load-port save-port)
                      (set! save-port tmp)))
                  (dynamic-wind swap-ports load-loop swap-ports)
                  (close-input-port port)))))
          (set! primitive-load-replaced #t))))

    (define (replace-read-with f)
      (setup-primitive-load)
      (set! read f))

    ; On Guile 1.6 and 1.8 the only comments are ; and #! !#
    ; On Guile 2.0, #; (SRFI-62) and #| #| |# |# (SRFI-30) comments exist.
    ; On Guile 2.0, #' #` #, #,@ have the R6RS meaning; on
    ; Guile 1.8 and 1.6 there is a #' syntax but I have yet
    ; to figure out what exactly it does.
    ; On Guile, #:x is a keyword.  Keywords have symbol
    ; syntax.
    (define (parse-hash no-indent-read char fake-port)
      (let* ((ver (effective-version))
             (c   (string-ref ver 0))
             (>=2 (and (not (char=? c #\0)) (not (char=? c #\1)))))
        (cond
          ((char=? char #\:)
            ; On Guile 1.6, #: reads characters until it finds non-symbol
            ; characters.
            ; On Guile 1.8 and 2.0, #: reads in a datum, and if the
            ; datum is not a symbol, throws an error.
            ; Follow the 1.8/2.0 behavior as it is simpler to implement,
            ; and even on 1.6 it is unlikely to cause problems.
            ; NOTE: This behavior means that #:foo(bar) will cause
            ; problems on neoteric and higher tiers.
            (let ((s (no-indent-read fake-port)))
              (if (symbol? s)
                  `( ,(symbol->keyword s) )
                  #f)))
          ; On Guile 2.0 #' #` #, #,@ have the R6RS meaning.
          ; guard against it here because of differences in
          ; Guile 1.6 and 1.8.
          ((and >=2 (char=? char #\'))
            `( (syntax ,(no-indent-read fake-port)) ))
          ((and >=2 (char=? char #\`))
            `( (quasisyntax ,(no-indent-read fake-port)) ))
          ((and >=2 (char=? char #\,))
            (let ((c2 (my-peek-char fake-port)))
              (cond
                ((char=? c2 #\@)
                  (my-read-char fake-port)
                  `( (unsyntax-splicing ,(no-indent-read fake-port)) ))
                (#t
                  `( (unsyntax ,(no-indent-read fake-port)) )))))
          ; #{ }# syntax
          ((char=? char #\{ )  ; Special symbol, through till ...}#
            `( ,(list->symbol (special-symbol fake-port))))
          (#t
            #f))))

    ; detect the !#
    (define (non-nest-comment fake-port)
      (let ((c (my-read-char fake-port)))
        (cond
          ((eof-object? c)
            (values))
          ((char=? c #\!)
            (let ((c2 (my-peek-char fake-port)))
              (if (char=? c2 #\#)
                  (begin
                    (my-read-char fake-port)
                    (values))
                  (non-nest-comment fake-port))))
          (#t
            (non-nest-comment fake-port)))))

  ; Return list of characters inside #{...}#, a guile extension.
  ; presume we've already read the sharp and initial open brace.
  ; On eof we just end.  We could error out instead.
  ; TODO: actually conform to Guile's syntax.  Note that 1.x
  ; and 2.0 have different syntax when spaces, backslashes, and
  ; control characters get involved.
  (define (special-symbol port)
    (cond
      ((eof-object? (my-peek-char port)) '())
      ((eqv? (my-peek-char port) #\})
        (my-read-char port) ; consume closing brace
        (cond
          ((eof-object? (my-peek-char port)) '(#\}))
          ((eqv? (my-peek-char port) #\#)
            (my-read-char port) ; Consume closing sharp.
            '())
          (#t (append '(#\}) (special-symbol port)))))
      (#t (append (list (my-read-char port)) (special-symbol port)))))

    (define hash-pipe-comment-nests? #t)

    (define (my-string-foldcase s)
      (string-downcase s))
    )
; -----------------------------------------------------------------------------
; R5RS Compatibility
; -----------------------------------------------------------------------------
  (else
    ; assume R5RS with define-syntax

    ; On R6RS, and other Scheme's, module contents must
    ; be entirely inside a top-level module structure.
    ; Use module-contents to support that.  On Schemes
    ; where module declarations are separate top-level
    ; expressions, we expect module-contents to transform
    ; to a simple (begin ...), and possibly include
    ; whatever declares exported stuff on that Scheme.
    (define-syntax readable-kernel-module-contents
      (syntax-rules ()
        ((readable-kernel-module-contents exports body ...)
          (begin body ...))))

    ; We use my-* procedures so that the
    ; "port" automatically keeps track of source position.
    ; On Schemes where that is not true (e.g. Racket, where
    ; source information is passed into a reader and the
    ; reader is supposed to update it by itself) we can wrap
    ; the port with the source information, and update that
    ; source information in the my-* procedures.

    (define (my-peek-char port) (peek-char port))
    (define (my-read-char port) (read-char port))

    ; this wrapper procedure wraps a reader procedure
    ; that accepts a "fake" port above, and converts
    ; it to an R5RS-compatible procedure.  On Schemes
    ; which support source-information annotation,
    ; but use a different way of annotating
    ; source-information from Guile, this procedure
    ; should also probably perform that attachment
    ; on exit from the given inner procedure.
    (define (make-read f)
      (lambda args
        (let ((real-port (if (null? args) (current-input-port) (car args))))
          (f real-port))))

    ; invoke the given "actual" reader, most likely
    ; the builtin one, but make sure to unwrap any
    ; fake ports.
    (define (invoke-read read port)
      (read port))
    ; R5RS doesn't have any method of extracting
    ; or attaching source location information.
    (define (get-sourceinfo _) #f)
    (define (attach-sourceinfo _ x) x)

    ; Not strictly R5RS but we expect at least some Schemes
    ; to allow this somehow.
    (define (replace-read-with f)
      (set! read f))

    ; R5RS has no hash extensions
    (define (parse-hash no-indent-read char fake-port) #f)

    ; Hash-pipe comment is not in R5RS, but support
    ; it as an extension, and make them nest.
    (define hash-pipe-comment-nests? #t)

    ; If your Scheme supports "string-foldcase", use that instead of
    ; string-downcase:
    (define (my-string-foldcase s)
      (string-downcase s))
    ))



; -----------------------------------------------------------------------------
; Module declaration and useful utilities
; -----------------------------------------------------------------------------
(readable-kernel-module-contents
  ; exported procedures
  (; tier read procedures
   curly-infix-read neoteric-read sweet-read
   ; comparison procedures
   compare-read-file ; compare-read-string
   ; replacing the reader
   replace-read restore-traditional-read
   enable-curly-infix enable-neoteric enable-sweet)

  ; Should we fold case of symbols by default?
  ; #f means case-sensitive (R6RS); #t means case-insensitive (R5RS).
  ; Here we'll set it to be case-sensitive, which is consistent with R6RS
  ; and guile, but NOT with R5RS.  Most people won't notice, I
  ; _like_ case-sensitivity, and the latest spec is case-sensitive,
  ; so let's start with #f (case-sensitive).
  ; This doesn't affect character names; as an extension,
  ; We always accept arbitrary case for them, e.g., #\newline or #\NEWLINE.
  (define is-foldcase #f)

  ; special tag to denote comment return from hash-processing

  ; Define the whitespace characters, in relatively portable ways
  ; Presumes ASCII, Latin-1, Unicode or similar.
  (define tab (integer->char #x0009))             ; #\ht aka \t.
  (define linefeed (integer->char #x000A))        ; #\newline aka \n. FORCE it.
  (define carriage-return (integer->char #x000D)) ; \r.
  (define line-tab (integer->char #x000D))
  (define form-feed (integer->char #x000C))
  (define vertical-tab (integer->char #x000b))
  (define space '#\space)

  ; Period symbol.  A symbol starting with "." is not
  ; validly readable in R5RS, R6RS, R7RS (except for
  ; the peculiar identifier "..."); with the
  ; R6RS and R7RS the print representation of
  ; string->symbol(".") should be |.| .  However, as an extension, this
  ; Scheme reader accepts "." as a valid identifier initial character,
  ; in part because guile permits it.
  ; For portability, use this formulation instead of '. in this
  ; implementation so other implementations don't balk at it.
  (define period-symbol (string->symbol "."))

  (define line-ending-chars (list linefeed carriage-return))

  ; This definition of whitespace chars is derived from R6RS section 4.2.1.
  ; R6RS doesn't explicitly list the #\space character, be sure to include!
  (define whitespace-chars-ascii
     (list tab linefeed line-tab form-feed carriage-return #\space))
  ; Note that we are NOT trying to support all Unicode whitespace chars.
  (define whitespace-chars whitespace-chars-ascii)

  ; Returns a true value (not necessarily #t)
  (define (char-line-ending? char) (memv char line-ending-chars))

  ; Create own version, in case underlying implementation omits some.
  (define (my-char-whitespace? c)
    (or (char-whitespace? c) (memv c whitespace-chars)))

  ; Consume an end-of-line sequence, ('\r' '\n'? | '\n'), and nothing else.
  ; Don't try to handle reversed \n\r (LFCR); doing so will make interactive
  ; guile use annoying (EOF won't be correctly detected) due to a guile bug
  ; (in guile, peek-char incorrectly *consumes* EOF instead of just peeking).
  (define (consume-end-of-line port)
    (let ((c (my-peek-char port)))
      (cond
        ((eqv? c carriage-return)
          (my-read-char port)
          (if (eqv? (my-peek-char port) linefeed)
            (my-read-char port)))
        ((eqv? c linefeed)
          (my-read-char port)))))

  (define (consume-to-eol port)
    ; Consume every non-eol character in the current line.
    ; End on EOF or end-of-line char.
    ; Do NOT consume the end-of-line character(s).
    (let ((c (my-peek-char port)))
      (cond
        ((and (not (eof-object? c)) (not (char-line-ending? c)))
          (my-read-char port)
          (consume-to-eol port)))))

  (define (consume-to-whitespace port)
    ; Consume to whitespace
    (let ((c (my-peek-char port)))
      (cond
        ((eof-object? c) c)
        ((my-char-whitespace? c)
          '())
        (#t
          (my-read-char port)
          (consume-to-whitespace port)))))

  (define (ismember? item lyst)
    ; Returns true if item is member of lyst, else false.
    (pair? (member item lyst)))

  (define debugger-output #f)
  ; Quick utility for debugging.  Display marker, show data, return data.
  (define (debug-show marker data)
    (cond
      (debugger-output
        (display "DEBUG: ")
        (display marker)
        (display " = ")
        (write data)
        (display "\n")))
    data)

  (define (my-read-delimited-list my-read stop-char port)
    ; Read the "inside" of a list until its matching stop-char, returning list.
    ; stop-char needs to be closing paren, closing bracket, or closing brace.
    ; This is like read-delimited-list of Common Lisp, but it
    ; calls the specified reader instead.
    ; This implements a useful extension: (. b) returns b. This is
    ; important as an escape for indented expressions, e.g., (. \\)
    (consume-whitespace port)
    (let*
      ((pos (get-sourceinfo port))
       (c   (my-peek-char port)))
      (cond
        ((eof-object? c) (read-error "EOF in middle of list") c)
        ((char=? c stop-char)
          (my-read-char port)
          (attach-sourceinfo pos '()))
        ((ismember? c '(#\) #\] #\}))  (read-error "Bad closing character") c)
        (#t
          (let ((datum (my-read port)))
            (cond
               ((eq? datum period-symbol)
                 (let ((datum2 (my-read port)))
                   (consume-whitespace port)
                   (cond
                     ((eof-object? datum2)
                      (read-error "Early eof in (... .)")
                      '())
                     ((not (eqv? (my-peek-char port) stop-char))
                      (read-error "Bad closing character after . datum")
                      datum2)
                     (#t
                       (my-read-char port)
                       datum2))))
               (#t
                 (attach-sourceinfo pos
                   (cons datum
                     (my-read-delimited-list my-read stop-char port))))))))))

; -----------------------------------------------------------------------------
; Read Preservation and Replacement
; -----------------------------------------------------------------------------

  (define default-scheme-read read)
  (define replace-read replace-read-with)
  (define (restore-traditional-read) (replace-read-with default-scheme-read))

  (define (enable-curly-infix)
    (if (not (or (eq? read curly-infix-read)
                 (eq? read neoteric-read)
                 (eq? read sweet-read)))
        (replace-read curly-infix-read)))

  (define (enable-neoteric)
    (if (not (or (eq? read neoteric-read)
                 (eq? read sweet-read)))
        (replace-read neoteric-read)))

  (define (enable-sweet)
    (replace-read sweet-read))

; -----------------------------------------------------------------------------
; Scheme Reader re-implementation
; -----------------------------------------------------------------------------

; We have to re-implement our own Scheme reader.
; This takes more code than it would otherwise because many
; Scheme readers will not consider [, ], {, and } as delimiters
; (they are not required delimiters in R5RS and R6RS).
; Thus, we cannot call down to the underlying reader to implement reading
; many types of values such as symbols.
; If your Scheme's "read" also considers [, ], {, and } as
; delimiters (and thus are not consumed when reading symbols, numbers, etc.),
; then underlying-read could be much simpler.
; We WILL call default-scheme-read on string reading (the ending delimiter
; is ", so that is no problem) - this lets us use the implementation's
; string extensions if any.

  ; Identifying the list of delimiter characters is harder than you'd think.
  ; This list is based on R6RS section 4.2.1, while adding [] and {},
  ; but removing "#" from the delimiter set.
  ; NOTE: R6RS has "#" has a delimiter.  However, R5RS does not, and
  ; R7RS probably will not - http://trac.sacrideo.us/wg/wiki/WG1Ballot3Results
  ; shows a strong vote AGAINST "#" being a delimiter.
  ; Having the "#" as a delimiter means that you cannot have "#" embedded
  ; in a symbol name, which hurts backwards compatibility, and it also
  ; breaks implementations like Chicken (has many such identifiers) and
  ; Gambit (which uses this as a namespace separator).
  ; Thus, this list does NOT have "#" as a delimiter, contravening R6RS
  ; (but consistent with R5RS, probably R7RS, and several implementations).
  ; Also - R7RS draft 6 has "|" as delimiter, but we currently don't.
  (define neoteric-delimiters
     (append (list #\( #\) #\[ #\] #\{ #\}  ; Add [] {}
                   #\" #\;)                 ; Could add #\# or #\|
             whitespace-chars))

  (define (consume-whitespace port)
    (let ((char (my-peek-char port)))
      (cond
        ((eof-object? char))
        ((eqv? char #\;)
          (consume-to-eol port)
          (consume-whitespace port))
        ((my-char-whitespace? char)
          (my-read-char port)
          (consume-whitespace port)))))

  (define (read-until-delim port delims)
    ; Read characters until eof or a character in "delims" is seen.
    ; Do not consume the eof or delimiter.
    ; Returns the list of chars that were read.
    (let ((c (my-peek-char port)))
      (cond
         ((eof-object? c) '())
         ((ismember? c delims) '())
         (#t (cons (my-read-char port) (read-until-delim port delims))))))

  (define (read-error message)
    (display "Error: ")
    (display message)
    (newline)
    ; Guile extension, but many Schemes have exceptions
    (throw 'readable)
    '())

  (define (read-number port starting-lyst)
    (string->number (list->string
      (append starting-lyst
        (read-until-delim port neoteric-delimiters)))))


  (define (process-char port)
    ; We've read #\ - returns what it represents.
    (cond
      ((eof-object? (my-peek-char port)) (my-peek-char port))
      (#t
        ; Not EOF. Read in the next character, and start acting on it.
        (let ((c (my-read-char port))
              (rest (read-until-delim port neoteric-delimiters)))
          (cond
            ((null? rest) c) ; only one char after #\ - so that's it!
            (#t
              (let ((rest-string (list->string (cons c rest))))
                (cond
                  ; Implement R6RS character names, see R6RS section 4.2.6.
                  ; As an extension, we will ALWAYS accept character names
                  ; of any case, no matter what the case-folding value is.
                  ((string-ci=? rest-string "space") #\space)
                  ((string-ci=? rest-string "newline") #\newline)
                  ((string-ci=? rest-string "tab") tab)
                  ((string-ci=? rest-string "nul") (integer->char #x0000))
                  ((string-ci=? rest-string "alarm") (integer->char #x0007))
                  ((string-ci=? rest-string "backspace") (integer->char #x0008))
                  ((string-ci=? rest-string "linefeed") (integer->char #x000A))
                  ((string-ci=? rest-string "vtab") (integer->char #x000B))
                  ((string-ci=? rest-string "page") (integer->char #x000C))
                  ((string-ci=? rest-string "return") (integer->char #x000D))
                  ((string-ci=? rest-string "esc") (integer->char #x001B))
                  ((string-ci=? rest-string "delete") (integer->char #x007F))
                  ; Additional character names as extensions:
                  ((string-ci=? rest-string "ht") tab)
                  ((string-ci=? rest-string "cr") (integer->char #x000d))
                  ((string-ci=? rest-string "bs") (integer->char #x0008))
                  (#t (read-error "Invalid character name"))))))))))

  ; If fold-case is active on this port, return string "s" in folded case.
  ; Otherwise, just return "s".  This is needed to support our
  ; is-foldcase configuration value when processing symbols.
  ; TODO: Should be port-specific
  (define (fold-case-maybe port s)
    (if is-foldcase
      (my-string-foldcase s)
      s))

  (define (process-directive dir)
    (cond
      ; TODO: These should be specific to the port.
      ((string-ci=? dir "sweet")
        (enable-sweet))
      ((string-ci=? dir "no-sweet")
        (restore-traditional-read))
      ((string-ci=? dir "curly-infix")
        (replace-read curly-infix-read))
      ((string-ci=? dir "fold-case")
        (set! is-foldcase #t))
      ((string-ci=? dir "no-fold-case")
        (set! is-foldcase #f))
      (#t (display "Warning: Unknown process directive"))))

  (define (process-sharp-bang port)
    (let ((c (my-peek-char port)))
      (cond
        ((char=? c #\space) ; SRFI-22
          (consume-to-eol port)
          (consume-end-of-line port)
          '() ) ; Treat as comment.
        ((memv c '(#\/ #\.)) ; Multi-line, non-nesting #!/ ... !# or #!. ...!#
          (non-nest-comment port)
          '() ) ; Treat as comment.
        ((char-alphabetic? c)  ; #!directive
          (process-directive
            (list->string (read-until-delim port neoteric-delimiters)))
          '() )
        ((or (eqv? c carriage-return) (eqv? c #\newline))
          ; Extension: Ignore lone #!
          (consume-to-eol port)
          (consume-end-of-line port)
          '() ) ; Treat as comment.
        (#t (read-error "Unsupported #! combination")))))

  (define (process-sharp no-indent-read port)
    ; We've read a # character.  Returns a list whose car is what it
    ; represents; empty list means "comment".
    ; Note: Since we have to re-implement process-sharp anyway,
    ; the vector representation #(...) uses my-read-delimited-list, which in
    ; turn calls no-indent-read.
    ; TODO: Create a readtable for this case.
    (let ((c (my-peek-char port)))
      (cond
        ((eof-object? c) (list c)) ; If eof, return eof.
        (#t
          ; Not EOF. Read in the next character, and start acting on it.
          (my-read-char port)
          (cond
            ((char-ci=? c #\t)  '(#t))
            ((char-ci=? c #\f)  '(#f))
            ((ismember? c '(#\i #\e #\b #\o #\d #\x
                            #\I #\E #\B #\O #\D #\X))
              (list (read-number port (list #\# (char-downcase c)))))
            ((char=? c #\( )  ; Vector.
              (list (list->vector
                (my-read-delimited-list no-indent-read #\) port))))
            ((char=? c #\u )  ; u8 Vector.
              (cond
                ((not (eqv? (my-read-char port) #\8 ))
                  (read-error "#u must be followed by 8"))
                ((not (eqv? (my-read-char port) #\( ))
                  (read-error "#u8 must be followed by left paren"))
                (#t (list (list->u8vector
                      (my-read-delimited-list no-indent-read #\) port))))))
            ((char=? c #\\) (list (process-char port)))
            ; Handle #; (item comment).
            ((char=? c #\;)
              (no-indent-read port)  ; Read the datum to be consumed.
              '()) ; Return comment
            ; handle nested comments
            ((char=? c #\|)
              (nest-comment port)
              '()) ; Return comment
            ((char=? c #\!)
              (process-sharp-bang port))
            (#t
              (let ((rv (parse-hash no-indent-read c port)))
                (cond
                  ((not rv)
                    (read-error "Invalid #-prefixed string"))
                  (#t
                    rv)))))))))

  ; detect #| or |#
  (define (nest-comment fake-port)
    (let ((c (my-read-char fake-port)))
      (cond
        ((eof-object? c)
          (values))
        ((char=? c #\|)
          (let ((c2 (my-peek-char fake-port)))
            (if (char=? c2 #\#)
                (begin
                  (my-read-char fake-port)
                  (values))
                (nest-comment fake-port))))
        ((and hash-pipe-comment-nests? (char=? c #\#))
          (let ((c2 (my-peek-char fake-port)))
            (if (char=? c2 #\|)
                (begin
                  (my-read-char fake-port)
                  (nest-comment fake-port))
                (values))
            (nest-comment fake-port)))
        (#t
          (nest-comment fake-port)))))

  (define digits '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

  (define (process-period port)
    ; We've peeked a period character.  Returns what it represents.
    (my-read-char port) ; Remove .
    (let ((c (my-peek-char port)))
      (cond
        ((eof-object? c) period-symbol) ; period eof; return period.
        ((ismember? c digits)
          (read-number port (list #\.)))  ; period digit - it's a number.
        (#t
          ; At this point, Scheme only requires support for "." or "...".
          ; As an extension we can support them all.
          (string->symbol
            (fold-case-maybe port
              (list->string (cons #\.
                (read-until-delim port neoteric-delimiters)))))))))

  ; Read an inline hex escape (after \x), return the character it represents
  (define (read-inline-hex-escape port)
    (let* ((chars (read-until-delim port '(#\;)))
           (n (string->number (list->string chars) 16)))
      (if (not (eof-object? (my-peek-char port)))
        (my-read-char port))
      (if (not n)
        (read-error "Bad inline hex escape"))
      (integer->char n)))

  ; We're inside |...| ; return the list of characters inside.
  ; Do NOT call fold-case-maybe, because we always use literal values here.
  (define (read-symbol-elements port)
    (let ((c (my-read-char port)))
      (cond
        ((eof-object? c) '())
        ((eqv? c #\|)    '()) ; Expected end of symbol elements
        ((eqv? c #\\)
          (let ((c2 (my-read-char port)))
            (cond
              ((eof-object? c) '())
              ((eqv? c2 #\|)   (cons #\| (read-symbol-elements port)))
              ((eqv? c2 #\\)   (cons #\\ (read-symbol-elements port)))
              ((eqv? c2 #\a)   (cons (integer->char #x0007)
                                     (read-symbol-elements port)))
              ((eqv? c2 #\b)   (cons (integer->char #x0008)
                                     (read-symbol-elements port)))
              ((eqv? c2 #\t)   (cons (integer->char #x0009)
                                     (read-symbol-elements port)))
              ((eqv? c2 #\n)   (cons (integer->char #x000a)
                                     (read-symbol-elements port)))
              ((eqv? c2 #\r)   (cons (integer->char #x000d)
                                     (read-symbol-elements port)))
              ((eqv? c2 #\f)   (cons (integer->char #x000c) ; extension
                                     (read-symbol-elements port)))
              ((eqv? c2 #\v)   (cons (integer->char #x000b) ; extension
                                     (read-symbol-elements port)))
              ((eqv? c2 #\x) ; inline hex escape =  \x hex_scalar_value ;
                (cons 
                      (read-inline-hex-escape port)
                      (read-symbol-elements port)))
          )))
        (#t (cons c (read-symbol-elements port))))))

  ; This implements a simple Scheme "read" implementation from "port",
  ; but if it must recurse to read, it will invoke "no-indent-read"
  ; (a reader that is NOT indentation-sensitive).
  ; This additional parameter lets us easily implement additional semantics,
  ; and then call down to this underlying-read procedure when basic reader
  ; procedureality (implemented here) is needed.
  ; This lets us implement both a curly-infix-ONLY-read
  ; as well as a neoteric-read, without duplicating code.
  (define (underlying-read no-indent-read port)
    (consume-whitespace port)
    (let* ((pos (get-sourceinfo port))
           (c   (my-peek-char port)))
      (cond
        ((eof-object? c) c)
        ((char=? c #\")
          ; old readers tend to read strings okay, call it.
          ; (guile 1.8 and gauche/gosh 1.8.11 are fine)
          (invoke-read default-scheme-read port))
        (#t
          ; attach the source information to the item read-in
          (attach-sourceinfo pos
            (cond
              ((ismember? c digits) ; Initial digit.
                (read-number port '()))
              ((char=? c #\#)
                (my-read-char port)
                (let ((rv (process-sharp no-indent-read port)))
                  ; process-sharp convention: null? means comment,
                  ; pair? means object (the object is in its car)
                  (cond
                    ((null? rv)
                      ; recurse
                      (no-indent-read port))
                    ((pair? rv)
                      (car rv))
                    (#t ; convention violated
                      (read-error "readable/kernel: ***ERROR IN COMPATIBILITY LAYER parse-hash must return #f '() or `(,a)")))))
              ((char=? c #\.) (process-period port))
              ((or (char=? c #\+) (char=? c #\-))  ; Initial + or -
                (my-read-char port)
                (let*
                  ((maybe-number (list->string (cons c
                     (read-until-delim port neoteric-delimiters))))
                   (as-number (string->number maybe-number)))
                  (if as-number
                    as-number
                    (string->symbol (fold-case-maybe port maybe-number)))))
              ((char=? c #\')
                (my-read-char port)
                (list (attach-sourceinfo pos 'quote)
                  (no-indent-read port)))
              ((char=? c #\`)
                (my-read-char port)
                (list (attach-sourceinfo pos 'quasiquote)
                  (no-indent-read port)))
              ((char=? c #\,)
                (my-read-char port)
                  (cond
                    ((char=? #\@ (my-peek-char port))
                      (my-read-char port)
                      (list (attach-sourceinfo pos 'unquote-splicing)
                       (no-indent-read port)))
                   (#t
                    (list (attach-sourceinfo pos 'unquote)
                      (no-indent-read port)))))
              ((char=? c #\( )
                  (my-read-char port)
                  (my-read-delimited-list no-indent-read #\) port))
              ((char=? c #\) )
                (my-read-char port)
                (read-error "Closing parenthesis without opening")
                (underlying-read no-indent-read port))
              ((char=? c #\[ )
                  (my-read-char port)
                  (my-read-delimited-list no-indent-read #\] port))
              ((char=? c #\] )
                (my-read-char port)
                (read-error "Closing bracket without opening")
                (underlying-read no-indent-read port))
              ((char=? c #\} )
                (my-read-char port)
                (read-error "Closing brace without opening")
                (underlying-read no-indent-read port))
              ((char=? c #\| )
                ; Read |...| symbol (like Common Lisp)
                ; This is present in R7RS draft 9.
                (my-read-char port) ; Consume the initial vertical bar.
                (string->symbol (list->string (read-symbol-elements port))))
              (#t ; Nothing else.  Must be a symbol start.
                (string->symbol (fold-case-maybe port
                  (list->string
                    (read-until-delim port neoteric-delimiters)))))))))))

; -----------------------------------------------------------------------------
; Curly Infix
; -----------------------------------------------------------------------------

  ; Return true if lyst has an even # of parameters, and the (alternating)
  ; first parameters are "op".  Used to determine if a longer lyst is infix.
  ; If passed empty list, returns true (so recursion works correctly).
  (define (even-and-op-prefix? op lyst)
    (cond
      ((null? lyst) #t)
      ((not (pair? lyst)) #f)
      ((not (equal? op (car lyst))) #f) ; fail - operators not the same
      ((not (pair? (cdr lyst)))  #f) ; Wrong # of parameters or improper
      (#t   (even-and-op-prefix? op (cddr lyst))))) ; recurse.

  ; Return true if the lyst is in simple infix format
  ; (and thus should be reordered at read time).
  (define (simple-infix-list? lyst)
    (and
      (pair? lyst)           ; Must have list;  '() doesn't count.
      (pair? (cdr lyst))     ; Must have a second argument.
      (pair? (cddr lyst))    ; Must have a third argument (we check it
                             ; this way for performance)
      (even-and-op-prefix? (cadr lyst) (cdr lyst)))) ; true if rest is simple

  ; Return alternating parameters in a list (1st, 3rd, 5th, etc.)
  (define (alternating-parameters lyst)
    (if (or (null? lyst) (null? (cdr lyst)))
      lyst
      (cons (car lyst) (alternating-parameters (cddr lyst)))))

  ; Not a simple infix list - transform it.  Written as a separate procedure
  ; so that future experiments or SRFIs can easily replace just this piece.
  (define (transform-mixed-infix lyst)
     (cons '$nfx$ lyst))

  ; Given curly-infix lyst, map it to its final internal format.
  (define (process-curly lyst)
    (cond
     ((not (pair? lyst)) lyst) ; E.G., map {} to ().
     ((null? (cdr lyst)) ; Map {a} to a.
       (car lyst))
     ((and (pair? (cdr lyst)) (null? (cddr lyst))) ; Map {a b} to (a b).
       lyst)
     ((simple-infix-list? lyst) ; Map {a OP b [OP c...]} to (OP a b [c...])
       (cons (cadr lyst) (alternating-parameters lyst)))
     (#t  (transform-mixed-infix lyst))))


  (define (curly-infix-read-real no-indent-read port)
    (let* ((pos (get-sourceinfo port))
            (c   (my-peek-char port)))
      (cond
        ((eof-object? c) c)
        ((eqv? c #\;)
          (consume-to-eol port)
          (curly-infix-read-real no-indent-read port))
        ((my-char-whitespace? c)
          (my-read-char port)
          (curly-infix-read-real no-indent-read port))
        ((eqv? c #\{)
          (my-read-char port)
          ; read in as infix
          (attach-sourceinfo pos
            (process-curly
              (my-read-delimited-list neoteric-read-real #\} port))))
        (#t
          (underlying-read no-indent-read port)))))

  ; Read using curly-infix-read-real
  (define (curly-infix-read-nocomment port)
    (curly-infix-read-real curly-infix-read-nocomment port))

; -----------------------------------------------------------------------------
; Neoteric Expressions
; -----------------------------------------------------------------------------

  ; Implement neoteric-expression's prefixed (), [], and {}.
  ; At this point, we have just finished reading some expression, which
  ; MIGHT be a prefix of some longer expression.  Examine the next
  ; character to be consumed; if it's an opening paren, bracket, or brace,
  ; then the expression "prefix" is actually a prefix.
  ; Otherwise, just return the prefix and do not consume that next char.
  ; This recurses, to handle formats like f(x)(y).
  (define (neoteric-process-tail port prefix)
      (let* ((pos (get-sourceinfo port))
             (c   (my-peek-char port)))
        (cond
          ((eof-object? c) prefix)
          ((char=? c #\( ) ; Implement f(x)
            (my-read-char port)
            (neoteric-process-tail port
              (attach-sourceinfo pos
                (cons prefix (my-read-delimited-list neoteric-read-nocomment #\) port)))))
          ((char=? c #\[ )  ; Implement f[x]
            (my-read-char port)
            (neoteric-process-tail port
                (attach-sourceinfo pos
                  (cons (attach-sourceinfo pos '$bracket-apply$)
                    (cons prefix
                      (my-read-delimited-list neoteric-read-nocomment #\] port))))))
          ((char=? c #\{ )  ; Implement f{x}
            (my-read-char port)
            (neoteric-process-tail port
              (attach-sourceinfo pos
                (let
                  ((tail (process-curly
                      (my-read-delimited-list neoteric-read-nocomment #\} port))))
                  (if (eqv? tail '())
                    (list prefix) ; Map f{} to (f), not (f ()).
                    (list prefix tail))))))
          (#t prefix))))


  ; This is the "real" implementation of neoteric-read.
  ; It directly implements unprefixed (), [], and {} so we retain control;
  ; it calls neoteric-process-tail so f(), f[], and f{} are implemented.
  ;  (if (eof-object? (my-peek-char port))
  (define (neoteric-read-real port)
    (let*
      ((pos (get-sourceinfo port))
       (c   (my-peek-char port))
       (result
         (cond
           ((eof-object? c) c)
           ((char=? c #\( )
             (my-read-char port)
             (attach-sourceinfo pos
               (my-read-delimited-list neoteric-read-nocomment #\) port)))
           ((char=? c #\[ )
             (my-read-char port)
             (attach-sourceinfo pos
               (my-read-delimited-list neoteric-read-nocomment #\] port)))
           ((char=? c #\{ )
             (my-read-char port)
             (attach-sourceinfo pos
               (process-curly
                 (my-read-delimited-list neoteric-read-nocomment #\} port))))
           ((my-char-whitespace? c)
             (my-read-char port)
             (neoteric-read-real port))
           ((eqv? c #\;)
             (consume-to-eol port)
             (neoteric-read-real port))
           (#t (underlying-read neoteric-read-nocomment port)))))
      (if (eof-object? result)
        result
        (neoteric-process-tail port result))))

  (define (neoteric-read-nocomment port)
    (neoteric-read-real port))

; -----------------------------------------------------------------------------
; Sweet Expressions (this implementation maps to the BNF)
; -----------------------------------------------------------------------------

  (define group_split (string->symbol "\\\\"))
  (define group-split-char #\\ ) ; First character of split symbol.
  (define non-whitespace-indent #\!) ; Non-whitespace-indent char.
  (define sublist (string->symbol "$"))
  (define sublist-char #\$) ; First character of sublist symbol.
  (define period_symbol period-symbol)

; TODO: Further cleanup - remove unnecessary declarations, etc.

  (define (indentation>? indentation1 indentation2)
    (let ((len1 (string-length indentation1))
            (len2 (string-length indentation2)))
      (and (> len1 len2)
             (string=? indentation2 (substring indentation1 0 len2)))))

  (define initial_comment_eol (list #\; #\newline carriage-return))
  (define (is_initial_comment_eol c)
    (or
       (eof-object? c)
       (memv c initial_comment_eol)))

  ; Return #t if char is space or tab.
  (define (char-hspace? char)
    (or (eqv? char #\space)
        (eqv? char tab)))

  ; Consume 0+ spaces or tabs
  (define (hspaces port)
    (cond
      ((char-hspace? (my-peek-char port))
        (my-read-char port)
        (hspaces port))))

  ; Return #t if char is space, tab, or !
  (define (char-ichar? char)
    (or (eqv? char #\space)
        (eqv? char tab)
        (eqv? char non-whitespace-indent)))

  (define (accumulate-ichar port)
    (if (char-ichar? (my-peek-char port))
        (cons (my-read-char port) (accumulate-ichar port))
        '()))

  (define (consume-ff-vt port)
    (let ((c (my-peek-char port)))
      (cond
        ((or (eqv? c form-feed) (eqv? c vertical-tab))
          (my-read-char port)
          (consume-ff-vt port)))))

  ; Read an n-expression.  Returns ('scomment '()) if it's an scomment,
  ; else returns ('normal n_expr).
  ; Note: If a *value* begins with #, process any potential neoteric tail,
  ; so constructs like #f() work.
  (define (n_expr_or_scomment port)
    (if (eqv? (my-peek-char port) #\#)
      (let* ((consumed-sharp (my-read-char port))
             (result (process-sharp neoteric-read-nocomment port)))
        (if (null? result)
          (list 'scomment '())
          (list 'normal
            (neoteric-process-tail port
              (car result)))))
      (list 'normal (neoteric-read-nocomment port))))

  ; Read an n-expression.  Returns ('normal n_expr) in most cases;
  ; if it's a special marker, the car is the marker name instead of 'normal.
  ; Markers only have special meaning if their first character is
  ; the "normal" character, e.g., {$} is not a sublist.
  ; Call "process-sharp" if first char is "#".
  (define (n_expr port)
    (let* ((c (my-peek-char port))
           (results (n_expr_or_scomment port))
           (type (car results))
           (expr (cadr results)))
      (if (eq? (car results) 'scomment)
        results
        (cond
          ((and (eq? expr sublist) (eqv? c sublist-char))
            (list 'sublist_marker '()))
          ((and (eq? expr group_split) (eqv? c group-split-char))
            (list 'group_split_marker '()))
          ((and (eq? expr '<*) (eqv? c #\<))
            (list 'collecting '()))
          ((and (eq? expr '*>) (eqv? c #\*))
            (list 'collecting_end '()))
          ((and (eq? expr '$$$) (eqv? c #\$))
            (read-error "Error - $$$ is reserved"))
          (#t
            results)))))

  ; read n_expr, but process collecting list.
  (define (n_expr_or_collecting port)
    (let* ((results (n_expr port))
           (type (car results))
           (expr (cadr results)))
      (cond
        ((eq? type 'collecting)
          (list 'normal (collecting_tail port)))
        (#t results))))

  ; Check if we have abbrev+whitespace.  If the current peeked character
  ; is one of certain whitespace chars,
  ; return 'abbrevw as the marker and abbrev_procedure
  ; as the value (the cadr). Otherwise, return ('normal n_expr).
  ; We do NOT consume the peeked char (so EOL can be examined later).
  ; Note that this calls the neoteric-read procedure directly, because
  ; quoted markers are no longer markers. E.G., '$ is just (quote $).
  (define (maybe-initial-abbrev port abbrev_procedure)
    (let ((c (my-peek-char port)))
      (if (or (char-hspace? c) (eqv? c carriage-return) (eqv? c linefeed))
        (list 'abbrevw abbrev_procedure)
        (list 'normal (list abbrev_procedure (neoteric-read-nocomment port))))))

  ; Read the first n_expr on a line; handle abbrev+whitespace specially.
  ; Returns ('normal VALUE) in most cases.
  (define (n_expr_first port)
    (case (my-peek-char port)
      ((#\') 
        (my-read-char port)
        (maybe-initial-abbrev port 'quote))
      ((#\`) 
        (my-read-char port)
        (maybe-initial-abbrev port 'quasiquote))
      ((#\,) 
        (my-read-char port)
        (if (eqv? (my-peek-char port) #\@)
          (begin
            (my-read-char port)
            (maybe-initial-abbrev port 'unquote-splicing))
          (maybe-initial-abbrev port 'unquote)))
      (else
        (n_expr port))))

  ; Consume ;-comment (if there), consume EOL, and return new indent.
  ; Skip ;-comment-only lines; a following indent-only line is empty.
  (define (comment_eol_read_indent port)
    (consume-to-eol port)
    (consume-end-of-line port)
    (let* ((indentation (list->string (cons #\^ (accumulate-ichar port))))
           (c (my-peek-char port)))
      (cond
        ((eqv? c #\;)  ; A ;-only line, consume and try again.
          (comment_eol_read_indent port))
        ((is_initial_comment_eol c) ; Indent-only line
          "^")
        (#t indentation))))

  ; Utility function:
  ; If x is a 1-element list, return (car x), else return x
  (define (monify x)
    (cond
      ((not (pair? x)) x)
      ((null? (cdr x)) (car x))
      (#t x)))

  ; Return contents of collecting_tail.
  (define (collecting_tail port)
    (let* ((c (my-peek-char port)))
      (cond
        ((eof-object? c)
         (read-error "Collecting tail: EOF before collecting list ended"))
        ((is_initial_comment_eol c)
          (consume-to-eol port)
          (consume-end-of-line port)
          (collecting_tail port))
        ((char-ichar? c)
          (let ((indentation (accumulate-ichar port)))
            (if (is_initial_comment_eol (my-peek-char port))
              (collecting_tail port)
              (read-error "Collecting tail: Only ; or EOL after indent"))))
        ((or (eqv? c form-feed) (eqv? c vertical-tab))
          (consume-ff-vt port)
          (if (is_initial_comment_eol (my-peek-char port))
            (collecting_tail port)
            (read-error "Collecting tail: FF and VT must be alone on line")))
        (#t
          (let* ((it_full_results (it_expr port "^"))
                 (it_new_indent   (car it_full_results))
                 (it_value        (cadr it_full_results)))
            (cond
              ((string=? it_new_indent "")
                ; Specially compensate for "*>" at the end of a line if it's
                ; after something else.  This must be interpreted as EOL *>,
                ; which would cons a () after the result.
                ; Directly calling list for a non-null it_value has
                ; the same effect, but is a lot quicker and simpler.
                (if (null? it_value)
                   it_value
                   (list it_value)))
              (#t (cons it_value (collecting_tail port)))))))))

  ; Returns (stopper computed_value).
  ; The stopper may be 'normal, 'scomment (special comment),
  ; 'abbrevw (initial abbreviation), 'sublist_marker, or 'group_split_marker
  (define (head port)
    (let* ((basic_full_results (n_expr_first port))
           (basic_special      (car basic_full_results))
           (basic_value        (cadr basic_full_results)))
      (cond
        ((eq? basic_special 'collecting)
          (hspaces port)
          (let* ((ct_results (collecting_tail port)))
            (hspaces port)
            (if (not (is_initial_comment_eol (my-peek-char port)))
              (let* ((rr_full_results (rest port))
                     (rr_stopper      (car rr_full_results))
                     (rr_value        (cadr rr_full_results)))
                (list rr_stopper (cons ct_results rr_value)))
              (list 'normal (list ct_results)))))
        ((not (eq? basic_special 'normal)) basic_full_results)
        ((eq? basic_value period_symbol)
          (if (char-hspace? (my-peek-char port))
            (begin
              (hspaces port)
              (if (not (is_initial_comment_eol (my-peek-char port)))
                (let* ((pn_full_results (n_expr port))
                       (pn_stopper      (car pn_full_results))
                       (pn_value        (cadr pn_full_results)))
                  (hspaces port)
                  (if (not (is_initial_comment_eol (my-peek-char port)))
                    (read-error "Illegal value after . value in head"))
                  (list pn_stopper pn_value))
                (list 'normal (list period_symbol))))
            (list 'normal (list period_symbol))))
        ((char-hspace? (my-peek-char port))
          (hspaces port)
          (if (not (is_initial_comment_eol (my-peek-char port)))
            (let* ((br_full_results (rest port))
                   (br_stopper      (car br_full_results))
                   (br_value        (cadr br_full_results)))
              (list br_stopper (cons basic_value br_value)))
            (list 'normal (list basic_value))))
        (#t 
          (list 'normal (list basic_value))))))

  ; Returns (stopper computed_value); stopper may be 'normal, etc.
  ; Read in one n_expr, then process based on whether or not it's special.
  (define (rest port)
    (let* ((basic_full_results (n_expr port))
           (basic_special      (car basic_full_results))
           (basic_value        (cadr basic_full_results)))
      (cond
        ((eq? basic_special 'scomment)
          (hspaces port)
          (if (not (is_initial_comment_eol (my-peek-char port)))
            (rest port)
            (list 'normal '())))
        ((eq? basic_special 'collecting)
          (hspaces port)
          (let* ((ct_results (collecting_tail port)))
            (hspaces port)
            (if (not (is_initial_comment_eol (my-peek-char port)))
              (let* ((rr_full_results (rest port))
                     (rr_stopper      (car rr_full_results))
                     (rr_value        (cadr rr_full_results)))
                (list rr_stopper (cons ct_results rr_value)))
              (list 'normal (list ct_results)))))
        ((not (eq? basic_special 'normal)) (list basic_special '())) 
        ((eq? basic_value period_symbol) ; special case: period.
          (if (char-hspace? (my-peek-char port))
            (begin
              (hspaces port)
              (if (not (is_initial_comment_eol (my-peek-char port)))
                (let* ((pn_full_results (n_expr_or_collecting port))
                       (pn_stopper      (car pn_full_results))
                       (pn_value        (cadr pn_full_results)))
                  (hspaces port)
                  (if (not (is_initial_comment_eol (my-peek-char port)))
                    (let* ((e_full_results (n_expr port))
                           (e_stopper      (car e_full_results))
                           (e_value        (cadr e_full_results)))
                      (if (memq e_stopper '(normal sublist_marker))
                        (read-error "Illegal value after . value, rest of line")
                        (list e_stopper pn_value)))
                    (list pn_stopper pn_value)))
                (list 'normal (list period_symbol))))
            (list 'normal (list period_symbol))))
        ((char-hspace? (my-peek-char port))
          (hspaces port)
          (if (not (is_initial_comment_eol (my-peek-char port)))
            (let* ((br_full_results (rest port))
                   (br_stopper      (car br_full_results))
                   (br_value        (cadr br_full_results)))
              (list br_stopper (cons basic_value br_value)))
            (list 'normal (list basic_value))))
        (#t (list 'normal (list basic_value))))))

  ; Returns (new_indent computed_value)
  (define (body port starting_indent)
    (let* ((i_full_results (it_expr port starting_indent))
           (i_new_indent   (car i_full_results))
           (i_value        (cadr i_full_results)))
      (if (string=? starting_indent i_new_indent)
        (if (eq? i_value period_symbol)
          (let* ((f_full_results (it_expr port i_new_indent))
                 (f_new_indent   (car f_full_results))
                 (f_value        (cadr f_full_results)))
            (if (not (indentation>? starting_indent f_new_indent))
              (read-error "Dedent required after lone . and value line"))
            (list f_new_indent f_value)) ; final value of improper list
          (let* ((nxt_full_results (body port i_new_indent))
                 (nxt_new_indent   (car nxt_full_results))
                 (nxt_value        (cadr nxt_full_results)))
            (list nxt_new_indent (cons i_value nxt_value))))
        (list i_new_indent (list i_value))))) ; dedent - end list.

  ; Returns (new_indent computed_value)
  (define (it_expr_real port starting_indent)
    (let* ((head_full_results (head port))
           (head_stopper      (car head_full_results))
           (head_value        (cadr head_full_results)))
      (if (and (not (null? head_value)) (not (eq? head_stopper 'abbrevw)))
        ; The head... branches:
        (cond
          ((eq? head_stopper 'group_split_marker)
            (hspaces port)
            (if (is_initial_comment_eol (my-peek-char port))
              (read-error "Cannot follow split with end of line")
              (list starting_indent (monify head_value))))
          ((eq? head_stopper 'sublist_marker)
            (hspaces port)
            (if (is_initial_comment_eol (my-peek-char port))
              (read-error "EOL illegal immediately after sublist"))
            (let* ((sub_i_full_results (it_expr port starting_indent))
                   (sub_i_new_indent   (car sub_i_full_results))
                   (sub_i_value        (cadr sub_i_full_results)))
              (list sub_i_new_indent
                (append head_value (list sub_i_value)))))
          ((eq? head_stopper 'collecting_end)
            ; Note that indent is "", forcing dedent all the way out.
            (list "" (monify head_value)))
          ((is_initial_comment_eol (my-peek-char port))
            (let ((new_indent (comment_eol_read_indent port)))
              (if (indentation>? new_indent starting_indent)
                (let* ((body_full_results (body port new_indent))
                       (body_new_indent (car body_full_results))
                       (body_value      (cadr body_full_results)))
                  (list body_new_indent (append head_value body_value)))
                (list new_indent (monify head_value)))))
          (#t
            (read-error "Must end line with end-of-line sequence")))
        ; Here, head begins with something special like GROUP_SPLIT:
        (cond
          ((or (eq? head_stopper 'group_split_marker)
               (eq? head_stopper 'scomment))
            (hspaces port)
            (if (not (is_initial_comment_eol (my-peek-char port)))
              (it_expr port starting_indent) ; Skip and try again.
              (let ((new_indent (comment_eol_read_indent port)))
                (cond
                  ((indentation>? new_indent starting_indent)
                    (body port new_indent))
                  ((string=? starting_indent new_indent)
                    (if (not (is_initial_comment_eol (my-peek-char port)))
                      (it_expr port new_indent)
                      (list new_indent (t_expr port)))) ; Restart, no indent.
                  (#t
                    (read-error "GROUP_SPLIT EOL DEDENT illegal"))))))
          ((eq? head_stopper 'sublist_marker)
            (hspaces port)
            (if (is_initial_comment_eol (my-peek-char port))
              (read-error "EOL illegal immediately after solo sublist"))
            (let* ((is_i_full_results (it_expr port starting_indent))
                   (is_i_new_indent   (car is_i_full_results))
                   (is_i_value        (cadr is_i_full_results)))
              (list is_i_new_indent
                (list is_i_value))))
          ((eq? head_stopper 'abbrevw)
            (hspaces port)
            (if (is_initial_comment_eol (my-peek-char port))
              (begin
                (let ((new_indent (comment_eol_read_indent port)))
                  (if (not (indentation>? new_indent starting_indent))
                    (read-error "Indent required after solo abbreviation"))
                  (let* ((ab_full_results (body port new_indent))
                         (ab_new_indent   (car ab_full_results))
                         (ab_value      (cadr ab_full_results)))
                    (list ab_new_indent
                      (append (list head_value) ab_value)))))
              (let* ((ai_full_results (it_expr port starting_indent))
                     (ai_new_indent (car ai_full_results))
                     (ai_value    (cadr ai_full_results)))
                (list ai_new_indent
                  (list head_value ai_value)))))
          ((eq? head_stopper 'collecting_end)
            (list "" head_value))
          (#t 
            (read-error "Initial head error"))))))

  ; Read it_expr.  This is a wrapper that attaches source info
  ; and checks for consistent indentation results.
  (define (it_expr port starting_indent)
    (let* ((pos (get-sourceinfo port))
           (results (it_expr_real port starting_indent))
           (results_indent (car results))
           (results_value (cadr results)))
      (if (indentation>? results_indent starting_indent)
        (read-error "Inconsistent indentation"))
      (list results_indent (attach-sourceinfo pos results_value))))

  ; Top level - read a sweet-expression (t-expression).  Handle special
  ; cases, such as initial indent; call it_expr for normal case.
  (define (t_expr_real port)
    (let* ((c (my-peek-char port)))
      (if (eof-object? c) ; Check EOF early (a guile bug consumes EOF on peek)
        c
        (cond
          ((is_initial_comment_eol c)
            (consume-to-eol port)
            (consume-end-of-line port)
            (t_expr port))
          ((or (eqv? c form-feed) (eqv? c vertical-tab))
            (consume-ff-vt port))
          ((char-ichar? c)
            (let ((indentation-list (cons #\^ (accumulate-ichar port))))
              (if (memv #\! indentation-list)
                (read-error "Initial ident must not use '!'")
                (if (not (memv (my-peek-char port) initial_comment_eol))
                  (let ((results (n_expr_or_scomment port)))
                    (if (eq? (car results) 'scomment)
                      (begin
                        (hspaces port)
                        (t_expr port)) ; Consume scomment, try again.
                      (cadr results))) ; Return one value.
                  (begin ; Indented comment_eol, consume and try again.
                    (consume-to-eol port)
                    (consume-end-of-line port)
                    (t_expr port))))))
          (#t (cadr (it_expr port "^")))))))

  (define (read_to_blank_line port)
    (consume-to-eol port)
    (consume-end-of-line port)
    (let* ((c (my-peek-char port)))
      (if (not (or (eof-object? c)
                   (eqv? c carriage-return)
                   (eqv? c linefeed)))
        (read_to_blank_line port))))

  ; Call on sweet-expression reader - use guile's nonstandard catch/throw
  ; so that errors will force a restart.
  (define (t_expr port)
    (catch 'readable
      (lambda () (t_expr_real port))
      (lambda (key . args) (read_to_blank_line port) (t_expr port))))

; -----------------------------------------------------------------------------
; Comparison procedures
; -----------------------------------------------------------------------------

  (define compare-read-file '()) ; TODO

; -----------------------------------------------------------------------------
; Exported Interface
; -----------------------------------------------------------------------------

  (define curly-infix-read (make-read curly-infix-read-nocomment))
  (define neoteric-read (make-read neoteric-read-nocomment))
  (define sweet-read (make-read t_expr))

  )

; vim: set expandtab shiftwidth=2 :
