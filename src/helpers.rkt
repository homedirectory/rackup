#lang racket/base

(require racket/path racket/string racket/date racket/list
         racket/bool racket/port racket/system)

(provide (all-defined-out))

; -----------------------------------------------------------------------------

(define (true? x) (eq? #t x))

(define (!true? x) (not (true? x)))

(define (apply-if pred-val proc val)
  (if pred-val (proc val) val))

(define (file-any-exists? path)
  (and (file-or-directory-type path #f) #t))

; Appends `str` to filename found in `path`.
; In other words, inserts `str` between the filename and the extension (including the dot).
; Example:
;   (append-to-filename "file.txt" "_1") -> "file_1.txt"
; Useful for inserting text betwen before file extension.
;
; path: (or path? string?)
; str : string?
(define (append-to-filename path str)
  (path-replace-extension 
    path 
    (string-append str 
                   (or (bytes->string/utf-8 (or (path-get-extension path) #"")) ""))))

; Appends "_" and current date to a filename found in `path`.
; Example:
;   (append-curr-date "file.txt") -> "file_2022-01-01T10-30-00.txt"
;
; path       : (or path? string?)
; date-format: string?
(define (append-curr-date path [date-format 'iso-8601])
  (date-display-format date-format)
  (append-to-filename 
    path
    (string-append "_" (string-replace (date->string (current-date) #t) ":" "-"))))
 
(define (single-item-list? lst)
  (= 1 (length lst)))


(define letters (string->list "abcdefghijklmnopqrstuvwxyz"))
(define numbers (string->list "0123456789"))
(define rand-string-char-list (append letters (map char-upcase letters) numbers))
(define (rand-string n)
  (let ([len (length rand-string-char-list)])
    (list->string 
      (for/list ([i (in-range n)])
        (list-ref rand-string-char-list (random len))))))


(define (string-blank? str) 
  (not (non-empty-string? (string-trim str))))

; strs : (listof string?)
; sep : string?
(define (my-string-join strs sep)
  (string-join (filter-not string-blank? strs) sep))

; recursive map
(define (recmap f lst)
  (map (lambda (x) 
         (if (list? x) (recmap f x)
           (f x)))
       lst))

(define (flatmap f lst)
  (flatten (recmap f lst)))

; Applies proc to val and returns the result if val is #t,
; otherwise returns #f.
(define (apply-if-t proc val)
 (if (false? val)
  val
  (proc val)))

; Returns a differencec between lst1 and lst2.
; i.e. lst1 \ lst2
(define (list-diff lst1 lst2)
  (filter-not 
    (lambda (x)
      (member x lst2))
    lst1))

; Runs a shell command cmd. Returns stdout as string.
;
; cmd : string?
; -> string?
(define (run-cmd cmd)
  ;(displayln (format "run-cmd ~s" cmd))
  (with-output-to-string (lambda () (system cmd))))

; Runs a shell command cmd and writes stdout to a file at path.
; Returns #t in case of success, otherwise #f.
; 
; cmd : string?
; path : path-string?
; -> boolean?
(define (run-cmd->file cmd path)
 (parameterize ([current-output-port 
                (open-output-file path #:exists 'truncate/replace)])
  (system cmd)))

(define (path-string->string path-string)
  (cond [(string? path-string) path-string]
        [(path? path-string) (path->string path-string)]
        [else (error "Wrong argument type: " path-string)]))

(define (flatlist . args)
  (flatten (list args)))

(define (memf? proc lst)
 (displayln (format "memf? ~a ~a" proc lst))
 (not (false? (findf proc lst))))

(define (!empty? lst)
 (not (empty? lst)))

(define (maybe-string+suffix s suf)
 (if (string-suffix? s suf) s (string-append s suf)))

(define (file/dir-exists? path)
  (not (false? (file-or-directory-type path #;must-exist? #f))))

(define (displayln* . lines)
  (for-each displayln lines))
