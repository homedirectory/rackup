#lang racket/base

(require racket/path racket/string racket/date racket/list
         racket/bool racket/port)

(provide (all-defined-out))

; -----------------------------------------------------------------------------

(define (true? x) (not (false? x)))

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

(define (displayln* . args)
 (unless (empty? args)
  (displayln (car args))
  (apply displayln* (cdr args))))

; Returns a differencec between lst1 and lst2.
; i.e. lst1 \ lst2
(define (list-diff lst1 lst2)
  (filter-not 
    (lambda (x)
      (member x lst2))
    lst1))

#|
Runs a shell command cmd with arguments args.
Returns stdout in case of success, otherwise #f.

cmd : string?
args : (listof string?)
-> (or string? #f)
|#
(define (run-cmd cmd args)
  (displayln (format "run-cmd ~s ~s" cmd args))
  (let-values ([(sp out in err)
                (apply subprocess 
                       (append 
                         (list #;stdout #f #;stdin #f #;stderr #f 
                               (find-executable-path cmd))
                         args))])
    (subprocess-wait sp)
    (let ([result (string-trim (port->string out) 
                               "\n" 
                               #:left? #f)])
      (apply-if-t close-input-port out)
      (apply-if-t close-output-port in)
      (apply-if-t close-input-port err)
      result)))

#|
Runs a shell command cmd with arguments args and writes stdout to a file at path.
Returns #f in case of success, otherwise #f.

cmd : string?
args : (listof string?)
path : path-string?
-> boolean?
|#
(define (run-cmd->file cmd args path)
  (let-values ([(sp out in err)
                (apply subprocess 
                       (append (list 
                                 ;stdout 
                                 (open-output-file path #:exists 'truncate/replace)
                                 #;stdin #f #;stderr #f 
                                 (find-executable-path cmd))
                               args))])
    (subprocess-wait sp)
    (apply-if-t close-input-port out)
    (apply-if-t close-output-port in)
    (apply-if-t close-input-port err))
  #t)

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
