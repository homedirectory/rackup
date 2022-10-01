#lang racket/base

(require racket/bool racket/promise racket/string racket/file racket/system
         racket/math racket/list racket/generic)
(require "macros.rkt" "helpers.rkt")

(provide (all-defined-out))

; ---------------------------------------------------------

; >>>>>>>>>> CONSTANTS >>>>>>>>>>
(define MAX-FILENAME-LEN 64)
; <<<<<<<<<< CONSTANTS <<<<<<<<<<

(define-generics copyable-struct
                 (mk-struct-copy copyable-struct))
(define-generics readable-file
                 (can-read-file? readable-file))

; ---------------------------------------------------------
#|
Base struct for all files/directories/symlinks.

path : string-path?
|#
(struct sfile ([path #:mutable])
        #:methods gen:copyable-struct
        [(define (mk-struct-copy file)
           (struct-copy sfile file))]
        #:methods gen:readable-file
        [(define (can-read-file? file)
           (file/dir-exists? ))])

(define (mk-file path) (sfile path))

;(alias-proc file-path sfile-path)
(define (file-path file) (sfile-path file))

(define (_can-read-file?-sfile file)
  (and (file/dir-exists? (file-path file)) 
       (true? (memq 'read 
                    (file-or-directory-permissions (file-path file))))))

; TODO make a macro to change whatever fields
(define (change-file-path file path)
  (let ([new-file (mk-struct-copy file)])
    (set-sfile-path! new-file path)
    new-file))

; path : path?
(define (file/dir-exists? path)
  (not (false? (file-or-directory-type path #f))))
(define (dir? file) (eq? 'directory (file-or-directory-type file #f)))
(define (link? file) (eq? 'link (file-or-directory-type file #f)))
