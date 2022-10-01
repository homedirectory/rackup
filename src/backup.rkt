#lang racket/base

(require "file-utils.rkt" "bak-file-structs.rkt" "file-structs.rkt" "helpers.rkt")

(provide (all-defined-out))

; --------------------------------------------------------------------------------

#|
Creates a backup file from an absoulte path that includes specified files.

path:  (or path? string?)
files: (listof bak-file?)
|#
(define (make-backup path files)
  (displayln (format "make-backup ~a ~a" path (map file-path files)))
  ;(exit)
 ; 1. pack all files into an archive
 (for-each (lambda (file) 
             (let ([preprocessed (preprocessed-bak-file file)])
               (archive-file path preprocessed (bak-file-follow? file))
               (postprocess-bak-file preprocessed)))
           files)
 ; 2. compress the archive
 (compress path))


; Adds file to the backup at path.
(define (archive-file path file #:follow? [follow? #t])
  (tar-archive-file path file follow?))

; Compresses the file at path.
(define (compress path)
  (run-cmd "gzip" (list "--best" 
                        (path-string->string path))))

(define (simulate-backup path files)
  '())
