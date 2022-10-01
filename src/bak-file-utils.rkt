#lang racket/base

(require racket/list)
(require "file-structs.rkt" "bak-file-structs.rkt" "file-utils.rkt"
         "helpers.rkt")

(provide (all-defined-out))

; --------------------------------------------------------------------------------

; files: (listof bak-file?)
(define (print-included-files-paths files)
  (for-each displayln (map file-path
                           (only-included-files files))))

; files : (listof bak-file?)
(define (only-included-files files)
  (flatten
    (map (lambda (file)
           (cond [(bak-dir? file)
                  (bak-dir-included-files file)]
                 [(bak-file? file) file]
                 [else (error "Unknown file type:" file)]))
         files)))


; Extracts only included files from `bakdir`. 
; If #:exclude? is false for `bakdir`, then simply returns `bakdir`'s files.
; Otherwise, constructs a list of files that would be included.
;
; bakdir: bak-dir?
; -> (listof bak-file?)
(define (bak-dir-included-files bakdir)
 ; (displayln* (sfile-path bakdir) (map sfile-path (get-bak-dir-files bakdir))
 ;             (bak-dir-exclude? bakdir))
  (let ([files (get-bak-dir-files bakdir)])
    (if (not (bak-dir-exclude? bakdir))
      (only-included-files files)
      (map _mk-bak-file 
           (directory-list-except (map sfile-path (bak-dir-files bakdir)) 
                                  (sfile-path bakdir))))))

(define (included-bak-files excluded-paths dir-path)
  (map _mk-bak-file (directory-list-except excluded-paths dir-path)))
