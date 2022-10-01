#lang racket/base

(require "file-utils.rkt" "backup-item.rkt")

(provide (all-defined-out))

; --------------------------------------------------------------------------------

; Creates a backup file. 
; 
; path:  (or path? string?) ; path to resultant backup file
; items: (listof BackupItem?) ; list of items to include
(define (make-backup path items)
  ;(displayln (format "make-backup ~a ~a" path (map BackupItem-desc items)))
  ; 1. pack all files into an archive
  (for-each (lambda (item)
              ((get-backup-proc item) path))
            items)
  ; 2. compress the backup file
  (compress path))
