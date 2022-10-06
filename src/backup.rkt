#lang racket/base

(require racket/list)
(require "file-utils.rkt" "backup-item.rkt" "helpers.rkt")

(provide (all-defined-out))

; --------------------------------------------------------------------------------

; Creates a backup file. Returns a list of items that were backed up.
; 
; path:  (or path? string?) ; path to resultant backup file
; items: (listof BackupItem?) ; list of items to include
; -> (listof BackupItem?)
(define (make-backup path items)
  ;(displayln (format "make-backup path=~a" path))
  ; 1. pack all items into an archive
  (let ([valid-items (filter (lambda (item)
                               (let ([invalid-reason (validate-backup-item item)])
                                 ; logging
                                 (when (!true? invalid-reason)
                                   (displayln (format "SKIP: ~a" invalid-reason)))
                                 ; filter result
                                 (true? invalid-reason)))
                             items)])
    (for-each (lambda (item) 
                ((get-backup-proc item) path))
              valid-items)
    ; 2. compress the backup file
    (when (not (empty? valid-items))
      (compress path)
      (displayln* (format "Backup created -> ~a" (path-string->string path))
                  (format "Files: ~a" (length valid-items))
                  (format "Size: ~a" 'SIZE_NOT_SUPPORTED)))
    valid-items))
