#lang racket/base

(require racket/list racket/file)
(require "file-utils.rkt" "backup-item.rkt" "helpers.rkt")

(provide (all-defined-out))

; --------------------------------------------------------------------------------

; Creates a backup file. Returns a list of items that were backed up.
; 
; path:  (or path? string?) ; path to resultant backup file
; items: (listof BackupItem?) ; list of items to include
; -> (listof BackupItem?)
(define (make-backup path items [quiet? #f])
  ;(displayln (format "make-backup path=~a" path))
  ; 1. pack all items into an archive
  (let ([valid-items (filter (lambda (item)
                               (let ([invalid-reason (validate-backup-item item)])
                                 ; logging
                                 (when (and (not quiet?) (!true? invalid-reason))
                                   (displayln (format "SKIP: ~a" invalid-reason)))
                                 ; filter result
                                 (true? invalid-reason)))
                             items)])

    ; 2. pack all items to encrypt into a single archive
    (let-values ([(to-encrypt rest)
                  (partition (lambda (item)
                               (and (BackupItemFile? item) (BackupItemFile-encrypt? item)))
                             valid-items)])
      (when (!empty? to-encrypt)
        ; disable the encrypt? flag on to-encrypt items
        (for-each (lambda (item) (set-BackupItemFile-encrypt?! item #f)) to-encrypt)

        (when (not quiet?)
          (apply displayln* 
                 (list* "The following items will be packed together and encrypted:" 
                        (map short-desc to-encrypt))))

        (let ([to-encrypt-backup-path 
                (make-backup (make-temporary-file "all-encrypted_~a.bak")
                             to-encrypt #;quiet? #t)])
                             ; backup the backup with to-encrypt files as a regular file,
                             ; which needs to be encrypted
                             ((get-backup-proc (+BackupItemRegularFile to-encrypt-backup-path #t))
                              path)
                             (delete-directory/files to-encrypt-backup-path)))

                ; 3. backup rest of the items individually
                (when (not quiet?) (displayln "\nRest of the items:"))
                (for-each (lambda (item) 
                            ((get-backup-proc item) path)
                            (when (not quiet?) (displayln (short-desc item))))
                          rest))

                ; 4. compress the backup file
                (if (not (empty? valid-items))
                  (let ([compressed-path (compress path)])
                    (when (not quiet?)
                      (displayln* (format "Backup created -> ~a" (path->string compressed-path))
                                  (format "Files: ~a" (length valid-items))
                                  (format "Size: ~a" 'SIZE_NOT_SUPPORTED)))
                    compressed-path)
                  #f)))
