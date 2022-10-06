#lang racket/base

(require racket/generic)
(require "backup-item.rkt")

(provide (all-defined-out))

; --------------------------------------------------------------------------------

(define-generics BackupItem-convertable
                 (SpecItem->BackupItem BackupItem-convertable))

(struct SpecItem ())

(struct SpecItemFile SpecItem (file-path-string encrypt? follow?)
  #:methods gen:BackupItem-convertable
  [(define (SpecItem->BackupItem spec-item)
     (BackupItemFile (expand-user-path (SpecItemFile-file-path-string spec-item))
                     (SpecItemFile-encrypt? spec-item)
                     (SpecItemFile-follow? spec-item)))])

(define (+SpecItemFile file-path-string #:encrypt? [encrypt? #f] #:follow? [follow? #t])
  (SpecItemFile file-path-string encrypt? follow?))

(struct SpecItemDir SpecItemFile (excluded)
  #:methods gen:BackupItem-convertable
  [(define (SpecItem->BackupItem spec-item)
     (BackupItemDir (expand-user-path (SpecItemFile-file-path-string spec-item))
                    (SpecItemFile-encrypt? spec-item)
                    (SpecItemFile-follow? spec-item)
                    (SpecItemDir-excluded spec-item)))])

(define (+SpecItemDir file-path-string #:encrypt? [encrypt? #f] #:follow? [follow? #t]
                      #:excluded [excluded (list)])
  (SpecItemDir file-path-string encrypt? follow? excluded))

(struct SpecItemCmd SpecItem (file-name cmd)
  #:methods gen:BackupItem-convertable
  [(define (SpecItem->BackupItem spec-item)
     (BackupItemCmd (SpecItemCmd-file-name spec-item)
                    (SpecItemCmd-cmd spec-item)))])

(define (+SpecItemCmd file-name cmd)
  (SpecItemCmd file-name cmd))

