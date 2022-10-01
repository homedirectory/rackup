#lang racket/base

(require racket/generic racket/file)
(require "helpers.rkt" "file-utils.rkt")
(provide (all-defined-out))

; --------------------------------------------------------------------------------

(define-generics backupable
                 (get-backup-proc backupable))

(struct BackupItem ())

(struct BackupItemFile BackupItem (file-path encrypt? follow?)
  #:methods gen:backupable
  [(define (get-backup-proc item)
     (let ([file-path (BackupItemFile-file-path item)]
           [follow? (BackupItemFile-follow? item)]
           [encrypt? (BackupItemFile-encrypt? item)])
       (lambda (backup-path)
         (cond [(and (link-exists? file-path) follow?)
                ; backup the linked file
                ((get-backup-proc (+BackupItemRegularFile 
                                    (dereference file-path) encrypt?))
                 backup-path)
                ; backup the link itself
                ((get-backup-proc (+BackupItemRegularFile file-path))
                 backup-path)]
               [else ((get-backup-proc (+BackupItemRegularFile file-path encrypt?))
                      backup-path)]))))])

(define (+BackupItemFile file-path [encrypt? #f] [follow? #t])
  (BackupItemFile file-path encrypt? follow?))

(struct BackupItemRegularFile BackupItem ()
  #:methods gen:backupable
  [(define (get-backup-proc item)
     (let ([file-path (BackupItemFile-file-path item)]
           [encrypt? (BackupItemFile-encrypt? item)])
       (lambda (backup-path)
         (cond [(encrypt? 
                  (let ([enc-file-path (encrypt-file file-path)])
                    (archive-file backup-path enc-file-path)
                    (delete-file enc-file-path)))]
               [else (archive-file backup-path file-path)]))))])

; regular file is never a link so follow? doesn't make sense, hence always #f
(define (+BackupItemRegularFile file-path [encrypt? #f])
  (BackupItemFile file-path encrypt? #f))

; excluded : (listof string?)
(struct BackupItemDir BackupItemFile (excluded)
  #:methods gen:backupable
  [(define (get-backup-proc item)
     (let ([file-path (BackupItemFile-file-path item)]
           [encrypt? (BackupItemFile-encrypt? item)]
           [follow? (BackupItemFile-follow? item)]
           [excluded (BackupItemDir-excluded item)])
       (let ([included-items 
               (map
                 (lambda (path)
                   (+BackupItemFile path encrypt? follow?))
                 (directory-list-except excluded file-path))])
         (lambda (backup-path)
           (for-each (lambda (item)
                       ((get-backup-proc item) backup-path))
                     included-items)))))])

(define (+BackupItemDir file-path [encrypt? #f] [follow? #t] [excluded (list)])
  (BackupItemDir file-path encrypt? follow? excluded))

(struct BackupItemCmd BackupItem (file-name cmd)
  #:methods gen:backupable
  [(define (get-backup-proc item)
     (let ([file-name (BackupItemCmd-file-name item)]
           [cmd (BackupItemCmd-cmd item)])
       (let ([out-file-path (make-temporary-file (string-append file-name "_~a.out"))])
         (run-cmd->file cmd out-file-path)
         (lambda (backup-path)
           (archive-file backup-path out-file-path #:prefix "stdout")
           (delete-file out-file-path)))))])

(define (+BackupItemCmd file-name cmd)
  (BackupItemCmd file-name cmd))
