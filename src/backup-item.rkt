#lang racket/base

(require racket/generic racket/file racket/bool)
(require "helpers.rkt" "file-utils.rkt")
(provide (all-defined-out))

; --------------------------------------------------------------------------------

(define-generics backupable
                 (get-backup-proc backupable))
(define-generics validatable
                 (validate-backup-item validatable))

; for gen:custom-write
(define (get-write-proc port mode)
  (case mode
    [(#t) write]
    [(#f) display]
    [else (lambda (p port) (print p port mode))]))

(struct BackupItem ())

; >>>>>>>>>> BackupItemFile >>>>>>>>>>

; gen:validatable
(define (validate-BackupItemFile item)
  (let ([file-path (BackupItemFile-file-path item)])
    (cond [(false? (file/dir-exists? file-path))
           (format "Invalid backup item: file doesn't exist: ~a" (path->string file-path))]
          [else #t])))

; gen:backupable
; define this as a separate procedure since it calls get-backup-proc inside
(define (BackupItemFile-get-backup-proc item)
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
                   backup-path)]))))

(struct BackupItemFile BackupItem (file-path encrypt? follow?)
  #:methods gen:backupable
  [(define get-backup-proc BackupItemFile-get-backup-proc)]
  #:methods gen:custom-write
  [(define (write-proc item port mode) 
     ((get-write-proc port mode) 
      (format "(#BackupItemFile file-path=~s encrypt?=~a follow?=~a)"
              (BackupItemFile-file-path item) (BackupItemFile-encrypt? item) 
              (BackupItemFile-follow? item))
      port))]
  #:methods gen:validatable
  [(define validate-backup-item validate-BackupItemFile)])

; constructor
(define (+BackupItemFile file-path [encrypt? #f] [follow? #t])
  (BackupItemFile file-path encrypt? follow?))

; <<<<<<<<<< BackupItemFile <<<<<<<<<<


; >>>>>>>>>> BackupItemRegularFile >>>>>>>>>>

; gen:backupable
(define (BackupItemRegularFile-get-backup-proc item)
  (let ([file-path (BackupItemFile-file-path item)]
        [encrypt? (BackupItemFile-encrypt? item)])
    (lambda (backup-path)
      (cond [encrypt? 
              (let ([enc-file-path (encrypt-file file-path)])
                (archive-file backup-path enc-file-path)
                (delete-file enc-file-path))]
            [else (archive-file backup-path file-path)]))))

; regular file, i.e., not a link
(struct BackupItemRegularFile BackupItemFile ()
  #:methods gen:backupable
  [(define get-backup-proc BackupItemRegularFile-get-backup-proc)]
  #:methods gen:custom-write
  [(define (write-proc item port mode) 
     ((get-write-proc port mode) 
      (format "(#BackupItemRegularFile file-path=~s encrypt?=~a)"
              (BackupItemFile-file-path item) (BackupItemFile-encrypt? item))
      port))]
  #:methods gen:validatable
  [(define validate-backup-item validate-BackupItemFile)])

; constructor
; regular file is never a link so follow? doesn't make sense, hence always #f
(define (+BackupItemRegularFile file-path [encrypt? #f])
  (BackupItemRegularFile file-path encrypt? #f))

; <<<<<<<<<< BackupItemRegularFile <<<<<<<<<<


; >>>>>>>>>> BackupItemDir >>>>>>>>>>

; gen:backupable
(define (BackupItemDir-get-backup-proc item)
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
                  included-items)))))

; excluded : (listof string?)
(struct BackupItemDir BackupItemFile (excluded)
  #:methods gen:backupable
  [(define get-backup-proc BackupItemDir-get-backup-proc)]
  #:methods gen:custom-write
  [(define (write-proc item port mode) 
     ((get-write-proc port mode)
      (format "(#BackupItemDir file-path=~s encrypt?=~a follow?=~a excluded=~a)"
              (BackupItemFile-file-path item) (BackupItemFile-encrypt? item)
              (BackupItemFile-follow? item) (BackupItemDir-excluded item))
      port))]
  #:methods gen:validatable
  [(define validate-backup-item validate-BackupItemFile)])

; constructor
(define (+BackupItemDir file-path [encrypt? #f] [follow? #t] [excluded (list)])
  (BackupItemDir file-path encrypt? follow? excluded))

; <<<<<<<<<< BackupItemDir <<<<<<<<<<


; >>>>>>>>>> BackupItemCmd >>>>>>>>>>

; gen:backupable
(define (BackupItemCmd-get-backup-proc item)
  (let ([file-name (BackupItemCmd-file-name item)]
        [cmd (BackupItemCmd-cmd item)])
    (let ([out-file-path (make-temporary-file (string-append file-name "_~a.out"))])
      (run-cmd->file cmd out-file-path)
      (lambda (backup-path)
        (archive-file backup-path out-file-path #:prefix "stdout")
        (delete-file out-file-path)))))

; gen:validatable
(define (validate-BackupItemCmd item)
  (let ([file-name (BackupItemCmd-file-name item)]
        [cmd (BackupItemCmd-cmd item)])
    (cond [(string-blank? file-name)
           "Invalid command output backup item: file name can not be empty"]
          [(string-blank? cmd)
           (format "Invalid command output backup item: command can not be empty (file-name=~s)"
                   file-name)]
          [else #t])))

(struct BackupItemCmd BackupItem (file-name cmd)
  #:methods gen:backupable
  [(define get-backup-proc BackupItemCmd-get-backup-proc)]
  #:methods gen:custom-write
  [(define (write-proc item port mode) 
     ((get-write-proc port mode) 
      (format "(#BackupItemCmd file-name=~s cmd=~s)"
              (BackupItemCmd-file-name item) (BackupItemCmd-cmd item))
      port))]
  #:methods gen:validatable
  [(define validate-backup-item validate-BackupItemCmd)])

; constructor
(define (+BackupItemCmd file-name cmd)
  (BackupItemCmd file-name cmd))

; <<<<<<<<<< BackupItemCmd <<<<<<<<<<
