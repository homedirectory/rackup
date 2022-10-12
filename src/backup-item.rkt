#lang racket/base

(require racket/generic racket/file racket/bool racket/list)
(require racket/trace)
(require "helpers.rkt" "file-utils.rkt")
(provide (all-defined-out))

; --------------------------------------------------------------------------------

(define-generics backupable
                 (get-backup-proc backupable))
;(trace get-backup-proc)
(define-generics validatable
                 (validate-backup-item validatable))
(define-generics excludable
                 (get-included excludable))
(define-generics describable
                 (short-desc describable))
(define-generics encryptable
                 (to-encrypt? encryptable)
                 (set-no-encrypt encryptable))

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

; gen:describable
(define (BackupItemFile-short-desc item)
  (let ([file-path (BackupItemFile-file-path item)]
        [follow? (BackupItemFile-follow? item)]
        [encrypt? (BackupItemFile-encrypt? item)])
    (format "~s~a~a" (path->string file-path)
            (if encrypt? " ENCRYPT" "")
            (if follow? "" " NOFOLLOW"))))

(struct BackupItemFile BackupItem (file-path [encrypt? #:mutable] follow?)
  #:methods gen:backupable
  [(define get-backup-proc BackupItemFile-get-backup-proc)]
  #:methods gen:custom-write
  [(define (write-proc item port mode) 
     ((get-write-proc port mode) 
      (format "(#BackupItemFile file-path=~s encrypt?=~a follow?=~a)"
              (BackupItemFile-file-path item) (BackupItemFile-encrypt? item) 
              (BackupItemFile-follow? item))
      port))]
  #:methods gen:describable
  [(define short-desc BackupItemFile-short-desc)]
  #:methods gen:validatable
  [(define validate-backup-item validate-BackupItemFile)]
  #:methods gen:encryptable
  [(define (to-encrypt? item) (BackupItemFile-encrypt? item))
   (define (set-no-encrypt item) (set-BackupItemFile-encrypt?! item #f))])

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
                (archive-file backup-path enc-file-path #:prefix "encrypted")
                (delete-file enc-file-path))]
            [else (archive-file backup-path file-path)]))))

; gen:describable
(define (BackupItemRegularFile-short-desc item)
  (let ([file-path (BackupItemFile-file-path item)]
        [encrypt? (BackupItemFile-encrypt? item)])
    (format "~s~a" (path->string file-path)
            (if encrypt? " ENCRYPT" ""))))

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
  #:methods gen:describable
  [(define short-desc BackupItemRegularFile-short-desc)]
  #:methods gen:validatable
  [(define validate-backup-item validate-BackupItemFile)])

; constructor
; regular file is never a link so follow? doesn't make sense, hence always #f
(define (+BackupItemRegularFile file-path [encrypt? #f])
  (BackupItemRegularFile file-path encrypt? #f))

; <<<<<<<<<< BackupItemRegularFile <<<<<<<<<<


; >>>>>>>>>> BackupItemDir >>>>>>>>>>

; gen:excludable
(define (BackupItemDir-get-included item)
  (let ([file-path (BackupItemFile-file-path item)]
        [encrypt? (BackupItemFile-encrypt? item)]
        [follow? (BackupItemFile-follow? item)]
        [excluded (BackupItemDir-excluded item)])
    (map (lambda (path)
           (+BackupItemFile path encrypt? follow?))
         (directory-list-except excluded file-path))))

; gen:backupable
(define (BackupItemDir-get-backup-proc item)
  (lambda (backup-path)
    (for-each (lambda (item)
                ((get-backup-proc item) backup-path))
              (BackupItemDir-get-included item))))

; gen:describable
(define (BackupItemDir-short-desc item)
  (let ([file-path (BackupItemFile-file-path item)]
        [follow? (BackupItemFile-follow? item)]
        [encrypt? (BackupItemFile-encrypt? item)]
        [excluded (BackupItemDir-excluded item)])
    (format "~s~a~a~a" (path->string file-path)
            (if encrypt? " ENCRYPT" "")
            (if follow? "" " NOFOLLOW")
            (if (empty? excluded) "" excluded))))

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
  [(define validate-backup-item validate-BackupItemFile)]
  #:methods gen:describable
  [(define short-desc BackupItemDir-short-desc)]
  #:methods gen:excludable
  [(define get-included BackupItemDir-get-included)])

; constructor
(define (+BackupItemDir file-path [encrypt? #f] [follow? #t] [excluded (list)])
  (BackupItemDir file-path encrypt? follow? excluded))

; <<<<<<<<<< BackupItemDir <<<<<<<<<<


; >>>>>>>>>> BackupItemCmd >>>>>>>>>>

; gen:backupable
(define (BackupItemCmd-get-backup-proc item)
  (lambda (backup-path)
    (let ([file-name (BackupItemCmd-file-name item)]
          [cmd (BackupItemCmd-cmd item)]
          [encrypt? (BackupItemCmd-encrypt? item)])
      (let ([out-file-path (make-temporary-file (string-append file-name "_~a.out"))])
        (run-cmd->file cmd out-file-path)
        (if encrypt?
          (let ([enc-file-path (encrypt-file out-file-path)])
            (archive-file backup-path enc-file-path #:prefix "stdout")
            (delete-file enc-file-path))
          ; else
          (archive-file backup-path out-file-path #:prefix "stdout"))
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

; gen:describable
(define (BackupItemCmd-short-desc item)
  (let ([file-name (BackupItemCmd-file-name item)]
        [cmd (BackupItemCmd-cmd item)]
        [encrypt? (BackupItemCmd-encrypt? item)])
    (format "CMD ~a~s ~s" (if encrypt? "ENCRYPT " "") file-name cmd)))

(struct BackupItemCmd BackupItem (file-name cmd [encrypt? #:mutable])
  #:methods gen:backupable
  [(define get-backup-proc BackupItemCmd-get-backup-proc)]
  #:methods gen:custom-write
  [(define (write-proc item port mode) 
     ((get-write-proc port mode) 
      (format "(#BackupItemCmd file-name=~s cmd=~s)"
              (BackupItemCmd-file-name item) (BackupItemCmd-cmd item))
      port))]
  #:methods gen:describable
  [(define short-desc BackupItemCmd-short-desc)]
  #:methods gen:validatable
  [(define validate-backup-item validate-BackupItemCmd)]
  #:methods gen:encryptable
  [(define (to-encrypt? item) (BackupItemCmd-encrypt? item))
   (define (set-no-encrypt item) (set-BackupItemCmd-encrypt?! item #f))])

; constructor
(define (+BackupItemCmd file-name cmd [encrypt? #f])
  (BackupItemCmd file-name cmd encrypt?))

; <<<<<<<<<< BackupItemCmd <<<<<<<<<<
