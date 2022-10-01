#lang racket/base

(require racket/bool racket/promise racket/string racket/file racket/system
         racket/math racket/list racket/generic racket/file)
(require "macros.rkt" "helpers.rkt" "file-structs.rkt" "file-utils.rkt")

(provide (all-defined-out))

; ---------------------------------------------------------

(define-generics preprocessable-bak-file
                 (preprocess preprocessable-bak-file))
(alias-proc preprocessed-bak-file preprocess)

(define-generics postprocessable-bak-file
                 (postprocess postprocessable-bak-file))
(alias-proc postprocess-bak-file postprocess)

#|
; --------------------------------------------------------------------------------
A struct for files that are backed up.
Can be encrypted or marked as a temporary file, which get automatically deleted 
after the backup process has finished.

fields = (path encrypt? temp? follow?)

follow? : boolean? ; indicates whether to follow symlinks when backing up this file
|#
(struct bak-file sfile ([encrypt? #:mutable] [temp? #:mutable] [follow? #:mutable])
        #:methods gen:copyable-struct
        [(define (mk-struct-copy file)
           (struct-copy bak-file file))]
        #:methods gen:preprocessable-bak-file
        [(define (preprocess file) (_preprocess-bak-file file))]
        #:methods gen:postprocessable-bak-file
        [(define (postprocess file) (_postprocess-bak-file file))])

; private bak-file constructor
(define (_mk-bak-file path/str #:encrypt? [encrypt? #f] #:temp? [temp? #f] #:follow? [follow? #t])
  ;(debug "_mk-bak-file: ~a" path/str) 
  (let ([path (expand-user-path path/str)])
    (bak-file path encrypt? temp? follow?)))

; public bak-file constructor
(define (+mk-bak-file path/str #:encrypt? [encrypt? #f] #:follow? [follow? #t])
  (bak-file (expand-user-path path/str) encrypt? #f follow?))

(define (_preprocess-bak-file file)
  (cond [(bak-file-encrypt? file) (encrypted-file file)]
        [else file]))

(define (_postprocess-bak-file file)
  (cond [(bak-file-temp? file) 
         (with-handlers 
           ([exn:fail:filesystem?
              (lambda (e) 
                (displayln (format "WARN: couldn't delete temporary file ~a"
                                   (file-path file))))])
           (delete-directory/files (file-path file)))]
        [else file]))

(define (sfile->bak-file file)
  (_mk-bak-file (file-path file)))

(define (bak-file->string fil)
  (my-string-join
    (list
      (if (bak-dir? fil) "bak-dir" "")
      (path->string (file-path fil))
      (if (bak-file-encrypt? fil) "ENCRYPT" "")
      (if (bak-file-temp? fil) "TEMP" "")
      (if (and (bak-dir? fil) (bak-dir-exclude? fil))
        (format "exclude:~a" (get-bak-dir-files fil))
        ""))
    ","))

; ---------------------------------------------------------
#|
A special case of bak-file to retain additional information about directories,
such as files to exclude.

files : (or/c bak-file? bak-dir? string?)
path encrypt? temp? exclude? files
|#
(struct bak-dir bak-file (exclude? files)
        #:methods gen:copyable-struct
        [(define (mk-struct-copy file)
           (struct-copy bak-dir file))])
; bak-dir does NOT implement gen:preprocessable-bak-file, since this struct is not meant
; to be used that way

; bak-dir constructor
(define (mk-bak-dir path/str #:exclude? [exclude? #f] #:encrypt? [encrypt? #f] #:follow? [follow? #t] files)
  ; convert all string's in files to bak-file's (if not exclude)
  (let ([files 
          (map (lambda (file)
                 (cond [(string? file) (_mk-bak-file file)]
                       [(bak-file? file) file]
                       [else (error "Unknown file type: " file)]))
               files)])
        (bak-dir 
          ; allow empty strings as dir path
          (if (and (string? path/str) (string-blank? path/str)) 
            "" 
            (expand-user-path path/str)) 
          encrypt? #|temp?|# #f exclude? files)))

; Retrieving files from bak-dir should result into a list of files having their paths
; prefixed by the path of bak-dir and some flags set.
(define (get-bak-dir-files bakdir)
  (let ([bakdir-path (file-path bakdir)]
        [bakdir-encrypt? (bak-file-encrypt? bakdir)]
        [bakdir-follow? (bak-file-follow? bakdir)])
    (map (lambda (file)
         (let ([new-file (change-file-path 
                           file 
                           (build-path bakdir-path (file-path file)))])
           (set-bak-file-encrypt?! new-file bakdir-encrypt?)
           (set-bak-file-follow?! new-file bakdir-follow?)
           new-file))
       (bak-dir-files bakdir))))

; -> (listof bak-file? bak-dir?)
; if files of obj contain bak-dir, then:
;   if bak-dir has exclude #t, throws an error
;   else bak-dir->list is applied
;(define (bak-dir->list obj)
;  (when (bak-dir-exclude? obj)
;    (error "bak-dir->list: bak-dir has exclude? = #t"))
;  (let ([dir-path (file-path obj)])
;    ;(debug "~s ~s" dir-path (get-bak-dir-files obj))
;    (flatmap (lambda (x)
;               (cond [(bak-dir? x) 
;                      (let ([x 
;                              (struct-copy bak-dir x 
;                                           [path #:parent file 
;                                                 (my-build-path dir-path (file-path x))])])
;                        (if (bak-dir-exclude? x)
;                          x
;                          (bak-dir->list x)))]
;                     [(bak-file? x) 
;                      (set-file-path! 
;                        x 
;                        (my-build-path dir-path (file-path x)))
;                      x]
;                     [else (error "bak-dir->list: Unexpected file: ~a" x)]))
;             ; (listof bak-file? bak-dir?)
;             (get-bak-dir-files obj))))


;; -> (listof bak-file? bak-dir?)
;(define (bak-dir->included-list obj)
;  (if (not (bak-dir-exclude? obj))
;    (get-bak-dir-files obj)
;    (map _mk-bak-file
;         (remove* (map file-path (map-if string? _mk-bak-file (get-bak-dir-files obj)))
;                  (directory-list (file-path obj) #:build? #t)))))

; ------------------------------------------------------------
(define STDOUT-FILENAME-PREFIX "stdout_")

#|
A struct for temporary files that store the output of a shell command.

fields = (path encrypt? temp? cmd)

cmd : string? ; contains the command together with arguments
|#
(struct stdout-file bak-file (cmd)
        #:methods gen:copyable-struct
        [(define (mk-struct-copy file)
           (struct-copy stdout-file file))]
        #:methods gen:preprocessable-bak-file
        [(define (preprocess file) (_preprocess-stdout-file file))]
        #:methods gen:readable-file
        ; stdout-file is always readable, since, well, it's a stdout file
        [(define (can-read-file? file) #t)])

; TODO support encryption
; private stdout-file constructor
(define (_mk-stdout-file name cmd #:encrypt? [encrypt? #f])
    (stdout-file name #;encrypt? #f #;temp? #t cmd))
(define-syntax mk-stdout-file
  (syntax-rules ()
    [(_ name cmd encrypt?) (_mk-stdout-file name cmd encrypt?)]
    [(_ name cmd) (_mk-stdout-file name cmd)]
    [(_ cmd) (_mk-stdout-file (filename-for-stdout cmd) cmd)]))

#|
Returns a new bak-file that contains the output of the command assocciated with
file.

file : stdout-file?
-> bak-file?
|#
(define (_preprocess-stdout-file file)
  (let ([path (make-temporary-file (string-append (file-path file) "_~a.tmp"))])
    (let ([split (string-split (stdout-file-cmd file))])
      (run-cmd->file (car split) (list (string-join (cdr split))) path))
    (_mk-bak-file path #:encrypt? (bak-file-encrypt? file) #:temp? #t)))

; Generates a random name for stdout file from cmd.
;
; cmd: string?
; -> path-element?
(define (filename-for-stdout cmd)
  (let* ([cmd-name (car (string-split cmd))]
         [result 
           (string-append STDOUT-FILENAME-PREFIX
                          (or (apply-if-t path-element->string 
                                          (string->path-element cmd-name))
                              ""))])
    (let ([len (string-length result)])
      (if (> len MAX-FILENAME-LEN)
        (substring result 0 MAX-FILENAME-LEN)
        (string-append result "_" (rand-string (min 20 (- MAX-FILENAME-LEN len 1))))))))

;(define (touch-mbf fil)
;  (display-to-file (get-mbf-data fil) (file-path fil))
;  fil)


