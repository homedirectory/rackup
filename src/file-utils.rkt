#lang racket/base

(require racket/list racket/path racket/file)
(require "helpers.rkt" "file-structs.rkt")

(provide (all-defined-out))

; --------------------------------------------------------------------------------

; Like explode-path, but if the 1st path element is "/",
; then join in with the 2nd one.
;
; path: path-string?
; -> (listof path?)
(define (my-explode-path path)
  (let ([exploded (explode-path (simplify-path path #t))])
    (cond [(< (length exploded) 2) exploded]
          [(equal? "/" (path->string (car exploded)))
           (cons (build-path (car exploded) (cadr exploded))
                 (cddr exploded))]
          [else exploded])))

; Recursively list all files at `path` excluding those in `except-list`.
; Once `except-list` has been exhausted, does not go deeper into three,
; meaning that, in fact, not all files will be listed.
;
; except-list: (listof path-string?)
; path       : path-string?
; ->         : (listof path?)
(define (directory-list-except except-list path)
  ; `exc-list` is a list of excluded paths, where each path is represented by a list,
  ; which is obtained by splitting the path by "/" (exploding the path)
  ;
  ; curr-path: path?
  ; included: (listof path?)
  ; exc-list: (listof (listof path?))
  (define (iter curr-path included exc-list)
    (cond [(empty? exc-list) included]
          [else 
            (let* ([children (directory-list curr-path #:build? #f)]
                   ;1. ignore all excluded children (i.e. single item lists in exc-list)
                   [exc-children (map car (filter single-item-list? exc-list))]
                   [inc-children (list-diff children exc-children)]
                   #|Exclude deep children from inc-children.
                   A deep child is a child that is a part of an excluded path 
                   going deeper into the tree.|#
                   [exc-deep (filter
                               (lambda (path-elements)
                                 (and
                                   ;does this path go deeper?
                                   (not (single-item-list? path-elements))
                                   #|it could have been excluded already
                                   for example: ("dir", "dir/file.txt")
                                   "dir" exclusion shadows "dir/file.txt"|#
                                   (not (member (car path-elements) exc-children))))
                               exc-list)]
                   [inc-children (list-diff inc-children (map car exc-deep))])
              (append*
                included ; already included
                ; included children on this level
                (map (lambda (file-name) (build-path curr-path file-name)) 
                     inc-children)
                ; traverse the tree for each path that goes deeper
                (map
                  (lambda (path-elements)
                    (iter (build-path curr-path (car path-elements))
                          (list)
                          (list (cdr path-elements))))
                  exc-deep)))]))

  ; file at `path` must exist, otherwise it's an error
  (let ([file-type (file-or-directory-type path #t)])
    (if (eq? file-type 'file)
      (list)
      (iter path 
            (list)
            (map my-explode-path except-list)))))

#|
Adds file to a tar archive at archive-path.
Returns a bool indicating a successful outcome.

archive-path : string-path?
file : sfile?
-> bool?
|#
(define (tar-archive-file archive-path file #:follow? [follow? #t])
  ;(displayln (format "tar-archive-file ~a ~a" archive-path (file-path file)))
  (let ([options (format "-rP~a"
                         (if follow? "h" ""))])
    #| -r recursive
    -P preserve absolute paths
    -h dereference symlinks |#
    (run-cmd "tar" (list options "-f" 
                         (format "~a" 
                                 (path-string->string archive-path))
                         (format "~a" 
                                 (path-string->string (file-path file)))))))

#|
file : file?
-> file?
|#
(define (encrypted-file file)
 ;(warn "encryption is not supported")
 #|
 (let* ([path (file-path file)]
        [enc-path (path-add-extension path ".enc" ".")])
  (with-handlers ([exn:fail (lambda (e) (#;warn display e) #f)])
   (system (format "gpg -c -o ~s ~s" (path->string enc-path) (path->string path)))
   (file enc-path)))
|#
    file
    )

#|
Compresses the file at path.
Returns a bool indicating success of the operation.

path : path-string?
-> bool?
|#
(define (compress path)
  #f)

#|
 file : file?
 -> file?
|#
;(define (compressed-file file)
;  (let ([path (file-path file)])
;    (with-handlers ([exn:fail (lambda (e) (warn e) #f)])
;      (system (format "gzip -f ~s" (path->string path)))
;      (file (path-add-extension path ".gz" ".")))))

; returns physical size of a file in bytes
; fil : file?
; -> exact-integer?
;(define (file-phys-size fil)
;  (exact-ceiling 
;    (string->number 
;      (first 
;        (string-split (run-cmd "du" "-bLs" (path->string (file-path fil)))))
;      10)))
