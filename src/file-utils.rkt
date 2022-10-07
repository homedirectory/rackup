#lang racket/base

(require racket/file racket/string racket/list)
(require "helpers.rkt")

(provide (all-defined-out))

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

(define (dirname path)
  (apply build-path (drop-right (my-explode-path path) 1)))

; path : path-string?
; -> path?
(define (encrypt-file path)
  (let ([out-path (reroot-path path (find-system-path 'temp-dir))])
    (make-directory* (dirname out-path))
    (let* ([out-path (make-temporary-file 
                       (string-append (path->string out-path)
                                      "_~a.enc"))]
           [out-path-string (path-string->string out-path)])
      ; if a directory needs to be encrypted - first create a tarball 
      (if (directory-exists? path)
        (let ([tar-path (path->string (make-temporary-file))])
          (run-cmd (format "tar -cf ~s ~s" tar-path path))
          (run-cmd (format "gpg --batch --yes --symmetric --output ~s ~s"
                           out-path-string tar-path))
          (delete-file tar-path))
        (run-cmd (format "gpg --batch --yes --symmetric --output ~s ~s" 
                         out-path-string (path-string->string path))))
      out-path-string)))

(define (compress path)
  (run-cmd (string-join (list "gzip"
                              "--best" 
                              "--suffix .gz"
                              (path-string->string path))))
  (path-add-extension path ".gz" "."))


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


(define (archive-file arch-path file-path #:prefix [prefix #f])
 (let ([options (string-join (list "-rP"
                              (if prefix
                                ;TODO BSD tar doesn't support --transform 
                               (format "--transform='~a'" 
                                (format "s|\\(.*\\)|~a\\1|" prefix))
                               "")))])
  (run-cmd (string-join (list "tar" options "-f"
                         (format "~a" (path-string->string arch-path))
                         (format "~a" (path-string->string file-path)))))))

(define (file-link? path)
  (memq (file-or-directory-type path) (list 'link 'directory-link)))

(define (dereference link-path)
  (if (not (or (file-exists? link-path) (directory-exists? link-path)))
    #f
    (resolve-path link-path)))
