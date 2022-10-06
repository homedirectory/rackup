#lang racket/base

(require racket/cmdline racket/promise racket/list racket/file)
(require "helpers.rkt" "backup-item.rkt" "macros.rkt" "backup-item-utils.rkt"
         "spec-item.rkt" "backup.rkt")

(provide (all-defined-out))

; --------------------------------------------------------------------------------

; Use this procedure to specify files to be included in a backup.
; Each one of `args` will have its path prefixed by `dir-path`.
; If `exclude?` is true, then all files inside `dir-path` except `args` shall be included.
; 
; If args is a list of single element 'all, then all directory contents are included.
; This might be used to specify individual file conditions, like "don't follow links":
;     (in-dir #:follow? #f 'all)
;
; Presence of #:excluded with empty args implies 'all.
;
; Returns a list of included bak-file's.
; 
; dir-path : string?
; encrypt? : boolean? ; encryption (global)
; follow?  : boolean? ; follow links (global)
; excluded : (listof string?)
; args     : (listof bak-file? string? (listof ...))
; 
; -> (listof bak-file?)
;(define (in-dir dir-path #:encrypt? [encrypt? #f] #:follow? [follow? #t] 
;                #:excluded [excluded (list)] . args)
;  ; DEBUG
;  ;(displayln (format "in-dir ~a #:excluded ~a ~a" dir-path excluded args))
;
;  (define (arg->bak-file arg)
;    (cond [(string? arg) (+mk-bak-file (build-path dir-path arg))]
;          [(bak-file? arg) (change-file-path arg (build-path dir-path (file-path arg)))]
;          ; we expect this list to be (listof bak-file?)
;          [(list? arg) (map (lambda (file)
;                              (change-file-path file 
;                                                (build-path dir-path (file-path file))))
;                            bak-file)]
;          [else (error "Unexpected arg: " arg)]))
;
;  ; to determine which files are included we have to check if (all ...) was specified or implied
;  (if (or (and (!empty? excluded) (empty? args))
;          (memf? is-all? args))
;    ; TODO apply global encrypt? and follow?
;    (included-bak-files excluded dir-path)
;    (flatten (map arg->bak-file args))))

(alias-proc file +SpecItemFile)

(alias-proc cmd +SpecItemCmd)

; >>>>>>>>>> COMMAND-LINE ARGUMENTS >>>>>>>>>>
(define (options-from-cmdline)
  (define simulate? (make-parameter #f))
  (define output (make-parameter #f))
  (define overwrite? (make-parameter #f))
  (define print-files? (make-parameter #f))
  (define estimate-size? (make-parameter #f))
  (command-line 
    #:once-each
    [("-s" "--simulate") "Don't create any files" (simulate? #t)]
    [("-o" "--output") arg "Output file (takes precedence over the defined one)"
                       (output arg)]
    [("--overwrite") "Overwrite output file if it exists"
                     (overwrite? #t)]
    [("-p" "--print-files") "Simply print a list of files to be backed up (useful for piping to other commands)"
                            (print-files? #t)]
    [("-e" "--estimate-size") "Estimate physical size of the resulting backup"
                              (estimate-size? #t)])
  (list (cons 'simulate? (simulate?))
        (cons 'output (output))
        (cons 'overwrite? (overwrite?))
        (cons 'print-files? (print-files?))
        (cons 'estimate-size? (estimate-size?)))
  )
; <<<<<<<<<< COMMAND-LINE ARGUMENTS <<<<<<<<<<

; >>>>>>>>>> LAUNCH OPTIONS >>>>>>>>>>
(define (get-option name options)
  ;(printf "~a: ~s\n" name options)
  (apply-if-t cdr (assoc name options)))

; Returns a union of opts1 and opts2, giving priority to options found in opts2.
(define (merge-options opts1 opts2)
  (append 
    ; opts1 with values from opts2 (if intersection exists)
    (map (lambda (opt)
           (let* ([name (car opt)]
                  [opts2-opt (assoc name opts2)])
             (if opts2-opt (cons name (cdr opts2-opt)) opt)))
         opts1)
          ; opts2 without opts1
          (filter (lambda (opt)
                    (not (assoc (car opt) opts1)))
                  opts2)))
; <<<<<<<<<< LAUNCH OPTIONS <<<<<<<<<<

; The main function to be called by the user. 
; Options specified on the command line take priority over the ones defined in the file.
;
; path: string?
; args: (listof bak-file? string?)
(define (backup path 
                #:overwrite? [overwrite? #f]
                #:append-date? [append-date? #f] 
                ;#:date-format [date-format "%Y-%mm-%dd_%HH%MM%Ss"]
                . args)

  ; -> (listof BackupItem?)
  (define (args->backup-items args)
    (map (lambda (arg)
           (cond [(string? arg) (SpecItem->BackupItem (+SpecItemFile arg))]
                 [(SpecItem? arg) (SpecItem->BackupItem arg)]
                 [else (error "Unexpected argument type:" arg)]))
         args))

  (let* ([options (merge-options
                    (list (cons 'overwrite? overwrite?) 
                          (cons 'append-date? append-date?) 
                          ;(cons 'date-format #:date-format)
                          )
                    (options-from-cmdline))]
         [out-path (apply-if (get-option 'append-date? options) 
                             append-curr-date 
                             (or (get-option 'output options ) path))])
    ; remove the file at `out-path` if overwrite? and `args` not empty
    (when (and (get-option 'overwrite? options) (not (empty? args)))
      (delete-directory/files out-path #:must-exist? #f))

    (let ([backup-items (args->backup-items args)])
      ; handle some options
      (cond [(get-option 'print-files? options )
             (print-included backup-items)]
            ;[(get-option 'simulate? options )
            ; (simulate-backup out-path backup-items)]
            ; main call
            [else (void (make-backup out-path backup-items))]))
    )
  )


