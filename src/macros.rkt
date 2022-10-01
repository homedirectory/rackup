#lang racket/base

(provide (all-defined-out))

; -----------------------------------------------------------------------------

; Create an alias for a procedure.
; Example:
;   (alias-proc + add)
; `add` has been registered as a syntax that transforms to `+`.
; (add 2 3) => (+ 2 3)
(define-syntax-rule 
  (alias-proc alias-id proc-id)
  (define-syntax-rule (alias-id args (... ...)) (proc-id args (... ...))))

;(define-syntax-rule (alias-proc alias-id proc-id)
;  (define-syntax (alias-id stx) 
;    (let ([dat (syntax->datum stx)]) 
;      (if (pair? dat) 
;        (datum->syntax stx (cons 'proc-id (cdr (syntax->datum stx))))
;        #'proc-id ))))

