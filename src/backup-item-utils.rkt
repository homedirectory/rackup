#lang racket/base

(require "backup-item.rkt")

(provide (all-defined-out))

; --------------------------------------------------------------------------------

; TODO
(define (print-included items)
  (for-each (lambda (item)
              (if (excludable? item)
                (for-each displayln (map short-desc (get-included item)))
                (displayln (short-desc item))))
            items))
