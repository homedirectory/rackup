#lang racket/base

(require "backup-item.rkt")

(provide (all-defined-out))

; --------------------------------------------------------------------------------

; TODO
(define (print-included items)
  (for-each (lambda (item)
              (if (excludable? item)
                (for-each displayln (get-included item))
                (displayln item)))
            items))
