#lang racket

(provide add-element-to-hash-table)
(provide check-state)


(define hash-set-all-elements-with-state (make-hash))

;can overwrite old values if key is the same (shouldn't happen normally)
(define (add-element-to-hash-table key value)
  (hash-set! hash-set-all-elements-with-state key value))

(define (check-state key)
  (hash-ref hash-set-all-elements-with-state key))

