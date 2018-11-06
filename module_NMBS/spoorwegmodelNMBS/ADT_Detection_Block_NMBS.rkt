#lang racket

(require "ADT_Light_NMBS.rkt")

(provide make-ADT-detection-block-nmbs)

(define (make-ADT-detection-block-nmbs detection-block)
  (let ((detection-block-id (car detection-block))
        (name (number->string (car (cdr detection-block))))
        (nodes (car (cdr (cdr detection-block)))))
    
    (define  (get-name)
      name)
    
    (define (get-type)
      detection-block-id)
    
    (define (get-nodes)
      nodes)
    
    ;if this is us, we give ourselves back, else false
    (define (check-if-this-is-me identifier)
      (if (eq? name identifier) dispatch #f))
    
    (define (dispatch msg)
      (cond ((eq? msg 'get-name) get-name)
           ((eq? msg 'get-type) get-type)
           ((eq? msg 'get-nodes) get-nodes)
           ((eq? msg 'check-if-this-is-me) check-if-this-is-me)))
    
    dispatch))