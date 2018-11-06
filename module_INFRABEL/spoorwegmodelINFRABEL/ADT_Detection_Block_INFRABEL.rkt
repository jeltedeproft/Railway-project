#lang racket

(require "ADT_Light_INFRABEL.rkt")

(provide make-ADT-detection-block-mivb)

(define (make-ADT-detection-block-mivb detection-block)
  (let* ((detection-block-id (car detection-block))
        (name (number->string (car (cdr detection-block))))
        (nodes (car (cdr (cdr detection-block))))
        (right-light (make-ADT-light-mivb 'green (car (cdr nodes))))
        (left-light (make-ADT-light-mivb 'green (car nodes)))) 
    
    (define  (get-name)
      name)
    
    (define (get-left-light)
      left-light)

        (define (get-right-light)
      right-light)
    
    (define (get-type)
      detection-block-id)
    
    (define (get-nodes)
      nodes)
    
    (define (get-state-left-light)
      ((left-light 'get-color)))
    
    (define (set-state-left-light color)
      ((left-light 'set-color) color))

        (define (get-state-right-light)
      ((right-light 'get-color)))
    
    (define (set-state-right-light color)
      ((right-light 'set-color) color))
    
    ;if this is us, we give ourselves back, else false
    (define (check-if-this-is-me identifier)
      (if (eq? name identifier) dispatch #f))
    
    (define (dispatch msg)
      (cond ((eq? msg 'get-name) get-name)
           ((eq? msg 'get-left-light) get-left-light)
           ((eq? msg 'get-right-light) get-right-light)
           ((eq? msg 'get-type) get-type)
           ((eq? msg 'get-nodes) get-nodes)
           ((eq? msg 'get-state-left-light) get-state-left-light)
           ((eq? msg 'set-state-left-light) set-state-left-light)
           ((eq? msg 'get-state-right-light) get-state-right-light)
           ((eq? msg 'set-state-right-light) set-state-right-light)
           ((eq? msg 'check-if-this-is-me) check-if-this-is-me)))
    
    dispatch))