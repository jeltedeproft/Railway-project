#lang racket

(provide make-ADT-light-mivb)

(define (make-ADT-light-mivb color node)
  (let ((current_color color)
         (position (symbol->string node)))
    
    (define  (get-color)
      current_color)

        (define  (get-node)
      position)

    (define (set-color color)
      (set! current_color color))
    
    (define (dispatch msg)
      (cond ((eq? msg 'get-color) get-color)
           ((eq? msg 'get-node) get-node)
           ((eq? msg 'set-color) set-color)))
    
    dispatch))