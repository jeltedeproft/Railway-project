#lang racket

(provide make-ADT-node-mivb)

(define (make-ADT-node-mivb node)
  (let ((node-symbol (car node))
        (name  (symbol->string (car (cdr node)))))
    
    (define  (get-name)
      name)
    
    ;if this is us, we give ourselves back, else false
    (define (check-if-this-is-me identifier)
      (if (eq? name identifier) dispatch #f))
    
    (define (dispatch msg)
      (cond ((eq? msg 'get-name) get-name)
            ((eq? msg 'check-if-this-is-me) check-if-this-is-me)))
    
    dispatch))