#lang racket

(provide make-ADT-arc-nmbs)

(define (make-ADT-arc-nmbs arc)
  (let* ((first-node (car arc))
         (second-node (car (cdr arc)))
         (weight (car (cdr (cdr arc))))
         (first-id (car first-node))
         (second-id (car second-node))
         (first-name (car (cdr first-node)))
         (second-name (car (cdr second-node))))
    
    (define  (get-first-node)
      first-node)
    
    (define  (get-first-node-id)
      first-id)

        (define  (get-first-node-name)
             first-name)
    
    (define  (get-second-node)
      second-node)
    
    (define  (get-second-node-id)
      second-id)

        (define  (get-second-node-name)
             second-name)

        (define (get-weight)
      weight)
    
    (define (dispatch msg)
      (cond ((eq? msg 'get-first-node) get-first-node)
           ((eq? msg 'get-first-node-id) get-first-node-id)
           ((eq? msg 'get-first-node-name) get-first-node-name)
           ((eq? msg 'get-second-node-id) get-second-node-id)
           ((eq? msg 'get-second-node) get-second-node)
           ((eq? msg 'get-weight) get-weight)
           ((eq? msg 'get-second-node-name) get-second-node-name)))
    
    dispatch))