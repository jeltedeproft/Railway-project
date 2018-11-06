#lang racket

(require "../ADT_NMBS--INFRABEL.rkt")

(provide make-ADT-train-nmbs)

(define (make-ADT-train-nmbs train)
  (let* ((trainsymbol (car train))
         (name (symbol->string (car (cdr train))))
         (last-seen-detection-block (send-get-loco-detection-block  (string->symbol name))))

        ;update the last seen detection block if necesaryy
   (define  (get-last-seen-detection-block)
     (let ((pos  (send-get-loco-detection-block (string->symbol name))))
       ;if the pos is false, we are travelling between detection blocks
       (if (eq? pos #f)
          last-seen-detection-block
          (if (not (eq? pos last-seen-detection-block))
             (begin
            (set! last-seen-detection-block pos)
            pos)
             pos))))

        (define (get-current-detection-block)
      (send-get-loco-detection-block (string->symbol name)))
    
    (define (get-name)
      name)
    
    (define (get-type)
      trainsymbol)
    
    (define (get-status)
      (send-get-loco-state name))

   (define (set-status boolean)
      (send-set-loco-state name boolean))

    (define (get-last-passed-node)
      (send-get-loco-last-passed-node name))

    ;true = forward, false = backward
    (define (get-direction)
      (send-get-loco-direction name))
   
    
    ;if this is us, we give ourselves back, else false
    (define (check-if-this-is-me identifier)
      (if (eq? name identifier) dispatch #f))
    
    (define (dispatch msg)
      (cond ((eq? msg 'get-last-seen-detection-block) get-last-seen-detection-block)
           ((eq? msg 'get-name) get-name)
           ((eq? msg 'get-type) get-type)
           ((eq? msg 'get-current-detection-block) get-current-detection-block)
           ((eq? msg 'get-status) get-status)
           ((eq? msg 'set-status!) set-status)
           ((eq? msg 'get-last-passed-node) get-last-passed-node)
           ((eq? msg 'get-direction) get-direction)
           ((eq? msg 'check-if-this-is-me) check-if-this-is-me)))
    
    dispatch))
