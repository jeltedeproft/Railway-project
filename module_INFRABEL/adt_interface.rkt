#lang racket

(require "utility_functions.rkt")
(require "ADT_Waitlist.rkt")
(require "spoorwegmodelINFRABEL/ADT_modelspoor_INFRABEL.rkt")
(require "adt_INFR-Z21.rkt")

(provide
 execute-deeltraject
 get-loco-detection-block-infrabel
 get-switch-position-infrabel
 get-switch-lock-infrabel
 get-loco-state-infrabel
 set-loco-state-infrabel
 start-model-z21
 stop-model-z21
 get-loco-last-passed-node
 get-loco-direction)

;we make our railwaymodel
(define railwaymodel-infrabel (make-model "../module_INFRABEL/be_simple_infrabel.txt"))

;tcp connectie tussen nmbs en infrabel is niet geimplementeerd, wat hier zou moeten gebeuren, is dat de server requests afhandelt
;wat neerkomt op een conditional die parameters deserialiseerd en vervolgens verwijst naar elke desbetreffende procedure.
;open up a server socket
(define server (tcp-listen 4242))

(define (execute-deeltraject deeltraject amount-of-parts)
  (let ((detection-block-start ((railwaymodel-infrabel 'lookup-ADT) detection-block-identifier ((((deeltraject 'get-start-detection-block)) 'get-name))))
        (detection-block-end ((railwaymodel-infrabel 'lookup-ADT) detection-block-identifier ((((deeltraject 'get-end-detection-block)) 'get-name))))                                                
        (train ((deeltraject 'get-train)))
        (speed 0)
        (number-of-parttraject ((deeltraject 'get-order)))
        (amount amount-of-parts)
        (last-seen-node #f)
        (switches-with-state ((deeltraject 'get-switches-with-state))))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;1)check if switches are available
    ;;  |
    ;;  |--> 2) check if lights are green or orange (adjust speed if orange)
    ;;           |
    ;;           |--> 3)are we on the right starting detection block?
    ;;                                            |
    ;;                                            |--> lock switches
    ;;                                                   set direction
    ;;                                                  execute track
    ;;  else) add trajectory to waitlist
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;step 1) check if all switches are available, it is possible that 2 trains need the same switch with different destinations
    ;if there is a switch unavailable, we put the track in the waitlist
    (if
     ;if there are no switches, we're clear to go
     (if (null? switches-with-state) #t
        (let switch-loop ((remaining-switches switches-with-state)
                          (current-switch (car (cdr (car switches-with-state)))))
          (cond
            ;as long as we're not at the end and our locks are still free, keep going
            ((and (not ((current-switch 'get-lock))) (not (null? (cdr remaining-switches))))
             (switch-loop (cdr remaining-switches) (car (cdr (car remaining-switches)))))
            ;if the lock is not free, stop here and return true
            (((current-switch 'get-lock)) #f)
            ;if its the last element just check this one
            ((null? (cdr remaining-switches)) (not ((current-switch 'get-lock)))))))   
     ;step 2) check if we are at the correct starting block and the train is not bussy
     (if
      (and
       (eq?
        ;check if we are at the correct starting detection block
        (string->symbol ((detection-block-start 'get-name)))
        (get-loco-detection-block-z21 (string->symbol ((train 'get-name)))))
       ;check if we are bussy
       (eq? ((train 'get-status)) #f))      
      
      ;step 3) check lights
      ;wat we nu moeten doen is kijken of het eerste element dat deel uitmaakt van het aankomstdetectieblok,
      ;aan de linkerkant of rechterkant van het dteectiebok staat. en dan het linker of rechterlicht checken.
      (if
       (let ((light
              (let loop ((path ((deeltraject 'get-path)))
                         (next-element (car ((deeltraject 'get-path)))))
                ;check if element is part of the end-detection-block
                (if (not (eq? (findf (lambda (node)
                                       (eq? (string->symbol ((next-element 'get-name))) node))
                                    ((detection-block-end 'get-nodes))) #f))
                   ;if it is, see if its the left or rigth side
                   (begin
                     ;we already have to know which node we are gna pass at the detection block, so we might as well save it here for later use
                     (set! last-seen-node next-element)
                     (if (string=?
                          ((((detection-block-end 'get-left-light)) 'get-node))
                          ((next-element 'get-name)))
                        ((detection-block-end 'get-state-left-light))
                        ((detection-block-end 'get-state-right-light))))
                                     
                   ;if it isnt, see if we reached the end, if we reached the end, its an error,
                   ;we are always suposed to reach the next detection-block, if not keep looping
                   (if (null? (cdr path))
                      (error "this path does not reach the correct detection block")
                      (loop (cdr path) (car (cdr path))))))))
         
         ;if light is red, put speed to 0 and add this trajectory to the waitlist
         (cond ((eq? light 'red)
                (begin
                  (set-loco-speed-z21! (string->symbol ((train 'get-name))) 0)
                  #f))
              ;if light is orange, keep going, but slow down
              ((eq? light 'orange)
               (begin
                 (set! speed 0.4)
                 #t))
              ((eq? light 'green)
               (begin
                 (set! speed 0.8)
                 #t))))
       
       ;track can be executed
       ;set the switches on the correct place and lock them
       (begin
         (for-each (lambda (switch-with-state)
                     (let* ((previous-node (car switch-with-state))
                            (switch (car (cdr switch-with-state)))
                            (next-node (car (cdr (cdr switch-with-state))))
                            (actual-switch-position-number ((switch 'get-number-from-node) next-node)))
                       ;we have to see trough which way we will traverse the switch
                       ;in order to know if it has to be changed or not
                       (if (string=? next-node ((switch 'get-always-connected-node)))
                          ;were going towards the base of the switch, so put the switch towards the previous node
                          (set-switch-position-z21! (string->symbol ((switch 'get-name))) ((switch 'get-number-from-node) previous-node))
                          ;else were going towards on of the arms, put the switch to the next node then
                          (set-switch-position-z21! (string->symbol ((switch 'get-name))) ((switch 'get-number-from-node) next-node)))
                       ((((railwaymodel-infrabel 'lookup-ADT) switch-identifier ((switch 'get-name))) 'set-lock!) #f)))
                  switches-with-state)
         ;mark the train as bussy, so thta no other trajectories get executed in the meantime
         ((train 'set-status!) #t)
         
         ;set direction train
         ;if its the first ride of this train, we cannot deduce the direction from the previous ride,
         ;we thus arbitrarily initialize the direction to forward
         (let ((previous-node ((train 'get-last-passed-node))))
           (if (eq? previous-node #f)
              ((((railwaymodel-infrabel 'lookup-ADT) train-identifier ((train 'get-name))) 'set-direction!) #t)
              ;if not we check the next node and the previous node, if they are the same, that means we switched directions
              (when (string=?
                     ((((deeltraject 'get-first-element)) 'get-name))
                     ((((((railwaymodel-infrabel 'lookup-ADT) train-identifier ((train 'get-name))) 'get-last-passed-node)) 'get-name)))
                ;we're changing directions , inverse the train direction and speed
                ((((railwaymodel-infrabel 'lookup-ADT) train-identifier ((train 'get-name))) 'set-direction!) (not ((((railwaymodel-infrabel 'lookup-ADT) train-identifier ((train 'get-name))) 'get-direction)))))))

         ;we set the lights of the detection block we start from to green again
         ((detection-block-start 'set-state-left-light) 'green)
         ((detection-block-start 'set-state-right-light) 'green)

         ;we set the lights of the detection block we arrive at to red
         ((detection-block-end 'set-state-left-light) 'red)
         ((detection-block-end 'set-state-right-light) 'red)

         ;start moving train, if direction is backwards, negate speed
         (if ((((railwaymodel-infrabel 'lookup-ADT) train-identifier ((train 'get-name))) 'get-direction))
            (set-loco-speed-z21! (string->symbol ((((railwaymodel-infrabel 'lookup-ADT) train-identifier ((train 'get-name))) 'get-name))) speed)
            (set-loco-speed-z21! (string->symbol ((((railwaymodel-infrabel 'lookup-ADT) train-identifier ((train 'get-name))) 'get-name))) (* speed -1)))

         ;we create a thread that will listen to the arrival of the train
         (thread
          (lambda ()
            (let arrival-loop ((lid (string->symbol ((train 'get-name))))
                               (position (get-loco-detection-block-z21 (string->symbol ((train 'get-name))))))
              (cond ((eq? position #f)
                     ;positions is false, this means we are currently driving between 2 det-blocks
                     (begin
                       (sleep 1)
                       (arrival-loop lid (get-loco-detection-block-z21 lid))))
                   ((eq? position (string->symbol ((detection-block-end 'get-name))))
                    ;arrived at destination, set speed to 0 and unlock switches, free train and set last passed node
                    (begin
                      (display " locomotive ")
                      (display lid)
                      (display " has arrived at ")
                      (displayln ((detection-block-end 'get-name)))
                      (set-loco-speed-z21! (string->symbol ((train 'get-name))) 0)
                      (for-each (lambda (switch-with-state)
                                  (let* ((switch (car (cdr switch-with-state))))
                                    ((((railwaymodel-infrabel 'lookup-ADT) switch-identifier ((switch 'get-name))) 'set-lock!) #f)))
                               switches-with-state)
                      ((train 'set-status!) #f)
                      ((((railwaymodel-infrabel 'lookup-ADT) train-identifier ((train 'get-name))) 'set-last-passed-node!) last-seen-node)
                      ;na executie probere we alle trajecte op de wachtlijst opnieuw uit te voeren
                      (for-each (lambda (deeltraject)
                                  (execute-deeltraject (car deeltraject) (cdr deeltraject)))
                               (recheck-waitlist))))
                   ((eq? position (string->symbol ((detection-block-start 'get-name))))
                    ;still at the start, so sleep
                    (begin
                      (sleep 1)
                      (arrival-loop lid (get-loco-detection-block-z21 lid))))
                   (else
                    (begin
                      (displayln ((detection-block-start 'get-name)))
                      (display "there is a problem with the destination of train ")
                      (displayln ((train 'get-name)))
                      (display "it has arrived at ")
                      (displayln position)
                      (display "in stead of ")
                      (displayln ((detection-block-end 'get-name))))))))))
       
       ;else we add this trajectory to the waitlist
       (add-to-waitlist deeltraject amount-of-parts))
      
      (add-to-waitlist deeltraject amount-of-parts))
     
     (add-to-waitlist deeltraject amount-of-parts))))   

(define (get-loco-detection-block-infrabel lid)
  (get-loco-detection-block-z21 lid))

(define (get-switch-position-infrabel snode)
  (get-switch-position-z21 snode))

(define (get-switch-lock-infrabel snode)
  ((((railwaymodel-infrabel 'lookup-ADT)  switch-identifier snode) 'get-lock)))

(define (get-loco-state-infrabel lid)
  ((((railwaymodel-infrabel 'lookup-ADT)  train-identifier lid) 'get-status)))

(define (set-loco-state-infrabel lid boolean)
  ((((railwaymodel-infrabel 'lookup-ADT)  train-identifier lid) 'set-status!) boolean))

(define (start-model-z21)
  (start-simulation))

(define (stop-model-z21)
  (stop-simulation))


(define (get-loco-last-passed-node lid)
  ((((railwaymodel-infrabel 'lookup-ADT)  train-identifier lid) 'get-last-passed-node)))

;true = forward, false = backward
(define (get-loco-direction lid)
  ((((railwaymodel-infrabel 'lookup-ADT)  train-identifier lid) 'get-direction)))







