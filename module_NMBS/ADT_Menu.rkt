#lang racket/gui

(require "ADT_traject.rkt")
(require "ADT-Time.rkt")

(provide make-gui)

(define (make-gui railwaymodel planneradt)
  (let ((model railwaymodel)
        (planner planneradt)
        (trains ((railwaymodel 'get-trains)))
        (nodes ((railwaymodel 'get-nodes)))
        (detection-blocks ((railwaymodel 'get-detection-blocks)))
        (switches ((railwaymodel 'get-switches))))

    ;om de GUI te laten werken, hebben we de namen nodig van elke trein en detectieblok als een string
    (define list-of-train-names '())
    (define list-of-detection-block-names '())
    
    (define (fill-up-as-string-name empty-list list-of-ADT)
      (define (fill-up-loop current-list to-add-list)
        (let ((current-ADT (car to-add-list)))
          ;laatste element van lijst?
          (if (eq? (cdr to-add-list) '()) (cons ((current-ADT 'get-name)) current-list)
             ;nee? blijf loopen
             (fill-up-loop (cons ((current-ADT 'get-name)) current-list) (cdr to-add-list)))))
      (fill-up-loop '() list-of-ADT))

    (set! list-of-train-names (fill-up-as-string-name '() trains))
    (set! list-of-detection-block-names (fill-up-as-string-name '() detection-blocks))

    
    ; Maak een frame door frame% class te instantieren
    (define frame (new frame% [label "NMBS GUI"]
                      [alignment '(center top)]))
    ;we maken 3 panels, voor de treinen, het schema en het spoorwegmodel(gelinkte lijst)
    (define trainpanel (new vertical-panel% [parent frame]))
    (define schedulepanel (new vertical-panel% [parent frame]
                              [style '(deleted)]))
    (define railwaypanel (new vertical-panel% [parent frame]
                             [style '(deleted)]))
    
    ;benoem ze
    (send trainpanel set-label "trainpanel")
    (send schedulepanel set-label "schedulepanel")
    (send railwaypanel set-label "railwaypanel")
    
    ;maak treinpaneel
    (define trainselection (new radio-box% [parent trainpanel]
                               [label "Select a train : "]
                               [choices list-of-train-names]))

    
    
    (define destinationselection (new choice% [parent trainpanel]
                                     [label "Select a destination : "]
                                     [choices list-of-detection-block-names]))
    
    (define hourselection (new slider% [parent trainpanel]
                              [label "Select the hour : "]
                              [min-value 0]
                              [max-value 23]))
    
    (define minuteselection (new slider% [parent trainpanel]
                                [label "Select the minutes : "]
                                [min-value 0]
                                [max-value 59]))
    
    (define secondselection (new slider% [parent trainpanel]
                                [label "Select the seconds : "]
                                [min-value 0]
                                [max-value 59]))
    
    (new button% [parent trainpanel]
        [label "Click here to add this trajectory to the queue "]
        ; Callback procedure voor een button click
        [callback (lambda (button event)
                    (let* ((trainchoice (send trainselection get-item-label (send trainselection get-selection)))
                           (destinationchoice (send destinationselection get-string-selection))
                           (hourchoice (send hourselection get-value))
                           (minutechoice (send minuteselection get-value))
                           (secondchoice (send secondselection get-value))
                           (time (make-timestamp hourchoice minutechoice secondchoice)))
                      ;creeer een traject en voeg toe aan de planner
                      (display "trainchoice = ")
                      (displayln trainchoice)
                     ((planner 'add-trajectory) (make-ADT-traject trainchoice destinationchoice time model))
                      ;toon wat we hebben geselecteerd
                      (display "selected train = ")
                      (displayln trainchoice)
                      (display "selected destination = ")
                      (displayln destinationchoice)))])
    
    ;maak spoorwegpaneel
    (define trainpositions '())
    (let show-trains ((trainlist trains))
      (when (not (null? trainlist))
        (let ((train (car trainlist)))
          ;maak een text veld voor elke trein
          (set! trainpositions (cons (cons (new text-field% [parent railwaypanel]
                          [label ((train 'get-name))]
                          [init-value (symbol->string ((train 'get-current-detection-block)))])
                     train)
               trainpositions))
          (show-trains (cdr trainlist)))))
    (thread
     (lambda()
       (let update-trains()
         (for-each (lambda (train+textfield)
                     (let ((textfield (car train+textfield))
                           (train (cdr train+textfield)))
                     (when (not (eq?
                                 ((train 'get-current-detection-block))
                                 (send textfield get-value)))
                       (send textfield set-value (symbol->string (if ((train 'get-current-detection-block))
                                                                    ((train 'get-current-detection-block))
                                                                    'travelling))))))
                  trainpositions)
         (sleep 1)
         (update-trains))))
                       
    
    ; Make a static text message in trainpanel
    (define msg (new message% [parent trainpanel]
                    [label "i am the trainpanel"]))
    
    ; Make a static text message in schedulepanel
    (define msg2 (new message% [parent schedulepanel]
                     [label "i am the schedulepanel"]))
    
    ; Make a static text message in railwaypanel
    (define msg3 (new message% [parent railwaypanel]
                     [label "i am the railwaypanel"]))
    
    
    ; Make a button for accessing the previous panel
    (new button% [parent frame]
        [label "Previous"]
        ; Callback procedure for a button click:
        [callback (lambda (button event)
                    (let ((trainpanel-shown? (send trainpanel is-shown?))
                          (schedulepanel-shown? (send schedulepanel is-shown?))
                          (railwaypanel-shown? (send railwaypanel is-shown?))
                          (trainpanel-parent (send trainpanel get-parent))
                          (schedulepanel-parent (send schedulepanel get-parent))
                          (railwaypanel-parent (send railwaypanel get-parent)))
                      (cond
                        (trainpanel-shown?
                         (begin
                           (send trainpanel-parent  delete-child trainpanel)
                           (send railwaypanel-parent add-child railwaypanel)))
                        (schedulepanel-shown?
                         (begin
                           (send schedulepanel-parent  delete-child schedulepanel)
                           (send trainpanel-parent add-child trainpanel)))
                        (railwaypanel-shown?
                         (begin
                           (send railwaypanel-parent  delete-child railwaypanel)
                           (send schedulepanel-parent add-child schedulepanel)))
                        (else (error "no panels active")))))])
    
    
    ; Make a button for accessing the next panel
    (new button% [parent frame]
        [label "Next"]
        ; Callback procedure for a button click:
        [callback (lambda (button event)
                    (let ((trainpanel-shown? (send trainpanel is-shown?))
                          (schedulepanel-shown? (send schedulepanel is-shown?))
                          (railwaypanel-shown? (send railwaypanel is-shown?))
                          (trainpanel-parent (send trainpanel get-parent))
                          (schedulepanel-parent (send schedulepanel get-parent))
                          (railwaypanel-parent (send railwaypanel get-parent)))
                      (cond
                        (trainpanel-shown?
                         (begin
                           (send trainpanel-parent  delete-child trainpanel)
                           (send schedulepanel-parent add-child schedulepanel)))
                        (schedulepanel-shown?
                         (begin
                           (send schedulepanel-parent  delete-child schedulepanel)
                           (send railwaypanel-parent add-child railwaypanel)))
                        (railwaypanel-shown?
                         (begin
                           (send railwaypanel-parent  delete-child railwaypanel)
                           (send trainpanel-parent add-child trainpanel)))
                        (else (error "no panels active")))))])
    
    
    ; Show the frame by calling its show method
    (send frame show #t)))