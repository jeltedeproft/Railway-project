#lang racket

(require "utility_functions.rkt")
(require "ADT_NMBS--INFRABEL.rkt")
(require "ADT_parttraject.rkt")

(provide make-ADT-traject)

(define (make-ADT-traject trainid destination time trainmodel)
  (let* ((train ((trainmodel 'get-train) trainid))
         (train-pos-detection-block  ((trainmodel 'lookup-ADT) 'De (symbol->string ((train 'get-last-seen-detection-block)))))
         (destination-detection-block  ((trainmodel 'lookup-ADT) 'De destination))
         (start-node ((trainmodel 'lookup-ADT) 'N  (last-element ((train-pos-detection-block 'get-nodes)))))
         (traject-destination destination)
         (end-node ((trainmodel 'lookup-ADT) 'N (car ((destination-detection-block 'get-nodes)))))
         (starttime time)
         (model trainmodel)
         (partnumber 1))
    
    (define path ((model 'find-path) start-node end-node))
    (define all-parts '())
    
    ;deze methode kijkt of we het volgende detectieblok bereikt hebben
    (define (check-if-reached-next-detection-block old-detection-block node)
      (let ((all-detection-blocks ((trainmodel 'get-detection-blocks)))
            (element-found-in-another-block #f))
        (for-each (lambda (block)
                    (when
                        ;kijk of we onze node gevonden hebben in een ander blok
                        (and
                         (not (eq? ((block 'get-name)) ((old-detection-block 'get-name))))
                         (findf (lambda (nodename) (eq? nodename (string->symbol ((node 'get-name))))) ((block 'get-nodes))))
                      ;de return value zou een blok moeten zijn
                      (set! element-found-in-another-block block)))
                 all-detection-blocks)
        element-found-in-another-block))
    
    (define (execute)
      ;sorteer alle trajecten en voer ze uit    
      (set! all-parts (sort all-parts (lambda (el1 el2) (< ((el1 'get-order)) ((el2 'get-order))))))
      (for-each (lambda (part-trajectory) (send-execute-deeltraject part-trajectory  partnumber)) all-parts))
    
    ;als path #f is, dan is het pad niet mogelijk
    (if path
       (begin
         
         ;we verdelen het traject in deelstukken
         ;we doen enkel iets als onze positie en de bestemming niet gelijk zijn
         (when (not (eq? train-pos-detection-block destination-detection-block))
           ;he detectieblok waar we momenteel zijn
           (let ((current-detection-block train-pos-detection-block)
                 (all-part-trajectories '())
                 (current-part-trajectory '()))
             (map (lambda (path-element)
                    ;als het een node is, zie of het nog steeds op het pad tussen X-Y is
                    (if (eq? ((path-element 'get-type)) 'N)
                       ;als de node nog steeds op hetzelfde detectieblok is of niet op een detectieblok, dan zijn we nog steeds in hetzelfde deeltraject
                       (let ((next-det-block (check-if-reached-next-detection-block current-detection-block path-element)))
                         (if (not next-det-block)
                            (set! current-part-trajectory (cons path-element current-part-trajectory))
                            ;indien niet
                            (begin
                              ;voeg de eerste node toe van het volgende blok
                              (set! current-part-trajectory (cons path-element current-part-trajectory))
                              ;keer de lijst om, terug in de juiste volgorde
                              (set! current-part-trajectory (reverse current-part-trajectory))
                              ;voeg dit deeltraject toe aan een lijst met alle deeltrajecten
                              (set! all-part-trajectories (cons
                                                           (make-ADT-part-traject current-part-trajectory trainmodel train current-detection-block next-det-block partnumber)
                                                           all-part-trajectories))
                              ;incrementeer het nummer van het deeltraject
                              (set! partnumber (+ partnumber 1))
                              ;zet het huidige detectieblok naar het volgende
                              (set! current-detection-block next-det-block)
                              ;refresh de lijst
                              (set! current-part-trajectory '()))))
                       ;als het een switch is, zet hem er gewoon bij
                       (set! current-part-trajectory (cons path-element current-part-trajectory))))
                 path)
             (set! all-parts all-part-trajectories))))
       (displayln " this path cannot be executed "))
    
    (define (dispatch msg)
      (cond ((eq? msg 'execute) execute)))
    
    dispatch))









