#lang racket

(require "utility_functions.rkt")
(require "spoorwegmodelNMBS/ADT_modelspoor_NMBS.rkt")

(provide make-ADT-part-traject)

(define (make-ADT-part-traject path trainmodel train start-det-block end-det-block partnumber)
  (let ((all-elements path)
        (executing-train train)
        (start-detection-block start-det-block)
        (end-detection-block end-det-block)
        (switches-with-states '())
        (order partnumber))

    ;we maken een lijst van switches en de toestand die ze zouden moeten hebben
    ;we controleren eerst op het zeldzame geval dat het traject 1 node lang is, dit gebeurt enkel bij aangrenzende detectieblokken
    ;daarom kan dit laatste ook nooit voorvallen met een switch, enkel met nodes
    (when (not (null? (cdr all-elements)))
    (let iteration ((previous-element #f)
                    (leftover-elements all-elements)
                    (first-element (car all-elements))
                    (second-element (car (cdr all-elements))))
      ;als de cdr van de cdr null is, dan zijn we aan de laatste iteratie
      (if (not (null? (cdr (cdr leftover-elements))))
         (if (eq? ((first-element 'get-type)) 'S)
            ;als het een switch is, zet de switch samen met zijn opvolger
            (begin
              (set! switches-with-states (cons (list ((previous-element 'get-name)) first-element ((second-element 'get-name))) switches-with-states))
              (iteration (car leftover-elements) (cdr leftover-elements) (car (cdr leftover-elements)) (car (cdr (cdr leftover-elements)))))
            ;anders itereren we verder
            (iteration (car leftover-elements) (cdr leftover-elements) (car (cdr leftover-elements)) (car (cdr (cdr leftover-elements)))))
         ;laatste iteratie, voer nog eenmaal uit
         (when (eq? ((first-element 'get-type)) 'S)
           (set! switches-with-states (cons (list ((previous-element 'get-name)) first-element ((second-element 'get-name))) switches-with-states))))))
    
    
    (define (get-path)
      all-elements)

      (define (get-order)
      order)
    
    (define (get-train)
      executing-train)

        (define (get-first-element)
      (car path))
    
    (define (get-start-detection-block)
      start-detection-block)
    
    (define (get-end-detection-block)
      end-detection-block)
    
    (define (get-switches-with-state)
      switches-with-states)
    
    (define (dispatch msg)
      (cond ((eq? msg 'get-path) get-path)
           ((eq? msg 'get-start-detection-block) get-start-detection-block)
           ((eq? msg 'get-train) get-train)
           ((eq? msg 'get-order) get-order)
           ((eq? msg 'get-first-element) get-first-element)
           ((eq? msg 'get-end-detection-block) get-end-detection-block)
           ((eq? msg 'get-switches-with-state) get-switches-with-state)))
    dispatch))

