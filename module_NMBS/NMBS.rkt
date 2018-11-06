#lang racket

(require "ADT_NMBS--INFRABEL.rkt")
(require "spoorwegmodelNMBS/ADT_modelspoor_NMBS.rkt")
(require "ADT_Menu.rkt")
(require "ADT_Traject.rkt")
(require "ADT_Planner.rkt")

;maak een planner (doet niets)
(define planner (make-planner))

;maak het spoorwegmodel
(define railwaymodel (make-model "be_simple_nmbs.txt"))

;geef spoorwegmodel en planner aan de GUI
(make-gui railwaymodel planner)





