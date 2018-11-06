#lang racket

(require graph)
(require "ADT_Train_NMBS.rkt")
(require "ADT_Arc_NMBS.rkt")
(require "ADT_Detection_Block_NMBS.rkt")
(require "ADT_Node_NMBS.rkt")
(require "ADT_Switch_NMBS.rkt")

(provide make-model)

(define train-identifier 'L)
(define node-identifier 'N)
(define switch-identifier 'S)
(define detection-block-identifier 'De)

(define (make-model filename)
  (let* ((current-graph (weighted-graph/undirected '()))
         (full-graph (file->list filename))
         (arcs-list (car full-graph))
         (trains-list (car (cdr full-graph)))
         (nodes-list (car (cdr (cdr full-graph))))
         (switches-list (car (cdr (cdr (cdr full-graph)))))
         (detection-blocks-list (car (cdr (cdr (cdr (cdr full-graph)))))))
    
    ;we create the ADT's corresponding to the elements of the railway
    ;1) create the train ADT's
    (define trains (map (lambda (train) (make-ADT-train-nmbs train))
                       trains-list))
    
    ;2) create the node ADT's
    (define nodes (map (lambda (node) (make-ADT-node-nmbs node))
                      nodes-list))
    
    ;3) create the switch ADT's
    (define switches (map (lambda (switch) (make-ADT-switch-nmbs switch))
                         switches-list))
    
    ;4) create the detection-block ADT's
    (define detection-blocks (map (lambda (detection-block) (make-ADT-detection-block-nmbs detection-block))
                                 detection-blocks-list))
    
    ;5) create the arc ADT's
    (define arcs (map (lambda (arc)
                        (make-ADT-arc-nmbs arc))
                     arcs-list))
    
    ;procedure to return an ADT found by using an identifier
    (define (lookup-ADT identifier name)
      (let ((name-string (if (symbol? name) (symbol->string name) name)))
        (cond
          ;is it a train?
          ((eq? identifier train-identifier)
           (findf (lambda (train)
                    (string=? name-string
                             ((train 'get-name))))
                 trains))
          ;or a node?
          ((eq? identifier node-identifier)
           (findf (lambda (node)
                    (string=? name-string
                             ((node 'get-name))))
                 nodes))
          ;or a detection-block?
          ((eq? identifier detection-block-identifier)
           (findf (lambda (detection-block)
                    (string=? name-string
                             ((detection-block 'get-name))))
                 detection-blocks))
          ;or a switch?
          ((eq? identifier switch-identifier)
           (findf (lambda (switch)
                    (string=? name-string
                             ((switch 'get-name))))
                 switches)))))    
    
    ;add nodes to graph
    (map (lambda (vertex) 
           (add-vertex! current-graph vertex))
        nodes)
    
    ;add switches to graph
    (map (lambda (switch)
           (add-vertex! current-graph switch))
        switches)   
    
    ;now we create an edge in the graph between every ADT that is also connected in our railwaymodel
    (map (lambda (arc)
           (add-edge! current-graph
                     (lookup-ADT ((arc 'get-first-node-id)) ((arc 'get-first-node-name)))
                     (lookup-ADT ((arc 'get-second-node-id)) ((arc 'get-second-node-name)))
                     ((arc 'get-weight))))                      
        arcs)
    
    ;getters
    (define (get-trains)
      trains)
    
    (define (get-nodes)
      nodes)
    
    (define (get-switches)
      switches)
    
    (define (get-arcs)
      arcs)
    
    (define (get-detection-blocks)
      detection-blocks)
    
    (define (get-train train-name)
      (define (inner-search-loop trainidentifier trainlist-rest)
        (if (null? trainlist-rest) #f
           (let* ((train (car trainlist-rest))
                  (trainname ((train 'get-name))))
             (if (equal? trainidentifier trainname)
                train
                (inner-search-loop trainidentifier (cdr trainlist-rest))))))
      (inner-search-loop train-name trains))
    
    (define (get-path-between source-node destination-node)
      (define complete-paths '())
      (let loop
        ;we get the already visited nodes, the balance and all the current neighbours
        ((trail (list source-node))
         (unbalance 0)
         (neighbors (get-neighbors current-graph source-node)))
        ;for every neighbor that wasnt already in the path
        (for-each (lambda (neighbor)
                    (when (not (findf (lambda (node)
                                        (eq? node neighbor))
                                     trail))
                      ;if we previously had a balance difference, we have to make sure it cancels out again in this step
                      ;this is only an issue if the following node is a switch and the balance doesnt equal 0
                      (when (or
                             (eq? (edge-weight current-graph (car trail) neighbor) 0)
                             (or
                              (eq? unbalance 0)
                              (eq? (+ unbalance (edge-weight current-graph (car trail) neighbor)) 0)))
                        (cond
                          ;if its the destination, path complete!
                          ((eq? neighbor destination-node)
                           (begin
                             (set! trail (cons neighbor trail))
                             (set! complete-paths (cons trail complete-paths))))
                          ;if there is a non-zero weight, make sure to balance it out in the next step
                          ((not (eq? (edge-weight current-graph (car trail) neighbor) 0))
                           (loop (cons neighbor trail) (edge-weight current-graph (car trail) neighbor) (get-neighbors current-graph neighbor)))
                          ;in all other cases its just a node on the path, keep going
                          (else (loop (cons neighbor trail) 0 (get-neighbors current-graph neighbor)))))))
                 neighbors))
      
      ;now we have collected all the paths, return the shortest path
      (if (null? complete-paths)
         #f
         (let loop2
           ((current-shortest-path (car complete-paths))
            (leftover-paths (cdr complete-paths)))
           (if (not (null? leftover-paths))
              (if (not (null? (cdr leftover-paths)))
                 (loop2 (if (> (length current-shortest-path) (length (car leftover-paths)))
                           (car leftover-paths)
                           current-shortest-path)
                       (cdr leftover-paths))
                 (if (> (length current-shortest-path) (length (car leftover-paths)))
                    (reverse (car leftover-paths))
                    (reverse current-shortest-path)))
                 (reverse current-shortest-path)))))
    
    
    
    
    
    (define (get-shortest-path-from-to source-node destination-node)
      ;we collect 2 hash tables from the graph library, 1 has the distance from the source to every node
      ;the other has the node preceding every node on the path from source to destination
      (define-values (distances previous-nodes) (bfs current-graph source-node))
      
      (define (recreate-path gathered-nodes target)
        ;we keep collecting the previous node, check if it matches our source node, if not we keep going deeper
        (let ((previous-node (hash-ref previous-nodes target)))
          (if (not (equal? previous-node source-node))
             (recreate-path (cons  previous-node gathered-nodes) previous-node)
             ;found it! return our list of nodes
             gathered-nodes)))
      ;if the source and destination are the same node, return that node
      (if (equal? source-node destination-node)
         (list source-node)
         (recreate-path (list destination-node) destination-node)))
    
    (define (dispatch msg)
      (cond ((eq? msg 'get-trains) get-trains)
           ((eq? msg 'get-nodes) get-nodes)
           ((eq? msg 'get-train) get-train)
           ((eq? msg 'get-detection-blocks) get-detection-blocks)
           ((eq? msg 'get-switches) get-switches)
           ((eq? msg 'get-arcs) get-arcs)
           ((eq? msg 'lookup-ADT) lookup-ADT)
           ((eq? msg 'find-path) get-path-between)))
    dispatch))
