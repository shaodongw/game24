;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname game24) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
;; Data Definition
(define-struct node (lon empty) path))
(define empty-node (make-node empty empty))
;; a node: (make-node lon path)
;; where lon is a list of numbers, path is a series of number and operation symbol

;; solve: list of node -> list of node
;; At first the input list of node only has one node - root node.
;; Each time solve it, the number list in the node will be reduced,
;;   and the nodes in list are increased,
;;   until the node is reduced to leaf node.
;; The result is the list of node which match 24.

(define (solve lnode)
  (cond
    [(empty? lnode) empty]
    [else
     (cons
      (reduce (first lnode))
      (solve (rest lnode)))]))

(define (reduce node)
  (cond
    [(empty? node-lon) (make-node empty node-path) ]
    [else
     (cond
       [(empty? (rest node-lon))
        (cond
          [(= 24 (first node-lon)) node]
          [else empty])]
       [else (each-other node)])]))


(define (each-other node)
  (local(
         (define (each-other0 node leader path)
           (cond
             [(empty? node-lon) empty-node]
             [else
              (first-others node leader path)])))
    (each-other0 node empty empty)))

(define (first-others node leader path)
  (cond
    [(empty? node-lon) empty]
    [else
     (append
      (one-others (first node-lon) (rest node-lon) leader)
      (first-others (rest node-lon) (append leader (cons (first node-lon) empty))))]))

(define (one-others number node leader path)
  (cond
    [(empty? node-lon) empty]
    [else
     (cons
      (make-node (append
                  leader
                  (cons (+ number (first node-lon))
                        (rest node-lon)))
                 (cons '+ (cons number (cons (first node-lon) path))))
      (one-others number (make-node (rest node-lon) path) (append leader (cons (first node-lon) empty)) path))]))

(define (fst number node leader path)
  node-lon)
  ;;(+ number (first node-lon)))

;(define test (list (make-node (list 3 21) empty)))
(define test (fst 2 (make-node (list 2 8 5 9) empty) empty empty))
;(define test (one-others 2 (make-node (list 2 8 5 9) empty) empty empty))
;(define test (solve (list (make-node (list 3 21) empty))))

test
