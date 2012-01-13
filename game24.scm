;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname game24) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
;; Data Definition
(define-struct node (lon path))
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
    [(empty? (node-lon node)) (make-node empty (node-path node)) ]
    [else
     (cond
       [(empty? (rest (node-lon node)))
        (cond
          [(= 24 (first (node-lon node))) node]
          [else (make-node empty empty)])]
       [else (each-other node)])]))


(define (each-other node)
  (local(
         (define (each-other0 node leader path)
           (cond
             [(empty? (node-lon node)) empty-node]
             [else
              (first-others node leader path)])))
    (each-other0 node empty (node-path node))))

(define (first-others node leader path)
  (cond
    [(empty? (node-lon node)) empty]
    [else
     (append
      (one-others (first (node-lon node)) (make-node (rest (node-lon node)) path)  leader path)
      (first-others (make-node (rest (node-lon node)) path) (append leader (cons (first (node-lon node)) empty)) path))]))

(define (one-others number node leader path)
  (cond
    [(empty? (node-lon node)) empty]
    [else
     (cons
      (make-node (append
                  leader
                  (cons (+ number (first (node-lon node)))
                        (rest (node-lon node))))
                 (cons '+ (cons number (cons (first (node-lon node)) path))))
      (one-others number (make-node (rest (node-lon node)) path) (append leader (cons (first (node-lon node)) empty)) path))]))


;(define test (each-other (make-node (list 2 7 5 9) empty)))
;(define test (reduce (make-node (list 5 3 9) (list '+ 88 99))))
;(define test2 (solve (list (make-node (list 4 11 1 8) empty))))

test
