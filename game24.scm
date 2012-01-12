;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname game24) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
;; Data Definition
(define-struc node (lon path))
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
    [(empty? node-lon) (make-node epmty node-path) ]
    [(empty? (rest node-lon))
     (cond
       [(= 24 (first node-lon)) node]
       [else empty])]
    [else
     (each-other node)]))

(define (each-other lon)
  (local(
         (define (each-other0 lon lon-leader)
           (cond
             [(empty? lon) empty]
             [else
              (first-others lon lon-leader)])))
    (each-other0 lon empty)))


(define (first-others lon lon-leader)
  (cond
    [(empty? lon) empty]
    [else
     (append
      (one-others (first lon) (rest lon) lon-leader)
      (first-others (rest lon) (append lon-leader (cons (first lon) empty))))]))

(define (one-others number lon lon-leader)
  (cond
    [(empty? lon) empty]
    [else
     (cons
      (append
       lon-leader
       (cons (+ number (first lon)) (rest lon)))
      (one-others number (rest lon) (append lon-leader (cons (first lon) empty))))]))

(define test6 (each-other (list 2 8 5 9 12)))
(define test5 (each-other (list 2 8 5 9)))
(define test1 (each-other (list 2 8 5)))
(define test2 (each-other (list 2 8)))
(define test3 (each-other (list 79)))
(define test4 (each-other empty))

test5
