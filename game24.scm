;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname game24) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
(define (resolutions result lon)
  (cond
    [(empty? lon) empty]
    [(and (empty? (rest lon)) (= result (first lon))) result]
    

;; Data Definition
(define-struct node (lon path))
;; a node: (make-node lon path)
;; where lon is a list of numbers, path is a series of number and operation symbol

;(define (go lnode)
;  (cond
;    [(is-leaf? (first lnode)) lnode]
;    [(empty? (first lnode)) lnode]
;    [else (go lnode)]))

;; solve: list of node -> list of node
;; At first the input list of node only has one node - root node.
;; Each time solve it, the number list in the node will be reduced,
;;   and the nodes in list are increased,
;;   until the node is reduced to leaf node.
;; The result is the list of node which match 24.

;(define (solve node)
;  (cond
;    [(cons? (reduce node))
;     (cons (reduce (first (reduce node))) 
;     (append (reduce (first lnode)) (solve (rest lnode)))]
;    [else empty]))

(define (l-reduce lnode)
  (cond
    [(empty? lnode) empty]
    [else
     (cond
       [(cons? (reduce (first lnode))) (l-reduce (append (reduce (first lnode)) (l-reduce (rest lnode))))]
       [else (cons (reduce (first lnode)) (l-reduce (rest lnode)))])]))

(define (is-solu? node)
  (and
   (is-leaf? node)
   (= 24 (first (node-lon node)))))

(define (is-leaf? node)
  (empty? (rest (node-lon node))))

(define (reduce node)
  (cond
    [(empty? node) empty]
    [(is-leaf? node) node]
    [else (each-other node)]))

(define (each-other node)
  (local(
         (define (each-other0 node leader path)
           (cond
             [(empty? (node-lon node)) empty]
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


(define-struct merge (result expression))

(define (operate op number1 number2)
  (make-merge (op number1 number2) (list op number1 number2)))

(define (four-op nu1 nu2)
  (list
   (operate + nu1 nu2)
   (operate - nu1 nu2)
   (operate - nu2 nu1)
   (operate * nu1 nu2)
   (operate / nu1 nu2)
   (operate / nu2 nu1)))
   

;(define test (l-reduce (list (make-node (list 5 3 9) (list '+ 88 99)) (make-node (list 4 1 6) (list '+ 66 77)) )))
;(define test (l-reduce (list (make-node (list 5 3 4 9) (list '+ 88 99)) )))

(define test (four-op 3 5))

test
