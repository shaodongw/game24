;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname game24) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
(define final 24)
(define-struct poker (number path))
(define (game24 lon)
  (cond
    [(empty? lon) empty]
    [else
     (resolution-all (list (pack lon)))]))

(define (resolution-all lolopoker)
  (cond
    [(empty? lolopoker)     empty]
    [else
     (append
      (resolution-one (first lolopoker))
      (resolution-all (rest lolopoker)))]))

(define (pack lon)
  (cond
    [(empty? lon) empty]
    [else
     (cons
      (make-poker (first lon) empty) (pack (rest lon)))]))

(define (unpack-num lop)
  (cond
    [(empty? lop) empty]
    [else
      (cons
        (poker-number (first lop)) (unpack-num (rest lop)))]))

(define (resolution-one lopoker) 
  (cond
    [(empty? lopoker) empty]        ; empty list
    [else
     (cond
       [(empty? (rest lopoker))    ; only one poker card in the list
        (cond
          [(= final (poker-number (first lopoker)))    (poker-path (first lopoker))]
          [else                                        empty])]
       [else                       ; have not been reduced to one poker card
        (resolution-one (reduce lopoker))])]))

(define (reduce lop)
  ; There must be 2 itmes in lop at leaset
  empty)


;; (define (first-and-others lop)
;;   (cond
;;     [(empty? lop)   empty]
;;     [(empty? (rest lop))        (error "Only one number in list!")]
;;     [else
;;       (list
;;         (cons
;;           (make-poker (+ (poker-number (first lop)) (poker-number (first(rest lop))))
;;           (list '+ (poker-number (first lop)) (poker-number (first(rest lop))))))
;;         (rest (rest lop)))]
;; 
;;  
;;     (cons
;;       (make-poker (* (poker-number (first lop)) (poker-number (first(rest lop))))
;;                   (list '* (poker-number (first lop)) (poker-number (first(rest lop)))))
;;       (rest (rest lop)))))
;; 
;; (define (each-others lop)
;;   (cond
;;     [(empty? (rest (rest lop))      (first-and-others lop))]
;;     [else
;;       (append (first-and-others lop) (each-others (cons (first lop) (rest lop))))]))
;; 
;;(define (reduce node)
;;  (cond
;;    [(empty? node) empty]
;;    [(is-leaf? node) node]
;;    [else (each-other node)]))
;;
;;(define (each-other node)
;;  (local(
;;         (define (each-other0 node leader path)
;;           (cond
;;             [(empty? (node-lon node)) empty]
;;             [else
;;              (first-others node leader path)])))
;;    (each-other0 node empty (node-path node))))
;;
;;(define (first-others node leader path)
;;  (cond
;;    [(empty? (node-lon node)) empty]
;;    [else
;;     (append
;;      (one-others (first (node-lon node)) (make-node (rest (node-lon node)) path)  leader path)
;;      (first-others (make-node (rest (node-lon node)) path) (append leader (cons (first (node-lon node)) empty)) path))]))
;;
(define (each-other lop)
  (local(
         (define (each-other0 lop leader acc)
           (cond 
             [(empty? lop) empty]
             [(empty? (rest lop)) empty]
             ;[(empty? (rest (rest lop)))
             ; (first lop leader)]
             [else 
              (append acc (first-others lop leader)
                      (each-other0 (rest lop) (cons (first lop) leader) acc))])))
    (each-other0 lop empty empty)))

(define (first-others lop leader)
  (cond
    [(empty? lop) leader]
    [else
      (one-others (first lop) (rest lop) empty)]))

(define (one-others p lop leader)
  (cond
    [(empty? lop) empty]
    [else
     (cons
      (append leader
              (cons
               (make-poker (+ (poker-number p) (poker-number (first lop)))
                           (list '+ (poker-number p) (poker-number (first lop))))
               (rest lop)))
      (one-others p (rest lop) (append leader (list (first lop)))))]))

(define testx (each-other (pack (list 1 2 3 4))))

(define test1 (game24 (list 2 3 10 9)))
(define test2 (game24 (list 2 3 10 8)))
(define test3 (game24 (list )))

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


;(display test1)
;(newline)
;(display test2)
;(newline)
;(display test3)
;(newline)
(define (show lolop)
  (cond
    [(empty? lolop) empty]
    [else
      (
      (display (first lolop))
      (newline)
      (show (rest lolop)))]))

(show testx)
