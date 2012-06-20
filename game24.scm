;; The first three lines of this file were inserted by DrScheme. They record metadata
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
      (make-poker (first lon) (first lon)) (pack (rest lon)))]))

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

(define (op-merge-plus p1 p2)
  (make-poker (+ (poker-number p1) (poker-number p2))
              (list '+ (poker-path p1) (poker-path p2))))



; (define (each-other lop)
;   (local(
;          (define (each-other0 lop leader acc)
;            (cond 
;              [(empty? lop) empty]
;              [(empty? (rest lop)) empty]
;              ;[(empty? (rest (rest lop)))
;              ; (first lop leader)]
;              [else 
;               (append acc (first-others lop leader)
;                       (each-other0 (rest lop) (cons (first lop) leader) acc))])))
;     (each-other0 lop empty empty)))
; 
(define (first-others0 lop leader)
  (cond
    [(empty? lop) empty]
    [(empty? (rest lop)) empty]
    [else
     (append 
      (append leader (list (op-merge-plus (first lop) (first (rest lop)))) (rest (rest lop)))
      (first-others0 (rest lop) (append leader (list (first lop)))))])) 

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

(define testx (first-others0 (pack (list 1 2 3)) empty ))

(define test1 (game24 (list 2 3 10 9)))
(define test2 (game24 (list 2 3 10 8)))
(define test3 (game24 (list )))




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

(display testx)
;(show testx)
;(op-merge-plus (make-poker 6 6) (make-poker 7 7))
