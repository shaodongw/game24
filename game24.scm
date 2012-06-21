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
      (make-poker (first lon) (first lon)) (pack (rest lon)))]))

(define (unpack-num lop)
  (cond
    [(empty? lop) empty]
    [else
     (cons
      (poker-number (first lop)) (unpack-num (rest lop)))]))

(define (resolution-one lop) 
  (cond
    [(empty? lop) empty]        ; empty list
    [(and (empty? (rest lop)) (= final (poker-number (first lop))))
     ; only one poker(combined) in the list and is right the final number
     lop]
    [(and (empty? (rest lop)) (not (= final (poker-number (first lop)))))
     ; only one poker(combined) in the list but is not the final number
     empty]
    [else
     ; there are more poker(combined) in the list, so merge one and the others...
     (resolution-all (each-other lop))]))


; (define (reduce lop)
;   ; There must be 2 itmes in lop at leaset
;   (cond
;     [(empty? lop) empty]
;     [else
;       (each-other lop)]))
(define (reduce lop)
  empty)

(define (op-merge-plus p1 p2)
  (make-poker (+ (poker-number p1) (poker-number p2))
              (list (poker-path p1) '+ (poker-path p2))))
(define (op-merge-mul p1 p2)
  (make-poker (* (poker-number p1) (poker-number p2))
              (list (poker-path p1) '* (poker-path p2))))
(define (op-merge-sub p1 p2)
  (make-poker (- (poker-number p1) (poker-number p2))
              (list (poker-path p1) '- (poker-path p2))))
(define (op-merge-rev-sub p1 p2)
  (make-poker (- (poker-number p2) (poker-number p1))
              (list (poker-path p2) '- (poker-path p1))))
(define (op-merge-div p1 p2)
  (make-poker (/ (poker-number p1) (poker-number p2))
              (list (poker-path p1) '/ (poker-path p2))))
(define (op-merge-rev-div p1 p2)
  (make-poker (/ (poker-number p2) (poker-number p1))
              (list (poker-path p2) '/ (poker-path p1))))

(define (each-other lop)
  (local(
         (define (each-other0 lop leader)
           (cond 
             [(empty? lop) empty]
             ;[(empty? (rest lop)) empty]
             ;[(empty? (rest (rest lop)))
             ; (first lop leader)]
             [else 
              (append (first-others lop leader)
                      (each-other0 (rest lop) (append leader (list (first lop)))))])))
    (each-other0 lop empty)))


(define (first-others lop prefix)
  (cond
    [(empty? lop) empty]
    ;[(empty? (rest lop)) empty]
    [else
     (add-prefix
      (one-to-list (first lop) (rest lop))
      prefix)]))

(define (add-prefix lolop prefix)
  (cond
    [(empty? lolop) empty]
    [else
     (cons
      (append prefix (first lolop))
      (add-prefix (rest lolop) prefix))]))

(define (one-to-list p lop)
  (local(
         (define (one-to-list0 p lop leader)
           (cond
             [(empty? lop) empty]
             [else
              (append
               (list
                (append leader
                        (list (op-merge-plus p (first lop)))
                        (rest lop)))
               (list
                (append leader
                        (list (op-merge-mul p (first lop)))
                        (rest lop)))
               (list
                (append leader
                        (list (op-merge-sub p (first lop)))
                        (rest lop)))
               (list
                (append leader
                        (list (op-merge-rev-sub p (first lop)))
                        (rest lop)))
               (cond
                 [(= 0 (poker-number (first lop))) empty]
                 [else
                    (list
                        (append leader
                                (list (op-merge-div p (first lop)))
                                (rest lop)))])
               (cond
                 [(= 0 (poker-number p)) empty]
                 [else
                    (list
                        (append leader
                                (list (op-merge-rev-div p (first lop)))
                                (rest lop)))])
               
               (one-to-list0 p (rest lop) (append leader (list (first lop)))))])))
    (one-to-list0 p lop empty)))

(define testx (game24 (list 1 5 5 5)))
;(define testx (resolution-all (list (pack (list 1 2 3 4)))))
;(define testx (resolution-one (list (make-poker 25 25) )))
;(define testx (resolution-one (list (make-poker 10 10) (make-poker 14 14))))
;(define testx (each-other (pack (list 1 2 3 4)) ))
;(define testx (first-others (pack (list 1 20 300 4000)) empty))
;(define testx (add-prefix (one-to-list (make-poker 10 10) (pack (list 1 2 3 4))) (pack (list 1 2 3))))
;(define testx (one-to-list (make-poker 10 10) (pack (list 1 2 3 4))))

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
     (begin
       (display (first lolop))
       (newline)
       (show (rest lolop)))]))

;(display testx)
(show testx)
;testx
;(op-merge-plus (make-poker 6 6) (make-poker 7 7))
