;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname game24) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
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
