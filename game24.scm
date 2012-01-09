;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname game24) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define (each-other lon)
  (cond
    [(empty? lon) empty]
    [else (one-others (first lon) (rest lon))]))

(define (one-others number lon)
  (cond
    [(empty? lon) number]
    [else (cons (+ number (first lon)) (rest lon))]))


(define test1 (each-other (list 2 8 5 3)))
(define test2 (each-other (list 3 3 20 4)))
(define test3 (each-other (list 79)))
(define test4 (each-other empty))

