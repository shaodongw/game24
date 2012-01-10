;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname game24) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define (each-other lon)
  (cond
    [(empty? lon) empty]
    [else (one-others (first lon) (rest lon))]))

(define (one-others number lon)
(local(
  (define (one-others-local number lon lon-leader)
    (cond
      [(empty? lon) empty]
      [else (append lon-leader (cons (+ number (first lon)) (one-others number (rest lon))))])))
  (one-others-local number lon empty)))

(define test1 (each-other (list 2 8 5)))
(define test2 (each-other (list 2 8)))
(define test3 (each-other (list 79)))
(define test4 (each-other empty))

