;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname game24) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define (is24? number)
  (cond
    [(= number 24) true ]
    [else false]
    ))

(define (have-a-sum-is-24? number lon)
  (cond
    [(empty? lon) false]
    [else (cond
            [(is24? (+ number (first lon) )) true]
            [else (have-a-sum-is-24? number (rest lon))])]))

(define (is-the-first-number-and-others-equal-to-24? lon)
  (cond
    [(empty? lon) false]
    [else (have-a-sum-is-24? (first lon) (rest lon))]))

(define (is-their-a-pair-their-sum-equal-to-24? lon)
  (cond
    [(empty? lon) false]
    [else
     (cond
       [(is-the-first-number-and-others-equal-to-24? lon) true]
       [else (is-their-a-pair-their-sum-equal-to-24? (rest lon))])]))


(define test-1 (is-their-a-pair-their-sum-equal-to-24? (list 2 8 5 3)))
(define test-2 (is-their-a-pair-their-sum-equal-to-24? (list 3 3 20 4)))
(define test-3 (is-their-a-pair-their-sum-equal-to-24? (list)))


