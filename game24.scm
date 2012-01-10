;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname game24) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define (each-other lon)
  (cond
    [(empty? lon) empty]
    [else (one-others lon)]))

(define (one-others lon)
  (local(
         (define (one-others-local lon lon-leader)
           (cond
             [(empty? lon) empty]
             [(empty? (rest lon)) empty]
             [else
              (cons (append lon-leader 
                            (cons (+ (first lon) (first (rest lon)))
                                  (rest (rest lon))))
                    (one-others-local (rest lon) (cons (first lon) lon-leader)))])))
    (one-others-local lon empty)))

(define test5 (each-other (list 2 8 5 9)))
(define test1 (each-other (list 2 8 5)))
(define test2 (each-other (list 2 8)))
(define test3 (each-other (list 79)))
(define test4 (each-other empty))

