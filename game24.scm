;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname game24) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define (is24? number)
  (cond
    [(= number 24) true ]
    [else false]
    ))

(define (have-a-sum-is-24? number lon)
  (cond
    [(empty lon) false]
    [else (cond
            [(empty (rest lon)) false]
            [else (cond
                    [(is24? (+ number (first lon) )) true]
                    [else (have-a-sum-is-24? number (rest lon))])])]))
                   
          