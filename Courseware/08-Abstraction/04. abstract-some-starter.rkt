
;; abstract-some-starter.rkt

; 
; PROBLEM:
; 
; Design an abstract function called some? (including signature, purpose, 
; and tests) to simplify the following two functions. When you are done
; rewrite the original functions to use your new some? function.
; 


; ;; ListOfNumber -> Boolean
; ;; produce true if some number in lon is positive
; (check-expect (some-positive? empty) false)
; (check-expect (some-positive? (list 2 -3 -4)) true)
; (check-expect (some-positive? (list -2 -3 -4)) false)
; 
; (define (some-positive? lon)
;   (cond [(empty? lon) false]
;         [else
;          (or (positive? (first lon))
;              (some-positive? (rest lon)))]))
; 
; 
; ;; ListOfNumber -> Boolean
; ;; produce true if some number in lon is negative
; (check-expect (some-negative? empty) false)
; (check-expect (some-negative? (list 2 3 -4)) true)
; (check-expect (some-negative? (list 2 3 4)) false)
; 
; (define (some-negative? lon)
;   (cond [(empty? lon) false]
;         [else
;          (or (negative? (first lon))
;              (some-negative? (rest lon)))]))


;; (X -> Boolean) (listof X) -> Boolean
;; produce true if an element in lox produces true for pred? 
(check-expect (some? positive? empty) false)
(check-expect (some? positive? (list 2 -3 -4)) true)
(check-expect (some? negative? (list 2 3 4)) false)

(define (some? pred lox)
  (cond [(empty? lox) false]
        [else
         (or (pred (first lox))
             (some? pred (rest lox)))]))


;; (listof Number) -> Boolean
;; produce true if some number in lon is positive
(check-expect (some-positive? empty) false)
(check-expect (some-positive? (list 2 -3 -4)) true)
(check-expect (some-positive? (list -2 -3 -4)) false)

(define (some-positive? lon) (some? positive? lon))

;; (listof Number) -> Boolean
;; produce true if some number in lon is negative
(check-expect (some-negative? empty) false)
(check-expect (some-negative? (list 2 3 -4)) true)
(check-expect (some-negative? (list 2 3 4)) false)

(define (some-negative? lon) (some? negative? lon))
