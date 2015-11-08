(require 2htdp/image)

;; wide-only-starter.rkt

; 
; PROBLEM:
; 
; Use the built in version of filter to design a function called wide-only 
; that consumes a list of images and produces a list containing only those 
; images that are wider than they are tall.
; 



;; (listof Image) -> (listof Image)
;; produce a list containing only images for which wide? produces true

(check-expect (wide-only empty) empty)
(check-expect (wide-only (list (rectangle 40 20 "outline" "black")
                               (rectangle 20 40 "outline" "black")))
              (list (rectangle 40 20 "outline" "black")))

(define (wide-only loi)
  (filter wide? loi))

;; Image -> Boolean
;; produce true if image-width is > image-height
(check-expect (wide? (rectangle 10 20 "solid" "blue")) false)
(check-expect (wide? (rectangle 20 20 "solid" "blue")) false)
(check-expect (wide? (rectangle 30 20 "solid" "blue")) true)

(define (wide? img)
  (> (image-width img) (image-height img)))
