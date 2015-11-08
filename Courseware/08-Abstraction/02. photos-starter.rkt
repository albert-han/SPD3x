
;; photos-starter.rkt

;; =================
;; Data definitions:

(define-struct photo (location album favourite))
;; Photo is (make-photo String String Boolean)
;; interp. a photo having a location, belonging to an album and having a
;; favourite status (true if photo is a favourite, false otherwise)
(define PHT1 (make-photo "photos/2012/june" "Victoria" true))
(define PHT2 (make-photo "photos/2013/birthday" "Birthday" true))
(define PHT3 (make-photo "photos/2012/august" "Seattle" true))
(define PHT4 (make-photo "photos/2013/birthday" "Birthday" false))

(define LOP1 (list PHT1 PHT2 PHT3 PHT4))

;; =================
;; Functions:

; 
; PROBLEM:
; 
; Design a function called to-frame that consumes an album name and a list of photos 
; and produces a list of only those photos that are favourites and that belong to 
; the given album. You must use built-in abstract functions wherever possible. 
; 


;; String (listof Photo) -> (listof Photo)
;; produce list of photos that are favorites and that belong to a given album
(check-expect (to-frame "Birthday" empty) empty)
(check-expect (to-frame "Birthday" LOP1) (list PHT2))

(define (to-frame str lop)
  (local [(define (album? pht) (string=? (photo-album pht) str))]
  (filter favorite? (filter album? lop))))

;; Photo -> Boolean
;; produce true if favorite
(define (favorite? pht)
  (photo-favourite pht))


(check-expect (to-frame "Family" empty) empty)
(define (to-frame2 str lop)
  (local [(define (chosen? pht) (and (string=? (photo-album pht) str) (photo-favourite pht)))]
    (filter chosen? lop)))
