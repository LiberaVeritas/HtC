(require 2htdp/image)
;; SPD2-Design-Quiz-1.rkt


;; ======================================================================
;; Constants
(define COOKIES .)

(define BLANK (circle 0 "solid" "white"))

;; ======================================================================
;; Data Definitions

;; Natural is one of:
;;  - 0
;;  - (add1 Natural)
;; interp. a natural number
(define N0 0)         ;0
(define N1 (add1 N0)) ;1
(define N2 (add1 N1)) ;2

#;
(define (fn-for-natural n)
  (cond [(zero? n) (...)]
        [else
         (... n   ; n is added because it's often useful                   
              (fn-for-natural (sub1 n)))]))

;; Template rules used:
;;  - one-of: two cases
;;  - atomic distinct: 0
;;  - compound: 2 fields
;;  - self-reference: (sub1 n) is Natural




; PROBLEM 1:
; 
; Complete the design of a function called pyramid that takes a natural
; number n and an image, and constructs an n-tall, n-wide pyramid of
; copies of that image.
; 
; For instance, a 3-wide pyramid of cookies would look like this:
; 
; .


;; Natural Image -> Image
;; produce an n-wide pyramid of the given image
(check-expect (pyramid 0 COOKIES) empty-image)
(check-expect (pyramid 1 COOKIES) COOKIES)
(check-expect (pyramid 3 COOKIES)
              (above COOKIES
                     (beside COOKIES COOKIES)
                     (beside COOKIES COOKIES COOKIES)))
#;
(define (pyramid n i) empty-image) ; stub

;; template copied

(define (pyramid n i)
  (cond [(zero? n) BLANK]
        [else (above (pyramid (sub1 n) i)
                     (row n i))]))
                     



;; Natural Image -> Image
;; produces an n-wide row of the image

(check-expect (row N0 COOKIES) BLANK)
(check-expect (row N1 COOKIES) COOKIES)
(check-expect (row N2 COOKIES) (beside COOKIES COOKIES))

;; template copied

(define (row n i)
  (cond [(zero? n) BLANK]
        [else
         (beside i   ; n is added because it's often useful                   
                 (row (sub1 n) i))]))




; Problem 2:
; Consider a test tube filled with solid blobs and bubbles.  Over time the
; solids sink to the bottom of the test tube, and as a consequence the bubbles
; percolate to the top.  Let's capture this idea in BSL.
; 
; Complete the design of a function that takes a list of blobs and sinks each
; solid blob by one. It's okay to assume that a solid blob sinks past any
; neighbor just below it.
; 
; To assist you, we supply the relevant data definitions.


;; Blob is one of:
;; - "solid"
;; - "bubble"
;; interp.  a gelatinous blob, either a solid or a bubble
;; Examples are redundant for enumerations

;; for testing
(define BS  "solid")
(define BB "bubble")

#;
(define (fn-for-blob b)
  (cond [(string=? b "solid") (...)]
        [(string=? b "bubble") (...)]))

;; Template rules used:
;; - one-of: 2 cases
;; - atomic distinct: "solid"
;; - atomic distinct: "bubble"


;; ListOfBlob is one of:
;; - empty
;; - (cons Blob ListOfBlob)
;; interp. a sequence of blobs in a test tube, listed from top to bottom.
(define LOB0 empty) ; empty test tube
(define LOB2 (cons "solid" (cons "bubble" empty))) ; solid blob above a bubble

#;
(define (fn-for-lob lob)
  (cond [(empty? lob) (...)]
        [else
         (... (fn-for-blob (first lob))
              (fn-for-lob (rest lob)))]))

;; Template rules used
;; - one-of: 2 cases
;; - atomic distinct: empty
;; - compound: 2 fields
;; - reference: (first lob) is Blob
;; - self-reference: (rest lob) is ListOfBlob

;; ListOfBlob -> ListOfBlob
;; produce a list of blobs that sinks the given solid blobs by one

(check-expect (sink empty) empty)
(check-expect (sink (cons "bubble" (cons "solid" (cons "bubble" empty))))
              (cons "bubble" (cons "bubble" (cons "solid" empty))))
(check-expect (sink (cons "solid" (cons "solid" (cons "bubble" empty))))
              (cons "bubble" (cons "solid" (cons "solid" empty))))
(check-expect (sink (cons "solid" (cons "bubble" (cons "bubble" empty))))
              (cons "bubble" (cons "solid" (cons "bubble" empty))))
(check-expect (sink (cons "solid" (cons "bubble" (cons "solid" empty))))
              (cons "bubble" (cons "solid" (cons "solid" empty))))
(check-expect (sink (cons "bubble" (cons "solid" (cons "solid" empty))))
              (cons "bubble" (cons "solid" (cons "solid" empty))))
(check-expect (sink (cons "solid"
                          (cons "solid"
                                (cons "bubble" (cons "bubble" empty)))))
              (cons "bubble" (cons "solid" 
                                   (cons "solid" (cons "bubble" empty)))))
#;
(define (sink lob) empty) ; stub


;; template copied

(define (sink lob)
  (cond [(empty? lob) empty]
        [(bubble? (first lob))
         (cons (first lob) (sink (rest lob)))]
        [else (if (empty? (next-non-solid (rest lob)))
                  lob
                  (rebuild (count-solids lob) (next-non-solid lob)))]))



;; Natural ListOfBlob -> ListOfBlob
;; inserts n solids after the first of given list

(define (rebuild n lob)
  (cons (first lob) (prepend n (rest lob))))


(define (solid? b)
  (string=? b "solid"))

(define (bubble? b)
  (string=? b "bubble"))


;; ListOfBlob -> Natural
;; count the nubmer of solids in a row
(check-expect (count-solids empty) N0)
(check-expect (count-solids (cons BS empty)) N1)
(check-expect (count-solids (cons BB empty)) N0)
(check-expect (count-solids (cons BS (cons BB empty))) N1)
(check-expect (count-solids (cons BS (cons BS empty))) N2)
(check-expect (count-solids (cons BS (cons BS (cons BB empty)))) N2)
(check-expect (count-solids (cons BS (cons BB (cons BS empty)))) N1)

;; template copied

(define (count-solids lob)
  (cond [(empty? lob) N0]
        [(bubble? (first lob)) N0]
        [else (add1 (count-solids (rest lob)))]))


;; ListOfBlob -> ListOfBlob
;; return the list from the next non-solid

(define NN1 (cons BB empty))
(define NN2 (cons BS empty))
(define NN3 (cons BS (cons BS (cons BB (cons BS empty)))))

(check-expect (next-non-solid empty) empty)
(check-expect (next-non-solid NN1) NN1)
(check-expect (next-non-solid NN2) empty)
(check-expect (next-non-solid NN3) (cons BB (cons BS empty)))

(define (next-non-solid lob)
  (cond [(empty? lob) empty]
        [else (if (bubble?  (first lob))
                  lob
                  (next-non-solid (rest lob)))]))


;; Natural ListOfBlob -> ListOfBlob
;; prepend given number of solids to list

(check-expect (prepend N0 empty) empty)
(check-expect (prepend N2 (cons BB (cons BB empty)))
              (cons BS (cons BS (cons BB (cons BB empty)))))

(define (prepend n lob)
  (cond [(zero? n) lob]
        [else (cons BS
                    (prepend (sub1 n) lob))]))
