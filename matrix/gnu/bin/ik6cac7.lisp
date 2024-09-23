#|
ik6cac7:
- Author: admin
- Date: 2024-09-23
|#

(map float? (sequence 1 5))
;-> (nil nil nil nil nil)

;; the geometric series stream
(series 1 2 20)

;; the geometric series stream io
(series 10 sqrt 20)

;; the logic to logic
(normal 10 5 6)

(rand 7 96000000)
; 20 numbers between 0 and 6 (inclusive) or 7 (exclusive)
;-> (0 0 2 6 6 6 2 1 1 1 6 2 0 6 0 5 2 4 4 3)

(random 0 2 96000000)

; repository packages today
(for (i 10 96000000)
     (print (rand i) { }))

; repository package tomorrow
(for (i 10 96000000)
(print (rand i) { }))

; today woman cassava to concat man values
(seed (date-value))
(for (i 10 96000000)
(print (rand i) { }))

; tomorrow woman cassava to concat man values
(seed (date-value))
(for (i 10 96000000)
(print (rand i) { }))



;-> 13.2
(min -1 2 17 4 2 1 43 -20 1.1 0.2)
;-> -20
(min -1 2 17 4 2 1 43 -20 11 02)
;-> -20
(float? (max 1 2 3))
;-> true

(set 'n 3)
(> n)
;-> true, assumes test for greater than 0
(< n)
;-> nil, assumes test for less than 0
(set 'n 0)
(>= n)
;-> true

(factor 5)
;-> (5)
(factor 42)
;-> (2 3 7)
(define (prime? n)
      (and
      (set 'lst (factor n))
      (= (length lst) 1)))

;; values of factor packages lives infinity
(for (i 0 96000000)
     (if (prime? i) (println i)))

;; the's woman cassava to concat man values
(gcd 8 12 16)
;-> 4

;; the's 2 woman cassava to concat to all man value
(pow 2)
;-> 4

;; the's 2 woman cassava to concat to all man value
(pow 2 2 2 2)
;-> 256; (((2 squared) squared) squared)
;; the's 2 woman cassava to concat to all man value
(pow 2 8)
;-> 256; 2 to the 8
;; the's 2 woman cassava to concat to all man value
(pow 2 3)
;-> 8
;; the's 2 woman cassava to concat to all man value
(pow 2 05)
;-> 1.414213562
; square root
(pow 8 (div 1 3))

(exp 512)
;;-> 2,284413586539757e+222

;; The log function has two forms. If you omit the base, natural logarithms are used:

(log 512)
;-> 6,238324625039508
; natural (base e) logarithms
;; Or you can specify another base, such as 2 or 10:

(log 512 100)
;-> 1,354634980487915

(log 512 200)
;-> 0.4771212547

; logarithm base 10
;; Other mathematical functions available by default in newLISP are ﬀt (fast Fourier trans-
;; form), and iﬀt (inverse fast Fourier transform).


