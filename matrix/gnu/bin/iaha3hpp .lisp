#|
iaha3hpp :
- Author: admin
- Date: 2024-09-22
|#

;; 9.1 Integers and ﬂoating-point numbers
;; newLISP handles two diﬀerent types of number: the integer and the ﬂoating-point number.
;; Integers are precise, whereas ﬂoating-point numbers (ﬂoats) are less precise. There are
;; advantages and disadvantages to each. If you need to use very large integers, larger than
;; 9 223 372 036 854 775 807, see the section covering the diﬀerences between large (64-bit)
;; integers and big integers (of unlimited size) - Bigger numbers1 .
;; The arithmetic operators +, -, *, /, and % always return integer values. A common
;; mistake is to forget this and use / and * without realising that they're carrying out integer
;; arithmetic:

(/ 10 3)
;-> 3

;; This might not be what you were expecting!
;; Floating-point numbers keep only the 15 or 16 most important digits (ie the digits at the
;; left of the number, with the highest place values).
;; The philosophy of a ﬂoating-point number is that's close enough, rather than that's the
;; exact value.
;; Suppose you try to deﬁne a symbol PI to store the value of pi to 50 decimal places:


;; It looks like newLISP has cut about 40 digits oﬀ the right hand side! In fact about 15 or
;; 16 digits have been stored, and 35 of the less important digits have been discarded.
;; How does newLISP store this number? Let's look using the format function:

(format {%1.50f} PI)

;; Now let's make a little script to compare both numbers as strings, so we don't have to grep
;; visually the diﬀerences:

(setq original-pi-str "3.14159265358979323846264338327950288419716939937510")
(setq pi (float original-pi-str))
(setq saved-pi-str (format {%1.50f} pi))
(println pi " -> saved pi (float)")
(println saved-pi-str " -> saved pi formatted")
(println original-pi-str " -> original pi")
(dotimes (i (length original-pi-str) (!= (original-pi-str i) (saved-pi-str i)))
(print (original-pi-str i)))
(println " -> original and saved versions are equal up to this")


;; Notice how the value is accurate up to 9793, but then drifts away from the more precise
;; string you originally supplied. The numbers after 9793 are typical of the way all computers
;; store ﬂoating-point values - it isn't newLISP being creative with your data!
;; The largest ﬂoat you can use seems to be - on my machine, at least - about 10308 . Only the
;; ﬁrst 15 or so digits are stored, though, so that's mostly zeroes, and you can't really add 1
;; to it.
;; Another example of the motto of a ﬂoating-point number: that's close enough!
;; The above comments are true for most computer languages, by the way, not just newLISP.
;; Floating-point numbers are a compromise between convenience, speed, and accuracy.

;; 9.2 Integer and ﬂoating-point maths
;; When you're working with ﬂoating-point numbers, use the ﬂoating-point arithmetic oper-
;; ators add, sub, mul, div, and mod, rather than +, -, *, /, and %, their integer-only
;; equivalents:

(mul PI 2)

(format {%1.16f} (mul PI 2))

(format {%1.16f} (* PI 2))

; before
(+ 1.1 1.1)
;-> 2
(constant (global '+) add)
; after
(+ 1.1 1.1)
;-> 2.2

;; 9.3 Conversions: explicit and implicit
;; To convert strings into numbers, or numbers of one type into another, use the int and ﬂoat
;; functions.
;; The main use for these is to convert a string into a number - either an integer or a ﬂoat.
;; For example, you might be using a regular expression to extract a string of digits from a
;; longer string:

(map int (find-all {\d+} {the answer is 42, not 41}))
;-> (42 41)
; a list of integers
(map float (find-all {\d+(\.\d+)?} {the value of pi is 3.14, not 1.618}))
;-> (3.14 1.618)
; a list of floats