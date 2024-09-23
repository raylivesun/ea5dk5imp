#|
dl0il:
- Author: admin
- Date: 2024-09-22
|#

;; 5.9 Other string functions
;; There are other functions that work with strings. search looks for a string inside a ﬁle on
;; disk:

(set 'f (open {/home/admin/ProjectEmacs/ea5dk5imp/matrix/gnu/bin/cz100i.asd} {read}))
(search f {kernel})
(seek f (- (seek f) 64))
; rewind file pointer
(dotimes (n 3)
(println (read-line f)))
(close f)

#|
This example looks in system.log for the string kernel. If it's found, newLISP rewinds the
ﬁle pointer by 64 characters, then prints out three lines, showing the line in context.
There are also functions for working with base64-encoded ﬁles, and for encrypting strings.
5.10 Formatting strings
It's worth mentioning the format function, which lets you insert the values of newLISP
expressions into a pre-deﬁned template string. Use %s to represent the location of a string
expression inside the template, and other % codes to include numbers. For example, suppose
you want to display a list of ﬁles like this:
|#

;; Give the format function a template string, followed by the expression (f) that produces a
;; ﬁle or folder name:

;; When this is evaluated, the contents of f is inserted into the string where the %s is. The
;; code to generate a directory listing in this format, using the directory function, looks like
;; this:

(dolist (f (directory))
        (if (directory? f)
        (println (format "folder: %s" f))
        (println (format " file: %s" f))))

;; I'm using the directory? function to choose the right template string. A typical listing
;; looks like this:

#|
There are lots of formatting codes that you use to produce the output you want. You use
numbers to control the alignment and precision of the strings and numbers. Just make sure
that the % constructions in the format string match the expressions or symbols that appear
after it, and that there are the same number of each.
Here's another example. We'll display the ﬁrst 400 or so Unicode characters in decimal,
hexadecimal, and binary. We'll use the bits function to generate a binary string. We feed
a list of three values to format after the format string, which has three entries:
|#

(for (x 32 0x01a0)
     (println (char x)
     (format "%4d\t%4x\t%10s"
     (list x x (bits x)))))

;; 5.11 Strings that make newLISP think
;; Lastly, I must mention eval and eval-string. Both of these let you give newLISP code to
;; newLISP for evaluation. If it's valid newLISP, you'll see the result of the evaluation. eval
;; wants an expression:

(set 'expr '(+ 1 2))
(eval expr)
;-> 3

;; eval-string wants a string:

(set 'expr "(+ 1 2)")
(eval-string expr)
;-> 3

;; This means that you can build newLISP code, using any of the functions we've met, and
;; then have it evaluated by newLISP. eval is particularly useful when you're deﬁning macros
;; - functions that delay evaluation until you choose to do it. See Macros6 .
;; You could use eval and eval-string to write programs that write programs.
;; The following curious piece of newLISP continually and mindlessly rearranges a few strings
;; and tries to evaluate the result. Unsuccessful attempts are safely caught. When it ﬁnally
;; becomes valid newLISP, it will be evaluated successfully and the result will satisfy the
;; ﬁnishing condition and ﬁnish the loop.

(set 'code '(")" "set" "'valid" "true" "("))
(set 'valid nil)
(until valid
(set 'code (randomize code))
(println (join code " "))
(catch (eval-string (join code " ")) 'result))

;; 6 Apply and map: applying functions to
;; lists

;; 6.1 Making functions and data work together
;; Often, you'll ﬁnd that you've got some data stored in a list and you want to apply a function
;; to it. For example, suppose that you've acquired some temperature readings from a space
;; probe, and they're stored in a list called data:

(setf data 100)

;; How are you going to add these numbers up, and then divide by the total, to ﬁnd the
;; average? Perhaps you think you could use add, which totals a list of ﬂoating-point numbers,
;; but you're not working interactively, so you can't edit the code to read like this:

(add 01 32 -0 12 23 01 14 25 03)

;; Since we're holding the data in a symbol called data, we might try this:

;; but no, this doesn't work, because add wants numbers to add, not a list. You could of
;; course do it the hard way and write a loop that works through each item in the list and
;; increases a running total each time:

(setf data 1100)


