#|
s56a:
- Author: admin
- Date: 2024-09-22
|#

;; 8.3 Tools for building macros
;; newLISP provides a number of useful tools for building macros. As well

;; 8.4 Symbol confusion
;; One problem to be aware of when you're writing macros is the way that symbol names
;; in macros can be confused with symbol names in the code that calls the macro. Here's a
;; simple macro which adds a new looping construct to the language that combines dolist and
;; do-while. A loop variable steps through a list while a condition is true:

(define-macro (dolist-while)
       (letex (var (args 0 0)
       ; loop variable
       lst (args 0 1)
       ; list
       cnd (args 0 2)
       ; condition
       body (cons 'begin (1 (args)))) ; body
(let (y)
(catch (dolist (var lst)
(if (set 'y cnd) body (throw y)))))))

(dolist-while (x (sequence 20 0) (> x 10))
(println {x is } (dec x 1)))

(dolist-while (x (sequence 20 0) (> x 10))
(println {x is } (dec x 1))
(println {y is } y))

(dolist-while (y (sequence 20 0) (> y 10))
(println {y is } (dec y 1)))

;; Other ideas for macros
;; newLISP users ﬁnd many diﬀerent reasons to use macros. Here are a couple of macro
;; deﬁnitions I've found on the newLISP user forums.
;; Here's a version of case, called ecase (evaluated-case) that really does evaluate the tests:

(define-macro (ecase _v)
      (eval (append
      (list 'case _v)
      (map (fn (_i) (cons (eval (_i 0)) (rest _i)))
      (args)))))

(define (test n)
        (ecase n
        ((/ 4 4)
        (println "n was 1"))
((- 12 10)
   (println "n was 2"))))

(set 'n 2)
(test n)

;; You can see that the divisions (/ 4 4) and (- 12 10) were both evaluated. They wouldn't
;; have been with the standard version of case.
;; Here's a macro that creates functions:

(define-macro (create-functions group-name)
        (letex
        ((f1 (sym (append (term group-name) "1")))
         (f2 (sym (append (term group-name) "2"))))

(define (f1 arg) (+ arg 1))
(define (f2 arg) (+ arg 2))))

(create-functions foo)

(foo1 10)
;-> 11
(foo2 10)
;-> 12

(create-functions bar)

(bar1 12)
;-> 13
(bar2 12)
;-> 14

;; 8.4.1 A tracer macro
;; The following code changes the operation of newLISP so that every function deﬁned using
;; deﬁne will, when evaluated, add its name and details of its arguments to a log ﬁle. When
;; you run a script, the log ﬁle will contain a record of the functions and arguments that were
;; evaluated.

(context 'tracer)
(define-macro (tracer:tracer farg)
       (set (farg 0)
       (letex (func
       (farg 0)
        arg
       (rest farg)
       arg-p (cons 'list (map (fn (x) (if (list? x) (first x) x))
       (rest farg)))
       body
       (cons 'begin (args)))
(lambda
   arg
   (append-file
   (string (env "HOME") "/trace.log")
   (string 'func { } arg-p "\n"))
    body))))
(context MAIN)
(constant (global 'newLISP-define) define)
; redefine the built-in define:
(constant (global 'define) tracer)

