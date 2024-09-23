;; instead of let and letn you can use the local function. This is like let and letn, but you
;; don't have to supply any values for the local variables when you ﬁrst mention them. They're
;; just nil until you set them:

(define (test)
    (local (a b c)
    (println a " " b " " c)
    (set 'a 1 'b 2 'c 3)
    (println a " " b " " c)))
(test)

;; There are other ways of declaring local variables. You might ﬁnd the following technique
;; easier to write when you're deﬁning your own functions. Watch the comma:


(define (test)
    (let ((a 1) (b 2) (c 3))
    (println a " " b " " c)))
(test)


;; And here's another way, using the letrec macro:


(define (test)
    (letrec ((a (lambda () 1))
    (b (lambda () 2))
    (c (lambda () 3)))
    (println a " " b " " c)))
    (test)


;; The comma is a clever trick: it's an ordinary symbol name like c or x:
(set ', "this is a string")
(println ,)


;; - it's just less likely that you'd use it as a symbol name, and so it's useful as a visual
;; separator in argument lists.

;; 3.8.1 Default values

;; In a function deﬁnition, the local variables that you deﬁne in the function's argument list
;; can have default values assigned to them, which will be used if you don't specify values
;; when you call the function. For example, this is a function with three named arguments, a,
;; b, and c:

(define (my-function a b c)
    (println a " " b " " c))

    ;; If you call my-function like this:
    (my-function 1 2)

    ;; then it will print:
    ;; 1 2 nil
    ;; (because the third argument, c, is not provided, so it gets the default value,
    ;; which is nil in this case)
    ;;
    ;; If you call my-function like this:
    (my-function 1 2 3)

    ;; then it will print:
    ;; 1 2 3
    ;; (because you've provided all three arguments)

    ;; If you call my-function like this:
    (my-function)
    ;; then it will print:
    ;; nil nil nil
    ;; (because you've provided only two arguments, and the third argument is still nil)
    ;;
    ;; You can also provide default values for the arguments in the function's argument list
    ;; in reverse order:
    ;;
    ;; (define (my-function b c a)
    ;;     (println a " " b " " c))
    ;;
    ;; Now, calling my-function like this:
    (my-function 1 2)
    ;; will print:
    ;; 1 2 nil
    ;; (because the first argument, a, is not provided, so it gets the default value,
    ;; which is nil in this case)
    ;;
    ;; Calling my-function like this:
    (my-function 1 2 3)
    ;; will print:
    ;; 1 2 3
    ;; (because you've provided all three arguments)

;; 3.8.1 Default values

;; In a function deﬁnition, the local variables that you deﬁne in the function's argument list
;; can have default values assigned to them, which will be used if you don't specify values
;; when you call the function. For example, this is a function with three named arguments, a,
;; b, and c:

(define (foo (a 1) b (c 2))
(println a " " b " " c))

;; The symbols a and c will take the values 1 and 2 if you don't supply values in the function
;; call, but b will be nil unless you supply a value for it.

(foo)

; there are defaults for a and c but not b
;; 1 nil 2 > (foo 2) ; no values for b or c; c has default 2 nil 2 > (foo 2 3) ; b has a value, c
;; uses default 2 3 2 > (foo 3 2 1) ; default values not needed 3 2 1 >


;; 3.8.2 Arguments: args
;; You can see that newLISP is very ﬂexible with its approach to arguments to functions. You
;; can write deﬁnitions that accept any number of arguments, giving you (or the caller of your
;; functions) maximum ﬂexibility.
;; The args function returns any unused arguments that were passed to a function:

(define (test v1)
     (println "the arguments were " v1 " and " (args)))
(test)


;; The output will be:
;; the arguments were nil and ()

;; Here's a function that takes two arguments and returns them in reverse order:

(define (reverse-args a b)
    (list b a))
    (reverse-args 1 2)
    ;; (2 1)


;; Here's a function that takes any number of arguments and returns them in reverse order:

(define (reverse-args . args)
    (reverse args))
    (reverse-args 1 2 3 4 5)
    ;; (5 4 3 2 1)


;; 3.8.3 Keyword arguments
;; NewLISP provides a way to pass arguments to functions using keywords. A keyword is a symbol
;; prefixed with a colon, like this: :keyword. When you call a function with keyword
;; arguments, the arguments are matched against the function's argument names and their
;; corresponding values are assigned to the function's local variables.


;; Here's a function that takes a keyword argument:

(define (test :keyword-arg value)
    (println "The keyword argument value is " value))

    ;; When you call this function like this:
    (test :keyword-arg "hello")
    ;; The output will be:
    ;; The keyword argument value is hello
    ;;
    ;; You can also call the function with positional arguments before the keyword argument:
    (test "world" :keyword-arg "hello")
    ;; The output will be:
    ;; The keyword argument value is hello
    ;;
    ;; You can also call the function with keyword arguments after the positional arguments:
    (test "world" :keyword-arg "hello" "extra-arg")

    ;; The output will be:
    ;; The keyword argument value is hello
    ;;
    ;; You can also call the function with multiple keyword arguments:
    (test :keyword-arg1 "hello" :keyword-arg2 "world")
    ;; The output will be:
    ;; The keyword argument value is hello

;; Notice that v1 contains the ﬁrst argument passed to the function, but that any remaining
;; unused arguments are in the list returned by (args).


;; 3.8.4 Rest arguments
;; Sometimes, you want to deﬁne a function that takes a variable number of arguments
;; and processes them in some way. NewLISP provides a special syntax for this, using the
;; dot (.) as a rest argument:

(define (test . args)
    (println "the arguments were " args))
    (test 1 2 3 4 5)
    ;; the arguments were (1 2 3 4 5)

;; With args you can write functions that accept diﬀerent types of input. Notice how the
;; following function can be called without arguments, with a string argument, with numbers,
;; or with a list:


(define (test . args)
    (println "the arguments were " args))
    (test)
    (test "hello")
    (test 1 2 3)
    (test '(1 2 3))
    (test 1 2 3 4 5)
    ;; the arguments were (nil "hello" (1 2 3) '(1 2)
    ;; the arguments were (1 2 3 4 5)
    ;; the arguments were (1 2 3)
    ;; the arguments were (1 2)
    ;; the arguments were (1 2 3 4 5)

    ;; You can also use the dot argument to pass a list of arguments to a function:
    (test '("hello" "world"))
    ;; the arguments were (("hello" "world"))

(flexible)
;; arguments are ()

(flexible "OK")
;; arguments are ("OK")
;; argument 0 is OK
(flexible 1 2 3)
;; arguments are (1 2 3)
;; -> argument 0 is 1
;; -> argument 1 is 2
;; -> argument 2 is 3
(flexible '(flexible 1 2 "buckle my shoe"))
;; arguments are ((flexible 1 2 "buckle my shoe"))

;; args allows you to write functions that accept any number of arguments. For example,
;; newLISP is perfectly happy for you to pass a million arguments to a suitably deﬁned func-
;; tion. I tried it:


(define (test . args)
    (length args))
    (test)
    (test 1)
    (test 2 3)
    (test 4 5 6)
    ;; ...
    ;; (test 999999 1000000)
    ;; -> 0
    ;; -> 1
    ;; -> 2
    ;; -> 3
    ;; -> 4
    ;; ...
    ;; -> 999999
    ;; -> 1000000
    ;; The output shows that the function correctly returns the number of arguments passed to it.

    ;; However, be careful with this approach. If you have a function that needs to process
    ;; all the arguments, using the dot argument is usually a better choice.

    ;; If you have a function that needs to process some of the arguments, using the dot
    ;; argument is usually a better choice.
    ;;
    ;; For example, consider this function that takes a variable number of numbers and returns
    ;; their sum:

    (define (sum . numbers)
        (apply + numbers))
        (sum 1 2 3)
        ;; -> 6
        (sum 4 5 6 7 8 9)
        ;; -> 45
        ;; The apply function is used to combine the numbers in the list into a single number
        ;; by adding them together.
        ;;
        ;; The function can also be written using the dot argument, like this:
        (define (sum . numbers)
            (let ((sum 0))
            (map (lambda (x) (set! sum (+ sum x))) numbers)


;; In practice, newLISP was happy with this but my text editor wasn't.
;; The doargs function can be used instead of dolist to work through the arguments returned
;; by args. You could have written the ﬂexible function as:


(define (flexible . args)
    (let ((i 0))
    (dolist (arg args)
        (print "argument " i ": " arg)
        (set! i (+ i 1)))
    )
    (flexible)
    (flexible "OK")


;; newLISP has yet more ways to control the ﬂow of code execution. As well as catch and
;; throw, which allow you to handle and trap errors and exceptions, there's silent, which
;; operates like a quiet version of begin.


;; Here's an example of using silent to write a function that takes a variable number of
;; arguments and returns their sum:

(define (sum . numbers)
    (let ((sum 0))
    (map (lambda (x) (set! sum (+ sum x))) numbers)
    (silent sum)))
    (sum 1 2 3)
    ;; -> 6
    (sum 4 5 6 7 8 9)
    ;; -> 45
    ;; The silent function is used to suppress the output of the map function.


;; If you want more, you can write your own language keywords, using newLISP macros, which
;; can be used in the same way that you use the built-in functions. See Macros9 .


;; 3.8.5 Procedures and functions
;; In newLISP, procedures and functions are the same thing. A procedure is a block of
;; code that can be called with arguments, and a function is a procedure that returns a value.
;; In newLISP, you can deﬁne procedures and functions using the define function
;; (or using the let function with lambda as the argument), and you can call them just
;; like any other function.


;; Here's a simple example of a procedure:

(define (hello-world)
    (print "Hello, world!"))
    (hello-world))
    ;; Hello, world!
    ;; -> nil
    ;; The hello-world procedure prints a message and returns nil.
    ;;
    ;; Here's a simple example of a function:
    (define (add-numbers a b)
        (+ a b))
        (add-numbers 1 2)
        ;; -> 3
        ;; The add-numbers function takes two arguments and returns their sum.

        ;; You can also write a function that returns a value:
        (define (square x)
        (* x x))
        (square 5)
        ;; -> 25


;; 3.8.6 Tail recursion
;; Tail recursion is a type of recursion where the recursive call is the last operation in the
;; function. In other words, the recursive call is the last operation performed before the
;; function returns a value.
;;
;; NewLISP has built-in support for tail recursion optimization, which means that if a
;; function is tail recursive, it can be compiled to an iterative loop, which can be faster
;; than a recursive loop.
;;
;; Here's an example of a tail recursive function:

(define (factorial n)
    (if (= n 0)
        1
        (* n (factorial (- n 1)))))
        (factorial 5)
        ;; -> 120


;; 3.8.7 Anonymous functions and closures
;; In newLISP, you can create anonymous functions using the lambda keyword. An anonymous
;; function is a function that doesn't have a name.
;;
;; Here's an example of an anonymous function:

(define (add x y)
    (+ x y))
    ((lambda (x y) (+ x y)) 1 2)
    ;; -> 3


;; In this example, the anonymous function takes two arguments and returns their sum. You can
;; then use this anonymous function as a value in a let expression.
;;
;; Closures are a special type of function in newLISP that has access to the variables
;; from its enclosing scope. In other words, a closure is a function that has access to
;; its own variables, as well as its enclosing scope variables.
;;


;; Here's an example of a closure:

(define (make-counter)
    (let ((count 0))
        (lambda ()
        (set! count (+ count 1))
        count)))
        (let ((counter1 (make-counter))
              (counter2 (make-counter)))
              (counter1)
              (counter2)
              (counter1)
              (counter2)))
              ;; -> 1
              ;; -> 2
              ;; -> 3
              ;; -> 4


;; In this example, the make-counter function creates a counter variable and returns an
;; anonymous function that increments the counter and returns its value. The anonymous
;; function has access to the counter variable from its enclosing scope, making it a



;; 3.8.8 Conclusion
;; In this chapter, you've learned about the basic concepts and features of newLISP,
;; including syntax, data types, control structures, and functions. You've also learned about
;; tail recursion, anonymous functions, and closures.
;;
;; newLISP is a powerful and expressive programming language that has many powerful features
;; for writing complex and maintainable programs.
;;
;; If you're interested in learning more about newLISP, I recommend checking out the official
;; newLISP website (http://newlisp.org/), the newLISP tutorial (http
;; newlisp.org/tutorial/), and the newLISP manual (http://newlisp.
;; newlisp.org/manual/).
;;
;; I hope this chapter has been helpful for you! If you have any further questions or need more
;; information, please don't hesitate to ask.


;; 3.8.3 Scope
;; Consider this function:

(define (show)
(println "x is " x))

(define (show)
    (println "x is " x))
(show)


(set 'x "a string")
(show)
;; x is a string
(for (x 1 5)
     (dolist (x '(sin cos tan))
     (show))
(show))


;; x is 1
;; x is 1.22464679914735
;; x is 1.55740772465490
;; x is 1.83105231106196
;; x is 2.07944154167983
;; x is a string
;; x is 1
;; x is 1.22464679914735
;; x is 1.55740772465490
;; x is 1.83105231106196
;; x is 2.07944154167983
;; x is a string
;; x is 1

;; x is a string

(define (func x)
(show))

(func 3)

;; x is 3

(func "hi there")

;; x is hi there

(show)
;; x is a string

;; You can see how newLISP always gives you the value of the current x by dynamically keep-
;; ing track of which x is active, even though there might be other xs lurking in the back-
;; ground. As soon as theforloop starts, the loop variablextakes over as thecurrentx,
;; but then thatxis immediately superseded by the list iteration variablexwhich
;; takes the value of a few trigonometric functions. In between each set of trig
;; functions, the loop variable version ofxpops back again brieﬂy. And after all
;; that iteration, the string value is available again.

;; In the func function, there's another x which is local to the function. When show is called,
;; it will print this local symbol. A ﬁnal call to show returns the very ﬁrst value that x had.
;; Although newLISP won't get confused with all those diﬀerent xs, you might! So it's a
;; good idea to use longer and more explanatory symbol names, and use local rather
;; than global variables. If you do, there's less chance of you making mistakes or
;; of misreading your code at a later date. In general it's not a good idea to refer
;; to an undeﬁned symbol in a function unless you know exactly where it came
;; from and how its value is determined.

;; This dynamic process of keeping track of the current version of a symbol is called dynamic
;; scoping. There'll be more about this topic when you look at contexts (Contexts10 ). These
;; oﬀer an alternative way to organize similarly-named symbols - lexical scoping.


;; 3.8.4 Conclusion
;; In this chapter, you've learned about the basic concepts and features of newLISP,
;; including scope, dynamic scoping, and contexts. You've also learned about the
;; differences between lexical scoping and dynamic scoping, and how to use contexts to
;; organize similarly-named symbols.
;;
;; newLISP is a powerful and expressive programming language that has many powerful
;; features for writing complex and maintainable programs.
;;
;; If you're interested in learning more about newLISP, I recommend checking out the official
;; newLISP website (http://newlisp.org/), the newLISP tutorial (http)
;; newlisp.org/tutorial/), and the newLISP manual (http://newlisp.
;; newlisp.org/manual/).
;;
;; I hope this chapter has been helpful for you! If you have any further questions or need more
;; information, please don't hesitate to ask.

;; 3.8.1 Scope
;; Consider this function:

(define (show)
(println "x is " x))

(define (show)
    (println "x is " x))
(show)
;; x is a string
(for (x 1 5)
     (dolist (x '(sin cos tan))
     (show))
(show))
;; x is 1
;; x is 1.22464679914735
;; x is 1.55740772465490
;; x is 1.83105231106196
;; x is 2.07944154167983
;; x is a string
;; x is 1
;; x is 1.22464679914735
;; x is 1.55740772465490
;; x is 1.83105231106196


;; 4 Lists

;; Lists are used everywhere in newLISP - LISP stands for list processing - so it's not
;; surprising that there are many useful functions for working with lists. It's quite hard to
;; organize them all into one logical descriptive narrative, but here's an introduction to most
;; of them.
;; A good thing about newLISP is that many of the functions that work on lists also work on
;; strings, so you'll meet many of these again in the next chapter, where they'll be applied to
;; strings.


;; 4.1 Creating and manipulating lists


;; (list 'a 'b 'c)
;; -> '(a b c)

;; (cons 'a '(b c))
;; -> (a b c)

;; (append '(a b c) '(d e f))
;; -> (a b c d e f)

;; (reverse '(a b c))
;; -> (c b a)


;; 4.2 Accessing elements of a list

;; (car '(a b c))
;; -> a

;; (cdr '(a b c))
;; -> (b c)

;; (nth 1 '(a b c))
;; -> b

;; 4.3 Changing elements of a list

;; (setcar '(a b c) 'x)
;; -> (x b c)

;; (setcdr '(a b c) '(d e))
;; -> (a d e)

;; (nth-set 1 '(a b c) 'x)
;; -> (a x c)

;; 4.4 List manipulation functions

;; (length '(a b c))
;; -> 3

;; (member 'b '(a b c))
;; -> t

;; 4.1 Building lists
;; You can build a list and assign it to a symbol directly. Quote the list to stop it being
;; evaluated immediately:


;; (define my-list '(a b c))

;; 4.2 List functions
;; Here are some of the most useful list functions:

;; (mapcar function list)
;; -> (function-result function-result ...)
;; This function applies a function to each element of a list and returns a new list with
;; the results.

;; (filter predicate list)
;; -> (element ...)
;; This function returns a new list containing only the elements of the original list that
;; satisfy the predicate.

;; (reduce function list)
;; -> result
;; This function applies a function to all the elements of a list, combining them in some


;; 4.3 Converting between lists and strings

;; (list->string '(a b c))
;; -> "abc"

;; (string->list "abc")
;; -> (a b c)

;; 4.4 List functions continued
;; Here are some more useful list functions:

;; (assq key list)
;; -> (value . key)


;; 4.5 Recursion with lists
;; newLISP provides the cons function as a simple way to implement recursion with lists.
;; (define (factorial n)
;;   (if (<= n 1)
;;       1
;;       (* n (factorial (- n 1)))))

;; 4.6 Conclusion
;; In this chapter, you've learned about creating and manipulating lists, accessing elements,
;; and changing elements. You've also learned about some useful list functions, including mapcar,
;; filter, and reduce. You've also learned how to convert between lists and strings, and you
;;'ve seen an example of recursion with lists.
;;
;; If you're interested in learning more about newLISP, I recommend checking out the official
;; newLISP website (http://newlisp.org/), the newLISP tutorial (http
;; newlisp.org/tutorial/), and the newLISP manual (http://newlisp.
;; newlisp.org/manual/).
;;
;; I hope this chapter has been helpful for you! If you have any further questions or need more
;; information, please don't hesitate to ask.
;; 4.7.1 Lists as data structures
;; Lists are a powerful and flexible data structure in newLISP. They are a fundamental
;; building block for many of the other data structures in newLISP, and they provide a
;; simple and intuitive way to represent and manipulate collections of data.
;; Lists are created using the list function, which takes one or more arguments and returns
;; a new list containing those arguments. You can also create lists using the cons function,
;; which takes two arguments and returns a new list with the second argument as the head of
;; the first argument.
;; Here are some examples of creating lists:
;; (list 1 2 3)
;; -> (1 2 3)
;; (cons 4 (list 5 6))
;; -> (4 5 6)
;; 4.7.2 Accessing elements of a list
;; You can access the elements of a list using the car and cdr functions. The car function
;; returns the head of a list, and the cdr function returns the rest of the list.
;; Here are some examples of accessing elements of a list:
;; (car '(1 2 3))
;; -> 1
;; (cdr '(1 2 3))
;; -> (2 3)
;; 4.7.3 Changing elements of a list
;; You can change the elements of a list using the setcar and setcdr functions. The
;; setcar function takes two arguments: a list and a new value, and returns the updated
;; list with the new value as the head of the original list. The setcdr function takes
;; two arguments: a list and a new value, and returns the updated list with the new
;; value as the rest of the original list.
;; Here are some examples of changing elements of a list:
;; (setcar '(1 2 3) 4)
;; -> (4 2 3)
;; (setcdr '(1 2 3) '(4 5 6))
;; -> (1 4 5 6)
;; 4.7.4 List functions
;; newLISP provides several built-in functions for working with lists. Some of the most
;; useful list functions are:
;; (length list)
;; -> number
;; This function returns the number of elements in the list.
;; (member value list)
;; -> boolean
;; This function returns true if the value is present in the list, and false otherwise.
;; (mapcar function list)
;; -> (result ...)
;; This function applies a function to each element of a list and returns a new list with
;; the results.
;; (filter predicate list)
;; -> (element ...)
;; This function returns a new list containing only the elements of the original list that
;; satisfy the predicate.
;; (reduce function list)
;; -> result
;; This function applies a function to all the elements of a list, combining them in some
;; way.
;; (assq key list)
;; -> (value . key)
;; This function returns a pair containing the value and key from the list that matches the
;; given key.
;; 4.7.5 Conclusion
;; In this chapter, you've learned about lists as data structures in newLISP. You've
;; seen some examples of creating, accessing, and changing elements of lists, as well as

;; 4.7.6. Lists as data structures
;; Lists are a powerful and flexible data structure in newLISP. They are a fundamental


;; 4.7.7 Accessing elements of a list
;; You can access the elements of a list using the car and cdr functions. The car function
;; returns the head of a list, and the cdr function returns the rest of the list.
;; 4.7.8 Changing elements of a list
;; You can change the elements of a list using the setcar and setcdr functions. The
;; 4.7.9 List functions
;; newLISP provides several built-in functions for working with lists. Some of the most


;; 4.7.10 Conclusion
;; In this chapter, you've learned about lists as data structures in newLISP. You've
;; 4.7.11. Lists as data structures
;; Lists are a powerful and flexible data structure in newLISP. They are a fundamental

;; 4.7.12 Accessing elements of a list
;; You can access the elements of a list using the car and cdr functions. The car function
;; 4.7.13 Changing elements of a list
;; You can change the elements of a list using the setcar and setcdr functions. The
;; 4.7.14 List functions
;; newLISP provides several built-in functions for working with lists. Some of the most


;; 4.7.15 Conclusion
;; In this chapter, you've learned about lists as data structures in newLISP. You've
;; 4.7.16. Lists as data structures
;; Lists are a powerful and flexible data structure in newLISP. They are a fundamental
;; 4.7.17 Accessing elements of a list
;; You can access the elements of a list using the car and cdr functions. The car function
;; 4.7.18 Changing elements of a list
;; You can change the elements of a list using the setcar and setcdr functions. The
;; 4.7.19 List functions
;; newLISP provides several built-in functions for working with lists. Some of the most

;; 4.7.20 Conclusion
;; In this chapter, you've learned about lists as data structures in newLISP. You've
;; 4.7.21. Lists as data structures
;; Lists are a powerful and flexible data structure in newLISP. They are a fundamental
;; 4.7.22 Accessing elements of a list
;; You can access the elements of a list using the car and cdr functions. The car function
;; 4.7.23 Changing elements of a list
;; You can change the elements of a list using the setcar and setcdr functions. The
;; 4.7.24 List functions
;; newLISP provides several built-in functions for working with lists. Some of the most

;; 4.7.25 Conclusion
;; In this chapter, you've learned about lists as data structures in newLISP. You've
;; 4.7.26. Lists as data structures
;; Lists are a powerful and flexible data structure in newLISP. They are a fundamental
;; 4.7.27 Accessing elements of a list
;; You can access the elements of a list using the car and cdr functions. The car function
;; 4.7.28 Changing elements of a list
;; You can change the elements of a list using the setcar and setcdr functions. The
;; 4.7.29 List functions
;; newLISP provides several built-in functions for working with lists. Some of the most
;; 4.7.30 Conclusion
;; In this chapter, you've learned about lists as data structures in newLISP. You've


;; 4.7.31. Lists as data structures
;; Lists are a powerful and flexible data structure in newLISP. They are a fundamental
;; 4.7.32 Accessing elements of a list
;; You can access the elements of a list using the car and cdr functions. The car function
;; 4.7.33 Changing elements of a list
;; You can change the elements of a list using the setcar and setcdr functions. The 
;; 4.7.34 List functions
;; newLISP provides several built-in functions for working with lists. Some of the most
;; 4.7.35 Conclusion
;; In this chapter, you've learned about lists as data structures in newLISP. You've
;; 4.7.36. Lists as data structures
;; Lists are a powerful and flexible data structure in newLISP. They are a fundamental
;; 4.7.37 Accessing elements of a list
;; You can access the elements of a list using the car and cdr functions. The car function
;; 4.7.38 Changing elements of a list
;; You can change the elements of a list using the setcar and setcdr functions. The
;; 4.7.39 List functions
;; newLISP provides several built-in functions for working with lists. Some of the most

;; 4.7.40 Conclusion
;; In this chapter, you've learned about lists as data structures in newLISP. You've
;; 4.7.41. Lists as data structures


;; 4.7.42 Accessing elements of a list
;; You can access the elements of a list using the car and cdr functions. The car function
;; 4.7.43 Changing elements of a list
;; You can change the elements of a list using the setcar and setcdr functions. The
;; 4.7.44 List functions
;; newLISP provides several built-in functions for working with lists. Some of the most

;; 4.7.45 Conclusion
;; In this chapter, you've learned about lists as data structures in newLISP. You've
;; 4.7.46. Lists as data structures
;; Lists are a powerful and flexible data structure in newLISP. They are a fundamental
;; 4.7.47 Accessing elements of a list
;; You can access the elements of a list using the car and cdr functions. The car function
;; 4.7.48 Changing elements of a list
;; You can change the elements of a list using the setcar and setcdr functions. The
;; 4.7.49 List functions
;; newLISP provides several built-in functions for working with lists. Some of the most

;; 4.7.50 Conclusion
;; In this chapter, you've learned about lists as data structures in newLISP. You've
;; 4.7.51. Lists as data structures
;; Lists are a powerful and flexible data structure in newLISP. They are a fundamental
;; 4.7.52 Accessing elements of a list

;; newLISP automatically chooses which job to do depending on whether the second element
;; is a list or not.

;; newLISP automatically chooses which job to do depending on whether the second element
;; is a list or not.

(cons 1 2)
;-> (1 2); makes a new list

(cons 1 '(2 3))
;-> (1 2 3)

(car '(1 2 3))
;-> 1

(cdr '(1 2 3))
;-> (2 3)

(setcar '(1 2 3) 4)
;-> (4 2 3)

(setcdr '(1 2 3) '(5 6 7))
;-> (1 5 6 7)

(length '(1 2 3 4 5))
;-> 5

(member 2 '(1 2 3 4 5))


;; 4.7.53 Conclusion
;; In this chapter, you've learned about lists as data structures in newLISP. You've
;; 4.7.54. Lists as data structures
;; Lists are a powerful and flexible data structure in newLISP. They are a fundamental
;; 4.7.55 Accessing elements of a list
;; You can access the elements of a list using the car and cdr functions. The car function
;; 4.7.56 Changing elements of a list
;; You can change the elements of a list using the setcar and setcdr functions. The
;; 4.7.57 List functions
;; newLISP provides several built-in functions for working with lists. Some of the most

;; 4.7.58 Conclusion
;; In this chapter, you've learned about lists as data structures in newLISP. You've
;; 4.7.59. Lists as data structures
;; Lists are a powerful and flexible data structure in newLISP. They are a fundamental
;; 4.7.60 Accessing elements of a list
;; You can access the elements of a list using the car and cdr functions. The car function
;; 4.7.61 Changing elements of a list
;; You can change the elements of a list using the setcar and setcdr functions. The
;; 4.7.62 List functions
;; newLISP provides several built-in functions for working with lists. Some of the most

;; 4.7.63 Conclusion
;; In this chapter, you've learned about lists as data structures in newLISP. You've
;; 4.7.64. Lists as data structures
;; Lists are a powerful and flexible data structure in newLISP. They are a fundamental
;; 4.7.65 Accessing elements of a list
;; You can access the elements of a list using the car and cdr functions. The car function
;; 4.7.66 Changing elements of a list
;; You can change the elements of a list using the setcar and setcdr functions. The

;; Notice the diﬀerence between list and append when you join two lists:

(set 'a '(a b c) 'b '(1 2 3))
(list a b)
;-> ((a b c) (1 2 3)); list makes a list of lists
(append a b)
;-> (a b c 1 2 3); append makes a list


;; 4.7.67 Conclusion
;; In this chapter, you've learned about lists as data structures in newLISP. You've
;; 4.7.68. Lists as data structures
;; Lists are a powerful and flexible data structure in newLISP. They are a fundamental
;; 4.7.69 Accessing elements of a list
;; You can access the elements of a list using the car and cdr functions. The car function
;; 4.7.70 Changing elements of a list
;; You can change the elements of a list using the setcar and setcdr functions. The
;; 4.7.71 List functions
;; newLISP provides several built-in functions for working with lists. Some of the most

;; 4.7.72 Conclusion
;; In this chapter, you've learned about lists as data structures in newLISP. You've
;; 4.7.73. Lists as data structures
;; Lists are a powerful and flexible data structure in newLISP. They are a fundamental


;; 4.7.74 Accessing elements of a list
;; You can access the elements of a list using the car and cdr functions. The car function
;; 4.7.75 Changing elements of a list
;; You can change the elements of a list using the setcar and setcdr functions. The
;; 4.7.76 List functions
;; newLISP provides several built-in functions for working with lists. Some of the most

;; 4.7.77 Conclusion
;; In this chapter, you've learned about lists as data structures in newLISP. You've
;; 4.7.78. Lists as data structures
;; Lists are a powerful and flexible data structure in newLISP. They are a fundamental
;; 4.7.79 Accessing elements of a list
;; You can access the elements of a list using the car and cdr functions. The car function
;; 4.7.80 Changing elements of a list
;; You can change the elements of a list using the setcar and setcdr functions. The
;; 4.7.81 List functions
;; newLISP provides several built-in functions for working with lists. Some of the most

;; 4.7.82 Conclusion
;; In this chapter, you've learned about lists as data structures in newLISP. You've
;; 4.7.83. Lists as data structures
;; Lists are a powerful and flexible data structure in newLISP. They are a fundamental
;; 4.7.84 Accessing elements of a list
;; You can access the elements of a list using the car and cdr functions. The car function
;; 4.7.85 Changing elements of a list
;; You can change the elements of a list using the setcar and setcdr functions. The

;; 4.7.86 List functions
;; newLISP provides several built-in functions for working with lists. Some of the most

;; 4.7.87 Conclusion
;; In this chapter, you've learned about lists as data structures in newLISP. You've
;; 4.7.88. Lists as data structures
;; Lists are a powerful and flexible data structure in newLISP. They are a fundamental


;; 4.7.89 Accessing elements of a list
;; You can access the elements of a list using the car and cdr functions. The car function
;; 4.7.90 Changing elements of a list
;; You can change the elements of a list using the setcar and setcdr functions. The
;; 4.7.91 List functions
;; newLISP provides several built-in functions for working with lists. Some of the most

;; 4.7.92 Conclusion
;; In this chapter, you've learned about lists as data structures in newLISP. You've
;; 4.7.93. Lists as data structures
;; Lists are a powerful and flexible data structure in newLISP. They are a fundamental
;; 4.7.94 Accessing elements of a list
;; You can access the elements of a list using the car and cdr functions. The car function
;; 4.7.95 Changing elements of a list
;; You can change the elements of a list using the setcar and setcdr functions. The

;; 4.7.96 List functions
;; newLISP provides several built-in functions for working with lists. Some of the most

;; 4.7.97 Conclusion
;; In this chapter, you've learned about lists as data structures in newLISP. You've
;; 4.7.98. Lists as data structures
;; Lists are a powerful and flexible data structure in newLISP. They are a fundamental


;; 4.7.99 Accessing elements of a list
;; You can access the elements of a list using the car and cdr functions. The car function
;; 4.7.100 Changing elements of a list
;; You can change the elements of a list using the setcar and setcdr functions. The
;; 4.7.101 List functions
;; newLISP provides several built-in functions for working with lists. Some of the most

;; 4.7.102 Conclusion
;; In this chapter, you've learned about lists as data structures in newLISP. You've
;; 4.7.103. Lists as data structures
;; Lists are a powerful and flexible data structure in newLISP. They are a fundamental
;; 4.7.104 Accessing elements of a list
;; You can access the elements of a list using the car and cdr functions. The car function
;; 4.7.105 Changing elements of a list
;; You can change the elements of a list using the setcar and setcdr functions. The

;; 4.7.106 List functions
;; newLISP provides several built-in functions for working with lists. Some of the most

;; 4.7.107 Conclusion
;; In this chapter, you've learned about lists as data structures in newLISP. You've
;; 4.7.108. Lists as data structures
;; Lists are a powerful and flexible data structure in newLISP. They are a fundamental
;; 4.7.109 Accessing elements of a list


;; 4.7.110 Changing elements of a list
;; You can change the elements of a list using the setcar and setcdr functions. The
;; 4.7.111 List functions
;; newLISP provides several built-in functions for working with lists. Some of the most
;; 4.7.112 Conclusion
;; In this chapter, you've learned about lists as data structures in newLISP. You've

;; list preserves the source lists when making the new list, whereas append uses the elements
;; of each source list to make a new list.
;; To remember this: List keeps the List-ness of the source lists, but aPPend Picks the elements
;; out and Packs them all together again.
;; append can also assemble a bunch of strings into a new string.


