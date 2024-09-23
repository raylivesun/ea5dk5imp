;; 2.7 Destructive functions

;; Some newLISP functions modify the values of the symbols that they operate on, others
;; create a copy of the value and return that. Technically, the ones that modify the contents
;; of symbols are described as destructive functions - although you'll often be using them
;; to create new data. In this document I'll describe functions such as push and replace as
;; destructive. This simply means that they change the value of something rather than return
;; a modiﬁed copy.

;; The push function adds an element to the front of a list.
(defparameter my-list '(1 2 3))
(push 4 my-list)
my-list


;; The replace function replaces the ith element in a list with a new element.
(replace 1 my-list 5)
my-list

;; 3 Controlling the ﬂow

;; There are many diﬀerent ways to control the ﬂow of your code. If you've used other scripting
;; languages, you'll probably ﬁnd your favourites here, and many more besides.
;; All the control ﬂow functions obey the standard rules of newLISP. The general form of each
;; is usually a list in which the ﬁrst element is a keyword followed by one or more expressions
;; to be evaluated:

(keyword expression1 expression2 expression3 ...)


;; 3.1 Tests: if...

;; Perhaps the simplest control structure you can write in any language is a simple if list,
;; consisting of a test and an action:


;; The if function evaluates its test expression and then evaluates the corresponding action
;; expression if the test is true. If the test is false, it evaluates the next action expression
;; and so on.

(if (> 5 3)
    'yes
    'no)



;; 3.2 Loop constructs: do...while and while...do

;; The second expression, a call to the launch-missile function, is evaluated only if the symbol
;; button-pressed? evaluates to true. 1 is true. 0 is true - it's a number, after all. -1 is true.
;; Most of the things that newLISP already knows about are true. There are two important
;; things that newLISP knows are false rather than true: nil and the empty list (). And
;; anything that newLISP doesn't know the value of is false.

(if x 1)
; if x is true, return the value 1
(if 1 (launch-missile))
; missiles are launched, because 1 is true
(if 0 (launch-missile))
; missiles are launched, because 0 is true
(if nil (launch-missile))
;-> nil, there's no launch, because nil is false
(if '() (launch-missile))
;-> (), and the missiles aren't launched


;; You can use anything that evaluates to either true or false as a test:
(if (> 4 3) (launch-missile))
;-> it's true that 4 > 3, so the missiles are launched
(if (> 4 3) (println "4 is bigger than 3"))
"4 is bigger than 3"


;; 3.3 Break and continue: loop...do and loop...while

;; The break and continue statements are used to exit the loop prematurely.
;; break exits the loop immediately, and continue continues to the next iteration.

(loop :for i :from 1 :to 10 :do
      (if (> i 5)
          (break))
      (println i))

;; The loop above will print numbers from 1 to 5, and then exit.

(loop :for i :from 1 :to 10 :do
      (if (> i 5)
          (continue))
      (println i))


;; 3.4 Returning from a function: return

;; The return statement is used to exit a function and return a value to the caller.

(defun greet-and-multiply (name)
  (format t "Hello, ~a!" name)
  (return (* 2 (length name))))

(greet-and-multiply "Alice")


;; 4 Macros and Procedures


;; 4.1 Defining macros: defmacro


;; Macros are a powerful feature of Lisp that allow you to write functions that generate
;; other functions at compile time. They are particularly useful for creating higher-order
;; functions, or functions that manipulate other functions.
;; The defmacro function is used to define macros. The general form of a defmacro
;; is:

(defmacro my-macro (name &rest args)
  (let ((gensym-counter 0))
    `(defun,name ,@args
     (loop :for arg :in args
     :for index :from 0 :to (1- (length args)) :do
     (declare (ignore index))
     (if (symbolp arg)
     (format t ",~a" arg)
     (format t ",~a" (gensym "arg"))))
     (format t ",~a" (gensym "arg"))))))
      (if (null args)
        '(progn)
        (list 'progn (mapcar (lambda (arg) (gensym "arg")) args
        (cons arg (gensym "arg")))))))



;; 4.2 Using macros: macroexpand-1

;; The macroexpand-1 function is used to expand a macro into its expanded form.


;; Let's define a simple macro that replaces a symbol with its name:
(defmacro replace-symbol (sym new-name)
  (symbol-name sym))

(macroexpand-1 '(replace-symbol 'hello 'world))

;; The output will be:
;; "world"

;; 4.3 Defining procedures: defun

;; Procedures are functions that take a number of arguments and return a value.
;; The defun function is used to define procedures. The general form of a defun
;; is:

(defun my-procedure (arg1 arg2 ...)
  ;; body of the procedure
  ...)
  (return value))



;; 4.4 Using procedures: funcall

;; The funcall function is used to call a procedure with the given arguments.


;; Let's define a simple procedure that takes two arguments and returns their sum
(defun add (x y)
  (+ x y))
  
(funcall add 2 3)
;; The output will be:
;; 5

;; 5 Other features

;; 5.1 Dynamic scoping: let and let*


;; The let and let* functions are used to create local bindings in a procedure.
;; The general form of a let is:

(let ((var1 value1) (var2 value2 ...) ...)
  ;; body of the procedure
  ...)
  (return value))



;; The general form of a let* is:

(let* ((var1 value1) (var2 value2 ...) ...)
  ;; body of the procedure
  ...)
  (return value))



;; 5.2 Symbol tables: defvar and defparameter


;; The defvar and defparameter functions are used to create global bindings in a
;; program. The general form of a defvar is:

(defvar my-variable value)


;; The general form of a defparameter is:

(defparameter my-parameter value)


;; 5.3 Function composition: compose


;; The compose function is used to create a new function that takes a series of
;; functions and applies them in reverse order to produce a single result.
;; The general form of a compose is:

(defun compose (&rest functions)
  (lambda (x)
  (reduce (lambda (y f) (f y)) functions x)))

;; 5.4 Currying: curry

;; The curry function is used to create a new function that takes a single argument
;; and returns a new function that takes the remaining arguments.
;; The general form of a curry is:

(defun curry (func &rest args)
  (lambda (arg)
  (if (null args)
      func arg
      (curry func (cons arg args)))))

;; 5.5 Function application: apply

;; The apply function is used to apply a function to a list of arguments.
;; The general form of an apply is:

(apply function argument-list)


;; 6 Advanced features


;; 6.1 Multithreading: threads and locks

;; The threads and locks features are used to create and manage threads in a
;; program. The general form of a thread is:

(thread (lambda ()
          ;; body of the thread
          ...))


;; The general form of a lock is:

(defparameter my-lock (make-lock))

;; The general form of locking a lock is:

(with-lock (my-lock)
  ;; body of the thread
  ...)



;; 6.2 Asynchronous programming: promises and futures
;; The promises and futures features are used to create and manage asynchronous
;; operations in a program. The general form of a promise is:

(defparameter my-promise (make-promise))
;; The general form of fulfilling a promise is:

(promise- fulfill my-promise value)
;; The general form of waiting for a promise to be fulfilled is:

(let ((result (promise-wait my-promise)))
  ;; body of the thread
  ...)
  result)


;; 6.3 Error handling: error and try...catch

;; The error and try...catch features are used to handle errors in a program.
;; The general form of raising an error is:

(error "An error occurred")
;; The general form of handling an error is:

(try
  ;; body of the try block
  ;;...
  (catch 'error (error message))
  ;; body of the catch block
  ;;...)
  ;; body of the finally block
  ;;...)
  finally
  ;; body of the finally block
  ;;...))


;; 6.4 Continuation-passing style: cpc

;; The cpc features are used to implement continuation-passing style in a
;; program. The general form of cpc is:

(defun cpc-function (arg1 arg2 ...)
  ;; body of the function
  ;;...
  (values result continuation))

;; The general form of calling a cpc function is:

(let ((result (cpc-function arg1 arg2 ...)))
  ;; body of the continuation
  ;;...
  result)



;; 6.5 Lazy evaluation: thunks and delayed evaluation
;; The thunks and delayed evaluation features are used to implement lazy evaluation
;; in a program. The general form of a thunk is:
(let ((value (lazy-function)))
  ;; body of the thunk
  ;;...
  value))


;; The general form of evaluating a thunk is:

(force value)

;; 6.6 Immutable data structures: defstruct and defrecord

;; The defstruct and defrecord features are used to create immutable data structures
;; in a program. The general form of a defstruct is:

(defstruct my-struct field1 field2 ...)


;; The general form of creating an instance of a defstruct is:

(make-my-struct :field1 value1 :field2 value2 ...)


;; The general form of accessing a field of a defstruct is:

(my-struct-field1 my-instance)


;; The general form of a defrecord is:

(defrecord MyRecord ()
  field1 field2...)



;; The general form of creating an instance of a defrecord is:

(make MyRecord. :field1 value1 :field2 value2 ...)


;; The general form of accessing a field of a defrecord is:

(MyRecord.field1 my-instance)

;; 6.7 Immutable collections: defparameter and defparameterize

;; The defparameter and defparameterize features are used to create immutable
;; collections in a program. The general form of a defparameter is:

(defparameter my-parameter value)

;; The general form of creating an immutable collection from a list is:

(defparameter my-collection (list value1 value2 ...))

;; The general form of accessing an element of an immutable collection is:

(aref my-collection index)



;; The general form of creating an immutable collection from a vector is:

(defparameter my-collection (vector value1 value2 ...))

;; The general form of accessing an element of an immutable collection is:

(aref my-collection index)


;; If a symbol evaluates to nil (perhaps because it doesn't exist or hasn't been assigned a
;; value), newLISP considers it false and the test returns nil (because there was no alternative
;; action provided):

(if snark (launch-missile))
;-> nil ; that symbol has no value
(if boojum (launch-missile))
;-> nil ; can't find a value for that symbol
(if untrue (launch-missile))
;-> nil ; can't find a value for that symbol either
(if false (launch-missile))
;-> nil
; never heard of it, and it doesn't have a value

;; You can add a third expression, which is the else action. If the test expression evaluates
;; to nil or (), the third expression is evaluated, rather than the second, which is ignored:


(if snark (launch-missile) (print "No missile launched."))


;; Here's a typical real-world three-part if function, formatted to show the structure as clearly
;; as possible:

(if (and socket (net-confirm-request))
(net-flush)
(finish "could not connect"))
; test
; action when true
; action when false

;; Although there are two expressions after the test - (net-ﬂush) and (ﬁnish ...) - only one of
;; them will be evaluated.
;; The lack of the familiar signpost words such as then and else that you ﬁnd in other
;; languages, can catch you out if you're not concentrating! But you can easily put comments
;; in.
;; You can use if with an unlimited number of tests and actions. In this case, the if list
;; consists of a series of test-action pairs. newLISP works through the pairs until one of the
;; tests succeeds, then evaluates that test's corresponding action. If you can, format the list
;; in columns to make the structure more apparent:


(if (and socket (net-confirm-request))
(net-flush)


;; (finish "could not connect"))

;; Here's the same code, formatted in columns for better readability:

(if (and socket (net-confirm-request))
(net-flush)
; test
; action when true
(finish "could not connect"))


;; (net-flush)
;; action when false

;; (finish "could not connect")


(< x 20)
(>= x 20)
)
(define a "medium")
(define a "large")


;; The above code defines two variables a with two different values. When you try to compare
;; them using the < and >= operators, newLISP uses the variable's current value, which
;; is "medium" in this case.

;; If you want to compare the variables using their original values, you can use the
;; eqv? function. This function returns true if two objects are the same object in memory,
;; regardless of their values:

(eqv? a "medium")
;-> #f


;; (eqv? a "large")
;-> #t


;; Here's how you can use eqv? to compare variables:

(if (eqv? a "medium")
(print "a is medium")
(print "a is not medium"))


;; (if (eqv? a "large")
;; (print "a is large")
;; (print "a is not large"))


;; (print "a is medium")

;; (print "a is not large")


;; Here's how you can use eqv? to compare strings:

(eqv? "small" "medium")
;-> #f

;; (eqv? "small" "large")
;-> #t

;; (eqv? "medium" "large")
;-> #t


;; (eqv? "medium" "medium")
;-> #t

;; (eqv? "medium" "small")
;-> #f


;; (eqv? "large" "large")
;-> #t

;; (eqv? "large" "medium")
;-> #f

;; (eqv? "large" "small")
;-> #f


;; The eqv? function is useful when you want to compare objects based on their memory
;; locations, rather than their values.

;; The eq function is used to compare objects based on their values. If the objects have
;; different values, the eq function returns false. If the objects have the same value,
;; the eq function returns true:

(eq "small" "medium")
;-> #f

;; (eq "small" "large")
;-> #f

;; (eq "medium" "large")
;-> #t

;; (eq "medium" "medium")
;-> #t

;; (eq "medium" "small")
;-> #f

;; (eq "large" "large")
;-> #t

;; (eq "large" "medium")
;-> #f

;; (eq "large" "small")
;-> #f

;; The eq function is useful when you want to compare objects based on their values,
;; rather than their memory locations.


;; 7.1 Variables and functions: local variables and global variables

;; A local variable is a variable that is defined within a function or block. It only
;; exists within that block or function, and its value is discarded when the block or
;; function exits.

;; Here's an example of a local variable:

(defun add-numbers (x y)
  (let ((sum (+ x y)))

  ;; sum is a local variable
  sum))


;; A global variable is a variable that is defined outside of any function or block. It
;; can be accessed from anywhere in the program, and its value persists across
;; function calls.

;; Here's an example of a global variable:

(defparameter global-variable 0)

;; You can access and modify a global variable from within any function:

(defun increment-global-variable ()
  (incf global-variable 1))

;; (incf global-variable 1)

;; (print global-variable)


;; 7.2 Functions: function definitions and calling

;; A function definition is a statement that defines a new function. It consists of a
;; keyword (defun), the name of the function, a list of parameter names, and the body


;; of the function.

;; Here's an example of a function definition:

(defun add-numbers (x y)
  (+ x y))


;; A function call is a statement that invokes a function with specific arguments. It
;; consists of the name of the function, followed by a list of the arguments.
;; Here's an example of a function call:

(add-numbers 5 3)
;-> 8

;; 7.3 Functions: function arguments and return values

;; Function arguments are the values that are passed to a function when it is called.
;; There are three types of function arguments: positional arguments, keyword arguments,
;; and rest arguments.
;; Positional arguments are arguments that are passed in the order they appear in the
;; function definition.
;; Keyword arguments are arguments that are passed as a pair of a keyword and a value.
;; Rest arguments are arguments that are passed as a list of additional arguments.
;; Here's an example of a function that takes positional and keyword arguments:

(defun greet (name &key (language "English"))
  (format nil "~A, how are you?" name language))



;; A function can return a value using the return statement. The value returned by a
;; function is the value that is assigned to the variable that holds the function call.
;; Here's an example of a function that returns a value:

(defun add-numbers (x y)
  (+ x y))
  (add-numbers 5 3))


;; 7.4 Functions: function scoping and closures

;; Function scoping is the mechanism by which variables are accessed and modified in
;; different parts of a program. There are two main types of scoping: global scoping and
;; local scoping.
;; Global scoping means that variables are accessible from anywhere in the program,
;; while local scoping means that variables are accessible only within the scope of a
;; function or block.
;; Here's an example of global scoping:

(defparameter global-variable 0)

;; (defun increment-global-variable ()
;;   (incf global-variable 1))
;; (incf global-variable 1)

;; (print global-variable)


;; Here's an example of local scoping:

(defun add-numbers (x y)
  (let ((local-variable x))
  (+ x y))
  (add-numbers 5 3))
  (print local-variable))


;; Closures are functions that have access to variables from their surrounding scope, even
;; after the function has finished executing. Closures are a powerful feature of


;; 7.5 Functions: higher-order functions and lambda expressions
;; A higher-order function is a function that takes one or more functions as arguments or
;; returns a function as its result.
;; Here's an example of a higher-order function that takes a function as an argument:

(defun apply-function (function argument)
  (function argument))
  (apply-function #'add-numbers 5 3))
  (print result))



;; A lambda expression is a anonymous function that can be defined using the let keyword.


;; 7.6 Functions: recursion and tail recursion optimization
;; Recursion is a technique where a function calls itself to solve a smaller version of the
;; problem.
;; Here's an example of a recursive function that calculates the factorial of a number:

(defun factorial (n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))
      (factorial 5))
      (print result))


;; Tail recursion optimization is a technique that can help optimize recursive functions by
;; transforming them into iterative loops. In tail recursion optimization, the recursive call
;; is the last operation in the function's body.
;; Here's an example of tail recursion optimization for the factorial function:

(defun factorial (n result)
  (if (<= n 1)
   result
      (factorial (- n 1) (* n result))))
      (factorial 5 1))
      (print result))


;; 7.7 Functions: closures and lexical scoping
;; Closures are functions that have access to variables from their surrounding scope, even
;; after the function has finished executing. Closures are a powerful feature of
;; functional programming, allowing functions to be defined in terms of other functions.
;; Lexical scoping is the mechanism by which variables are accessed and modified in
;; different parts of a program. Lexical scoping refers to the way variables are
;; determined at compile-time, rather than runtime.
;; Here's an example of a closure that uses lexical scoping:

(defun outer-function ()
  (let ((x 0))
  (defun inner-function ()
    (incf x 1))
    (inner-function)))
    (outer-function))
    (print x))


;; 7.8 Functions: function composition and currying
;; Function composition is a technique where the output of one function becomes the input of
;; another function.
;; Here's an example of function composition:

(defun add (x y)
  (+ x y))
  (defun subtract (x y)
  (- x y))
  (subtract (add 5 3) 2))
  (print result))


;; If you've used other LISP dialects, you might recognise that this is a simple alternative to
;; cond, the conditional function. newLISP provides the traditional cond structure as well.
;; See Selection: if, cond, and case1 .
;; You might be wondering how to do two or more actions if a test is successful or not. There
;; are two ways to do this. You can use when, which is like an if without an 'else' part.


;; Currying is a technique where a function takes multiple arguments and returns a new function
;; that takes one argument and continues to build on the original function.
;; Here's an example of currying:

(defun add (x y)
  (+ x y))
  (let ((add-five (curry add 5)))
  (add-five 3))
  (print result))



;; 7.9 Functions: function optimization and performance tuning
;; Performance optimization is the process of improving the efficiency and speed of a program.
;; One way to optimize a program is to use efficient algorithms and data structures.
;; Another way to optimize a program is to use techniques like memoization, which stores the
;; results of expensive function calls and returns the cached result when the same inputs
;; occur again.
;; Here's an example of memoization in a recursive function:

(defun factorial (n result)
  (if (<= n 1)
   result
      (factorial (- n 1) (* n result))))
      (factorial 5 1))
      (print result))

(define x 1)
((if (< x 5) + *) 3 4)
7
; which function to use, + or *?
; it added


;; Here, the ﬁrst element of the expression, (if (< x 5) + *), returns an arithmetic operator
;; depending on the results of a test comparing x with 5. So the whole expression is either an
;; addition or multiplication, depending on the value of x.

(define x 10)
;-> 10
((if (< x 5) + *) 3 4)
12
; it multiplied


;; 7.10 Functions: function composition and currying
;; Function composition is a technique where the output of one function becomes the input of
;; another function.
;; Here's an example of function composition:

(defun add (x y)
  (+ x y))


;; This technique can help you write concise code. Instead of this:
(if (< x 5) (+ 3 4) (* 3 4))


;; You can write this:
((add 3 4) (if (< x 5) 0 (* 3) (if (< x 5))))


;; Function currying is a technique where a function takes multiple arguments and returns a new
;; function that takes one argument and continues to build on the original function.
;; Here's an example of currying:

(defun add (x y)
  (+ x y))
  (let ((add-five (curry add 5)))
  (add-five 3))
  (print result))


;; which evaluates like this:
((if (< x 5) + *) 3 4)
((if true + *) 3 4)
(+ 3 4)


;; 7.11 Functions: function optimization and performance tuning
;; Performance optimization is the process of improving the efficiency and speed of a program.
;; One way to optimize a program is to use efficient algorithms and data structures.
;; Another way to optimize a program is to use techniques like memoization, which stores the
;; results of expensive function calls and returns the cached result when the same inputs
;; occur again.
;; Here's an example of memoization in a recursive function:

(defun factorial (n result)
  (if (<= n 1)
   result
      (factorial (- n 1) (* n result))))
      (factorial 5 1))
      (print result))
      (define x 1)
      ((if (< x 5) + *) 3 4)
      7)
      ; which function to use, + or *?
      ; it added

(define x 10)
(define y 10)
;-> 10
((if (< x 5) + *) 3 4)
12
; it multiplied

;; 7.12 Functions: function composition and currying
;; Function composition is a technique where the output of one function becomes the input of
;; another function.
;; Here's an example of function composition:

(defun add (x y)
  (+ x y))
  (let ((add-five (curry add 5)))
  (add-five 3))
  (print result))
  ;; which evaluates like this:
  ((if (< x 5) + *) 3 4)
  ((if true + *) 3 4)
  (+ 3 4)
  ;; Function currying is a technique where a function takes multiple arguments and returns a new
  ;; function that takes one argument and continues to build on the original function.
  ;; Here's an example of currying:

  (defun add (x y)
  (+ x y))
  (let ((add-five (curry add 5)))
  (add-five 3))
  (print result))
  ;; which evaluates like this:
  ((if (< x 5) + *) 3 4)
  ((if true + *) 3 4)
  (+ 3 4)
  ;; 7.13 Functions: function optimization and performance tuning
  ;; Performance optimization is the process of improving the efficiency and speed of a program.
  ;; One way to optimize a program is to use efficient algorithms and data structures.
  ;; Another way to optimize a program is to use techniques like memoization, which stores the

  ;; results of expensive function calls and returns the cached result when the same inputs
  ;; occur again.
  ;; Here's an example of memoization in a recursive function:
  (defun factorial (n result)
  (if (<= n 1)
   result
   (factorial (- n 1) (* n result))))
   (factorial 5 1))
   (print result))
   (define x 1)
   ((if (< x 5) + *) 3 4)
   7)
   ;; which function to use, + or *?
   ;; it added
   (define x 10)
   (define y 10)
   ;;-> 10
   (define n nil) 
   ((if (< n 5) + *) 3 4)
   12
   ;; it multiplied
   ;; 7.14 Functions: function composition and currying
   ;; Function composition is a technique where the output of one function becomes the input of
   ;; another function.
   ;; Here's an example of function composition:
   (defun add (x y)
   (+ x y))

   ;; This technique can help you write concise code. Instead of this:
   ;; (if (< x 5) (+ 3 4) (* 3 4))
   ;; You can write this:
   ;; ((add 3 4) (if (< x 5) 0 (* 3
   ;; (if (< x 5))))

   ;; Function currying is a technique where a function takes multiple arguments and returns a new
   ;; function that takes one argument and continues to build on the original function.
   ;; Here's an example of currying:

   (defun add (x y)
   (+ x y))
   (let ((add-five (curry add 5)))
   (add-five 3))
   (print result))

   ;; which evaluates like this:
   ;; ((if (< x 5) + *) 3 4)

   ;; (if true + *) 3 4)

   ;; (+ 3 4)
   ;; 7.15 Functions: function optimization and performance tuning
   ;; Performance optimization is the process of improving the efficiency and speed of a program.
   ;; One way to optimize a program is to use efficient algorithms and data structures.
   ;; Another way to optimize a program is to use techniques like memoization, which stores the
   ;; results of expensive function calls and returns the cached result when the same inputs
   ;; occur again.
   ;; Here's an example of memoization in a recursive function:
   (defun factorial (n result)
   (if (<= n 1)))
   result

   ;; (factorial (- n 1) (* n result))))
   (factorial 5 1))
   (print result))
   (define x 1)
   ((if (< x 5) + *) 3 4)
   7)
   ;; which function to use, + or *?
   ;; it added
   (define x 10)
   (define y 10)
   ;;-> 10
   (define n nil)
   ((if (< n 5) + *) 3 4)
   12
   ;; it multiplied
   ;; 7.16 Functions: function composition and currying
   ;; Function composition is a technique where the output of one function becomes the input of
   ;; another function.
   ;; Here's an example of function composition:
   (defun add (x y)
   (+ x y))
   ;; This technique can help you write concise code. Instead of this:
   ;; (if (< x 5) (+ 3 4) (* 3 4))
   ;; You can write this:
   ;; ((add 3 4) (if (< x 5) 0 (* 3
   ;; (if (< x 5))))
   ;; Function currying is a technique where a function takes multiple arguments and returns a new
   ;; function that takes one argument and continues to build on the original function.
   ;; Here's an example of currying:

   (defun add (x y)
   (+ x y))
   (let ((add-five (curry add 5)))
   (add-five 3))
   (print result))
   ;; which evaluates like this:
   ;; ((if (< x 5) + *) 3 4)
   ;; (if true + *) 3 4)
   ;; (+ 3 4)

(define x (if flag 1 -1))

(define result
    (if
    (< x 0)
    (< x 10)
    (< x 20)
    "impossible"
    "small"
    "medium"
    "large"))

The value of x depends on the value returned by if expression. Now the symbol result
contains a string depending on the value of x.

8.1 Functions: function composition and currying
Function composition is a technique where the output of one function becomes the input of another function.
Here's an example of function composition:

(add (+ x y) (+ x y))


This technique can help you write concise code. Instead of this:
(if (< x 5) (+ 3 4) (* 3 4))

You can write this:
(add (+ x  y) (* x y))
(if (+ x y) (+ x y))

8.2 Functions: function optimization and performance tuning
Performance optimization is the process of improving the efficiency and speed of a program.
One way to optimize a program is to use efficient algorithms and data structures.
Another way to optimize a program is to use techniques like memoization, which stores the
results of expensive function calls and returns the cached result when the same inputs
occur again.
Here's an example of memoization in a recursive function:

(defun factorial (n result)
  (if (<= n 1)
   result
   (factorial (- n 1) (* n result))))
   (factorial 5 1))
   (print result))
   (define x 1)
   ((if (< x 5) + *) 3 4)
   7)
   which function to use, + or *?
   it added
   (define x 10)
   (define y 10)
   -> 10
   (define n nil)
   ((if (< n 5) + *) 3 4)
   12
   it multiplied
   (8.3 Functions: function composition and currying
   Function composition is a technique where the output of one function becomes the input of another function.
   Here's an example of function composition:

   (add (+ x y) (+ x y))
   This technique can help you write concise code. Instead of this:
   (if (< x 5) (+ 3 4) (* 3 4))
   You can write this:
   (add (+ x y) (* x y))
   (if (+ x y) (+ x y))
   (8.4 Functions: function optimization and performance tuning
   Performance optimization is the process of improving the efficiency and speed of a program.
   One way to optimize a program is to use efficient algorithms and data structures.
   Another way to optimize a program is to use techniques like memoization, which stores the
   results of expensive function calls and returns the cached result when the same inputs
   occur again.
   Here's an example of memoization in a recursive function:
   (defun factorial (n result)
     (if (<= n 1)
      result
      (factorial (- n 1) (* n result))))
      (factorial 5 1))
      (print result))
      (define x 1)
      ((if (< x 5) + *) 3 4)
      7)
      which function to use, + or *?
      it added
      (define x 10)
      (define y 10)
      -> 10
      (define n nil)
      ((if (< n 5) + *) 3 4)
      12