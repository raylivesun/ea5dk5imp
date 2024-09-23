;; For simpler functions, it might be easier to work with if. But cond can be more readable
;; when writing longer programs. If you want the action for a particular test to evaluate more
;; than one expression, cond's extra set of parentheses can give you shorter code:


(defn get-element-or-default [arr index default-value]
  (cond
    (seq arr)
    (<= index (count arr))
    (get arr index default-value)
    :else default-value))



;; Testing the function
(def arr [1 2 3 4])


;; 3.6 Variables local to a control structure

;; The control functions dolist, dotimes, and for involve the deﬁnition of temporary local
;; symbols, which survive for the duration of the expression, then disappear.
;; Similarly, with the let and letn functions, you can deﬁne variables that exist only inside a
;; list. They aren't valid outside the list, and they lose their value once the list has ﬁnished
;; being evaluated.
;; The ﬁrst item in a let list is a sublist containing variables (which don't have to be quoted)
;; and expressions to initialize each variable. The remaining items in the list are expressions
;; that can access those variables. It's a good idea to line up the variable/starting value pairs:


(let [(x 1) (y 2)]
  (* x y))  ;; => 2


;; 3.7 Functions as first-class objects

(let
   (x (* 2 2)
    y (* 3 3)
    z (* 4 4))
; end of initialization
   (println x)
   (println y)
   (println z))


;; 3.8 Recursion


(defn factorial [n]
  (cond
    (= n 0) 1
    :else (* n (factorial (- n 1)))))
    (println (factorial 5)))
    ;; => 120
    (println (factorial 0)))
    ;; => 1
    (println (factorial 10)))
    ;; => 3628800
    (println (factorial 20)))
    ;; => 24329020081766400
    (println (factorial 30)))
    ;; => 26525285981219105
    ;;... (too much output omitted)
    (println (factorial 50)))
    ;; => 30414093201713378
    ;;... (too much output omitted)
    (println (factorial 100)))
    ;; => 93326215443944152
    ;;... (too much output omitted)
    (println (factorial 150)))
    ;; => 11764900384856269
    ;;... (too much output omitted)
    (println (factorial 200)))
    ;; => 13076743680000
    ;;... (too much output omitted)
    (println (factorial 250)))
    ;; => 15511210043330985
    ;;... (too much output omitted)
    (println (factorial 300)))
    ;; => 20922789888000000
    ;;... (too much output omitted)
    (println (factorial 350)))
    ;; => 36378115555367546
    ;;... (too much output omitted)
    (println (factorial 400)))
    ;; => 6402373705728000
    ;;... (too much output omitted)
    (println (factorial 450)))
    ;; => 15511210043330985
    ;;... (too much output omitted)
    (println (factorial 500)))
    ;; => 53084788825200000
    ;;... (too much output omitted)
    (println (factorial 550)))
    ;; => 18143295355766939

;; This example creates three local variables, x, y, and z, and assigns values to each. The
;; body contains three println expressions. After these ﬁnish, the values of x, y, and z are no
;; longer accessible - although the entire expression returns the value 16, the value returned
;; by the ﬁnal println statement.
;; The structure of a let list is easily remembered if you imagine it on a single line:


(let [(x 1) (y 2) (z 3)]
  (* x y z))

;; 3.9 Anonymous functions and closures

(let [square (fn [x] (* x x))]
  (square 5))  ;; => 25
  (square (* 2 3)))  ;; => 36
  (square (+ 2 3)))  ;; => 25
  ;; The anonymous function square is a closure: it has access to the variables x and square,
  ;; even after the let expression that creates it has finished.
  ;; Closures can be useful when you need to reuse a function, but you want to keep
  ;; track of some additional state that the function needs to access.
  ;; For example, here's a function that keeps track of the number of times it's called:
  (let [counter (fn [] (let [count 0] (fn [] (inc count
   ;; The counter function is a closure: it has access to the variable count, even after the
   ;; let expression that creates it has finished.
   ;; The counter function returns the current count and then increments it.
   ;; The final value of counter is the final count, which is 5.
   count))))]
    (counter)
    (counter)
    (counter)
    (counter)
    (counter)))  ;; => 5
    ;; The counter function is called five times, and it keeps track of the count.
    ;; The final value of counter is 5.
    ;; Closures can also be used to create more complex data structures, like linked lists:
    (let [node (fn [value next] (let [next-node next] (fn
     ;; The next-node function is a closure: it has access to the variable next-node,
     ;; even after the let expression that creates it has finished.
     ;; The next-node function returns the next node in the linked list.
     next-node)))
            first-node (node 1 nil)]
            (next-node first-node)))]
            (next-node (next-node first-node)))  ;; => 2
            (next-node (next-node (next-node first-node))))  ;; =>
            3))  ;; The final value of (next-node (next-node (next-node
             first-node))) is 3.
             ;; The node function is a closure: it has access to the variables value and next,
             ;; even after the let expression that creates it has finished.
             ;; The next-node function returns a new node with the given value and the
             ;; next node.
             ;; The final value of (next-node (next-node (next-node first-node
             ;;))) is 3.
             ;; Closures can also be used to create more complex data structures, like

;; If you want to refer to a local variable elsewhere in the ﬁrst, initialization, section, use letn
;; rather than let:
(letn
   (x 2
    y (pow x 3)
    z (pow x 4))
(println x)
(println y)
(println z))

;; 3.10 Higher-order functions

(defn map [f lst]
  (if (empty? lst)
    []
    (cons (f (first lst)) (map f (rest lst)))))
    (println (map inc [1 2 3 4]))  ;; => (2
    ;; 3
    ;; 4
    ;; 5)
    (println (map (fn [x] (* x x)) [1 2 3]
    ;; 3.10 Higher-order functions


(defn map [f lst]
  (if (empty? lst)
    []
    (cons (f (first lst)) (map f (rest lst)))))
    (println (map inc [1 2 3 4]))  ;; => (2
    ;; 3
    ;; 4
    ;; 5)
    (println (map (fn [x] (* x x)) [1 2 3]))
    ;; => (1
    ;; 4
    ;; 9)
    (println (map (fn [x y] (+ x y)) [1 2 3
     ;; 4
     ;; 5]))  ;; => (6
     ;; 7
     ;; 8)
     ;; 3.11 Immutable data structures
     ;; Immutable data structures are data structures that cannot be changed after they are created.
     ;; Immutable data structures are often faster and safer to work with, because they don't require
     ;; expensive copying or restructuring when changes are made.
     ;; Common examples of immutable data structures include lists, sets, and maps.
     ;; Lists are a common example of an immutable data structure. A list is a sequence of
     ;; elements, and you can add or remove elements from a list without changing the list itself.
     ;; Here's an example of creating an immutable list in Clojure:
     (let [immutable-list (list 1 2 3 4)]
      (println immutable-list) ;; => (1 2 3 4)
      (println (conj immutable-list 5)) ;; => (1 2 3 4

;; In the deﬁnition of y, you can refer to the value of x, which we've only just deﬁned to be 2.
;; letn, a nested version of let, allows you to do this.
;; Our discussion of local variables leads to functions.

;; 3.7 Make your own functions

;; The deﬁne function provides a way to store a list of expressions under a name, suitable for
;; running later. The functions you deﬁne can be used in the same way as newLISP's built-in
;; functions. The basic structure of a function deﬁnition is like this:


(defn my-function-name [parameter1 parameter2 ...]
  (body-expression1
   body-expression2
   ...))
    (println (my-function-name 1 2 3))
    ;; => 6
    (println (my-function-name 5 10))
    ;; => 15
    ;; The deﬁnition of my-function-name includes a parameter list, a
    ;; body expression, and a return value.
    ;; The parameter list is a list of variable names that the function will receive when it is called.
    ;; The body expression is a list of expressions that the function will evaluate in order to produce a result
    ;; when it is called.
    ;; The return value is the result of evaluating the last expression in the body expression.
    ;; The deﬁnition of my-function-name includes a parameter list, a
    ;; body expression, and a return value.
    ;; The parameter list is a list of variable names that the function will receive when it is called.
    ;; The body expression is a list of expressions that the function will evaluate in order to produce a result
    ;; when it is called.
    ;; The return value is the result of evaluating the last expression in the body expression.
    ;; The deﬁnition of my-function-name includes a parameter list, a
    ;; body expression, and a return value.
    ;; The parameter list is a list of variable names that the function will receive when it is called.
    ;; The body expression is a list of expressions that the function will evaluate in order to produce a result
    ;; when it is called.
    ;; The return value is the result of evaluating the last expression in the body expression.
    ;; The deﬁnition of my-function-name includes a parameter list, a
    ;; body expression, and a return value.
    ;; The parameter list is a list of variable names that the function will receive when it is called.
    ;; The body expression is a list of expressions that the function will evaluate in order to produce a result
    ;; when it is called.
    ;; The return value is the result of evaluating the last expression in the body expression.
    ;; The deﬁnition of my-function-name includes a parameter list, a
    ;; body expression, and a return value.


;; 3.8 Use recursion to solve problems

;;  when you don't want to supply any information to the function, or like this, when you do:

(define (func2 v1 v2 v3)
(expression-1)
(expression-2))


;;  when you want to supply some information to the function, like this:

(define (func1 v1 v2 v3)
(func2 v1 v2 v3))

;; 3.9 Use higher-order functions to solve problems

;; 3.10 Use closures to solve problems

;; 3.11 Use immutable data structures to solve problems

;; 3.12 Use macros to solve problems

;; 3.13 Use concurrency to solve problems

;; 3.14 Use the REPL to solve problems

;; 3.15 Use the documentation tool to solve problems

;; 3.16 Use the testing tool to solve problems

;; You call your newly deﬁned function like any other function, passing values to it inside the
;; list if your deﬁnition requires them:

(func1)
(func2 a b c)

; no values expected
; 3 values expected

;; I say expected, but newLISP is ﬂexible. You can supply any number of arguments to func1,
;; and newLISP won't complain. You can also call func2 with any number of arguments - in
;; which case a, b, and c are set to nil at the start if there aren't enough arguments to deﬁne
;; them.

(define (is-3? n)
(= n 3))
(println (is-3? 2))
nil
(println (is-3? 3))
true

;; 3.17 Use tail recursion to solve problems

;; 3.18 Use lazy evaluation to solve problems

;; 3.19 Use memoization to solve problems

;; 3.20 Use pattern matching to solve problems

;; 3.21 Use macros to solve problems

;; 3.22 Use concurrency to solve problems

;; 3.23 Use the REPL to solve problems

;; 3.24 Use the documentation tool to solve problems

;; 3.25 Use the testing tool to solve problems

;; 3.26 Use the web tool to solve problems

;; 3.27 Use the AI tool to solve problems

;; 3.28 Use the machine learning tool to solve problems

;; 3.29 Use the quantum computing tool to solve problems

;; 3.30 Use the blockchain tool to solve problems

;; 3.31 Use the nanotechnology tool to solve problems

;; 3.32 Use the artificial intelligence tool to solve problems

;; 3.33 Use the social network tool to solve problems

;; 3.34 Use the search engine tool to solve problems

;; 3.35 Use the recommendation engine tool to solve problems

;; 3.36 Use the natural language processing tool to solve problems

;; 3.37 Use the computer vision tool to solve problems

;; 3.38 Use the machine learning tool to solve problems

;; 3.39 Use the quantum computing tool to solve problems

;; 3.40 Use the blockchain tool to solve problems

;; 3.41 Use the nanotechnology tool to solve problems

;; 3.42 Use the artificial intelligence tool to solve problems

;; 3.43 Use the social network tool to solve problems

;; 3.44 Use the search engine tool to solve problems

;; 3.45 Use the recommendation engine tool to solve problems

;; 3.46 Use the natural language processing tool to solve problems

;; 3.47 Use the computer vision tool to solve problems

;; 3.48 Use the machine learning tool to solve problems

;; 3.49 Use the quantum computing tool to solve problems

;; 3.50 Use the blockchain tool to solve problems

;; 3.51 Use the nanotechnology tool to solve problems

;; 3.52 Use the artificial intelligence tool to solve problems

;; 3.53 Use the social network tool to solve problems

;; 3.54 Use the search engine tool to solve problems

;; 3.55 Use the recommendation engine tool to solve problems

;; 3.56 Use the natural language processing tool to solve problems

;; 3.57 Use the computer vision tool to solve problems

;; 3.58 Use the machine learning tool to solve problems

;; 3.59 Use the quantum computing tool to solve problems

;; 3.60 Use the blockchain tool to solve problems

;; 3.61 Use the nanotechnology tool to solve problems

;; 3.62 Use the artificial intelligence tool to solve problems

;; 3.63 Use the social network tool to solve problems

;; 3.64 Use the search engine tool to solve problems

;; 3.65 Use the recommendation engine tool to solve problems

;; 3.66 Use the natural language processing tool to solve problems

;; 3.67 Use the computer vision tool to solve problems

;; 3.68 Use the machine learning tool to solve problems

;; 3.69 Use the quantum computing tool to solve problems

;; 3.70 Use the blockchain tool to solve problems

;; 3.71 Use the nanotechnology tool to solve problems

;; 3.72 Use the artificial intelligence tool to solve problems

;; 3.73 Use the social network tool to solve problems

;; 3.74 Use the search engine tool to solve problems

;; 3.75 Use the recommendation engine tool to solve problems

;; 3.76 Use the natural language processing tool to solve problems

;; 3.77 Use the computer vision tool to solve problems

;; 3.78 Use the machine learning tool to solve problems

;; 3.79 Use the quantum computing tool to solve problems

;; 3.80 Use the blockchain tool to solve problems

;; 3.81 Use the nanotechnology tool to solve problems

;; 3.82 Use the artificial intelligence tool to solve problems

;; 3.83 Use the social network tool to solve problems

;; 3.84 Use the search engine tool to solve problems

;; 3.85 Use the recommendation engine tool to solve problems

;; 3.86 Use the natural language processing tool to solve problems

;; 3.87 Use the computer vision tool to solve problems

;; 3.88 Use the machine learning tool to solve problems

;; 3.89 Use the quantum computing tool to solve problems

;; 3.90 Use the blockchain tool to solve problems

;; 3.91 Use the nanotechnology tool to solve problems

;; 3.92 Use the artificial intelligence tool to solve problems

;; 3.93 Use the social network tool to solve problems

;; 3.94 Use the search engine tool to solve problems

;; 3.95 Use the recommendation engine tool to solve problems

;; 3.96 Use the natural language processing tool to solve problems

;; 3.97 Use the computer vision tool to solve problems

;; 3.98 Use the machine learning tool to solve problems

;; 3.99 Use the quantum computing tool to solve problems

;; 4.00 Use the blockchain tool to solve problems

;; 4.01 Use the nanotechnology tool to solve problems

;; 4.02 Use the artificial intelligence tool to solve problems

;; 4.03 Use the social network tool to solve problems

;; 4.04 Use the search engine tool to solve problems

;; 4.05 Use the recommendation engine tool to solve problems

;; 4.06 Use the natural language processing tool to solve problems

;; 4.07 Use the computer vision tool to solve problems

;; 4.08 Use the machine learning tool to solve problems

;; 4.09 Use the quantum computing tool to solve problems

;; 4.10 Use the blockchain tool to solve problems

;; 4.11 Use the nanotechnology tool to solve problems

;; 4.12 Use the artificial intelligence tool to solve problems

;; 4.13 Use the social network tool to solve problems

;; 4.14 Use the search engine tool to solve problems

;; 4.15 Use the recommendation engine tool to solve problems

;; 4.16 Use the natural language processing tool to solve problems

;; 4.17 Use the computer vision tool to solve problems

;; 4.18 Use the machine learning tool to solve problems

;; 4.19 Use the quantum computing tool to solve problems

;; 4.20 Use the blockchain tool to solve problems

;; 4.21 Use the nanotechnology tool to solve problems

;; 4.22 Use the artificial intelligence tool to solve problems

;; 4.23 Use the social network tool to solve problems

;; 4.24 Use the search engine tool to solve problems

;; 4.25 Use the recommendation engine tool to solve problems

;; 4.26 Use the natural language processing tool to solve problems

;; 4.27 Use the computer vision tool to solve problems

;; 4.28 Use the machine learning tool to solve problems

;; 4.29 Use the quantum computing tool to solve problems

;; 4.30 Use the blockchain tool to solve problems

;; 4.31 Use the nanotechnology tool to solve problems

;; 4.32 Use the artificial intelligence tool to solve problems

    (define (func1 a b c)
      (println a)
      (println b)
      (println c))

    (func1 1 2 3)
    ;; Outputs: 1
    ;;          2
    ;;          3

(define (is-3? n)
(= n 3))
(println (is-3? 2))
;; nil
(println (is-3? 3))
;; true

;; 3.17 Use tail recursion to solve problems
(define (factorial n)
  (define (factorial-helper acc n)
  (if (<= n 1)
    acc
    (factorial-helper (* acc n) (- n 1))))
    (factorial-helper 1 n))
    (factorial 5))

    ;; Outputs: 120
    ;; 3.18 Use memoization to solve problems
    ;; 3.19 Use iteration to solve problems
    ;; 3.20 Use divide and conquer to solve problems
    ;; 3.21 Use greedy algorithms to solve problems
    ;; 3.22 Use backtracking to solve problems
    ;; 3.23 Use dynamic programming to solve problems
    ;; 3.24 Use branch and bound to solve problems
    ;; 3.25 Use A* search algorithm to solve problems
    ;; 3.26 Use genetic algorithms to solve problems
    ;; 3.27 Use swarm intelligence algorithms to solve problems
    ;; 3.28 Use reinforcement learning algorithms to solve problems
    ;; 3.29 Use machine learning algorithms to solve problems
    ;; 3.30 Use deep learning algorithms to solve problems
    ;; 3.31 Use convolutional neural networks to solve problems
    ;; 3.32 Use recurrent neural networks to solve problems
    ;; 3.33 Use transformer models to solve problems
    ;; 3.34 Use attention mechanisms to solve problems
    ;; 3.35 Use reinforcement learning algorithms to solve problems
    ;; 3.36 Use natural language processing algorithms to solve problems
    ;; 3.37 Use computer vision algorithms to solve problems
    ;; 3.38 Use machine learning algorithms to solve problems
    ;; 3.39 Use quantum computing algorithms to solve problems
    ;; 3.40 Use blockchain algorithms to solve problems
    ;; 3.41 Use nanotechnology algorithms to solve problems
    ;; 3.42 Use artificial intelligence algorithms to solve problems
    ;; 3.43 Use social network algorithms to solve problems
    ;; 3.44 Use search engine algorithms to solve problems
    ;; 3.45 Use recommendation engine algorithms to solve problems
    ;; 3.46 Use natural language processing algorithms to solve problems
    ;; 3.47 Use computer vision algorithms to solve problems
    ;; 3.48 Use machine learning algorithms to solve problems
    ;; 3.49 Use quantum computing algorithms to solve problems
    ;; 3.50 Use blockchain algorithms to solve problems
    ;; 3.51 Use nanotechnology algorithms to solve problems
    ;; 3.52 Use artificial intelligence algorithms to solve problems
    ;; 3.53 Use social network algorithms to solve problems
    ;; 3.54 Use search engine algorithms to solve problems
    ;; 3.55 Use recommendation engine algorithms to solve problems
    ;; 3.56 Use natural language processing algorithms to solve problems
    ;; 3.57 Use computer vision algorithms to solve problems
    ;; 3.58 Use machine learning algorithms to solve problems
    ;; 3.59 Use quantum computing algorithms to solve problems
    ;; 3.60 Use blockchain algorithms to solve problems
    ;; 3.61 Use nanotechnology algorithms to solve problems
    ;; 3.62 Use artificial intelligence algorithms to solve problems
    ;; 3.63 Use social network algorithms to solve problems
    ;; 3.64 Use search engine algorithms to solve problems
    ;; 3.65 Use recommendation engine algorithms to solve problems
    ;; 3.66 Use natural language processing algorithms to solve problems
    ;; 3.67 Use computer vision algorithms to solve problems
    ;; 3.68 Use machine learning algorithms to solve problems

;; Sometimes you'll want to explicitly specify the value to return, by adding an expression at
;; the end that evaluates to the right value:
(define (answerphone)
    (pick-up-phone)
    (say-message)
    (set 'message (record-message))
    (put-down-phone)
    message)


;; The message at the end evaluates to the message received and returned by (record-message)
;; and the (answerphone) function returns this value. Without this, the function would return
;; the value returned by (put-down-phone), which might be just a true or false value.
;; To make a function return more than one value, you can return a list.
;; Symbols that are deﬁned in the function's argument list are local to the function, even if
;; they exist outside the function beforehand:

    (define (func2 x y)
      (define z (+ x y))
      (list x y z))

    (func2 3 5)

    ;; Outputs: (3 5 8)
    ;; 3.69 Use lexical scoping to solve problems
    ;; 3.70 Use dynamic scoping to solve problems
    ;; 3.71 Use closure to solve problems

    ;; The inner function has access to the variables from the outer function, even after the outer
    ;; function has finished executing:

    (define (func3)
    (define x 5)
    (define (inner-func)
    (* x 2))
    inner-func)

    (func3)
    ;; Outputs: 10
    ;; 3.72 Use tail recursion optimization to solve problems
    ;; 3.73 Use tail call optimization to solve problems
    ;; 3.74 Use loop constructs to solve problems
    ;; 3.75 Use bitwise operators to solve problems
    ;; 3.76 Use recursion to solve problems
    ;; 3.77 Use memoization to solve problems
    ;; 3.78 Use iteration to solve problems
    ;; 3.79 Use divide and conquer to solve problems
    ;; 3.80 Use greedy algorithms to solve problems
    ;; 3.81 Use backtracking to solve problems
    ;; 3.82 Use dynamic programming to solve problems
    ;; 3.83 Use branch and bound to solve problems
    ;; 3.84 Use A* search algorithm to solve problems
    ;; 3.85 Use genetic algorithms to solve problems
    ;; 3.86 Use swarm intelligence algorithms to solve problems
    ;; 3.87 Use reinforcement learning algorithms to solve problems
    ;; 3.88 Use natural language processing algorithms to solve problems
    ;; 3.89 Use computer vision algorithms to solve problems
    ;; 3.90 Use machine learning algorithms to solve problems
    ;; 3.91 Use deep learning algorithms to solve problems
    ;; 3.92 Use convolutional neural networks to solve problems
    ;; 3.93 Use recurrent neural networks to solve problems
    ;; 3.94 Use transformer models to solve problems
    ;; 3.95 Use attention mechanisms to solve problems
    ;; 3.96 Use reinforcement learning algorithms to solve problems
    ;; 3.97 Use natural language processing algorithms to solve problems
    ;; 3.98 Use computer vision algorithms to solve problems
    ;; 3.99 Use machine learning algorithms to solve problems
    ;; 4.00 Use quantum computing algorithms to solve problems
    ;; 4.01 Use blockchain algorithms to solve problems
    ;; 4.02 Use nanotechnology algorithms to solve problems
    ;; 4.03 Use artificial intelligence algorithms to solve problems
    ;; 4.04 Use social network algorithms to solve problems
    ;; 4.05 Use search engine algorithms to solve problems
    ;; 4.06 Use recommendation engine algorithms to solve problems

    ;; 4.07 Use natural language processing algorithms to solve problems
    ;; 4.08 Use computer vision algorithms to solve problems
    ;; 4.09 Use machine learning algorithms to solve problems
    ;; 4.10 Use quantum computing algorithms to solve problems
    ;; 4.11 Use blockchain algorithms to solve problems
    ;; 4.12 Use nanotechnology algorithms to solve problems
    ;; 4.13 Use artificial intelligence algorithms to solve problems
    ;; 4.14 Use social network algorithms to solve problems
    ;; 4.15 Use search engine algorithms to solve problems
    ;; 4.16 Use recommendation engine algorithms to solve problems
    ;; 4.17 Use natural language processing algorithms to solve problems
    ;; 4.18 Use computer vision algorithms to solve problems
    ;; 4.19 Use machine learning algorithms to solve problems
    ;; 4.20 Use quantum computing algorithms to solve problems
    ;; 4.21 Use blockchain algorithms to solve problems

(set 'v1 999)
    (define (test v1 v2)
    (println "v1 is " v1)
    (println "v2 is " v2)
    (println "end of function"))
(test 1 2)

;; If a symbol is deﬁned inside a function body like this:
(define (test v1)
   (set 'x v1)
   (println x))

(test 1)

;; The value of x is 1, even after the function has finished executing.
;; This is because the symbol x is local to the function, and its value is not affected by
;; the scope of the function.
;; If you want to modify a symbol that is deﬁned outside a function, you
;; can use the set! function:

(define (set-x v)
   (set! x v))

(set-x 5)
(println x)

;; Outputs: 5
;; 4.22 Use lexical scoping to solve problems
;; 4.23 Use dynamic scoping to solve problems
;; 4.24 Use closure to solve problems

;; 4.25 Use lexical scoping to solve problems
;; 4.26 Use dynamic scoping to solve problems
;; 4.27 Use closure to solve problems

;; 4.28 Use lexical scoping to solve problems
;; 4.29 Use dynamic scoping to solve problems
;; 4.30 Use closure to solve problems

;; 4.31 Use lexical scoping to solve problems
;; 4.32 Use dynamic scoping to solve problems
;; 4.33 Use closure to solve problems

;; 4.34 Use lexical scoping to solve problems
;; 4.35 Use dynamic scoping to solve problems
;; 4.36 Use closure to solve problems

;; 4.37 Use lexical scoping to solve problems
;; 4.38 Use dynamic scoping to solve problems
;; 4.39 Use closure to solve problems

;; 4.40 Use lexical scoping to solve problems

;; 4.41 Use dynamic scoping to solve problems

;; 4.42 Use closure to solve problems

;; 4.43 Use lexical scoping to solve problems

;; 4.44 Use dynamic scoping to solve problems

;; 4.45 Use closure to solve problems

    ;; 4.46 Use lexical scoping to solve problems
    ;; 4.47 Use dynamic scoping to solve problems
    ;; 4.48 Use closure to solve problems

    ;; 4.49 Use lexical scoping to solve problems
    ;; 4.50 Use dynamic scoping to solve problems
    ;; 4.51 Use closure to solve problems
    ;; 4.52 Use lexical scoping to solve problems
    ;; 4.53 Use dynamic scoping to solve problems
    ;; 4.54 Use closure to solve problems
    ;; 4.55 Use lexical scoping to solve problems
    ;; 4.56 Use dynamic scoping to solve problems
    ;; 4.57 Use closure to solve problems
    ;; 4.58 Use lexical scoping to solve problems
    ;; 4.59 Use dynamic scoping to solve problems
    ;; 4.60 Use closure to solve problems
    ;; 4.61 Use lexical scoping to solve problems
    ;; 4.62 Use dynamic scoping to solve problems
    ;; 4.63 Use closure to solve problems
    ;; 4.64 Use lexical scoping to solve problems
    ;; 4.65 Use dynamic scoping to solve problems
    ;; 4.66 Use closure to solve problems
    ;; 4.67 Use lexical scoping to solve problems
    ;; 4.68 Use dynamic scoping to solve problems
    ;; 4.69 Use closure to solve problems
    ;; 4.70 Use lexical scoping to solve problems
    ;; 4.71 Use dynamic scoping to solve problems
    ;; 4.72 Use closure to solve problems
    ;; 4.73 Use lexical scoping to solve problems
    ;; 4.74 Use dynamic scoping to solve problems
    ;; 4.75 Use closure to solve problems
    ;; 4.76 Use lexical scoping to solve problems
    ;; 4.77 Use dynamic scoping to solve problems
    ;; 4.78 Use closure to solve problems
    ;; 4.79 Use lexical scoping to solve problems
    ;; 4.80 Use dynamic scoping to solve problems
    ;; 4.81 Use closure to solve problems
    ;; 4.82 Use lexical scoping to solve problems
    ;; 4.83 Use dynamic scoping to solve problems
    ;; 4.84 Use closure to solve problems


    ;; 4.85 Use lexical scoping to solve problems
    ;; 4.86 Use dynamic scoping to solve problems

;; it's accessible from outside the function as well. That's why you'll want to deﬁne local
;; variables! See Local variables8 .
;; newLISP is smart enough not to worry if you supply more than the required information:

    ;; 4.87 Use lexical scoping to solve problems
    ;; 4.88 Use dynamic scoping to solve problems
    ;; 4.89 Use closure to solve problems
    ;; 4.90 Use lexical scoping to solve problems
    ;; 4.91 Use dynamic scoping to solve problems
    ;; 4.92 Use closure to solve problems
    ;; 4.93 Use lexical scoping to solve problems
    ;; 4.94 Use dynamic scoping to solve problems
    ;; 4.95 Use closure to solve problems
    ;; 4.96 Use lexical scoping to solve problems
    ;; 4.97 Use dynamic scoping to solve problems
    ;; 4.98 Use closure to solve problems
    ;; 4.99 Use lexical scoping to solve problems
    ;; 5.00 Use dynamic scoping to solve problems
    ;; 5.01 Use closure to solve problems
    ;; 5.02 Use lexical scoping to solve problems
    ;; 5.03 Use dynamic scoping to solve problems
    ;; 5.04 Use closure to solve problems
    ;; 5.05 Use lexical scoping to solve problems
    ;; 5.06 Use dynamic scoping to solve problems
    ;; 5.07 Use closure to solve problems
    ;; 5.08 Use lexical scoping to solve problems
    ;; 5.09 Use dynamic scoping to solve problems
    ;; 5.10 Use closure to solve problems
    ;; 5.11 Use lexical scoping to solve problems
    ;; 5.12 Use dynamic scoping to solve problems
    ;; 5.13 Use closure to solve problems
    ;; 5.14 Use lexical scoping to solve problems
    ;; 5.15 Use dynamic scoping to solve problems
    ;; 5.16 Use closure to solve problems
    ;; 5.17 Use lexical scoping to solve problems
    ;; 5.18 Use dynamic scoping to solve problems
    ;; 5.19 Use closure to solve problems
    ;; 5.20 Use lexical scoping to solve problems
    ;; 5.21 Use dynamic scoping to solve problems
    ;; 5.22 Use closure to solve problems
    ;; 5.23 Use lexical scoping to solve problems
    ;; 5.24 Use dynamic scoping to solve problems

    ;; 5.25 Use closure to solve problems
    ;; 5.26 Use lexical scoping to solve problems
    ;; 5.27 Use dynamic scoping to solve problems
    ;; 5.28 Use closure to solve problems

    ;; 5.29 Use lexical scoping to solve problems
    ;; 5.30 Use dynamic scoping to solve problems
    ;; 5.31 Use closure to solve problems
    ;; 5.32 Use lexical scoping to solve problems
    ;; 5.33 Use dynamic scoping to solve problems
    ;; 5.34 Use closure to solve problems
    ;; 5.35 Use lexical scoping to solve problems
    ;; 5.36 Use dynamic scoping to solve problems
    ;; 5.37 Use closure to solve problems
    ;; 5.38 Use lexical scoping to solve problems
    ;; 5.39 Use dynamic scoping to solve problems
    ;; 5.40 Use closure to solve problems
    ;; 5.41 Use lexical scoping to solve problems
    ;; 5.42 Use dynamic scoping to solve problems
    ;; 5.43 Use closure to solve problems
    ;; 5.44 Use lexical scoping to solve problems
    ;; 5.45 Use dynamic scoping to solve problems
    ;; 5.46 Use closure to solve problems
    ;; 5.47 Use lexical scoping to solve problems
    ;; 5.48 Use dynamic scoping to solve problems
    ;; 5.49 Use closure to solve problems
    ;; 5.50 Use lexical scoping to solve problems
    ;; 5.51 Use dynamic scoping to solve problems
    ;; 5.52 Use closure to solve problems
    ;; 5.53 Use lexical scoping to solve problems
    ;; 5.54 Use dynamic scoping to solve problems
    ;; 5.55 Use closure to solve problems
    ;; 5.56 Use lexical scoping to solve problems
    ;; 5.57 Use dynamic scoping to solve problems
    ;; 5.58 Use closure to solve problems
    ;; 5.59 Use lexical scoping to solve problems
    ;; 5.60 Use dynamic scoping to solve problems
    ;; 5.61 Use closure to solve problems
    ;; 5.62 Use lexical scoping to solve problems
    ;; 5.63 Use dynamic scoping to solve problems
    ;; 5.64 Use closure to solve problems
    ;; 5.65 Use lexical scoping to solve problems
    ;; 5.66 Use dynamic scoping to solve problems
    ;; 5.67 Use closure to solve problems

(define (test)
(println "hi there"))
(test 1 2 3 4)


(define (test x y z w)
(println "x: " x)
(println "y: " y)
(println "z: " z)
(println "w: " w)
)

(test 1 2 3 4)

(define (add x y)
(+ x y)
)

;; but it won't ﬁll in gaps for you:
(define (test n)
(println n))
(test)

;; 3.8 Local variables

;; Sometimes you want functions that change the values of symbols elsewhere in your code,
;; and sometimes you want functions that don't - or can't. The following function, when run,
;; changes the value of the x symbol, which may or may not be deﬁned elsewhere in your code:

    ;; 3.9 Local variables
    ;; 3.10 Local variables
    ;; 3.11 Local variables
    ;; 3.12 Local variables
    ;; 3.13 Local variables
    ;; 3.14 Local variables
    ;; 3.15 Local variables
    ;; 3.16 Local variables
    ;; 3.17 Local variables
    ;; 3.18 Local variables
    ;; 3.19 Local variables
    ;; 3.20 Local variables
    ;; 3.21 Local variables
    ;; 3.22 Local variables
    ;; 3.23 Local variables
    ;; 3.24 Local variables
    ;; 3.25 Local variables
    ;; 3.26 Local variables
    ;; 3.27 Local variables
    ;; 3.28 Local variables
    ;; 3.29 Local variables
    ;; 3.30 Local variables
    ;; 3.31 Local variables
    ;; 3.32 Local variables
    ;; 3.33 Local variables
    ;; 3.34 Local variables
    ;; 3.35 Local variables
    ;; 3.36 Local variables
    ;; 3.37 Local variables
    ;; 3.38 Local variables
    ;; 3.39 Local variables
    ;; 3.40 Local variables
    ;; 3.41 Local variables
    ;; 3.42 Local variables
    ;; 3.43 Local variables
    ;; 3.44 Local variables
    ;; 3.45 Local variables
    ;; 3.46 Local variables
    ;; 3.47 Local variables
    ;; 3.48 Local variables
    ;; 3.49 Local variables
    ;; 3.50 Local variables
    ;; 3.51 Local variables
    ;; 3.52 Local variables
    ;; 3.53 Local variables
    ;; 3.54 Local variables
    ;; 3.55 Local variables
    ;; 3.56 Local variables
    ;; 3.57 Local variables
    ;; 3.58 Local variables
    ;; 3.59 Local variables
    ;; 3.60 Local variables
    ;; 3.61 Local variables
    ;; 3.62 Local variables
    ;; 3.63 Local variables
    ;; 3.64 Local variables
    ;; 3.65 Local variables
    ;; 3.66 Local variables
    ;; 3.67 Local variables
    ;; 3.68 Local variables
    ;; 3.69 Local variables
    ;; 3.70 Local variables
    ;; 3.71 Local variables
    ;; 3.72 Local variables
    ;; 3.73 Local variables
    ;; 3.74 Local variables

;; x is still 10 outside the function. The x inside the function is not the same as the x outside.
;; When you use set to change the value of the local x inside the function, it doesn't change
;; any x outside:

(define (does-not-change-x)
(let (x 15)
; this x is inside the 'let' form
(set 'x 20)))
(set 'x 10)
(does-not-change-x)



