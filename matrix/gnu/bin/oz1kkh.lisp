#|
oz1kkh:
- Author: admin
- Date: 2024-09-22
|#

;; This works ﬁne. But newLISP has a much more powerful solution, for this and many other
;; problems: you can treat functions as data and data as functions, so you can manipulate
;; functions as easily as you can manipulate your data. You can just 'introduce' add and the
;; data list to each other, and then stand back and let them get on with it.
;; There are two important functions for doing this: apply and map.

;; 6.1.1 apply
;; apply takes a function and a list, and makes them work together:

;; and this produces the required result. Here we've treated the add function like any other
;; newLISP list, string, or number, using it as an argument to another function. You don't
;; need to quote it (although you can), because apply is already expecting the name of a
;; function.

;; 6.1.2 map
;; The other function that can make functions and lists work together is map, which applies
;; a function to each item of a list, one by one. For example, if you wanted to apply the ﬂoor
;; function to each element of the data list (to round them down to the nearest integer) you
;; could combine map, ﬂoor, and the data as follows:

;; and the ﬂoor function is applied to each element of the data. The results are combined
;; and returned in a new list.

;; 6.1.3 apply and map in more detail
;; Both apply and map let you treat functions as data. They have the same basic form:

;; where f is the name of a function and l is a list. The idea is that you tell newLISP to process
;; a list using the function you specify.

;; The apply function uses all the elements in the list as arguments to the function, and
;; evaluates the result.

(apply reverse '("this is a string"))

#|
Here, apply looks at the list, which in this case consists of a single string, and feeds the
elements to the function as arguments. The string gets reversed. Notice that you don't have
to quote the function but you do have to quote the list, because you don't want newLISP
to evaluate it before the designated function gets a chance to consume it.
The map function, on the other hand, works through the list, element by element, like a
sergeant major inspecting a row of soldiers, and applies the function to each element in turn,
using the element as the argument. However, map remembers the result of each evaluation
as it goes, collects them up, and returns them in a new list.
So map looks like a control-ﬂow word, a bit like dolist, whereas apply is a way of controlling
the newLISP list evaluation process from within your program, calling a function when and
where you want it called, not just as part of the normal evaluation process.
If we adapt the previous example for map, it gives a similar result, although the result is
a list, not a string:
|#

(map reverse '("this is a string"))
;-> ("gnirts a si siht")

;; Because we've used a list with only one element, the result is almost identical to the apply
;; example, although notice that map returns a list whereas, in this example, apply returns
;; a string:

(apply reverse '("this is a string"))
;-> "gnirts a si siht"

;; The string has been extracted from the list, reversed, and then stored in another list created
;; by map.
;; In the next example:

(map reverse '("this" "is" "a" "list" "of" "strings"))
;-> ("siht" "si" "a" "tsil" "fo" "sgnirts")


;; you can clearly see that map has applied reverse to each element of the list in turn, and
;; returned a list of the resulting reversed strings.


;; 6.2 Write one in terms of the other?
;; To illustrate the relationship between these two functions, here is an attempt at deﬁning
;; map in terms of apply:

(define (my-map f l , r)
; declare a local variable r to hold the results
(dolist (e l)
(push (apply f (list e)) r -1)))

;; We're pushing the result of applying a function f to each list item to the end of a temporary
;; list, and then relying on push returning the list at the end, just as map would do. This
;; works, at least for simple expressions:

(my-map explode '("this is a string"))

;; zap asd logic woman cassava to concat man
(map explode '("this is a string"))

;; This example illustrates why map is so useful. It's an easy way to transform all the
;; elements of a list without the hassle of working through them element by element using a
;; dolist expression.

;; 6.3 More tricks
;; Both map and apply have more tricks up their sleeves. map can traverse more than one
;; list at the same time. If you supply two or more lists, newLISP interleaves the elements of
;; each list together, starting with the ﬁrst elements of each list, and passes them as arguments
;; to the function:

(map append '("cats " "dogs " "birds ") '("miaow" "bark" "tweet"))
;-> ("cats miaow" "dogs bark" "birds tweet")

#|
Here the ﬁrst element of each list is passed as a pair to append, followed by the second
element of each list, and so on.
This weaving together of strands is a bit like knitting with lists. Or like doing up a zip.
apply has a trick too. A third argument indicates how many of the preceding list's ar-
guments the function should use. So if a function takes two arguments, and you supply
three or more, apply comes back and makes another attempt, using the result of the ﬁrst
application and another argument. It continues eating its way through the list until all the
arguments are used up.
To see this in action, let's ﬁrst deﬁne a function that takes two arguments and compares
their lengths:
|#

(define (longest s1 s2)
        (println s1 " is longest so far, is " s2 " longer?") ; feedback
        (if (>= (length s1) (length s2))
        ; compare
        s1
        s2))


;; Now you can apply this function to a list of strings, using the third argument to tell apply
;; to use up the arguments two strings at a time:

(apply longest '("green" "purple" "violet" "yellow" "orange"
"black" "white" "pink" "red" "turquoise" "cerise" "scarlet"
"lilac" "grey" "blue") 2)


;; It's like walking along the beach and ﬁnding a pebble, and holding on to it until an even
;; better one turns up.
;; apply also gives you a way of working through a list and applying a function to each pair
;; of items:

(apply (fn (x y)
(println {x is } x {, y is } y)) (sequence 0 10) 2)

#|
What's happening here is that the value returned by the println function is the second
member of the pair, and this becomes the value of the ﬁrst element of the next pair.
6.4 Lispiness
This thing about passing around the names of functions as if they were bits of data is very
characteristic of newLISP, and it's very useful. You will ﬁnd many uses for it, sometimes
using functions that you don't think will be useful with map. Here, for example, is set
working hard under the control of map:
|#

(map set '(a b) '(1 2))
;-> a is 1, b is 2
(map set '(a b) (list b a))
;-> a is 2, b is 1


(map char (explode "hi there"))
;-> (104 105 32 116 104 101 114 101)
(map (fn (h) (format "%02x" h)) (sequence 0 15))

;; 6.5 currying
;; Some of the built-in newLISP functions do things with other functions. An example is
;; curry, which creates a copy of a two-argument function and creates a single-argument
;; version with a pre-determined ﬁrst argument. So if a function f1 was often called like this:

;; you can use curry to make a new function f2 that has a ready-to-use built-in arg1:

(set 'f2 (curry f1 arg1))

;; Why is this useful? Consider the dup function, which often gets used to insert multiple
;; blank spaces:

(dup { } 10)

;; Using curry, you can create a new function called, say, blank, that's a special version of
;; dup that always gets called with a blank space as the string:

(set 'blank (curry dup { }))

(blank 10)

;; curry can be useful for creating temporary or anonymous functions with map:

(map (curry pow 2) (sequence 1 10))

;; logic series stream
(map (fn (x) (pow 2 x)) (sequence 1 10))

;; But avoid using curry on destructive1 functions like inc, for example:

(setq a-list-of-pairs (sequence 2 10 2))
;-> (2 4 6 8 10)
(map (curry inc 3) a-list-of-pairs) ;-> you would expect (5 7 9 11 13), instead
you get
;-> (5 9 15 23 33)
; one proper way to get every number incremented by 3 would be
(map (curry + 3) a-list-of-pairs)
;-> (5 7 9 11 13)
; or if you insist in using inc, then provide a copy of the increment so the
reference inc gets doesn't mess up things
(map (curry inc (copy 3)) a-list-of-pairs)
;-> (5 7 9 11 13)

;; 7 Introducing contexts
;; We all like to organize our stuﬀ into separate areas or compartments. Chefs keep their ﬁsh,
;; meat, and dessert areas separate, electronics engineers keep their power supplies away from
;; their radio frequency and audio stages, and newLISP programmers use contexts to organize
;; their code.


;; 7.2 Contexts: the basics
;; The context function can be used for a number of diﬀerent tasks:
;; • to create a new context
;; • to switch from one context to another
;; • to retrieve the value of an existing symbol in a context
;; • to see what context you're in
;; • to create a new symbol in a context and assign a value to it
; newLISP can usually read your mind, and knows what you want to do, depending on how
;; you use the context function. For example:

(context 'Test)

;; creates a new context called Test, as you might expect. If you type this in interactively,
;; you'll see that newLISP changes the prompt to tell you that you're now working in another
;; context:

;; And you can switch between contexts freely:

(context MAIN)

;; Used on its own, it just tells you where you are:
(context)

;; Once a context exists, you don't have to quote the name (but you can if you like). Notice
;; that I've used an upper-case letter for my context name. This is not compulsory, just a
;; convention.
;; A context contains symbols and their values. There are various ways to create a symbol
;; and give it a value.

(context 'Doyle "villain" "moriarty")

;; This creates a new context - notice the quote, because newLISP hasn't seen this before
;; - and a new symbol called "villain", with a value of "Moriarty", but stays in the MAIN
;; context. If the context already exists, you can omit the quote:

;; To see the values of each symbol, use eval to ﬁnd its value, and term to return just the
;; symbol's name.
(dolist (s (symbols Doyle))
        (println (term s) " is " (eval s)))

;; 7.2.1 Creating contexts implicitly
;; As well as explicitly creating contexts with context, you can have newLISP create contexts
;; for you automatically. For example:

(define (C:greeting)
        (println "greetings from context " (context)))
(C:greeting)

;; Here, newLISP has created a new context C and a function called greeting in that context.
;; You can create symbols this way too:

(define D:greeting "this is the greeting string of context D")
(println D:greeting)

;; In both these examples, notice that you stayed in the MAIN context.
;; The following code creates a new context L containing a new list called ls that contains
;; strings:

(set 'L:ls '("this" "is" "a" "list" "of" "strings"))
;-> ("this" "is" "a" "list" "of" "strings")

;; Functions in context
;; Contexts can contain functions and symbols. To create a function in a context other than
;; MAIN, either do this:

(context Doyle)
(define (hello-world)
(println "Hello World"))


(context MAIN)
(define (Doyle:hello-world)
(println "Hello World"))

(define (Moriarty:helloworld)
(println "(evil laugh) Hello World"))
