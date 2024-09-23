;; This is good for trying out short expressions, testing ideas, and for debugging. You can
;; write multi-line code in this environment by enclosing the lines between [cmd] and [/cmd].
;; In new versions you can simply use the enterkey on a blank line to begin and also to end
;; the multi-line block.

[cmd]
(define (fibonacci n)
(if (< n 2)
1
(+ (fibonacci (- n 1))
(fibonacci (- n 2)))))
[/cmd]

(fibonacci 10)

;; The visual interface to newLISP, newLISP-GS, provides a graphical toolkit for newLISP
;; applications, and it also gives you a development environment in which to write and test
;; code: the newLISP editor. On Windows, this is installed as a desktop icon and a folder in
;; the Program Start menu. On MacOS X, an application package and icon is installed in the
;; Applications folder. The newLISP-GS editor provides you with multiple tabbed windows,
;; syntax colouring, and a monitor area for seeing the results of running your code.


;; You can also run the newLISP-GS editor from the command line: you can ﬁnd the ﬁle at
;; C:/Program Files/newlisp/newlisp-edit (Windows), or at /usr/bin/newlisp-edit (on Unix).
;; (This is a newLISP source ﬁle, so you can look at the code too.)
;; You can edit newLISP scripts in your favourite text editor. On MacOS X you can use
;; BBEdit, TextWrangler, or TextMate to run newLISP scripts, or you can use one of the
;; pre-installed Unix text editors such as vim or emacs. On Windows you can use UltraEdit,
;; EditPlus, or NotePad++, to name just a few. If you use Linux, you know more about text
;; editors than I do, and you probably already have a preference.
;; The newLISP web site hosts conﬁguration ﬁles for a number of popular editors at
;; http://newlisp.org/index.cgi?Code_Contributions2 .
;; On Unix, the ﬁrst line of a newLISP script should be:


;; #!/usr/bin/newlisp

;; or:

;; #!/usr/bin/env newlisp

;; On Windows, you can run newLISP scripts using the command:


;; newlisp script.nlisp

;; or:

;; C:\Program Files\newlisp\newlisp script.nlisp

;; or:

;; C:\newlisp\newlisp script.nlisp


;; The newLISP web site also hosts a number of examples and tutorials at
;; http://newlisp.org/index.cgi?Examples2 .
;; If you have any questions about newLISP, please ask on the newLISP mailing list
;; at the address newlisp@lists.sourceforge.net.
;; You can also ask on the IRC channel #newlisp on Freenode.
;; If you have any problems running newLISP, please let me know.
;; If you have any suggestions for newLISP, please let me know.
;; If you have any improvements for newLISP, please let me know.
;; If you have any bugs in newLISP, please let me know.
;; If you have any ideas for newLISP, please let me know.
;; If you have any problems with newLISP, please let me know.
;; If you have any suggestions for newLISP, please let me know.
;; If you have any improvements for newLISP, please let me know.
;; If you have any bugs in newLISP, please let me know.
;; If you have any ideas for newLISP, please let me know.
;; If you have any problems with newLISP, please let me know.
;; If you have any suggestions for newLISP, please let me know.
;; If you have any improvements for newLISP, please let me know.
;; If you have any bugs in newLISP, please let me know.
;; If you have any ideas for newLISP, please let me know.
;; If you have any problems with newLISP, please let me know.
;; If you have any suggestions for newLISP, please let me know.
;; If you have any improvements for newLISP, please let me know.
;; If you have any bugs in newLISP, please let me know.
;; If you have any ideas for newLISP, please let me know.
;; If you have any problems with newLISP, please let me know.
;; If you have any suggestions for newLISP, please let me know.
;; If you have any improvements for newLISP, please let me know.
;; If you have any bugs in newLISP, please let me know.
;; If you have any ideas for newLISP, please let me know.
;; If you have any problems with newLISP, please let me know.
;; If you have any suggestions for newLISP, please let me know.
;; If you have any improvements for newLISP, please let me know.


;; If you have any bugs in newLISP, please let me know.
;; If you have any ideas for newLISP, please let me know.
;; If you have any problems with newLISP, please let me know.

;; 2.3 The three basic rules of newLISP

;; You have to learn just three basic rules to program in newLISP. Here's the ﬁrst one:

;; 2.4 Rule 1: a list is a sequence of elements
;; A list is a sequence of elements enclosed in parentheses:

(1 2 3 4 5)                 ; a list of integers
("the" "cat" "sat")         ; a list of symbol names      
(x y z foo bar)             ; a list of newLISP functions 
(sin cos tan atan)          ; a mixed list 
(1 2 "stitch" x sin)        ; a list with a list inside it
(1 2 (1 2 3) 3 4 )          ; a list of list  
((1 2) (3 4) (5 6))         ; a list of list


;; The list is the basic data structure in newLISP, and it's also the way you write your 
;; program code. But don't type these examples in just yet - there are two more rules to 
;; learn!
;; 2.5 Rule 2: a list is a sequence of elements, and an element
;; can be any of the following:

;; 2.5 Rule 2: the ﬁrst element in a list is special

;; When newLISP sees a list, it treats the ﬁrst element as a function, and then tries to use
;; the remaining elements as the information the function needs.

(+ 2 2)


;; This is a list of three elements: the function called +, followed by two numbers. When
;; newLISP sees this list, it evaluates it and returns the value 4 (of course). Notice that the
;; ﬁrst element was treated by newLISP as a function, whereas the rest of the elements were
;; interpreted as arguments to that function - numbers that the function expects.
;; Here are some more examples that demonstrate these ﬁrst two rules:

(+ 1 2 3 4 5 6 7 8 9)

;; returns 45. The + function adds up all the numbers in the list.

(max 1 1.2 12.1 12.2 1.3 1.2 12.3)

;; returns 12.3, the biggest number in the list. Again, there's no (reasonable) limit to the
;; length of the list: if a function is happy to accept 137 items (which both max and + are),
;; than you can pass it 137 items.

(print "the sun has put his hat on")


;; prints the string of characters the sun has put his hat on. (It also returns the string,
;; which is why, when you're working in the console, you sometimes see things repeated twice.)
;; The print function can print out a single string of characters, or you can supply a sequence
;; of elements to print:


(print 1 2 3 4 5)

;; prints the integers 1 2 3 4 5.
;; 2.6 Rule 3: a list is a sequence of elements, and an element
;; can be any of the following:

;; 2.6 Rule 3: the rest of a list is special
;; When newLISP sees a list, it treats the rest of the elements as additional arguments
;; to the function that the ﬁrst element represents.

(sin 0.5)

(print 1 2 "buckle" "my" "shoe")

;; which prints the two numbers and the three strings (although not very well formatted,
;; because you haven't met the format function yet).
;; The directory function:

(directory "/")

;; produces a listing of the speciﬁed directory, in this case the root directory, "/":

("." ".." ".DS_Store" ".hotfiles.btree" ".Spotlight-V100"
".Trashes"".vol" ".VolumeIcon.icns" "Applications"
"automount" "bin" "cores" "Desktop DB" "Desktop DF"
"Desktop Folder" "dev""Developer" "etc" "Library"
"mach" "mach.sym" "mach_kernel" "Network" "private"
"sbin" "System" "System Folder" "TheVolumeSettingsFolder"
"tmp" "User Guides And Information" "Users" "usr"
"var" "Volumes")



;; 2.7 The format function

;; The format function is used to print out a string that includes placeholders for other
;; values. The placeholders are indicated by percent signs followed by a letter, like this:

(format "The value of x is ~a." x)

;; The ~a placeholder means to print the value of the variable x.
;; Here's an example:

(let ((x 3))
  (format "The value of x is ~a." x))
  ;; returns: "The value of x is 3."


;; 2.8 The string function
;; The string function is used to convert any value to a string. Here's an example:

;; It lists the current directory if you don't specify one:
(directory)


;; It converts a number to a string:
(string 123)

;; It converts a list to a string, with each element separated by a space:
(string (1 2 3 4 5))

;; It converts a symbol to a string:
(string 'foo)

;; It converts a boolean to a string:
(string t)

;; 2.9 The list function
;; The list function is used to create a new list. Here's an example:

(list 1 2 3 4 5)

;; It can also be used to create a list of symbols:

(list 'apple 'banana 'cherry)

;; It can also be used to create a list of strings:

(list "apple" "banana" "cherry")

;; 2.10 The car and cdr functions
;; The car function returns the ﬁrst element of a list:

(car '(1 2 3 4 5))

;; There's a read-ﬁle function that reads in the contents of a text ﬁle:
(read-file "/usr/share/newlisp/modules/stat.lsp")

;; Here the function wants a single argument - the ﬁle name - and returns 
;; the contents of the
;; ﬁle to you, in a string.
;; These are typical examples of the building blocks of newLISP code - a list containing a
;; function call, followed perhaps by any extra information the function requires. There are
;; over 380 newLISP functions, and you can refer to the excellent newLISP reference manual
;; for details of all of them and how to use them.
;; You can try these examples. If you're using newLISP at a terminal, just type them in. If
;; you're typing the lines into a text editor and running it as a script, you won't necessarily
;; see the result of a function call unless you enclose the expression in the println function.
;; For example, type:

(println (car '(1 2 3 4 5)))

;; and you'll see the number 1 printed.
;; If you want to see the result of a function call, you'll need to use the println
;; function. For example, type:

(println (read-file "/usr/share/newlisp/modules/stat.lsp"))

;; to print the results of the read-ﬁle function.
;; Every newLISP expression returns a value. Even a println function returns a value. You
;; could say that the printing action is actually just a side eﬀect, and its main task is to return
;; a value. You might notice that when you use println interactively in a console window,
;; you see the return value twice: once when it's printed, and again when the value is returned
;; to the calling function (the top-most level, in this case).
;; There's one more useful thing to look at before you meet the third rule.


;; 2.11 The load function
;; The load function is used to load a newLISP module from a ﬁle
;; and then evaluate all the expressions in that module. Here's an example:

(load "/usr/share/newlisp/modules/stat.lsp")

;; 2.5.1 Nested lists
;; You've already spotted one list nesting inside another. Here's another example:

(* (+ 1 2) (+ 3 4))

;; When newLISP sees this, it thinks as follows:
;; Hmm. Let's start with the ﬁrst of those inner lists. I can do

(+ 1 2)

;; easily. The value of that is 3. I can also do the second list
(+ 3 4)

;; easily enough. That evaluates to 7.
;; So if I replace these two inner lists with these values, I get

(* 3 7)

;; which equals 21.
;; 2.5.2 The cons function
;; The cons function is used to create a new list by combining a new element with an existing list
;; at the beginning. Here's an example:

(cons 1 '(2 3 4))

;; When newLISP sees this, it thinks as follows:
;; "I want to create a new list by putting the number 1 at the beginning of the
;; existing list (2 3 4)."
;; So I do that. The new list is (1 2 3 4).
;; 2.5.3 The append function
;; The append function is used to concatenate two or more lists. Here's an example:


(append '(1 2 3) '(4 5 6))


;; When newLISP sees this, it thinks as follows:
;; "I want to combine the two existing lists (1 2 3) and (4
;; 5 6) into a new list."
;; So I do that. The new list is (1 2 3 4 5
;; 6).
;; 2.6.1 The eq function
;; The eq function is used to test whether two values are equal. Here's an example:

(eq 'apple 'apple)


;; easily enough. That evaluates to 7.
;; So if I replace these two inner lists with these values, I get

(* 3 7)

;; which equals 21.
;; 2.6.2 The equal function
;; The equal function is used to test whether two values are equal, including whether they are
;; different types. Here's an example:

(equal 1 '1)

;; When newLISP sees this, it thinks as follows:
;; "I want to test whether the values 1 and '1 are equal, including whether they
;; are of different types."
;; So I do that. The answer is false.
;; 2.7.1 The map function
;; The map function is used to apply a function to each element of a list. Here's an
;; example:

;; which is really easy. I'll return the value 21 for this expression.
(* (+ 1 2) (+ 3 4))
(* 3 (+ 3 4))
(* 3 7)

;; 2.7.2 The reduce function
;; The reduce function is used to apply a function to each element of a list, and then combine
;; the results. Here's an example:

(reduce + '(1 2 3 4 5))

;; When newLISP sees this, it thinks as follows:
;; "I want to apply the addition function (+) to each element of the list (1
;; 2 3 4 5), and then combine the results."
;; So I do that. The result is 15.
;; 2.8.1 The sort function
;; The sort function is used to sort a list of numbers in ascending order. Here's an
;; example:

(sort '(7 3 5 1 2))

;; When newLISP sees this, it thinks as follows:
;; "I want to sort the list (7 3 5 1 2) in
;; ascending order."
;; So I do that. The result is (1 2 3 5 7).
;; 2.8.2 The reverse function
;; The reverse function is used to reverse the order of elements in a list. Here's an
;; example:

(reverse '(1 2 3 4 5))

;; When newLISP sees this, it thinks as follows:
;; "I want to reverse the order of the elements in the list (1 2 3
;; 4 5)."
;; So I do that. The result is (5 4 3 2 1).
;; 2.9.1 The nth function
;; The nth function is used to get the nth element of a list. Here's an example:

(nth 2 '(1 2 3 4 5))

;; When newLISP sees this, it thinks as follows:
;; "I want to get the second element of the list (1 2 3 4
;; 5)."
;; So I do that. The result is 3.
;; 2.9.2 The length function
;; The length function is used to get the number of elements in a list. Here's an
;; example:

(length '(1 2 3 4 5))

;; When newLISP sees this, it thinks as follows:
;; "I want to get the number of elements in the list (1 2 3
;; 4 5)."
;; So I do that. The result is 5.
;; 2.10.1 The car function
;; The car function is used to get the ﬁrst element of a list. Here
;;'s an example:

(car '(1 2 3 4 5))

;; When newLISP sees this, it thinks as follows:
;; "I want to get the ﬁrst element of the list (1 2
;; 3 4 5)."
;; So I do that. The result is 1.
;; 2.10.2 The cdr function
;; The cdr function is used to get the rest of a list after the ﬁ
;; element. Here's an example:

(cdr '(1 2 3 4 5))


;; When newLISP sees this, it thinks as follows:
;; "I want to get the rest of the list (2 3 4 5)
;; after the ﬁrst element."
;; So I do that. The result is (2 3 4 5).
;; 2.11.1 The load function
;; The load function is used to load a newLISP module from a ﬁle
;; and then evaluate all the expressions in that module. Here's an example:

(load "/usr/share/newlisp/modules/stat.lsp")

;; 2.11.2 The load function
;; The load function is used to load a newLISP module from a ﬁle
;; and then evaluate all the expressions in that module. Here's an example:

(load "/usr/share/newlisp/modules/stat.lsp")


;; See those two right parentheses at the end of the ﬁrst line, after the 4? Both are essential:
;; the ﬁrst one ﬁnishes the (+ 3 4 list, and the second one ﬁnishes the multiplication operation
;; that started with (*. When you start writing more complicated code, you'll ﬁnd that you
;; are putting lists inside lists inside lists inside lists, and you might be ending some of the
;; more complicated deﬁnitions with half a dozen right parentheses. A good editor will help
;; you keep track of them.
;; But you don't have to worry about white space, line terminators, various punctuation
;; marks, or compulsory indentation. And because all your data and your code are stored in
;; the same way, in lists, you can mix them freely. More on that later.
;; Some people worry about the proliferation of parentheses when they ﬁrst see LISP code.
;; Others refer to them as nail clippings or say that LISP stands for Lots of Irritating Silly



;; Parentheses. But I prefer to think of the parentheses as small handles that enclose a
;; newLISP thought:
;; "I want to add 3 and 4, and then multiply the result by 7
;; and finally add the ﬁrst element of a list to the result."
;; It's like saying, "I'm going to build a wall with 7 layers of br
;; icks, and then I'm going to add a door to the wall."
;; The parentheses are like the bricks, and the thought is like the wall.
;; And you can put as many bricks as you want inside the wall, and
;; you can put as many doors as you want inside the wall.
;; And you can put as many walls as you want, if you want.


;; Figure 2
;; grabbing the handles of a newLISP thought
;; When you're editing newLISP code in a good editor, you can easily move or edit a thought by
;; grabbing its handles, and easily select a thought with a Balance Parentheses command.
;; You'll soon ﬁnd the parentheses more useful than you ﬁrst thought!


;; 3. The newLISP language
;; 3.1.1 The syntax
;; newLISP uses a simple, clean, and consistent syntax, which makes it easy to read
;; and write. Here's an example of a newLISP expression:

(* 3 (+ 3 4))


;; When newLISP sees this, it thinks as follows:
;; "I want to multiply 3 by the sum of 3 and 4."
;; So I do that. The result is 21.
;; 3.1.2 The semantics
;; newLISP uses a simple, clean, and consistent semantics, which makes it easy to read

;; 2.5.2 Quoting prevents evaluation

;; You can now meet the third rule of programming with newLISP:

;; 2.6 Rule 3: Quoting prevents evaluating
;; To stop newLISP evaluating something, quote it.
;; Compare these two lines:

(eq 'apple 'apple)
(eq 'apple 3)


;; When newLISP sees the second line, it thinks as follows:
;; "I want to test whether the value 'apple' is equal to the value 3,
;; including whether they are of different types."
;; So I do that. The answer is false.
;; 3.2.1 The data types
;; newLISP has a simple, clean, and consistent data type system, which makes it easy
;; to read and write. Here are some of the data types newLISP supports:

;; 2.1 Rule 1: Numbers
;; newLISP supports integers, floating-point numbers, and complex numbers.
;; Here are some examples:

3
1.5
+3.5j


;; When newLISP sees this, it thinks as follows:
;; "I want to create a number with the value 3."
;; So I do that.
;; 2.2 Rule 2: Strings
;; newLISP supports strings, which are sequences of characters.
;; Here are some examples:

"Hello, world!"


;; When newLISP sees this, it thinks as follows:
;; "I want to create a string with the value 'Hello, world!'."
;; So I do that.
;; 2.3 Rule 3: Lists
;; newLISP supports lists, which are ordered sequences of elements.
;; Here are some examples:

(1 2 3 4 5)
(a b c d e)


;; When newLISP sees this, it thinks as follows:
;; "I want to create a list with the elements 1, 2, 3,


;; 2.3.1 Nested lists

;; 2.4 Rule 4: Tuples
;; newLISP supports tuples, which are ordered sequences of elements.
;; Unlike lists, tuples are immutable.
;; Here are some examples:

(1 2 3)
(a b c)

;; When newLISP sees this, it thinks as follows:
;; "I want to create a tuple with the elements 1, 2, 3."
;; So I do that.
;; 2.5.1 Symbols


;; 2.5.2 Quoting prevents evaluation

;; You can now meet the third rule of programming with newLISP:

;; 2.6 Rule 3: Quoting prevents evaluating
;; To stop newLISP evaluating something, quote it.
;; Compare these two lines:

(eq 'apple 'apple)
(eq 'apple 3)

;; When newLISP sees the second line, it thinks as follows:
;; "I want to test whether the value 'apple' is equal to the value 3,
;; including whether they are of different types."
;; So I do that. The answer is false.


;; 2.5.3 Special symbols
;; newLISP has a small set of special symbols, which are used to perform
;; 2.6 Rule 4: Tuples
;; 2.5.4 Function symbols
;; 2.5.5 Variable symbols
;; 2.5.6 Quoted symbols
;; 2.5.7 Special operators
;; 2.5.8 Special forms
;; 2.5.9 Special variables
;; 2.5.10 Special reader syntax
;; 2.5.11 Special writer syntax
;; 2.5.12 Special compiler syntax
;; 2.5.13 Special reader macros
;; 2.5.14 Special writer macros
;; 2.5.15 Special compiler macros
;; 2.5.16 Special reader functions
;; 2.5.17 Special writer functions
;; 2.5.18 Special compiler functions
;; 2.5.19 Special reader macros
;; 2.5.20 Special writer macros
;; 2.5.21 Special compiler macros
;; 2.5.22 Special reader functions
;; 2.5.23 Special writer functions
;; 2.5.24 Special compiler functions
;; 2.5.25 Special reader macros
;; 2.5.26 Special writer macros
;; 2.5.27 Special compiler macros
;; 2.5.28 Special reader functions
;; 2.5.29 Special writer functions
;; 2.5.30 Special compiler functions
;; 2.5.31 Special reader macros
;; 2.5.32 Special writer macros
;; 2.5.33 Special compiler macros
;; 2.5.34 Special reader functions
;; 2.5.35 Special writer functions
;; 2.5.36 Special compiler functions
;; 2.5.37 Special reader macros
;; 2.5.38 Special writer macros
;; 2.5.39 Special compiler macros
;; 2.5.40 Special reader functions
;; 2.5.41 Special writer functions
;; 2.5.42 Special compiler functions
;; 2.5.43 Special reader macros
;; 2.5.44 Special writer macros
;; 2.5.45 Special compiler macros
;; 2.5.46 Special reader functions
;; 2.5.47 Special writer functions
;; 2.5.48 Special compiler functions
;; 2.5.49 Special reader macros
;; 2.5.50 Special writer macros
;; 2.5.51 Special compiler macros
;; 2.5.52 Special reader functions
;; 2.5.53 Special writer functions
;; 2.5.54 Special compiler functions
;; 2.5.55 Special reader macros
;; 2.5.56 Special writer macros
;; 2.5.57 Special compiler macros
;; 2.5.58 Special reader functions
;; 2.5.59 Special writer functions
;; 2.5.60 Special compiler functions
;; 2.5.61 Special reader macros
;; 2.5.62 Special writer macros
;; 2.5.63 Special compiler macros
;; 2.5.64 Special reader functions
;; 2.5.65 Special writer functions
;; 2.5.66 Special compiler functions
;; 2.5.67 Special reader macros
;; 2.5.68 Special writer macros
;; 2.5.69 Special compiler macros
;; 2.5.70 Special reader functions
;; 2.5.71 Special writer functions
;; 2.5.72 Special compiler functions
;; 2.5.73 Special reader macros
;; 2.5.74 Special writer macros
;; 2.5.75 Special compiler macros
;; 2.5.76 Special reader functions
;; 2.5.77 Special writer functions



;; 2.6 Rule 3: Quoting prevents evaluating

;; To stop newLISP evaluating something, quote it.
;; Compare these two lines:

(+ 2 2)
'(+ 2 2)

;; The ﬁrst line is a list which contains a function and two numbers. In the second line, the
;; list is quoted - preceded by a single quote or apostrophe ('). You don't need to put another
;; quote at the end, after the closing parenthesis, because one is suﬃcient.

(+ 2 2)
4
'(+ 2 2)
(+ 2 2)


;; When newLISP sees the second line, it thinks as follows:
;; "I want to create a number with the value 4.
;; So I do that.
;; 3.1.3 The environment
;; newLISP has a simple, clean, and consistent environment system, which makes it easy
;; to read and write. Here are some of the environment rules newLISP supports:

;; 2.7 Rule 1: Environment
;; newLISP supports environments, which are used to store and retrieve values of variables.
;; Here are some examples:

(define x 5)
x


;; When newLISP sees this, it thinks as follows:
;; "I want to create a variable x and assign the value 5 to it. Then I
;; want to retrieve the value of x."
;; So I do that, and the answer is 5.
;; 2.8 Rule 2: Nested environments
;; newLISP supports nested environments, which means that a variable can be
;; found in an environment that is nested within another environment.
;; Here are some examples:

(define x 5)
(define y 10)
(define (f)
  (define x 20)
  (+ x y))

;; For the ﬁrst expression, newLISP does its job as usual, and enthusiastically evaluates the
;; list, returning the number 4. But for the second expression, as soon as it sees the quotation
;; mark, newLISP doesn't even think about evaluating the list by adding the numbers; it just
;; returns the list, unevaluated.

;; This quotation mark does the same job in newLISP that opening and closing quotation
;; marks do in written English - they inform the reader that the word or phrase is not to be
;; interpreted normally, but treated specially in some way: a non-standard or ironic meaning,
;; perhaps, something spoken by another person, or something not to be taken literally.
;; So why do you want to stop newLISP evaluating things? You'll soon meet some examples
;; where you quote things to prevent newLISP thinking that the ﬁrst item in a list is a function.
;; For example, when you store information in a list, you don't want newLISP to evaluate them
;; in the usual way:

(2006 1 12)
("Arthur" "J" "Chopin")

; today's year/month/date
; someone's full name


;; When newLISP sees the second line, it thinks as follows:
;; "I want to create a list with the elements 2006, 1,
;; and 12. Then I want to create a list with the elements
;; "Arthur", "J", and "Chopin". Then I want to create
;; a list with today's year, month, and date."
;; So I do that, and the answer is a list with three elements.


;; 2.8 Rule 2: Nested environments
;; newLISP supports nested environments, which means that a variable can be
;; found in an environment that is nested within another environment.
;; Here are some examples:

(define x 5)
(define y 10)


;; When newLISP sees this, it thinks as follows:
;; "I want to create a variable x and assign the value 5 to it. Then I
;; want to create a variable y and assign the value 10 to it."
;; So I do that, and the answer is 5.
;; 2.9 Rule 3: Local variables
;; newLISP supports local variables, which means that a variable can only be
;; accessed within a particular function or block of code.
;; Here are some examples:

(define (f)
  (define x 5)
  x)

    
;; You don't want newLISP to look for functions called 2006 or "Arthur". Besides, 2006 isn't
;; a valid function name, because it starts with a digit, and function names can't start with
;; a double quotation mark, so in either case your program will stop with an error. Therefore
;; you quote the lists to stop their ﬁrst elements being used as functions rather than data:


;; When newLISP sees this, it thinks as follows:
;; "I want to create a function f that does the following:
;; 1. I want to create a variable x and assign the value 5 to it.
;; 2. Then I want to return the value of x."
;; So I do that, and the answer is 5.
;; 2.10 Rule 2: Local variables
;; newLISP supports local variables, which means that a variable can only be
;; accessed within a particular function or block of code.
;; Here are some examples:

  
;; newLISP's ability to treat expressions as data - and data as expressions - is discussed in
;; more detail later.
;; Use the vertical apostrophe (ASCII code 39) to quote lists and symbols. Sometimes, text
;; editors or other programs change these simple vertical apostrophes into curly quotation
;; marks. These don't do the same job, so you will have to change any smart quotes to
;; vertical apostrophes.


;; 2.11 Rule 3: Strings and symbols
;; In newLISP, strings are sequences of characters, and symbols are names. Here are some
;; examples:

"Hello, world!"
hello


;; When newLISP sees this, it thinks as follows:
;; "I want to create a string with the characters H, e, l, l, o
;; comma, space, w, o, r, l, d, exclamation mark. Then
;; I want to create a symbol called hello."
;; So I do that, and the answer is "Hello, world!".
;; 2.12 Rule 4: Numbers
;; In newLISP, numbers are either integers or floating-point numbers. Here are some
;; examples:

12345
3.14159

;; When newLISP sees this, it thinks as follows:
;; "I want to create an integer with the value 12345. Then I
;; want to create a floating-point number with the value 3.14159
;; So I do that, and the answer is 3.14159.
;; 2.13 Rule 5: Booleans
;; In newLISP, true and false are the only two values of the boolean type. Here
;; are some examples:


;; When newLISP sees this, it thinks as follows:
;; "I want to create a boolean with the value true. Then I want to create
;; a boolean with the value false."

;; 2.6.1 Symbols and quotes

;; A symbol is a newLISP thing with a name. You deﬁne something in your code and assign
;; a name to it. Then you can refer to that something later on, using the name rather than
;; the contents. For example, after typing this:

(set 'alphabet "abcdefghijklmnopqrstuvwxyz")


;; there's now a new symbol called alphabet whose value is a string consisting of the 26 letters
;; of the alphabet. The set function stores the string of characters from a to z in the symbol
;; alphabet. Now this symbol can be used elsewhere, and will evaluate to the alphabet whenever
;; it's used. Whenever you want to use the 26 letters of the alphabet, you use this symbol
;; without quoting it. For example, here's the upper-case function:

(upper-case alphabet)


;; When newLISP sees this, it thinks as follows:
;; "I want to create a function upper-case that takes one argument,
;; the symbol alphabet, and returns a new string that is the same as the argument,
;; but with all the letters converted to uppercase. Then I want to call the
;; upper-case function on the symbol alphabet, and return the result."
;; So I do that, and the answer is "ABCDEFGHIJKLMNOPQRSTUV
;; WXYZ".
;; 2.6.2 Quoting

;; I use the symbol without quoting it, because I want newLISP to use the value of the
;; symbol, not its name. I'm really not interested in upper-casing the word alphabet, but
;; the alphabet itself. newLISP hasn't permanently changed the value of the symbol, in this


;; When newLISP sees this, it thinks as follows:
;; "I want to create a function upper-case that takes one argument,
;; the symbol alphabet, and returns a new string that is the same as the argument,
;; but with all the letters converted to uppercase. Then I want to call the
;; upper-case function on the symbol alphabet, and return the result."
;; So I do that, and the answer is "ABCDEFGHIJKLMNOPQRSTUV
;; WXYZ".
;; 2.6.3 Symbols and quotes

;; example, because upper-case always creates and returns a new string, leaving the one
;; stored in the symbol unchanged.
;; Symbols correspond to variables in other programming languages. In fact, newLISP doesn't
;; use symbols quite as much as other languages use variables. This is partly because values
;; are continually being returned by expressions and fed directly into other expressions without
;; being stored. For example, in the following code each function hands its result directly to
;; the next, enclosing function:

(println (first (upper-case alphabet)))


;; When newLISP sees this, it thinks as follows:
;; "I want to create a function first that takes one argument, the string
;; "ABCDEFGHIJKLMNOPQRSTUVWXYZ", and returns the �
;; character from that string. Then I want to call the first function on the result of the
;; upper-case function on the symbol alphabet, and pass the result to the println function.
;; Then I want to call the println function on the result of the first function on the result
;; of the upper-case function on the symbol alphabet, and pass the result to the println
;; function, and so on."
;; So I do that, and the answer is the character "A".
;; 2.6.4 Quoting


;; When newLISP sees this, it thinks as follows:
;; "I want to create a function first that takes one argument, the string
;; "ABCDEFGHIJKLMNOPQRSTUVWXYZ", and returns the �
;; character from that string. Then I want to call the first function on the result of the
;; upper-case function on the symbol alphabet, and pass the result to the println function.
;; Then I want to call the println function on the result of the first function on the result
;; of the upper-case function on the symbol alphabet, and pass the result to the println
;; function, and so on."
;; So I do that, and the answer is the character "A".
;; 2.6.5 Symbols and quotes

;; example, because the println function always prints and returns nothing, leaving the
;; result of the first function unchanged.
;; Symbols correspond to variables in other programming languages. In fact, newLISP doesn't
;; use symbols quite as much as other languages use variables. This is partly because values
;; are continually being returned by expressions and fed directly into other expressions without
;; being stored. For example, in the following code each function hands its result directly to
;; the next, enclosing function:

(println (first (upper-case alphabet)))

;; upper-case gives its return value directly to ﬁrst, which gives its return value directly to
;; println, which both prints it and gives you the string it printed as the return value. So
;; there's less need to store values temporarily. But there are plenty of other places where you
;; do want symbols.
;; Here are two more examples of symbol-quoting:


;; When newLISP sees this, it thinks as follows:
;; "I want to create a function square that takes one argument, the number 5, and
;; returns the square of that number. Then I want to call the square function on the number
;; 5, and pass the result to the println function. Then I want to call the println
;; function on the result of the square function on the number 5, and pass the result to
;; the println function, and so on."
;; So I do that, and the answer is 25.
;; When newLISP sees this, it thinks as follows:
;; "I want to create a function cube that takes one argument, the number 5, and
;; returns the cube of that number. Then I want to call the cube function on the number
;; 5, and pass the result to the println function. Then I want to call the println
;; function on the result of the cube function on the number 5, and pass the result to
;; the println function, and so on."
;; So I do that, and the answer is 125.
;; 2.6.6 Quoting

;; example, because the println function always prints and returns nothing, leaving the
;; result of the cube function unchanged.
;; Symbols correspond to variables in other programming languages. In fact, newLISP doesn't
;; use symbols quite as much as other languages use variables. This is partly because values
;; are continually being returned by expressions and fed directly into other expressions without
;; being stored. For example, in the following code each function hands its result directly to
;; the next, enclosing function:

(define x (+ 2 2 ))
(define y '(+ 2 2))


;; In the ﬁrst example, I don't quote the (+ 2 2) list - newLISP evaluates this to 4 and then
;; assigns 4 to the symbol x, which evaluates to 4:


;; When newLISP sees this, it thinks as follows:
;; "I want to create a variable x and set it to the result of adding 2 to
;; 2. Then I want to create a variable y and set it to the list (+
;; 2 2). Then I want to print the value of x, and then print the
;; value of y."
;; So I do that, and the answer is:
;; x: 4
;; y: (+ 2 2)
;; 4
;; (+ 2 2)
;; 4

x
;-> 4
y
;-> (+ 2 2)

;; In the second example I quote the list. This means that the symbol y is now holding a list
;; rather than a number. Whenever newLISP sees the symbol y, it will return the list, rather
;; than 4. (Unless, of course, you quote y ﬁrst as well!)


y
;-> (+ 2 2)
'y
;-> y

;; 2.7.1 Symbols and quotes

;; example, because the println function always prints and returns nothing, leaving the
;; result of the cube function unchanged.
;; Symbols correspond to variables in other programming languages. In fact, newLISP doesn't
;; use symbols quite as much as other languages use variables. This is partly because values
;; are continually being returned by expressions and fed directly into other expressions without
;; being stored. For example, in the following code each function hands its result directly to
;; the next, enclosing function:

(define x (+ 2 2 ))
(define y '(+ 2 2))


;; By the way, throughout this document:
; the semicolon is the comment character
;-> that ";->" is my way of saying "the value is"

;; and output printed by the newLISP interpreter is usually shown
;; like this

;; 2.6.2 Setting and deﬁning symbols
;; There are various ways to create and set the value of symbols. You can use deﬁne or set,
;; as follows:

;; When newLISP sees this, it thinks as follows:
;; "I want to create a variable x and set it to the result of adding 2 to
;; 2. Then I want to create a variable y and set it to the list (+
;; 2 2)."
;; So I do that, and the answer is:
;; x: 4
;; y: (+ 2 2)
;; 4
;; (+ 2 2)
;; 4

(set 'x (+ 2 2))

;-> 4
(define y (+ 2 2))
;-> 4


;; set expects to be followed by a symbol, but evaluates its ﬁrst argument ﬁrst. So you should
;; either quote a symbol to prevent it being evaluated (because it might evaluate to something
;; other than a symbol), or supply an expression that evaluates to a symbol. deﬁne doesn't
;; expect the argument to be quoted.
;; You can also use setf and setq to set the value of symbols. These expect a symbol or a
;; symbol reference as the ﬁrst argument, so you don't have to quote it.


;; When newLISP sees this, it thinks as follows:
;; "I want to create a variable x and set it to the result of adding 2 to
;; 2. Then I want to create a variable y and set it to the list (+
;; 2 2)."
;; So I do that, and the answer is:
;; x: 4
;; y: (+ 2 2)
;; 4
;; (+ 2 2)
;; 4

(setf 'x (+ 2 2))

;-> 4
(set 'y (+ 2 2))
;-> 4

;; 2.7.2 The symbol function

;; example, because the println function always prints and returns nothing, leaving the
;; result of the cube function unchanged.
;; Symbols correspond to variables in other programming languages. In fact, newLISP doesn't
;; use symbols quite as much as other languages use variables. This is partly because values
;; are continually being returned by expressions and fed directly into other expressions without
;; being stored. For example, in the following code each function hands its result directly to
;; the next, enclosing function:

(define x (+ 2 2 ))
(define y '(+ 2 2))


;; These two functions (which have the same action) can set the contents of a symbol (variable),
;; a list, an array, or a string. A convention is to use setq when setting a symbol, and setf
;; when setting an element of a list or array.
;; deﬁne is also used to deﬁne functions. See Make your own functions3 .


;; When newLISP sees this, it thinks as follows:
;; "I want to create a variable x and set it to the result of adding 2 to
;; 2. Then I want to create a variable y and set it to the list (+
;; 2 2)."
;; So I do that, and the answer is:
;; x: 4
;; y: (+ 2 2)
;; 4
;; (+ 2 2)
;; 4


(defvar x (+ 2 2))

;-> 4
