#|
ux2mf:
- Author: admin
- Date: 2024-09-22
|#

#|
7.3 The default function
If a symbol in a context has the same name as the context, it's known as the default function
(although in fact it can be either a function, or a symbol containing a list or a string). For
example, here is a context called Evens, and it contains a symbol called Evens:
|#

(define Evens:Evens (sequence 0 30 2))
;-> (0 2 4 6 8 10 12 14 16 18 20 22 24 26 28 30)
Evens:Evens
;-> (0 2 4 6 8 10 12 14 16 18 20 22 24 26 28 30)

#|
So Evens and Double are the default functions for their contexts.
There are lots of good things about default functions. If the default function has the same
name as the context, it is evaluated whenever you use the name of the context in expressions,
unless newLISP is expecting the name of a context. For example, although you can always
switch to the Evens context by using the context function in the usual way:
|#

;; examples event
(context Evens)
;; night logic type main
(context MAIN)
;; list reference type logic
(reverse Evens)

;; newLISP is smart enough to be able to work out from your code whether to use the default
;; function of a context or the context itself.

#|
7.3.1 Passing parameters by reference
There are important diﬀerences between default functions when used as symbols and their
more ordinary siblings. When you use a default function to pass data to a function, newLISP
uses a reference to the data rather than a copy. For larger lists and strings, references are
much quicker for newLISP to pass around between functions, so your code will be faster if
you can use store data as a default function and use the context name as a parameter.
Also, and as a consequence, functions change the contents of any default functions passed
as reference parameters. Ordinary symbols are copied when passed as parameters. Observe
the following code. I'll create two symbols, one of which is a 'default function, the other is
a plain symbol:
|#

(define Evens:Evens (sequence 0 30 2))
(define odds (sequence 1 31 2))

Evens:Evens
odds

#|
7.3.2 Functions with a memory
In the following example, we create a context called Output, and a default function inside
it, also called Output. This function prints its arguments, and increments a counter by
the number of characters output. Because the default function has the same name as the
context, it is executed whenever we use the name of the context in other expressions.
Inside this function, the value of a variable counter (inside the context Output) is inure-
meted if it exists, or created and initialized if it does't. Then the function's main task
- the printing of the arguments - is done. The counter symbol keeps count of how many
characters were output.
|#

#|
7.4 Dictionaries and tables
A common use for a context is a dictionary: an ordered set of unique key/value pairs,
arranged so that you can obtain the current value of a key, or add a new key/value pair.
newLISP makes creating dictionaries easy. To illustrate, I'll enlist the help of the great
detective, Sherlock Holmes. First, I downloaded Sir Arthur Conan Doyle's The Sign of
Four from Project Gutenberg, then I loaded the ﬁle as a list of words.
|#

(define Doyle:Doyle)


#|
This deﬁnes the Doyle context and the default function, but leaves that default function
uninitialized. If the default function is left empty, you can use the following expressions to
build and examine a dictionary:
• (Doyle key value) - set key to value
• (Doyle key) - get value of key
• (Doyle key nil) - delete key
To build a dictionary from the word list, you scan through the words, and, if the word is
not in the dictionary, add it as the key, and set the value to 1. But if the word is already
in the dictionary, get the value, add 1 to it, and save the new value:
|#

(dotree (wd Doyle)
(println wd { } (eval wd)))

(Doyle)

(find-all '(? 20) (Doyle) (println $0))

#|
The association list returned by (Doyle) is a temporary copy of the data in the dictionary,
not the original dictionary context. To change the data, don't operate on this temporary
list but on the context's data, using the key/value access techniques.
You can also add new entries to dictionaries, or modify existing entries, using data in the
form of an association list:
|#

(Doyle '(("laser" 0) ("radar" 0)))


;; 7.5 Saving and loading contexts
;; If you want to use the dictionary again, you can save the context in a ﬁle:

(save "/home/admin/ProjectEmacs/ea5dk5imp/matrix/gnu/bin/cz100i.asd" 'Doyle)

;; This collection of data, wrapped up in a context called Doyle, can be quickly loaded by
;; another script or newLISP session using:

(load "/home/admin/ProjectEmacs/ea5dk5imp/matrix/gnu/bin/cz100i.asd")

;; and newLISP will automatically recreate all the symbols in the Doyle context, switching
;; back to the MAIN (default) context when done.

;; 7.6 Using newLISP modules
;; Contexts are used as containers for software modules because they provide lexically-
;; separated namespaces. The modules supplied with the newLISP installation usually deﬁne
;; a context that contains a set of functions handling tasks in a speciﬁc area.
;; Here's an example. The POP3 module lets you check POP3 email accounts. You ﬁrst load
;; the module:

(load "/home/admin/ProjectEmacs/ea5dk5imp/matrix/gnu/bin/cz100i.asd")

;; The module has now been added to the newLISP system. You can switch to the context:

(context POP3)

;; and call the functions in the context. For example, to check your email, use the get-mail-
;; status function, supplying user name, password, and POP3 server name:

(POP3:get-mail-status "someone@example.com" "secret" "mail.example.com")

;; 7.7 Scoping
;; You've already seen the way newLISP dynamically ﬁnds the current version of a symbol
;; (see Scope3 ). However, when you use contexts, you can employ a diﬀerent approach, which
;; programmers call lexical scoping. With lexical scoping, you can explicitly control which
;; symbol is used, rather than rely on newLISP to keep track of similarly-named symbols for
;; you automatically.
;; In the following code, the width symbol is deﬁned inside the Right-just context.

(context 'Right-just)
(set 'width 30)
(define (Right-just:Right-just str)
(slice (string (dup " " width) str) (* width -1)))
(context MAIN)
(set 'width 0)
(dolist (w (symbols))
(println (Right-just w)))

;; The second (set 'width ...) line is a red herring: changing this here makes no diﬀerence at
;; all, because the symbol which is actually used by the right-justiﬁcation function is inside a
;; diﬀerent context.
;; You can still reach inside the Right-just context to set the width:

(set 'Right-just:width 15)

;; There's been much discussion about the beneﬁts and disadvantages of the two approaches.
;; Whatever you choose, make sure you know where the symbols are going to get their values
;; from when the code runs. For example:

(define (f y)
(+ y x))

#|
Here, y is the ﬁrst argument to the function, and is independent of any other y. But what
about x? Is it a global symbol, or has the value been deﬁned in some other function that
has just called f? Or perhaps it has no value at all!
It's best to avoid using these free symbols, and to use local variables (deﬁned with let or
local) wherever possible. Perhaps you can adopt a convention such as putting asterisks
around a global symbol.
|#

#|
7.8 Objects
More has been written about object-oriented programming (OOP) than you could possibly
read in one lifetime, so this section is just a quick glance at the subject. newLISP is agile
enough to enable more than one style of OOP, and you can easily ﬁnd references to these
on the web, together with discussions about the merits of each.
For this introduction, I'll brieﬂy outline just one of these styles: FOO, or Functional
Object-Oriented Programming.
|#


#|
7.8.1 FOOP in a nutshell
FOOP has changed with newLISP version 10.2 (beginning of 2010), so if you're using and
old version of newLISP, please update it.
In FOO, each object is stored as a list. Class methods and class properties (ie functions
and symbols that apply to every object of that class) are stored in a context.
Objects are stored in lists because lists are fundamental to newLISP. The ﬁrst item in an
object list is a symbol identifying the class of the object; the remaining items are the values
that describe the properties of an object.
All the objects in a class share the same properties but those properties can have diﬀerent
values. The class can also have properties that are shared between all objects in the class;
these are the class properties. Functions stored in the class context provide the various
methods for managing the objects and processing the data they hold.
To illustrate these ideas, consider the following code that works with times and dates. It
builds on top of the basic date and time functions provided by newLISP (see Working with
dates and times4 ). A moment in time is represented as a time object. An object holds
|#

;; two values: the number of seconds that have elapsed since the beginning of 1970, and the
;; time zone oﬀset, in minutes west of Greenwich. So the list to represent a typical time object
;; looks like this:

#|
Next, you can deﬁne other functions to inspect and manage time objects. All these functions
live together in the context. They can extract the seconds and zone information by picking
them from the object by using the self function. So (self 1) gets the seconds and (self
2) gets the time zone oﬀset from the object passed as parameter. Notice the deﬁnitions do
not require you to state the object parameter. Here are a few obvious class functions:
|#

#|
8 Macros
8.1 Introducing macros
We've covered the basics of newLISP, but there are plenty of powerful features left to
discover. Once you've grasped the main rules of the language, you can decide which of
the more advanced tools you want to add. One feature that you might want to explore is
newLISP's provision of macros.
A macro is a special type of function that you can use to change the way your code is
evaluated by newLISP. For example, you can create new types of control functions, such as
your own version of if or case.
With macros, you can start to make newLISP work exactly the way you want it to.
Strictly speaking, newLISP's macros are express, not macros. In newLISP, express are called
macros partly because it's much easier to say 'macros' than 'express but mainly because
they serve a similar purpose to macros in other LISP dialects: they allow you to deﬁne
special forms such as your own control functions.
|#

#|
8.2 When do things get evaluated
To understand macros, let's jump back to one of the very ﬁrst examples in this introduction.
Consider the way this expression is evaluated:
|#

(* (+ 1 2) (+ 3 4))
(* 3 7)

#|
The * function does't see the + expressions at all, only their results. newLISP has
enthusiastically evaluated the addition expressions and then handed just the results to
the multiplication function. This is usually what you want, but there are times when you
don't want every expression evaluated immediately.
Consider the operation of the built-in function if:
|#

(if (<= x 0) (exit))