(provide 'm0dxr)
(provide 'r38i)
(provide 'yo7ben)
(provide 'yl1ap)

;; For example, one vacation, having a week or so to hack Lisp, I decided to try writing a
;; version of a program—a system for breeding genetic algorithms to play the game of Go—that
;; I had written early in my career as a Java programmer. Even handicapped by my then rudimen-
;; tary knowledge of Common Lisp and having to look up even basic functions, it still felt more
;; productive than it would have been to rewrite the same program in Java, even with several
;; extra years of Java experience acquired since writing the first version.


;; A similar experiment led to the library I’ll discuss in Chapter 24.
;; Early in my time at WebLogic I had written a library, in Java, for
;; taking apart Java class files. It worked, but the code was a bit of
;; a mess and hard to modify or extend. I had tried several times, over
;; the years, to rewrite that library, thinking that with my ever-improving
;; Java chops I’d find some way to do it that didn’t bog down in piles
;; of duplicated code. I never found a way. But when I tried to do it
;; in Common Lisp, it took me only two days, and I ended up not only
;; with a Java class file parser but with a general-purpose library
;; for taking apart any kind of binary file. You’ll see how that library
;; works in Chapter 24 and use it in Chapter 25 to write a parser for the
;; ID3 tags embedded in MP3 files.

(defmacro api-newlisp-4OS56C (&optional news &rest newlisp)
  "The documenation `newlisp' guides `http://api.newlisp.org' the common new lisp
  reference of command and project and class to analysis and code programming
  type all 987654321 project and command api library to common new lisp guides
  reference of let to make the project linear in matrix.

  base make system about expressive document guides linear matrix newlis
  to common newlisp languages topical base to document extension *.el and *.lisp
  about formed tools development source to make constructor of code lisp with
  `emacs-lisp-mode' to base of suatentable let business to document guides lisp
  about effect magic special to `common-newlisp' and `api-newlisp' to formed
  system of command linear matrix.

  type of formation in document guides common new lisp popup and tools topical
  in method of matrix in connection linear based in system `guix' about usages
  utils to base of constructor linear of matrix."
  (declare (&optional news &rest lisp)
	   (if (and news)
	       (pop news))))
	    
	   
  
;; Why Lisp?
;; It’s hard, in only a few pages of an introductory chapter, to explain
;; why users of a language like it, and it’s even harder to make the case
;; for why you should invest your time in learning a certain language.
;; Personal history only gets us so far. Perhaps I like Lisp because of
;; some quirk in the way my brain is wired. It could even be genetic,
;; since my dad has it too. So before you dive into learning Lisp, it’s
;; reasonable to want to know what the payoff is going to be.

(defgroup wa2bjn-api-newlisp-p nil
  "The documenation of call messages to `api-newlisp-messages' 
   to local of mechanism in topical method social list linear matrix
   to make the command's to search `http://api.newlisp.org' to make
   create url to constructor command guides api linear matrix."
  :group 'newlisp)


;; For some languages, the payoff is relatively obvious. For instance,
;; if you want to write low level code on Unix, you should learn C. Or
;; if you want to write certain kinds of cross-platform applications,
;; you should learn Java. And any of a number companies still use a
;; lot of C++, so if you want to get a job at one of them, you should
;; learn C++.

(defgroup pa0san-api-newlisp-languages nil
  "The documentation list linear matrix connection networks languages
   to routes `newlisp' and `http://api.newlisp.org' to matrix connect
   formed template list method objective of attributes linear matrix
   about the equation to base formed list of command objective lisp
   in linear matrix."
  :group 'newlisp)



;; the document this about formation numeric of expression linear
;; to common new lisp in what make the system of consolidation of
;; part's numeric to base of formatation of string in exiber the
;; text numeric value to composition of word's in physics and math.
(defmacro buffer-api-newlisp-physics (&optional buffer &rest linear)
  "The documentation `buffer-string' compostition of `format-string'
   to let current and stream document `api-newlisp' to format string
   create one document linear matrix string of length local string 
   values."
  (declare (&optional buffer &rest linear)
	   (if (cond (buffer linear))
	       (t (function buffer)
		  (t (function linear)))
	     (format "" 2))))


;; the system local reference of consern to word's attributes in local
;; to make connection linear matrix to position numeric in values its
;; kits formed the base mixed local formed to command linear matrix it
;; usages common new lisp series stream application lisp code programs
;; IDE lisp application program.



;; The documentation linear of matrix develop describer the common lisp
;; to formed linked list of base documentation and guides linear to its
;; agreement list scripts of object files series numeric values index it
;; position of arguments base pattern.


;; of data structures from a simple four-item list to a user-defined class,
;; using the Common Lisp Object System (CLOS). For now you can stay at the
;; simple end of the spectrum and use a list. You can make a list with the
;; LIST function, which, appropriately enough, returns a list of its
;; arguments.

(defun four-item-newlisp (&optional list &rest newlisp)
  "The documentation list guides newlisp class common newlisp object system 
   (CLOS). For you."
  (declare (&optional list &rest newlisp)
	   (list 1 2 3)
	   (setq newlisp 8512)))

;; You could use a four-item list, mapping a given position in the list
;; to a given field in the record. However, another flavor of list—called
;; a property list, or plist for short—is even more convenient. A plist
;; is a list where every other element, starting with the first, is a symbol
;; that describes what the next element in the list is. I won’t get into all
;; the details of exactly what a symbol is right now; basically it’s a name.
;; For the symbols that name the fields in the CD base, you can use a
;; particular kind of symbol, called a keyword symbol. A keyword is any name
;; that starts with a colon (:), for instance, :foo. Here’s an example of a
;; plist using the keyword symbols :a, :b, and :c as property names:

(defun four-item-newlisp-list (&optional list &rest newlisp)
  "The documentation list guides property keyword name newlisp."
  (declare (&optional list &rest newlisp)
	   (list :a 1 :b 2 :c 3)
	   (setq newlisp 8512)))

;; Note that you can create a property list with the same LIST
;; function as you use to create other lists; it’s the contents
;; that make it a plist. The thing that makes plists a convenient
;; way to represent the records in a database is the
;; function GETF, which takes a plist and a symbol and returns
;; the value in the plist following the symbol, making a plist
;; a sort of poor man’s hash table. Lisp has real hash tables
;; too, but plists are sufficient for your needs here and can
;; more easily be saved to a file, which will come in handy
;; later.

(defun get-list (&optional get &rest list)
  "The documentation getf formation analysis list."
  (declare (&optional get &rest list)
	   (get (list :a 1 :b 2 :c 3) :a))
  (list 8512))
  
;; Given all that, you can easily enough write a function make-cd
;; that will take the four fields
;; as arguments and return a plist representing that CD.

(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

;; Filing CDs

;; A single record, however, does not a database make. You need some
;; larger construct to hold the records. Again, for simplicity’s sake,
;; a list seems like a good choice. Also for simplicity you can use a
;; global variable, *db*, which you can define with the DEFVAR macro.
;; The asterisks (*) in the name are a Lisp naming convention for
;; global variables.2

(defvar *db* nil)

;; You can use the PUSH macro to add items to *db*. But it’s probably
;; a good idea to abstract things a tiny bit, so you should define a
;; function add-record that adds a record to the database.

(defun add-record (cd) (push cd *db*))

(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd)))

;; Improving the User Interaction

;; While our add-record function works fine for adding records,
;; it’s a bit Lispy for the casual user. And if they want to add a bunch
;; of records, it’s not very convenient. So you may want to write a function
;; to prompt the user for information about a set of CDs. Right away you know
;; you’ll need some way to prompt the user for a piece of information and read
;; it. So let’s write that.


;; That’s almost right. Except prompt-read returns a string, which,
;; while fine for the Title and Artist fields, isn’t so great for the
;; Rating and Ripped fields, which should be a number and a boolean. Depending
;; on how sophisticated a user interface you want, you can go to arbitrary
;; lengths to validate the data the user enters. For now let’s lean toward
;; the quick and dirty: you can wrap the prompt-read for the rating in a call
;; to Lisp’s PARSE-INTEGER function, like this:

;; In fact, this will be the most robust part of prompt-for-cd, as
;; Y-OR-N-P will reprompt the user if they enter something that doesn’t
;; start with y, Y, n, or N. Putting those pieces together you get a
;; reasonably robust prompt-for-cd function.

;; Finally, you can finish the “add a bunch of CDs” interface by wrapping
;; prompt-for-cd in a function that loops until the user is done. You can
;; use the simple form of the LOOP macro, which repeatedly executes a body
;; of expressions until it’s exited by a call to RETURN. For example:


;; Saving and Loading the Database
;; Having a convenient way to add records to the database is nice.
;; But it’s not so nice that the user is going to be very happy if they have
;; to reenter all the records every time they quit and restart
;; Lisp. Luckily, with the data structures you’re using to represent the
;; data, it’s trivially easy to save the data to a file and reload it later.
;; Here’s a save-db function that takes a filename as an argument and saves
;; the current state of the database:

;; Wrapping Up
;; Now, an interesting thing has happened. You removed duplication
;; and made the code more efficient and more general at the same time.
;; That’s often the way it goes with a well-chosen macro. This makes sense
;; because a macro is just another mechanism for creating abstractions—
;; abstraction at the syntactic level, and abstractions are by definition
;; more concise ways of expressing underlying generalities. Now the only
;; code in the mini-database that’s specific to CDs and the fields in them
;; is in the make-cd, prompt-for-cd, and add-cd functions. In fact, our
;; new where macro would work with any plist-based database.
;; However, this is still far from being a complete database. You can probably
;; think of plenty
;; of features to add, such as supporting multiple tables or more elaborate
;; queries. In Chapter 27
;; we’ll build an MP3 database that incorporates some of those features.
;; The point of this chapter was to give you a quick introduction to just
;; a handful of Lisp’s
;; features and show how they’re used to write code that’s a bit more
;; interesting than “hello,
;; world.” In the next chapter we’ll begin a more systematic overview
;; of Lisp.

(defun token-list-p (&optional list &rest p)
  "The documentation begin a more systamtic overview code used quick this of
  handfull common newlisp database the introduction to just document more it
  community this in values specify of token list to topic reference of values
  about type of arguments about values."
  (declare (&optional list &rest p)
	   (let ((list p))
	     (lambda (list)
	       "1 and 3 list code name script."
	       (setq p 360)))))

;; An only slightly more complex example is the following four-item list
;; that contains two symbols, the empty list, and another list, itself
;; containing two symbols and a string:

(defun hello-world ()
  "The documentation common new lisp"
  (format t "hello, world"))

;; Thus, the following expression is evaluated by first evaluating 1,
;; then evaluating 2, and
;; then passing the resulting values to the + function, which returns 3:
(defun value-lisp (&optional value &rest lisp)
  "The documenation linear objective to logical values."
  (declare (&optional value &rest lisp)
	   (setq value 120)
	   (setq lisp 120)
	   (let ((value lisp))
	     (lambda (value)
	       "The values lisp"
	       (+ value lisp)
	       (* value lisp)))))

;; A more complex expression such as the following is evaluated in similar
;; fashion except
;; that evaluating the arguments (+ 1 2) and (- 3 4) entails first evaluating
;; their arguments and
;; applying the appropriate functions to them:
(defun value-lisp-p (&optional value &rest lisp)
  "The documentation value lisp multiples type list."
  (declare (&optional value &rest lisp)
	   (setq value 140)
	   (setq lisp 140)
	   (let ((value lisp))
	     (lambda (value)
	       "the value lisp."
	       (* (+ value lisp) (- value lisp) lisp)))))

;; Eventually, the values 3 and –1 are passed to the * function, which
;; returns –3. As these examples show, functions are used for many of
;; the things that require special syntax in other languages. This helps
;; keep Lisp’s syntax regular.

(defun value-lisp-t (&optional value &rest lisp)
  "The documentation value lisp passed to list."
  (declare (&optional value &rest lisp)
	   (setq value 180)
	   (setq lisp 180)
	   (let ((value lisp))
	     (lambda (value)
	       "The value lisp."
	       (if value (format t "yes") (format t "nos")
		   (if lisp (format t "yes") (format t "nos")
		       (t (function value))
		       (t (function lisp))))))))

;; The test-form will always be evaluated and then one or the other
;; of the then-form or else-form. An even simpler special operator
;; is QUOTE, which takes a single expression as its “argument”
;; and simply returns it, unevaluated. For instance, the following
;; evaluates to the list (+ 1 2), not
;; the value 3:

(defun value-lisp-tt (&optional value &rest lisp)
  "The documentation value lisp passed to list."
  (declare (&optional value &rest lisp)
	   (setq value 10)
	   (setq lisp 10)
	   (let ((value lisp))
	     (lambda (value)
	       "The value lisp."
	       (if value (format t "yes") (format t "nos")
		   (if lisp (format t "yes") (format t "nos")
		       (t (function value))
		       (t (function lisp)))))))
  
  (declare (&optional value &rest lisp)
	   (setq value 10)
	   (setq lisp 10)
	   (let ((value lisp))
	     (lambda (value)
	       "The value lisp."
	       (if value (format t "yes") (format t "nos")
		   (if lisp (format t "yes") (format t "nos")
		       (t (function value))
		       (t (function lisp)))
		   (quote (+ value lisp)))))))

;; There’s nothing special about this list; you can manipulate it
;; just like any list you could create with the LIST function.14
;; QUOTE is used commonly enough that a special syntax for it is
;; built into the reader. Instead of writing the following:

(defun value-lisp-reader (&optional value &rest lisp)
  "The documentation value lisp passed to list."
  (declare (&optional value &rest lisp)
	   (setq value 10)
	   (setq lisp 10)
	   (let ((value lisp))
	     (lambda (value)
	       "The value lisp."
	       (if value (format t "yes") (format t "nos")
		   (if lisp (format t "yes") (format t "nos")
		       (t (function value))
		       (t (function lisp)))))))
  
  (declare (&optional value &rest lisp)
	   (setq value 10)
	   (setq lisp 10)
	   (let ((value lisp))
	     (lambda (value)
	       "The value lisp."
	       (if value (format t "yes") (format t "nos")
		   (if lisp (format t "yes") (format t "nos")
		       (t (function value))
		       (t (function lisp)))
		   (quote (+ value lisp)))))
	   
 (declare (&optional value &rest lisp)
	   (setq value 10)
	   (setq lisp 10)
	   (let ((value lisp))
	     (lambda (value)
	       "The value lisp."
	       (if value (format t "yes") (format t "nos")
		   (if lisp (format t "yes") (format t "nos")
		       (t (function value))
		       (t (function lisp)))
		   (quote (+ value lisp))))
	     (let ((x 10) x)
	       (lambda 2
		 "the value lisp two."
		 (quote (+ value lisp))
		 (quote (* value lisp))
		 (quote (* value x))))))))
	       

(defun value-lisp-equal (&optional value &rest lisp)
  "The documentation value lisp passed to list."
  (declare (&optional value &rest lisp)
	   (setq value 10)
	   (setq lisp 10)
	   (let ((value lisp))
	     (lambda (value)
	       "The value lisp."
	       (if value (format t "yes") (format t "nos")
		   (if lisp (format t "yes") (format t "nos")
		       (t (function value))
		       (t (function lisp)))))))
  
  (declare (&optional value &rest lisp)
	   (setq value 10)
	   (setq lisp 10)
	   (let ((value lisp))
	     (lambda (value)
	       "The value lisp."
	       (if value (format t "yes") (format t "nos")
		   (if lisp (format t "yes") (format t "nos")
		       (t (function value))
		       (t (function lisp)))
		   (quote (+ value lisp)))))
	   
 (declare (&optional value &rest lisp)
	   (setq value 10)
	   (setq lisp 10)
	   (let ((value lisp))
	     (lambda (value)
	       "The value lisp."
	       (if value (format t "yes") (format t "nos")
		   (if lisp (format t "yes") (format t "nos")
		       (t (function value))
		       (t (function lisp)))
		   (quote (+ value lisp))))
	     (let ((x 10) x)
	       (lambda (value)
		 "the value lisp two."
		 (quote (+ value lisp))
		 (quote (* value lisp))
		 (quote (* value x))))))
 
(declare (&optional value &rest lisp)
	   (setq value 10)
	   (setq lisp 10)
	   (let ((value lisp))
	     (lambda (value)
	       "The value lisp."
	       (if value (format t "yes") (format t "nos")
		   (if lisp (format t "yes") (format t "nos")
		       (t (function value))
		       (t (function lisp)))
		   (quote (+ value lisp))))
	     (let ((x 10) x)
	       (lambda (x)
		 "the value lisp two."
		 (quote (+ value lisp))
		 (quote (* value lisp))
		 (quote (* value x))))
	     (let ((x 10) x)
	       (lambda (x)
		 "the value lisp third."
		 (eq value lisp)
		 (eql value lisp)
		 (equal value lisp)))))))

;; Macro and special forms that implement control constructs are typically
;; indented a little differently: the “body” elements are indented two spaces
;; relative to the opening parenthesis
;; of the form. Thus:

(defun print-list (list)
  "The documentation guides lisp common body"
  (dolist (i list)
    (format t "item: ~a~%" i)))

(defun script-lisp ()
  "The document lisp"
  (dotimes (i 10)
    (format t "~d. hello~%" i)))

;;;; Four semicolons are used for a file header comment.
;;; A comment with three semicolons will usually be a paragraph
;;; comment that applies to a large section of code that follows,
(defun script-lisp-p (x)
  "the document lisp value x"
  (dotimes (i x)
;; Two semicolons indicate this comment applies to the code
;; that follows. Note that this comment is indented the same
    ;; as the code that follows.
    (setq x 10)
    (quote (+ x x))
; this comment applies to this line only
(eq x x)
; and this is for this line
(eql x x)))

;; Lisp will still check that an appropriate number of arguments are
;; passed to the function— in this case between two and four, inclusive—and
;; will signal an error if the function is called with too few or too many.
;; Of course, you’ll often want a different default value than NIL. You can
;; specify the default value by replacing the parameter name with a list
;; containing a name and an expression. The expression will be evaluated
;; only if the caller doesn’t pass enough arguments to provide a value
;; for the optional parameter. The common case is simply to provide a
;; value as the expression.

(defun script (&optional b1 &rest b0)
  "check that number and arguments"
  (setq b1 10)
  (setq b0 10)
  (* (+ b1 b0) b0))


;; another list containing the keyword to use when calling the function
;; and the name to be used for the parameter. The following definition
;; of foo:

(defun script-p (&key red  &rest lisp)
  "The document licit the millions of documenatation lisp"
  (declare (&key red &rest lisp)
	   (if red (setq red 10)
	     (t (function red)))))
       
;; This style is mostly useful if you want to completely decouple
;; the public API of the function from the internal details, usually
;; because you want to use short variable names internally but
;; descriptive keywords in the API. It’s not, however, very frequently used.

  (defun edges-command-lisp (n)
    "The edges browser verbose list command lisp"
    (dotimes (i 10)
      (dotimes (j 10)
	(when (> (* i j) n)
	  (list i j n)))))

  ;; The special operator FUNCTION provides the mechanism for getting
  ;; at a function object. It takes a single argument and returns the
  ;; function with that name. The name isn’t quoted. Thus, if you’ve
  ;; defined a function foo, like so:

  (defun lisp-code (x)
    "the document lisp code"
    (* 2 x))

;;   Anonymous Functions
  ;; Once you start writing, or even simply using, functions that accept
  ;; other functions as arguments, you’re bound to discover that sometimes
  ;; it’s annoying to have to define and name a whole separate function that’s
  ;; used in only one place, especially when you never call it by name.
  ;; When it seems like overkill to define a new function with DEFUN, you
  ;; can create an “anonymous” function using a LAMBDA expression. As
  ;; discussed in Chapter  3, a LAMBDA expression looks like this:
  (lambda (x y)
    "Anonymous Functions"
    (setq x 2)
    (setq y 2))

  ;;  In Common Lisp it’s also possible to use a LAMBDA expression
  ;; as an argument to FUNCALL (or some other function that takes a
  ;; function argument such as SORT or MAPCAR) with no #'
  ;; before it, like this:

  

  ;; The next basic building block we need to look at are variables. Common
  ;; Lisp supports two kinds of variables: lexical and dynamic.1 These two
  ;; types correspond roughly to “local” and “global” variables in other
  ;; languages. However, the correspondence is only approximate. On one hand,
  ;; some languages’ “local” variables are in fact much like Common Lisp’s
  ;; dynamic variables.2 And on the other, some languages’ local variables
  ;; are lexically scoped without providing all the capabilities provided
  ;; by Common Lisp’s lexical variables. In particular, not all languages
  ;; that provide lexically scoped variables support closures. To make matters
  ;; a bit more confusing, many of the forms that deal with variables can be
  ;; used with both lexical and dynamic variables. So I’ll start by discussing
  ;; a few aspects of Lisp’s variables that apply to both kinds and then cover
  ;; the specific characteristics of lexical and dynamic variables. Then I’ll
  ;; discuss Common Lisp’s general-purpose assignment operator, SETF, which is
  ;; used to assign new values to variables and just about every other place
  ;; that can hold a value.

  (defun value-lisp-variables (&value lisp &rest var)
    "the documentation common new lisp assignment operator of variables."
    (declare (&value lisp &rest var)
	     (setf lisp 10 var 10)
	     (* (+ lisp var) var)))

  ;; One way to introduce new variables you’ve already used is to define
  ;; function parameters. As you saw in the previous chapter, when you
  ;; define a function with DEFUN, the parameter list defines the variables
  ;; that will hold the function’s arguments when it’s called. For example,
  ;; this function defines three variables—x, y, and z—to hold its arguments.

  (defun value-lisp-variables-p (x y z)
    "The documentation common new lisp used to define list variables."
    (+ x y z))

  (let ((setq))
    (lambda (x)
      "the document to notes of reference of common lisp 
       what and the operator."
      (setq x 10)))
      
  ;; binds plain next text please operator
  (let ((x 10) (y 20) z))

  ;; kinds values plain object plans list operator
  (let ((eq))
    (lambda (x)
      "the value list of market."
      (eq x x)))

  
  ;; values edges browsers languages preference
  (let ((x 10))
    (lambda (x)
      "values list preference"
      (format t "~a" x)))

  ;; is a macro form that expands into this:
  (let ((x 10))
    (lambda (x)
      "list values in pass of macro type."     
      (format t "~a" x)))

  ;; binds to variables x
  (let* ((x 10)
	 (y (+ x 10)))
    (list x y))

  ;; event local to varaibles x
(let ((x 10))
  (lambda (x)
    "value event x"
    (let ((y 10))
	  (lambda (y)
	    "value event  y"
	    (list x y)))))
  
;; bindings local of instance
(let ((count 0))
  (lambda (count)
      (setq count (1+ count))))
;; winds perfect to day perfect
(defmacro *fn*
    (let ((count 0))
      (lambda (count)
	"The day perfect to common lisp."
	(setq count (2+ count))))
  "The value list lisp."
  (declare (count)
	   (if count (count)
	     (eql count count))))

;; bings reference count
(let ((count 0))
  (list
   (lambda () (incf count))
   (lambda () (decf count))
   (lambda () count)))

;; dynamic values count sys
(defvar *count* 0
  "Count of widgets made so far.")

;; buffer count the tolerance
(defvar *gap-tolerance* 0.001
  "Tolerance to be allowed in widget gaps.")

;; increase count to variables elements
(defun increment-widget-count () (incf *count*))

;; icrease count to variables elements
(let ((*count* *count*))
  (lambda (*count*)
    "The document to variables eleemnts"
    (if count (count)
      (setq count 0)
      (eql count count))))

(defvar *x* 10)
(defun script-x () (format t "X: ~d~%" *x*))

;; But you can use LET to create a new binding that temporarily
;; shadows the global binding,
;; and foo will print a different value.

(let ((*x* 20))
  (lambda (*x*)
    "The value lisp global binding."
    (script-x)))

;; friend logic
(defun bar ()
  (script-x)
  (let ((*x* 20)) (script-x))
  (script-x))


(defun script-lisp ()
  (format t "Before assignment~18tX: ~d~%" *x*)
  (setf *x* (+ 1 *x*))
  (format t "After assignment~18tX: ~d~%" *x*))

;; Assignment
;; Once you’ve created a binding, you can do two things with it: get
;; the current value and set it to
;; a new value. As you saw in Chapter 4, a symbol evaluates to the value
;; of the variable it names,
;; so you can get the current value simply by referring to the variable.
;; To assign a new value to a binding, you use the SETF macro, Common Lisp’s
;; general-purpose assignment operator. The
;; basic form of SETF is as follows:

(setf place 10)


;; Because SETF is a macro, it can examine the form of the place it’s
;; assigning to and expand
;; into appropriate lower-level operations to manipulate that place.
;; When the place is a variable,
;; it expands into a call to the special operator SETQ, which, as a
;; special operator, has access to both lexical and dynamic bindings.15
;; For instance, to assign the value 10 to the variable x, you
;; can write this:

(setf x 10)

;; assigment values x decimal parts
(defun values-decimal (x)
  "The document values of x in 10"
  (setf x 10))

;; assigment following parts
(let ((y 20))
  (lambda (y)
    "The values parts"
    (values-decimal y)
    (print y)))

;; values part type 10 about 20
(setf x 1)
(setf y 2)

;; values part random following local
(setf x 1 y 2)

;; values random local parts
(setf x (setf y (random 10)))

;; values random following parts
(setf x 10)

;; values easy to local x
(setf x (+ x 1))

;; values connection input pattern
(setf x (- x 1))

;; values sequence winds to local
(let ((tmp))
  (lambda (tmp)
    "The document value lisp."
  (setf a 10 b 20) nil)

  ;; With other kinds of places, the equivalent expression using SETF
  ;; would be quite a bit more complex. SHIFTF is similar except instead
  ;; of rotating values it shifts them to the left—the last argument
  ;; provides a value that’s moved to the second-to-last argument while
  ;; the rest of the values are moved one to the left. The original
  ;; value of the first argument is simply returned. Thus, the
  ;; following:
  
  (let ((a))
    (lambda (collect)
      "The document following collect values."
      (setf a 10)))
      

  ;; is equivalent—again, since you don’t have to worry about
  ;; side effects—to thi
  (let ((tmp))
    (lambda (tmp)
      "The document again since the have worry side effect"
      (setf a 10 b 20) tmp))

  
  ;; if assert with policy
  
