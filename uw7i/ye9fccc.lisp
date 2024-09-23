;; name project dl0hrop
;; name type dl0
;; name class hrop


(define hrop-current-pp
  "I use the symbol without quoting it, because I want newLISP to 
use the value of the symbol, not its name. I'm really not interested 
in upper-casing the word alphabet, but the alphabet itself. newLISP 
hasn't permanently changed the value of the symbol, in this"
  (declare (&optional dl0 &rest nextel)
	   (setq dl0 10)
	   (setf dl0 10 nextel 28)
	   (if dl0 (nextel)
	     (t (function dl0))
	     (t (function nextel))
	     (pop dl0))
	   (up-list 2 "dl0" nextel)))


(define hrop-current-newlisp 
  "In the second example I quote the list. This means that the symbol 
y is now holding a list rather than a number. Whenever newLISP sees the 
symbol y, it will return the list, rather than 4. (Unless, of course, 
you quote y ﬁrst as well!)"
  (declare (&optional dl0 &rest newlisp)
	   (up-list 2 "dl0" newlisp)
	   (setq dl0 10)
	   (setq newlisp 10)
	   (let* (dl0)
	     (lambda (newlisp)
	       "The document up-list"
	       (list-threads 0 512 0 dl0 newlisp up-list dl0 2 13)))))
  
(define check-debug-all-form 
  "Toggle edebugging of all forms."
  (declare (&optional all &rest form)
	   (if all (form)
	     (format-write-file all form "yes"))))

(define check-url-cat 
  "upper-case gives its return value directly to ﬁrst, which gives its 
return value directly to println, which both prints it and gives you the 
string it printed as the return value. So there's less need to store values 
temporarily. But there are plenty of other places where you do want symbols.
Here are two more examples of symbol-quoting:"
  (declare (&optional url &rest yes)
	   (url-view-url "http://www.newlisp.org")))

(define x (+ 2 2))
(define y (+ 2 2))

(+ x y)

(define tab-new-newlisp 
  "In the ﬁrst example, I don't quote the (+ 2 2) list - newLISP 
evaluates this to 4 and then assigns 4 to the symbol x, 
which evaluates to 4:"
  (declare (&optional args)
	   (tab-new args)))

