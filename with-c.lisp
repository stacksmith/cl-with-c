(in-package :with-c)


;;==========================================================================					;

(defun cffitype-symbol (cstruct)
  (slot-value cstruct 'CFFI::NAME))


;; as-string = convert a thing into a string
;; listify = make a (thing) list, or just return things as a list;
;; catstring = concatenate as-string on a list
;; symbolicate = intern a symbol with a catstringd name.

(defmacro listify (thing)
  `(let ((thing ,thing))
     (if (listp thing)
	 thing
	 (list thing))))

;;------------------------------------------------------------------------------
;; create a single string from a thing, converting named objects such as
;; packages, symbols and foreign-types into strings.
;; Anything else? format it!
;; 
;;
(defun as-string (thing)
  "Create a string from thing.  If thing is a list or tree, flatten"
  (typecase thing
    (string thing)
    (symbol (symbol-name thing))
    (package (package-name thing))
    (cffi::foreign-type  (symbol-name (cffitype-symbol thing)))
    (t (format nil "~S" thing)))  )

;;------------------------------------------------------------------------------
;; catstring    create a string from a bunch of things
;;         Macro to avoid evaluation, so (catstring x y z) works
(defun catstring (&rest things)
  "Convert every thing to a string, and return concatenation."
  (apply #'concatenate 'string (mapcar #'as-string things)))
;;
;; symbolicate - create a symbol from a bunch of things
(defmacro symbolicate (&rest things)
  `(intern (catstring ,@things)))

;; get the name from cstruct class

;; Return a cstruct, that is an actual <CFFI-CLASS> object, given one of
;; - cstruct - just return it
;; - 'symbol if unbound, parse as bare type; if bound, get value and reparse.
;; - (...) - parse-type

;;==============================================================================
(defparameter *with-c-ban-list* nil)
;; Note that ranges reflect 'positions' in the text buffer.
(define-condition with-c-error (simple-error)
  ((msg   :reader with-c-error-msg
	  :initarg :msg :initform ""))
  (:report (lambda (condition stream)
	     (setf *with-c-ban-list* nil)
             (format stream "WITH-C Error: ~A"
                     (with-c-error-msg  condition)))))
;;==============================================================================
; convert to a cffitype
;
(defun as-cffitype (foreign-type-designator)
  (if (typep foreign-type-designator 'cffi::foreign-type)
      foreign-type-designator
      (if (and (symbolp foreign-type-designator)
	       (boundp foreign-type-designator))
          (symbol-value foreign-type-designator)
	  (cffi::parse-type foreign-type-designator))))

(defun cffitype-package (cffitype)
  (symbol-package (type-of cffitype)))

(defun find-symbol-or-die (name package &rest rest)
  (or (find-symbol name package)
      (apply #'error rest)))



;;==============================================================================
;;==============================================================================
;;==============================================================================
;;-----------------------------------------------------------------------------
;; In home-package, create a list of symbols matching struct's slotnames,
;; interning symbols as needed.
(defun default-slot-bindings (cffitype)
  (loop for slot in (foreign-slot-names cffitype)
     collect (symbolicate slot)))
;;-----------------------------------------------------------------------------
;; Given a list of proposed bindings created in user-package, create a list of
;; actual bindings, with slots in struct-package, using the following rules:
;;
;; name  (name xxx::name)
;; (:pointer name) (name :pointer xxx::name)
;; (name :pointer slotname) (name :pointer xxx::slotname)
;; (name slotname) (name xxx::slotname);;
;;
;; banlist is a list of local symbols that must not be used in bindings to avoid
;; naming accessors same as instance or such...
(defun fix-slot-bindings (bindings prefix cffitype instance )
  (let* ((cffitype-package (cffitype-package cffitype))
	 (slotsyms (foreign-slot-names cffitype)))
    (flet
	(;; Given a proposed slot-name-symbol, make sure it is a valid one.
	 (verify-slot (sym)
	   (let ((slotsym (find-symbol (symbol-name sym) cffitype-package)))
	     (if (and slotsym (member slotsym slotsyms :test #'eq))
		 slotsym
		 (error 'with-c-error
			:msg (format nil "in ~A type ~A does not have a slot ~A"
				     instance cffitype sym)))))
	   ;; Given a proposed symbol to bind, prefix and check for banned.
	   (verify-name (sym)
	     (let ((prefixed (symbolicate prefix sym)))
	     ;  (format t "~%... proposed:~A  banlist ~A" prefixed *with-c-ban-list*)
	       (if (member prefixed *with-c-ban-list*)
		   (error 'with-c-error
			  :msg (format nil "proposed binding name ~A of ~A  is already taken in this context"
				       sym instance))
		   (progn (push prefixed *with-c-ban-list*)
			  prefixed)))))
      ;; Fix proposed bindings by verifying slot-names and prefixing bindings
      (loop for (binding . cdr) on bindings
	 collect
	   (if (listp binding)
	       (case (length binding)
		 (1 (list (verify-name (first binding))
			  (verify-slot (first binding))))
		 (2 (if (eq :pointer (first binding))
			(list (verify-name (second binding))
			      :pointer
			      (verify-slot (second binding)))
			(list (verify-name (first binding))
			      (verify-slot (second binding)))))
		 (3 (if (eq :pointer (second binding))
			(list (verify-name (first binding))
			      :pointer
			      (verify-slot (third binding)))
			(error 'with-c-error
			       :msg (format nil "malformed binding ~A of ~A"
					    binding instance))))
		 (t (error 'with-c-error
			   :msg (format nil "malformed binding ~A of ~A"
					binding instance))))
	       (list (verify-name binding) (verify-slot binding)))))))
;;==============================================================================
;;
;; A universal macro for dealing with foreign objects.  Creates an environment
;; with one or more foreign objects - which may be any mix of existing, new, or
;; temporarily-allocated objects.
;;
;; For slotted objects such as structs or unions, creates package-local bindings
;; for accessors to slots.  To support multiple slotted objects of the same
;; type, a unique prefix may be specified for each object's slot bindings.
;;
;; For objects without slots, such as :int, creates a package-local value
;; accessor prefixed with "*", so a foreign instance of an :int named q may be
;; referred to as *q for its value and q for its pointer.  To facilitate
;; multiple objects with same names from different packages, a unique prefix
;; may be assigned for each object.
;;
;; USING   (using (one-or-more-descriptor) ...)    where each descriptor is:
;;
;;   (new foreign-type instance &optional prefix bind)
;;
;; new:  :NEW to allocate a new instance and bind to 'instance'
;;       :TEMP to allocate a temp instance and bind to 'instance'
;;       :OLD :EXISTING or anything else to use an existing bound instance
;;
;; foreign-type: A designator for a valid CFFI foreign type
;;
;; instance: a bound instance, or a symbol to bind
;;
;; prefix: prefix local bindings
;;
;; bind: for slotted types, a detailed binding description or :all (default)
;;
;; Examples:
;;
;; (using (:new :int i) i)    ;like (foreign-alloc :int)
;; 
;; (using ((:temp :int i)     ;create two foreign ints that have local scope
;;         (:temp :int j))    ;with 'with-foreign-object'.  *i is used to
;;   (setf *i 3               ;access the value, like (mem-ref i :int),
;;         *j 5)              ;to get the pointer just use i or j...
;;    (foo i j))              ;poof. i and j are gone - do not return ptrs..
;;
;; (using ((:old :int gdk::i "gdk-") ;existing :int i aliased to 'gdk-i
;;         (:old :int g::i   "g-")   ;and another i is now 'g-i
;;   (format t "~A ~A" *gdk-i *g-i)  ;get both values
;;   (format t "~A ~A" gdk-i *g-i))  ;get both pointers
;;
;; (using (:old (:struct point) gtk::some-point) ;use an exising binding
;;  (setf x 1                 ;slot accessors are always values
;;        y 2)
;;  (foo x y))
;;
;; (using (:old (:struct point) point1)        ;no prefix, slots 'x and 'y
;;        (:temp (:struct point) point2 "T-")) ;prefix accessors of this one
;;   (setf t-x (+ 2 x)
;;         t-y (+ 2 y))
;;   (foo point2))   ;use temp point as a pointer prior to deallocation
;;  

(defmacro with-c1 ((new type-des instance
			&optional (prefix "") (bind nil bound) )
		   &body body)
  "Create a lexical environment "
  (if (symbolp instance)
      (if (member instance *with-c-ban-list*)
	  (error 'with-c-error
		 :msg (format nil
			      "Instance name ~A of type ~A is already in use"
			      instance type-des))
	  (push instance *with-c-ban-list*)))

  (let* ((cffitype (as-cffitype type-des))
	 (form
	  (if (subtypep (type-of cffitype) 'cffi::foreign-struct-type)
	      (progn
		(when (or (not bound) (eq bind :all))
		  (setf bind (default-slot-bindings cffitype)))
		(let* ((fixed-bindings
			(fix-slot-bindings bind prefix cffitype instance)))
		  `((with-foreign-slots (,fixed-bindings ,instance ,cffitype)
		      ,@body))))
	      (progn
		(when (or bound)
		  (error 'with-c-error
			 :msg (format nil "there are no slots in foreign objects of type ~S"
				      type-des)))
		(let ((instance-v (symbolicate "*" prefix instance ))
		      (instance-p 
		       `((let ((,(symbolicate prefix instance) ,instance))
			   ,@body))))
		  `((symbol-macrolet
			((,instance-v (cffi:mem-ref ,instance ,type-des)))
		      ,@instance-p)))))))
    (case new
      (:TEMP `(with-foreign-object (,instance ,cffitype)
		,@form))
      (:NEW   `(let ((,instance (foreign-alloc ,cffitype)))
		 ,@form))
      (t   (car form)))))
(defmacro with-cx (foreign-object-or-objects-descriptor &body body)
  (if (listp (car foreign-object-or-objects-descriptor))
      `(with-c1 (,@(car foreign-object-or-objects-descriptor))
	 (with-cx ,(cadr foreign-object-or-objects-descriptor)
	   ,@body))
      `(with-c1 ,foreign-object-or-objects-descriptor ,@body)))
(defmacro with-c (foreign-object-or-objects-descriptor &body body)
  (setf *with-c-ban-list* nil)
  `(with-cx ,foreign-object-or-objects-descriptor ,@body)
  )


