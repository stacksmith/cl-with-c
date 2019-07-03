# WITH-C

A universal macro for dealing with foreign objects.  Creates an environment with one or more foreign objects - which may be any mix of existing, new, or temporarily-allocated objects.

For slotted objects such as structs or unions, creates package-local bindings for accessors to slots.  To support multiple slotted objects of the same type, a unique prefix may be specified for each object's slot bindings.

For objects without slots, such as :int, creates a package-local value accessor prefixed with "*", so a foreign instance of an :int named `q` may be referred to as `*q` for its value and `q` for its pointer.  To facilitate multiple objects with same names from different packages, a unique prefix may be assigned for each object.

All bindings are package-local; all slotnames are 'pretend package-local' - that is you never have to prefix slotnames with a package - the macro takes care of it (it already knows what package slots are from the foreign type).  Types and instances are the only things that need to be package-prefixed if not visible.

## License

BSD 3-clause license

## Installation:

Clone the repo to a directory known to quicklisp, and do
(ql:quickload cl-with-c).  Or do asdf magic.

## Requirements

CFFI with modernized 'with-foreign-slots' PR145:[Improved 'with-foreign-slots](https://github.com/cffi/cffi/pull/145).  This should be merged shortly; until then, a copy of improved `with-foreign-slots` is included.

## Usage

`WITH-C (descriptor) body)` or

`WITH-C ((descriptor-1)..(descriptor-n)) body)`

where each descriptor is:

`(new foreign-type instance &optional prefix bind)`
```
new:  :NEW to allocate a new instance and bind to 'instance'
      :TEMP to allocate a temp instance and bind to 'instance'
      :OLD :EXISTING or anything else to use an existing bound instance

foreign-type: A designator for a valid CFFI foreign type

instance: a bound instance, or a symbol to bind

prefix: prefix local bindings

bind: for slotted types, a detailed binding description or :all (default)

      A binding description is a list of forms acceptable to 
	  'with-foreign-slots, with no package prefixes.  Bindings should
	  be in the current package, and WITH-C knows the package of slots.
	  
	  - a local symbol representing a slot        x
	  - (name slotname)                           (myx x)
	  - (:pointer slotname)                       (:pointer x)
	  - (name :pointer slotname)                  (myx :pointer x)
	  
	  binding descriptions are automatically converted to account for
	  foreign-type's package; local symbols are prefixed with 'prefix'
	  parameter and checked to make sure that they are unique within 
	  the WITH-C scope (generating an error if they are not).


Examples:

(with-c (:new :int i) i)    ;allocate a new :int, like (foreign-alloc :int)
 
(with-c ((:temp :int i)     ;create two foreign ints that have local scope
         (:temp :int j))    ;with 'with-foreign-object'.  *i is used to
   (setf *i 3               ;access the value, like (mem-ref i :int),
         *j 5)              ;to get the pointer just use i or j...
   (foo i j))               ;poof. i and j are gone - do not return ptrs..

(with-c ((:old :int gdk::i "gdk-") ;existing :int gdk:i aliased to gdk-i
         (:old :int g::i   "g-")   ;and g::i is now 'g-i
   (format t "~A ~A" *gdk-i *g-i)  ;get both values
   (format t "~A ~A" gdk-i *g-i))  ;get both pointers

(with-c (:old (:struct point) 
         gtk::some-point)    ;use an exising binding in another package
   (setf x 1                 ;slot accessors are automatically created
         y 2)
   (foo x y))

(defparameter point               ;create a variable that contains
              (cffi::parse-type   ;the actual #<foreign-type-xxx>
                (:struct point)))
				
(with-c (:new point pt           ;note that point's symbol-value is used
           ((myx x)              ;use custom bindings
            (mypy :pointer y)))  ;pointer syntax
   (setf myx 1)
   (foo myx mypy)
   pt)                     ;don't forget to foreign-free it later!



(with-c (:old (:struct point) point1)        ;no prefix, slots 'x and 'y
        (:temp (:struct point) point2 "T-")) ;prefix accessors of this one
   (setf t-x (+ 2 x)
         t-y (+ 2 y))
   (foo point2))   ;use temp point as a pointer prior to deallocation
   
```
  

