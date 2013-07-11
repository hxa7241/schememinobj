;------------------------------------------------------------------------------;
;                                                                              ;
;  Minimal basic object system for Scheme R5RS                                 ;
;  Harrison Ainsworth / HXA7241 : 2011                                         ;
;                                                                              ;
;  http://www.hxa.name/tools/                                                  ;
;                                                                              ;
;  License: CC0 -- http://creativecommons.org/publicdomain/zero/1.0/           ;
;                                                                              ;
;------------------------------------------------------------------------------;




;; Minimal basic object system.
;;
;; For ordinary standard R5RS Scheme, with no other dependencies.
;; (In only 22 lines.)
;;
;; An alternative to SRFI-9 Records:
;;  * basically a dictionary data structure,
;;  * with fields (for each instance) and methods (shared between instances),
;;  * but no inheritance etc.
;; The main purpose was:
;;  * concise member addressing -- using OO-style instance 'scope/namespace',
;; to cure the pitifully verbose member addressing of records.
;;
;; Example object definition:
;;
;;   ;; constructor
;;   ;;  * using macro: defobject
;;   ;;  * referring to Point2D from the subsequent defclass
;;   ;;  * making an ad hoc set of fields for this particular instance
;;   ;;  * field definitions are evaluated sequentially, as in a let*
;;   (define (Point2D.create a b)
;;     (defobject Point2D
;;       (x a)
;;       (y b)))
;;
;;   ;; methods
;;   ;;  * using macro: defclass
;;   ;;  * making a set of methods to be shared between instances
;;   ;;  * fields are accessed using self
;;   (defclass Point2D
;;     (method (dot self other)
;;       (+ (* (self 'x) (other 'x))
;;          (* (self 'y) (other 'y))))
;;     (method (length self)
;;       (sqrt (self 'dot self))))
;;
;; Class (i.e. non-instance) fields and methods can just be defined ordinarily,
;; since they do not need to have self fed through (the create procedure above
;; is an example of a class method).
;;
;; Example instantiation and method call:
;; (must quote method name)
;;
;;   (let ((aPoint (Point2D.create 1 -2))
;;         (bPoint (Point2D.create 0  0)))
;;     (aPoint 'dot bPoint))
;;
;; Example field get and set:
;; (must quote field name)
;;
;;   (aPoint 'x)     ; get
;;   (bPoint 'y 42)  ; set
;;
;; Limitation:
;; Method parameters with dotted tails are not supported.
;;
;;
;; Implementation notes:
;;
;; Inheritance could be a straightforward and minimal extension: have defclass
;; take parent parameters, which could then be added as a single list member to
;; its resultant methods list, and have Object.create search those parents if
;; it cannot find the member name in the instance (the fields and methods).
;; That would be the core of it anyway . . .




;; usage interface ---------------------------------------------------------- ;;

;; These mainly neaten the lexical representation a little -- nothing
;; substantial. They core purpose is to define an association-list.


;; Make an instance -- i.e. a set of fields, with a shared set of methods.
;;
(define-syntax defobject
  (syntax-rules ()

    ((_ class (name field) ...)

     ;; sequentialise field definition
     (let* ((name field) ...)
       ;; make association-list of fields
       (let ((fields (list (cons 'name name) ...)))
         ;; make an object from the fields and methods lists
         (Object.create class fields))))))


;; Make a set of methods (shareable between instances).
;;
(define-syntax defclass
  (syntax-rules ()

    ((_ class (method (name param ...) expr1 ...) ...)

     ;; make association-list of methods
     (define class (list (cons 'name (lambda (param ...) expr1 ...)) ...)))))


;; making class fields and methods can be done with normal defines




;; implementation ----------------------------------------------------------- ;;

;; Dispatcher.
;;
(define (Object.create methods fields)

  ;; make and return a lambda closing over the members,
  ;; and make it recursable so that called methods can themselves call it
  (letrec ((self (lambda (name . params)

                   ;; first search fields
                   (let ((field (assq name fields)))
                     (if field
                       (if (null? params)
                         ;; no params: get field
                         (cdr field)
                         ;; params: set field
                         (set-cdr! field (car params)))

                       ;; no field, so search methods
                       (let ((method (assq name methods)))
                         (if method
                           ;; call method
                           (apply (cdr method) (cons self params))

                           ;; nothing found
                           ;; (maybe could replace with an exception)
                           'membernotfound)))))))
    self))
