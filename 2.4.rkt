;; The idea of data-driven functions is one that lets
;; us represent data in any way we want, then devise
;; selectors to retrieve the data from the given structure

(make-from-real-imag (real-part z) (imag-part z))
(make-from-mag-ang (magnitude z) (angle z))

(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))
(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))


;; rectangular representation
(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))
(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
           (square (imag-part-rectangular z)))))
(define (angle-rectangular z)
  (atan (imag-part-rectangular z) (real-part-rectangular z)))
(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))
(define (make-from-mag-ang-rectangular r a)
  (attach-tag 'rectangular 
              (cons (* r (cos a)) (* r (sin a)))))

;; polar representation
(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z))))
(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (angle-polar z))))
(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))
(define (make-from-real-imag-polar x y)
  (attach-tag 'polar
              (cons (sqrt (+ (square x) (square y)))
                    (atan y x))))
(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r a)))

;; polymorphic selectors for the data
(define (real-part z)
  (cond ((rectangular? z)
         (real-part-rectangular (contents z)))
        ((polar? z)
         (real-part-polar (contents z)))
        (else (error "Unknown type -- REAL-PART" z))))
(define (imag-part z)
  (cond ((rectangular? z)
         (imag-part-rectangular (contents z)))
        ((polar? z)
         (imag-part-polar (contents z)))
        (else (error "Unknown type -- IMAG-PART" z))))
(define (magnitude z)
  (cond ((rectangular? z)
         (magnitude-rectangular (contents z)))
        ((polar? z)
         (magnitude-polar (contents z)))
        (else (error "Unknown type -- MAGNITUDE" z))))
(define (angle z)
  (cond ((rectangular? z)
         (angle-rectangular (contents z)))
        ((polar? z)
         (angle-polar (contents z)))
        (else (error "Unknown type -- ANGLE" z))))

;; The way our add/sub/mul/div functions work
;; either data representation will function

;; 2.4.2
(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))
(define (polar? z)
  (eq? (type-tag z) 'polar))

(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))
(define (make-from-real-ang r a)
  (make-from-mag-ang-polar r a))

;; selecting which function to run based of the "type" of the
;; data is called "dispatching on data"
;; this has the weakness of spreading around all the dispatching
;; functions throughout the codebase with no easy way to see
;; them all... therefore when adding a new "type", one might
;; miss a representation

;; we will instead create a table mapping a desired function
;; to a type's representation of that function,
;; (put <op> <type> <item>)  => add new mapping
;; (get <op> <type>)         => retrieve item from map


;; New rect "package"
(define (install-rectangular-package)
  ;;internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-real-imag x y)
    (if (and (in-tower? x) (in-tower? y))
        (cons x y)
        (error "non-real real or imaginary value" (list x y))))
  (define (make-from-mag-ang r a) 
    (if (and (real? r) (real? a))
        (cons (* r (cos a)) (* r (sin a)))
        (error "non-real magnitude or angle" (list r a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;; This "interfaces" ben's representation to the rest of the
;; system, preventing naming conflicts with other functions.
;; Gone is angle-rectangular, because it is now passed as 
;; an anon function into put, making its internal name
;; "private", there is no internal "state" either, just
;; side-effect free functions still dispatched by type

(define (install-polar-package)
  ;; internal prodcedues
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-mag-ang r a)
    (if (and (in-tower? r) (in-tower? a))
        (cons r a)
        (error "non-real magnitude or angle" (list r a))))
  (define (make-from-real-imag x y) 
    (if (and (in-tower? x) (in-tower? y))
        (cons (sqrt (+ (square x) (square y)))
              (atan y x))
        (error "non-real real or imaginary value" (list x y))))
  ;; public interface 
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;; To now map our selectors to the table:
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types -- APPLY-GENERIC"
           (list op type-tags))))))
 ;; why is type-tags a list at all? why not a single element?

;; generic selectors!
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

;; now the entire stack is unchanged if a new
;; type-package is added to the system!

;; We can also pick a representation for the
;; default "constructor"
(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))


;; 2.73

;; a long-form manual "type" dispatch example
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ;;; blah
        (else (error "unknown expression type -- DERIV" exp))))

;; rewritten in a data-directed style
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)
                var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;; a. Explain what was done above. Why can't we assimilate the predicates number? and same-variable? into the data-directed dispatch?
;; Neither number or variable have operators, so their interface
;; would be different. You _could_ force them to have a 'deriv
;; mapping, but then you would still need to check them beforehand
;; to pass a bogus value as the parameters to deriv

;; b. Write the procedures for derivatives of sums and products, and the auxiliary code required to install them in the table used by the program above.

(define (make-sum x y) (list '+ x y))
(define (make-product x y) (list '* x y))
(define (install-sum-package)
  ;; internal procedues
  (define (addend x) (cadr c))
  (define (augend x) (caddr c))
  (define (ideriv exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))
  ;; public interface 
  (put 'deriv '(+) ideriv)
  'done)

(define (install-prod-package)
  ;; internal procedues
  (define (multiplier x) (cadr c))
  (define (multiplicand x) (caddr c))
  (define (ideriv exp var)
    (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
  ;; public interface 
  (put 'deriv '(*) ideriv)
  'done)



;; Message Passing

;; Rather than have the dispatch happen on types in the data
;; have it dispatch off the data objects itself as a closure
;; over the values of x y, since they never change, the
;; closure itself can hold the values
(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else
           (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(define (apply-generic op arg) (arg op))

;; 2.75
(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else
           (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)

;; 2.76

;; - Fastest to add new types
;; I would say message passing is fastest to add new types,
;; .. all the information is inside the "type", and therefore
;; easy to add new types

;; - Fastest to add new operations
;; The generic dispatch is very easy to add new operations,
;; you simply add them as needed where they are needed.

;; 2.5 Generic Operators

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

;; Ordinary package for numbers
(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  (put '=zero? 'scheme-number
       (lambda (x) (= 0 x)))
  (put 'raise 'scheme-number
       (lambda (x) (make-rational x 1)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-rational-package)
  ;; internal
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (make-rat n d)
    (if (and (integer? n) (integer? d))
        (let ((g (gcd n d)))
          (cons (/ n g) (/ d g)))
        (error "non-integer numerator or denominator"
               (list n d))))
  (define (rational->real r) (make-real (/ (numer r) (denom r))))
  (define (equali x y)
    (and (= (numer x) (numer y))
         (= (denom x) (denom y))))
  ;; public interface
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'equ? '(rational rational)
       (lambda (x y) (equali x y)))
  (put '=zero? 'rational
       (lambda (x) (= 0 (numer x))))
  (put-coercion 'rational 'real rational->real)
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-complex-package)
  ;; imported procedures from rect and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  ;; interface to system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
     (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (z1 z2) (tag (make-from-real-imag z1 z2))))
  (put 'make-from-mag-ang 'complex
       (lambda (z1 z2) (tag (make-from-mag-ang z1 z2))))
  (put 'equ? '(complex complex)
       (lambda (x y) (and (= (angle x)
                             (angle y))
                          (= (magnitude x)
                             (magnitude y)))))
  (put '=zero? 'complex
       (lambda (x) (= 0 (magnitude x))))
  'done)


;; 2.78
;; using scheme's internal type for numbers
(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))
(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (error "Bad tagged datum -- CONTENTS" datum)))

;; "Coercion scheme" aware apply-generic
;; allows types to be converted using a lookup from one type
;; to another, but then that means you might need at most
;; n^2 converters for n types
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                       (cond (t1->t2
                              (apply-generic op (t1->t2 a1) a2))
                             (t2->t1
                              (apply-generic op a1 (t2->t1 a2)))
                             (else
                              (error "No method for these types"
                                     (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))

;; 2.81
;; a. It causes an infinite loop because apply generic keeps
;; trying to convert each complex number to a complex number

;; b. no, something did not have to be done to coercise types
;; of the same type, and it does not work as is

;; c.
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                ;; prevent coercion between two equal types
                (if (= type1 type2)
                    (error "No method for these types"
                           (list op type-tags))
                    (let ((t1->t2 (get-coercion type1 type2))
                          (t2->t1 (get-coercion type2 type1)))
                      (cond (t1->t2
                             (apply-generic op (t1->t2 a1) a2))
                            (t2->t1
                             (apply-generic op a1 (t2->t1 a2)))
                            (else
                             (error "No method for these types"
                                    (list op type-tags)))))))
              (error "No method for these types"
                     (list op type-tags)))))))

;; 2.82 - allow apply-generic to handle more than two arguments
(define (apply-generic op . args)
  (define (coerce args types)
    (if (empty? types)
        args
        (let ((type-to-try (car types))
              (arg-types (map type-tag args)))
          (let ((type-converters (map
                                  (lambda (atype)
                                    (if (= type-to-try atype)
                                        (lambda (x) x)         ;; no cast needed
                                        (let ((t1->t2 (get-coercion atype type-to-try)))
                                          (if t1->t2
                                              t1->t2
                                              nil))))
                                  arg-types)))
            (if (member nil type-converters)
                (coerce args (cdr types))
                (coerce (map (lambda (arg converter) (converter arg))
                             args
                             type-converters)
                        (cdr types)))))))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (apply-generic op
                         (coerce args (map type-tag args)))))))

;; 2.83 - found on http://jots-jottings.blogspot.com/2012/03/sicp-exercise-283-raising-types.html
(define (install-integer-package)
  (define (integer->rational i) (make-rational i 1))
  (define (tag x) (attach-tag 'integer x))    
  (put 'add '(integer integer) (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer) (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer) (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer) (lambda (x y) (tag (/ x y))))
  (put 'equ? '(integer integer) (lambda (x y) (= x y)))
  (put-coercion 'integer 'rational integer->rational)
  (put 'make 'integer
       (lambda (x) (if (integer? x)
                       (tag x)
                       (error "non-integer value" x))))
  (put '=zero? '(integer) (lambda (x) (= 0 x)))
  'done)

(define (make-integer n)
  ((get 'make 'integer) n))

(define (install-real-package)
  (define (tag x) (attach-tag 'real x))    
  (define (real->complex r) (make-complex-from-real-imag r 0))
  (put 'add '(real real) (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real) (lambda (x y) (tag (- x y))))
  (put 'mul '(real real) (lambda (x y) (tag (* x y))))
  (put 'div '(real real) (lambda (x y) (tag (/ x y))))
  (put 'equ? '(real real) (lambda (x y) (= x y)))
  (put 'make 'real
       (lambda (x) (if (real? x)
                       (tag x)
                       (error "non-real value" x))))
  (put '=zero? '(real)
       (lambda (x) (= 0 x)))
  (put-coercion 'real 'complex real->complex)
  'done)
(define (make-real n)
  ((get 'make 'real) n))

(define number-tower '(integer rational real complex))
(define (install-number-package)
  (define (tag x) (attach-tag 'number x))    
  (define (real->complex r) (make-complex-from-real-imag r 0))
  (put 'is-type 'number (lambda (x) (member x number-tower)))
  (put 'make 'number (lambda (x) (tag x)))
  'done)
(define (make-number n)
  ((get 'make 'number) n))

(define (integer->rational i) (make-rational i 1))
(define (rational->real r) (make-real (/ (numer r) (denom r))))
(define (real->complex r) (make-complex-from-real-imag r 0))


(define (raise x)
  (define (apply-raise types)
    (cond ((null? types)
           (error "Type not found in the tower-of-types"
                  (list x type-tower)))
          ((eq? (type-tag x) (car types))
           (if (null? (cdr types))
               x
               (let ((raiser (get-coercion (type-tag x) (cadr types))))
                 (if raiser
                     (raiser (contents x))
                     (error "No coercion procedure found for types"
                            (list (type-tag x) (cadr types)))))))
          (else (apply-raise (cdr types)))))
  (apply-raise number-tower))

;; 2.84  - raising apply-generic

(define (higher? t1 t2 type-tower)
  (if (eq? t1 t2)
      false 
      (< (length (member t1 type-tower))
         (length (member t2 type-tower)))))
;; (define tower-of-types '(integer rational real complex))
;; (higher? 'rational 'integer tower-of-types) => #t
;; (higher? 'integer 'real tower-of-types) => #f

