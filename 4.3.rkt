(require (planet neil/sicp:1:17))

;; The desired syntax for a nondeterministic langauge 

#|(define (prime-sum-pair list1 list2)
  (let ((a (an-element-of list1))
        (b (an-element-of list2)))
    (require (prime? (+ a b)))
    (list a b)))|#

(list (amb 1 2 3) (amb 'a 'b))
;; => `(1 a)' `(1 b)' `(2 a)' `(2 b)' `(3 a)' `(3 b)'

(define (require? p)
  (if (not p) (amb) '()))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

;; 4.36
(define (an-integer-between n m)
  (amb n (when (<= n m) (an-integer-between (+ n 1) m))))

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let ((k (an-integer-between j high)))
        (require? (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))

;; 4.37
;; Ben Bitdiddle's more efficient solution, less results are
;; required to be enumerated
(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high))
        (hsq (* high high)))
    (let ((j (an-integer-between i high)))
      (let ((ksq (+ (* i i) (* j j))))
        (require (>= hsq ksq))
        (let ((k (sqrt ksq)))
          (require (integer? k))
          (list i j k))))))

;;   *Exercise 4.38:* Modify the multiple-dwelling procedure to omit
;;   the requirement that Smith and Fletcher do not live on adjacent
;;   floors.  How many solutions are there to this modified puzzle?

(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require
     (distinct? (list baker cooper fletcher miller smith)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    ;; (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

;;   *Exercise 4.40:* In the multiple dwelling problem, how many sets
;;   of assignments are there of people to floors, both before and
;;   after the requirement that floor assignments be distinct?  It is
;;   very inefficient to generate all possible assignments of people to
;;   floors and then leave it to backtracking to eliminate them.  For
;;   example, most of the restrictions depend on only one or two of the
;;   person-floor variables, and can thus be imposed before floors have
;;   been selected for all the people.  Write and demonstrate a much
;;   more efficient nondeterministic procedure that solves this problem
;;   based upon generating only those possibilities that are not already
;;   ruled out by previous restrictions.  (Hint: This will require a
;;   nest of `let' expressions.)

(define (the-lower-of a b)
  (if (< a b) a b))
(define (the-higher-of a b)
  (if (> a b) a b))

(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4))
        (cooper (amb 2 3 4 5)))
    (let ((miller (an-integer-between cooper 5)))
      (let ((fletcher (an-integer-between
                       (the-lower-of (+ 1 cooper) 4)
                       (the-higher-of (- 1 cooper) 2))))
        (let ((smith (an-integer-between (+ 1 fletcher) (- 1 fletcher))))
          (require
           (distinct? (list baker cooper fletcher miller smith)))
          (require (not (= (abs (- smith fletcher)) 1)))
          (require (not (= (abs (- fletcher cooper)) 1)))
          (list (list 'baker baker)
                (list 'cooper cooper)
                (list 'fletcher fletcher)
                (list 'miller miller)
                (list 'smith smith)))))))

;;   *Exercise 4.41:* Write an ordinary Scheme program to solve the
;;   multiple dwelling puzzle.
(define (multiple-dwelling-scheme)
  (map (lambda (baker)
         (map (lambda (cooper)
                (map (lambda (miller)
                       (when (> miller cooper)
                         (map (lambda (fletcher)
                                (map (lambda (smith)
                                       (when (and (unique? (list smith baker cooper fletcher miller))
                                                  (not (= (abs (- smith fletcher)) 1))
                                                  (not (= (abs (- fletcher cooper)) 1)))
                                         (list (list 'baker baker)
                                               (list 'cooper cooper)
                                               (list 'fletcher fletcher)
                                               (list 'miller miller)
                                               (list 'smith smith))))
                                     '(1 2 3 4 5)))
                              '(2 3 4))))
                     '(1 2 3 4 5)))
              '(2 3 4 5)))
       '(1 2 3 4)))

(multiple-dwelling-scheme)
;; => (sorta) {{smith 1} {cooper 2} {baker 3} {fletcher 4} {miller 5}} 

;; (unique? '(1 2 3 2)) => #f
;; (unique? '(1 2 3)) => #t
(define (unique? l)
  (define (i-d l r)
    (if (empty? l)
        #t
        (if (member (car l) r)
            #f
            (i-d (cdr l) (cons (car l) r)))))
  (i-d l '()))


   
