









;; Using low-level concrete forms to make higher level abstractions






































(define (make-rat n d) (cons n d))

(define (numer x) (car x))
(define (denom x) (cdr x))


(def num {:numer 1, :denom 2})
(num :numer)
(:numer num)

















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

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-half (make-rat 1 2))
(print-rat one-half)















;; this is the most important part to wrap your head around


(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (x y) x)))
(define (cdr z)
  (z (lambda (x y) y)))

((cons 1 2) +)
((lambda (m) (m 1 2)) +)
(lambda (+) (+ 1 2))

(car (cons 1 2))
(car (lambda (m) (m 1 2)))
((lambda (m) (m 1 2)) (lambda (x y) x))
((lambda (x y) x) 1 2)
(lambda (1 2) 1) 
1 

(cdr (cons 1 2))





























;; Church numerals
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define (inc n)
  (+ n 1))



((zero +) 0) ;; 0


(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x)))))

(lambda (f) (lambda (x) (f ( (lambda (x) x)  x))))
(lambda (f) (lambda (x) (f x)))
(define one (lambda (f) (lambda (x) (f x))))

















;; substitution for 1 
(define (add-1 zero)
  (lambda (f) (lambda (x) (f ((zero f) x)))))

(define (add-1 (lambda (f) (lambda (x) x)))
  (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x)))))

(define (add-1 zero)
  (lambda (f) (lambda (x) (f x))))

;; and that's one!
(define one 
  (lambda (f) (lambda (x) (f x))))

;; now for two!
(define (add-1 one)
  (lambda (f) (lambda (x) (f ((one f) x)))))

(define (add-1 (lambda (f1) (lambda (x1) (f1 x1))))
  (lambda (f) (lambda (x) (f (f x)))))

(define two 
  (lambda (f) (lambda (x) (f (f x)))))

((two inc) 0) ;; 2
((one inc) 0) ;; 1



