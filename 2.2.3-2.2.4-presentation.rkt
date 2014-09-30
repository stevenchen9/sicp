(define (square y) (* y y))

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))


(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))

(define (even-fibs n)
  (define (next k)
    (if (> k n)
        null
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))

;; Anyone spot the 'transducer' reference?


;; +-------------+   +-------------+   +-------------+   +-------------+
;; | enumerate:  |-->| filter:     |-->| map:        |-->| accumulate: |
;; | tree leaves |   | odd?        |   | square      |   | +, 0        |
;; +-------------+   +-------------+   +-------------+   +-------------+

;; +-------------+   +-------------+   +-------------+   +-------------+
;; | enumerate:  |-->| map:        |-->| filter:     |-->| accumulate: |
;; | integers    |   | fib         |   | even?       |   | cons, ()    |
;; +-------------+   +-------------+   +-------------+   +-------------+


;; We already saw map... here it is again

(define (map proc items)
  (if (null? items)
      null
      (cons (proc (car items))
            (map proc (cdr items)))))

(map square '(1 2 3))



;; What about filter?
(define (filter predicate sequence)
  (cond ((null? sequence) null)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))


(filter odd? (list 1 2 3 4 5))

;; 
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(accumulate + 0 (list 1 2 3 4 5))
(accumulate * 1 (list 1 2 3 4 5))
(accumulate cons null (list 1 2 3 4 5))


(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))
(enumerate-interval 2 7)


(define (enumerate-tree tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(enumerate-tree (list 1 (list 2 (list 3 4)) 5))


(define (sum-odd-squares tree)
  (accumulate +
              0
              (map square
                   (filter odd?
                           (enumerate-tree tree)))))


(define (even-fibs n)
  (accumulate cons
              null
              (filter even?
                      (map fib
                           (enumerate-interval 0 n)))))

;; This expression is extremely modular AND safer than bespoke logic



(define (list-fib-squares n)
  (accumulate cons
              null
              (map square
                   (map fib
                        (enumerate-interval 0 n)))))

(list-fib-squares 10)

(define (product-of-squares-of-odd-elements sequence)
  (accumulate *
              1
              (map square
                   (filter odd? sequence))))

(product-of-squares-of-odd-elements (list 1 2 3 4 5))


(define (salary-of-highest-paid-programmer records)
  (accumulate max
              0
              (map salary
                   (filter programmer? records))))



;; Example

;; Given a positive integer n, find all ordered pairs of distinct positive
;; integers i and j, where 1 <= j< i<= n, such that i + j is prime.  For
;; example, if n is 6, then the pairs are the following:

;;     i   | 2 3 4 4 5 6 6
;;     j   | 1 2 1 3 2 1 5
;;   ------+---------------
;;   i + j | 3 5 5 7 7 7 11



     (define (flatmap proc seq)
       (accumulate append null (map proc seq)))

     (define (prime-sum? pair)
       (prime? (+ (car pair) (cadr pair))))

     (define (make-pair-sum pair)
       (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

     (define (prime-sum-pairs n)
       (map make-pair-sum
            (filter prime-sum?
                    (flatmap
                     (lambda (i)
                       (map (lambda (j) (list i j))
                            (enumerate-interval 1 (- i 1))))
                     (enumerate-interval 1 n)))))
     (define (remove item sequence)
       (filter (lambda (x) (not (= x item)))
               sequence))
     (define (permutations s)
       (if (null? s)                    ; empty set?
           (list null)                   ; sequence containing empty set
           (flatmap (lambda (x)
                      (map (lambda (p) (cons x p))
                           (permutations (remove x s))))
                    s)))
