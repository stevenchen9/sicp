
;; Two ways of defining the count of primes in a range

;; Iteratively
(define (sum-primes a b)
  (define (iter count accum)
    (cond ((> count b accum))
          ((prime? count) (iter (+ count 1) (+ count accum)))
          (else (iter (+ count 1) accum))))
  (iter a 0))

;; As sequence abstractions
(define (sum-primes a b)
  (accumulate +
              0
              (filter prime? (enumerate-interval a b))))

;; The second, while more elegant is very inefficient
;; because it enumerates the entire list of integers
;; before filtering down only primes


;; A "stream" is basically just a lazy eval'ed list

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))
(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons (proc (stream-car s))
            (delay (stream-map proc (stream-cdr s))))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))
(define (display-line x)
  (newline)
  (display x))


;; We can delay the filling of the cdr of a stream like so:

(delay <A>) ;;returns a promise, that can be "forced" with (force)

;; These are equivalent
(cons-stream <A> <B>) ;; special form
(cons <A> (delay <B>))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(stream-car
 (stream-cdr
  (stream-filter prime?
                 (stream-enumerate-interval 10000 1000000))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons
       low
       (delay (stream-enumerate-interval (+ low 1) high)))))

;; In this example, the "subs" model shows how it is just
;; as efficient as an iterative solution
(cons 10000
      (delay (stream-enumerate-interval 10001 1000000)))

;; The cdr of the list hasn't been evaluated yet, it is a
;; promise to evaluate the "rest" of the intervals later

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons (stream-car stream)
               (delay (stream-filter pred
                                     (stream-cdr stream)))))
        (else (stream-filter pred (stream-cdr stream)))))
