;; Queues as mutable lists

(require scheme/mpair)

;; The queue will have (front-ptr . rear-ptr)
(define (front-ptr queue) (mcar queue))
(define (rear-ptr queue) (mcdr queue))

(define (set-front-ptr! queue item) (set-mcar! queue item))
(define (set-rear-ptr! queue item) (set-mcdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (make-queue) (mcons (mlist) (mlist)))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (mcar (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (mcons item (mlist))))
    (cond ((empty-queue? queue)
           (begin (set-front-ptr! queue new-pair)
                  (set-rear-ptr! queue new-pair))
           queue)
          (else
           (begin (set-mcdr! (rear-ptr queue) new-pair)
                  (set-rear-ptr! queue new-pair))
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (mcdr (front-ptr queue)))
        queue)))

(define q1 (make-queue))
(insert-queue! q1 'a)
;; => {{a} a}
(insert-queue! q1 'b)
;; => {{a b} b}
(delete-queue! q1)
;; => {{b} b}
(delete-queue! q1)
;; => {{} b}

;; 3.21
(string-append (~a 'a) "b")

(define (print-queue q)
  (define (inner-print q ret)
    (if (empty? q)
        ret
        (inner-print (mcdr q)
                     (string-append ret (~a (mcar q))))))
  (inner-print (mcar q) ""))
(print-queue q1)
;; => "ab"

;; 3.22
(define (make-queue)
  (let ((that (mcons (mlist) (mlist))))
    (let ((front-ptr (lambda () (mcar that)))
          (rear-ptr (lambda () (mcdr that))))
      (define (insert v)
        (let ((new-pair (mcons v (mlist))))
          (if (mty?)
              (begin (set-mcar! that new-pair)
                     (set-mcdr! that new-pair))
              (begin (set-mcdr! (rear-ptr) new-pair)
                     (set-mcdr! that new-pair)))))
      (define (delete)
        (if (mty?)
            (error "empty queue")
            (set-mcar! that (mcdr (front-ptr)))))
      (define (mty?) (empty? (front-ptr)))
      (define (dispatch m)
        (cond ((eq? m 'insert!) insert)
              ((eq? m 'delete!) delete)
              ((eq? m 'empty?) mty?)
              ((eq? m 'view) that)
              (else (error "bad dispatch - let queue"))))
      dispatch)))

(define (empty-queue? queue) ((queue 'empty?)))
(define (insert-queue! queue item) ((queue 'insert!) item))
(define (delete-queue! queue) ((queue 'delete!)))
(define q2 (make-queue))
(insert-queue! q2 'a)
(insert-queue! q2 'b)
(insert-queue! q2 'c)
(q2 'view)
(delete-queue! q2)
(q2 'view)

(define m (mlist 'a 'b 'c 'd))
(mcdr m)
(set-mcdr! m (mcons 'e (mlist)))
;; 3.23
(define (make-deque)
  (let ((that (mcons (mlist) (mlist))))
    (let ((front-ptr (lambda () (mcar that)))
          (rear-ptr (lambda () (mcdr that))))
      (define (make-new-pair v cdrs) (mcons v cdrs))
      (define (empty-insert new-pair)
        (begin (set-mcar! that new-pair)
               (set-mcdr! that new-pair)))
      (define (insert-front v)
        (let ((new-pair (make-new-pair v (mcar that))))
          (if (mty?) (empty-insert (make-new-pair v (mlist)))
              (set-mcar! that new-pair))))
      (define (insert-back v)
        (let ((new-pair (make-new-pair v (mlist))))
          (if (mty?) (empty-insert new-pair)
              (begin (set-mcdr! (rear-ptr) new-pair)
                     (set-mcdr! that new-pair)))))
      (define (delete-front)
        (if (mty?) (error "empty deque")
            (set-mcar! that (mcdr (front-ptr)))))
      (define (delete-back)
        (if (mty?) (error "empty deque")
            (set-mcar! that (mcdr (front-ptr)))))
      (define (mty?) (empty? (front-ptr)))
      (define (dispatch m)
        (cond ((eq? m 'front-deque) (mcar (front-ptr)))
              ((eq? m 'rear-deque) (mcar (rear-ptr)))
              ((eq? m 'front-insert-deque!) insert-front)
              ((eq? m 'rear-insert-deque!) insert-back)
              ((eq? m 'front-delete-deque!) delete-front)
              ((eq? m 'rear-delete-deque!) delete-back)
              ((eq? m 'empty?) mty?)
              ((eq? m 'view) that)
              (else (error "bad dispatch - let queue"))))
      dispatch)))

(begin (define q3 (make-deque))
       ((q3 'front-insert-deque!) 'a)
       ((q3 'front-insert-deque!) 'b)
       ((q3 'front-insert-deque!) 'c)
       ((q3 'rear-insert-deque!) 'd))
(q3 'view)
((q3 'front-delete-deque!))
((q3 'rear-delete-deque!))
(q3 'view)


;; Tables

(define (lookup key table)
  (let ((record (assoc key (mcdr table))))
    (if record
        (mcdr record)
        false)))

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (mcar (mcar records))) (mcar records))
        (else (assoc key (mcdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (mcdr table))))
    (if record
        (set-mcdr! record value)
        (set-mcdr! table
                  (mcons (mcons key value) (mcdr table)))))
  'ok)

(define (make-table)
  (mlist '*table*))


;; two-dimensional tables


(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
        (set-cdr! table
                  (cons (list key-1
                              (cons key-2 value))
                        (cdr table)))))
  'ok)

;; Object oriented table definition

(define (make-table same-key?)
  (let ((local-table (mlist '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (mcdr subtable))))
              (if record
                  (mcdr record)
                  false))
            false)))
    (define (assoc key records)
      (cond ((null? records) false)
            ((same-key? key (mcar (mcar records))) (mcar records))
            (else (assoc key (mcdr records)))))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (mcdr subtable))))
              (if record
                  (set-mcdr! record value)
                  (set-mcdr! subtable
                            (mcons (mcons key-2 value)
                                  (mcdr subtable)))))
            (set-mcdr! local-table
                      (mcons (mlist key-1
                                  (mcons key-2 value))
                            (mcdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc) insert!)
            ((eq? m 'view) local-table)
            (else (error "Unknown operarion -- TABLE" m))))
    dispatch))

(define operation-table (make-table equal?))
(begin ((operation-table 'insert-proc) 'math '+ 34)
       ((operation-table 'lookup-proc) 'math '+)
       (operation-table 'view))
;; => {*table* {math {+ . 34}}}


;; 3.24
(define operation-table (make-table eq?))
(begin ((operation-table 'insert-proc) 2 2 34)
       ((operation-table 'insert-proc) 1 1 35)
       ((operation-table 'lookup-proc) 1 1))
(operation-table 'view)
;; => {*table* {1 {1 . 35}} {2 {2 . 34}}}


;; 3.25
(define (multi-table same-key?)
  (let ((local-table (mlist '*table*)))
    (define (lookup keys)
      (define (lookup-i keys table)
        (if (null? keys)
            false
            (let ((record (assoc (mcar keys) (mcdr table))))
              (if record
                  (if (empty? (mcdr keys)) 
                      (mcdr record)
                      (lookup-i (mcdr keys) record))
                  false))))
      (lookup-i keys local-table))
    (define (assoc key records)
      (cond ((null? records) false)
            ((same-key? key (mcar (mcar records))) (mcar records))
            (else (assoc key (mcdr records)))))
    (define (make-new-insert keys value)
      (if (empty? (mcdr keys))
          (mcons (mcar keys) value)
          (mlist (mcar keys) (make-new-insert (mcdr keys) value))))
    (define (insert! keys value)
      (define (insert-i table keys)
        (if (empty? keys)
            (error "missing keys")
            (let ((subtable (assoc (mcar keys)
                                   (mcdr table))))
              (if subtable
                  (if (empty? (mcdr keys))
                      (set-mcdr! subtable value)
                      (insert-i subtable (mcdr keys)))
                  (set-mcdr! table
                             (mcons (make-new-insert keys value)
                                    (mcdr table)))))))
      (insert-i local-table keys)
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc) insert!)
            ((eq? m 'view) local-table)
            (else (error "Unknown operarion -- TABLE" m))))
    dispatch))


(begin
  (define t1 (multi-table eq?))   
  ((t1 'insert-proc) (mlist 2 2 2) 34)      
  ((t1 'insert-proc) (mlist 1 1 2) 35)      
  ((t1 'insert-proc) (mlist 1 2 2) 3)      
  ((t1 'lookup-proc) (mlist 1 2 2)))
(t1 'view)
;; {*table* {1 {2 {2 . 3}}
;;             {1 {2 . 35}}}
;;          {2 {2 {2 . 34}}}}


;; 3.26
;; It should be possible to store a reference to a given
;; key in a sorted list, that would allow for faster access
;; or even under a tree of cells, one for each initial character
;; of the key. In such an implementation, the key 'test'
;; would be found under 't 'e 's 't
;; The biggest issue with this assuming a restriction on only alpha characters
;; is that each character could require as many as 26 lookups.

;; Another option would be to store them sorted in a tree,
;; but that would require the tree to be balanced in order
;; to provide the most speed benefit.


;; 3.27

;; regular fib proc
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))
(define memo-fib
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))
(memo-fib 2)
;; (memo-fib 3) => 3
;;   (memo-fib 2) => 2
;;     (memo-fib 1) => 1
;;     (memo-fib 0) => 0
;;   (memo-fib 1) => gets memoized 1

;; The (memo-fib 1) only is actually executed
;; once, each time after it uses the pre-determined
;; value, as such it grows proprotional to n 

