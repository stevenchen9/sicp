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

;; 3.23
(define (make-deque)
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
            (error "empty deque")
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

(define q3 (make-deque))
((q3 'front-insert-deque!) 'a)
((q3 'front-insert-deque!) 'b)
((q3 'front-insert-deque!) 'c)
((q3 'rear-insert-deque!) 'd)
(q3 'view)
((q3 'front-delete-deque!))
((q3 'rear-delete-deque!) 'd)
(q3 'view)
