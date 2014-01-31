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
