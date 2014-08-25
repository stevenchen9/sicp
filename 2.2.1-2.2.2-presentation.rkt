;; Lists are created by chaining cons cells

;;       +---+---+     +---+---+     +---+---+     +---+---+
;;  ---->| * | *-+---->| * | *-+---->| * | *-+---->| * | / |
;;       +-|-+---+     +-|-+---+     +-|-+---+     +-|-+---+
;;         |             |             |             |
;;         V             V             V             V
;;       +---+         +---+         +---+         +---+
;;       | 1 |         | 2 |         | 3 |         | 4 |
;;       +---+         +---+         +---+         +---+


(define l1 (cons 1
                 (cons 2
                       (cons 3
                             (cons 4 null)))))

(define l2 (list 1 2 3 4))

(define l3 '(1 2 3 4))



(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(length '(1 2 2))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))
(append '(1 2 3) '(4 5 6))



(define (scale-list items factor)
  (if (null? items)
      null
      (cons (* (car items) factor)
            (scale-list (cdr items) factor))))

(scale-list (list 1 2 3 4 5) 10)



(define (map proc items)
  (if (null? items)
      null
      (cons (proc (car items))
            (map proc (cdr items)))))

(map abs (list -10 2.5 -11.6 17))



(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))
