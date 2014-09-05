

















;; Lists 

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
















(define l3 (1 2 3 4))




(car (list 1 2))
(cdr (list 1 2 3 4))


(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (length2 items)
  (define (length-h items accum)
    (if (null? items)
        accum
        (length-h (cdr items) (+ 1 accum))))
  (length-h items 0))


(length '(1 2 2))
(length2 '(1 2 2))


















(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))
(cons 1
      (cons 2
            (cons 3
                  '(4 5 6))))
(append '(1 2 3) '(4 5 6))














(define (scale-list items factor)
  (if (null? items)
      null
      (cons (* (car items) factor)
            (scale-list (cdr items) factor))))

(scale-list (list 1 2 3 4 5) 10)











(+ 1 1 () 3)
(define (map proc items)
  (if (null? items)
      null
      (cons (proc (car items))
            (map proc (cdr items)))))

(map abs (list -10 2.5 -11.6 17))



(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))


(scale-list (list 1 2 3 4 5) 10)









;; Trees
;;                                            (3 4)
;;                                              |
;;                                              V
;;  ((1 2) 3 4)  +---+---+                  +---+---+     +---+---+
;;          ---->| * | *-+----------------->| * | *-+---->| * | / |
;;               +-|-+---+                  +-|-+---+     +-|-+---+
;;                 |                          |             |
;;                 V                          V             V
;;        (1 2)  +---+---+     +---+---+    +---+         +---+
;;          ---->| * | *-+---->| * | / |    | 3 |         | 4 |
;;               +-|-+---+     +-|-+---+    +---+         +---+
;;                 |             |
;;                 V             V
;;               +---+         +---+
;;               | 1 |         | 2 |
;;               +---+         +---+






(define x (cons (list 1 2) (list 3 4)))
(length x) ;; => 3











(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))


(count-leaves x)









(define (scale-tree tree factor)
  (cond ((null? tree) null)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

(scale-tree x 10)












(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))
(scale-tree x 10)
