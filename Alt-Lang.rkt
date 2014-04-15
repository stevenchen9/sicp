(load "4.1-MetacircularEvaluator.rkt")
;; The "language specific" representation of the MC interfaces
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))
(define (variable? exp) (symbol? exp))

;; Quotations
;; (raw (this))
(define (quoted? exp)
  (tagged-list? exp 'raw))
(define (text-of-quotation exp) (cadr exp))

(define (in-tagged-list? exp tag)
  (if (pair? exp)
      (eq? (cadr exp) tag)
      false))
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

;; (test <= 5)
(define (assignment? exp)
  (in-tagged-list? exp '<=))
(define (assignment-variable exp) (car exp))
(define (assignment-value exp) (caddr exp))

;; Definitions
;; (def x 5)
(define (definition? exp)
  (tagged-list? exp 'def))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ; formal parameters
                   (cddr exp)))) ; body

;; Lambdas
;; ((x) => (+ x x))
(define (lambda? exp) (in-tagged-list? exp '=>))
(define (lambda-parameters exp) (car exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons parameters (cons '=> (list body))))
;; (make-lambda '(x y) '(+ y y))
;;  ((x y) => (+ x y))

;; Conditionals
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

;; "Begin" sexp
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))


;; Application
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))


;; Cond
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))

;; We choose to make cond a derived expression of nested if statements
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no `else' clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))
