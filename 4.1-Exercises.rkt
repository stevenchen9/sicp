;;   *Exercise 4.1:* Notice that we cannot tell whether the
;;   metacircular evaluator evaluates operands from left to right or
;;   from right to left.  Its evaluation order is inherited from the
;;   underlying Lisp: If the arguments to `cons' in `list-of-values'
;;   are evaluated from left to right, then `list-of-values' will
;;   evaluate operands from left to right; and if the arguments to
;;   `cons' are evaluated from right to left, then `list-of-values'
;;   will evaluate operands from right to left.

;;   Write a version of `list-of-values' that evaluates operands from
;;   left to right regardless of the order of evaluation in the
;;   underlying Lisp.  Also write a version of `list-of-values' that
;;   evaluates operands from right to left.

(define (lr-list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let* ((first (eval (first-operand exps) env))
             (rest (eval (rest-operands exps) env)))
        (cons left rest))))

(define (rl-list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let* ((rest (eval (rest-operand exps) env))
             (first (eval (first-operands exps) env)))
        (cons left rest))))

(load "4.1-MetacircularEvaluator.rkt")
(load "4.1-Scheme-Impl.rkt")


;;   *Exercise 4.2:* Louis Reasoner plans to reorder the `cond' clauses
;;   in `eval' so that the clause for procedure applications appears
;;   before the clause for assignments.  He argues that this will make
;;   the interpreter more efficient: Since programs usually contain more
;;   applications than assignments, definitions, and so on, his
;;   modified `eval' will usually check fewer clauses than the original
;;   `eval' before identifying the type of an expression.

;;     a. What is wrong with Louis's plan?  (Hint: What will Louis's
;;        evaluator do with the expression `(define x 3)'?)

;;     b. Louis is upset that his plan didn't work.  He is willing to
;;        go to any lengths to make his evaluator recognize procedure
;;        applications before it checks for most other kinds of
;;        expressions.  Help him by changing the syntax of the
;;        evaluated language so that procedure applications start with
;;        `call'.  For example, instead of `(factorial 3)' we will now
;;        have to write `(call factorial 3)' and instead of `(+ 1 2)'
;;        we will have to write `(call + 1 2)'.

;; a: it will try to treat define as a regular function application,
;;    for which there is no function called "define", it is a
;;    special form in our little scheme

;; b: Since all the code to change the basics of the langauge are found
;;    in the "impl" code, we only need to change these three functions:

(define (application? exp) (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))


;;   *Exercise 4.3:* Rewrite `eval' so that the dispatch is done in
;;   data-directed style.  Compare this with the data-directed
;;   differentiation procedure of *Note Exercise 2-73::.  (You may use
;;   the `car' of a compound expression as the type of the expression,
;;   as is appropriate for the syntax implemented in this section.)

;; (define (add-proc sym proc) (...))
;; (define (get-proc sym) (...))
;; (define (is-proc sym) (...))
(add-proc 'lambda (lambda (exp env)
                    (make-procedure (lambda-parameters exp)
                                    (lambda-body exp)
                                    env)))
(add-proc 'define (lambda (exp env) (eval-definition exp env)))
(add-proc 'if (lambda (exp env) (eval-if exp env)))
(add-proc 'begin (lambda (exp env) (eval-sequence (begin-actions exp) env)))
(add-proc 'cond (lambda (exp env) (eval (cond->if exp) env)))
(add-proc 'set! (lambda (exp env) (eval-assignment exp env)))
(add-proc 'quote (lambda (exp env) (text-of-quotation exp)))
(add-proc 'call (lambda (exp env) (apply (eval (operator exp) env)
                                         (list-of-values (operands exp) env))))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((is-proc (car exp)) ((get-proc (car exp)) exp env))
        (else
         (error "Unknown expression type -- EVAL" exp))))


;; *Exercise 4.4:* Recall the definitions of the special forms `and'
;; and `or' from *Note Chapter 1:::
  
;;    * `and': The expressions are evaluated from left to right.  If
;;      any expression evaluates to false, false is returned; any
;;      remaining expressions are not evaluated.  If all the
;;      expressions evaluate to true values, the value of the last
;;      expression is returned.  If there are no expressions then
;;      true is returned.
  
;;    * `or': The expressions are evaluated from left to right.  If
;;      any expression evaluates to a true value, that value is
;;      returned; any remaining expressions are not evaluated.  If
;;      all expressions evaluate to false, or if there are no
;;      expressions, then false is returned.
  
  
;; Install `and' and `or' as new special forms for the evaluator by
;; defining appropriate syntax procedures and evaluation procedures
;; `eval-and' and `eval-or'.  Alternatively, show how to implement
;; `and' and `or' as derived expressions.

(define (and? exp) (tagged-list? exp 'and))
(define (eval-and exp env)
  (define (inner exps)
    (if (empty? exps)
        #t
        (if (eval (car exps) env)
            (inner (cdr exps))
            #f)))
  (inner exp))

(define (or? exp) (tagged-list? exp 'or))
(define (eval-or exp env)
  (define (inner exps)
    (if (empty? exps)
        #f
        (let ((e (eval (car exps) env)))
          (if e
              e
              (inner (cdr exps))))))
  (inner exp))


;; DO THIS FOR SICP CLASS
;;   *Exercise 4.5:* Scheme allows an additional syntax for `cond'
;;   clauses, `(<TEST> => <RECIPIENT>)'.  If <TEST> evaluates to a true
;;   value, then <RECIPIENT> is evaluated.  Its value must be a
;;   procedure of one argument; this procedure is then invoked on the
;;   value of the <TEST>, and the result is returned as the value of
;;   the `cond' expression.  For example

;;        (cond ((assoc 'b '((a 1) (b 2))) => cadr)
;;              (else false))

;;   returns 2.  Modify the handling of `cond' so that it supports this
;;   extended syntax.
 
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

(define (cond-alt? clause)
  (eq? (cadr clause) '=>))

(define (cond-predicate clause) (car clause))
(define (cond-alt-func clause) (cadr clause))

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
            (if (cond-alt? first)
                (make-let 'a (cond-predicate first)
                          (make-alt-if 'a
                                       ((cond-alt-func first) 'a)
                                       (expand-clauses rest)))
                (make-if (cond-predicate first)
                         (sequence->exp (cond-actions first))
                         (expand-clauses rest)))))))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))
(define (make-let binding value body)
  (list 'let (list (list binding value)) body))
;; (make-let 'a 5 '(+ a b))


;;   *Exercise 4.6:* `Let' expressions are derived expressions, because

;;        (let ((<VAR_1> <EXP_1>) ... (<VAR_N> <EXP_N>))
;;          <BODY>)

;;   is equivalent to

;;        ((lambda (<VAR_1> ... <VAR_N>)
;;           <BODY>)
;;         <EXP_1>
;;         ...
;;         <EXP_N>)

;;   Implement a syntactic transformation `let->combination' that
;;   reduces evaluating `let' expressions to evaluating combinations of
;;   the type shown above, and add the appropriate clause to `eval' to
;;   handle `let' expressions.

(define (let? exp) (tagged-list? exp 'let))
(define (bindings exp) (cadr exp))
(define (body exp) (cddr exp))
(define (let->combination exp)
  (cons (list 'lambda (map car (bindings exp)) (body exp))
        (map cadr (bindings exp))))
(let->combination '(let ((x 1) (y 2)) (+ x y)))
;; => ((lambda (x y) ((+ x y))) 1 2)


;;   *Exercise 4.7:* `Let*' is similar to `let', except that the
;;   bindings of the `let' variables are performed sequentially from
;;   left to right, and each binding is made in an environment in which
;;   all of the preceding bindings are visible.  For example

;;        (let* ((x 3)
;;               (y (+ x 2))
;;               (z (+ x y 5)))
;;          (* x z))

;;   returns 39.  Explain how a `let*' expression can be rewritten as a
;;   set of nested `let' expressions, and write a procedure
;;   `let*->nested-lets' that performs this transformation.  If we have
;;   already implemented `let' (*Note Exercise 4-6::) and we want to
;;   extend the evaluator to handle `let*', is it sufficient to add a
;;   clause to `eval' whose action is

;;        (eval (let*->nested-lets exp) env)

;;   or must we explicitly expand `let*' in terms of non-derived
;;   expressions?

(define (let*? exp) (tagged-list? exp 'let*))
(define (bindings exp) (cadr exp))
(define (body exp) (cddr exp))
(define (let*->combination exp)
  (define (i-let bindings)
    (if (empty? bindings)
        (car (body exp))
        (cons (list 'lambda
                    (list (caar bindings))
                    (i-let (cdr bindings)))
              (cadar bindings))))
  (i-let (bindings exp)))
(let*->combination '(let ((x 1) (y 2)) (+ x y)))
;; => ((lambda (x) ((lambda (y) (+ x y)) . 2)) . 1)


;;   *Exercise 4.8:* "Named `let'" is a variant of `let' that has the
;;   form

;;        (let <VAR> <BINDINGS> <BODY>)

;;   The <BINDINGS> and <BODY> are just as in ordinary `let', except
;;   that <VAR> is bound within <BODY> to a procedure whose body is
;;   <BODY> and whose parameters are the variables in the <BINDINGS>.
;;   Thus, one can repeatedly execute the <BODY> by invoking the
;;   procedure named <VAR>.  For example, the iterative Fibonacci
;;   procedure (section *Note 1-2-2::) can be rewritten using named
;;   `let' as follows:

;;        (define (fib n)
;;          (let fib-iter ((a 1)
;;                         (b 0)
;;                         (count n))
;;            (if (= count 0)
;;                b
;;                (fib-iter (+ a b) a (- count 1)))))

;;   Modify `let->combination' of *Note Exercise 4-6:: to also support
;;   named `let'.
(define (let? exp) (tagged-list? exp 'let))

(define (named-let? exp) (and (not (pair? (cadr exp)))
                              (tagged-list? exp 'let)))
(define (named-let-var exp) (cadr exp))
(define (named-let-bindings exp) (caddr exp))
(define (named-let-body exp) (cdddr exp))

(define (bindings exp) (cadr exp))
(define (body exp) (cddr exp))
(define (let->combination exp)
  (if (named-let? exp)
      (sequence->exp (list
                      (list 'define (cons (named-let-var exp)
                                          (map car (named-let-bindings exp)))
                            (sequence->exp (named-let-body exp)))
                      (cons (named-let-var exp)
                            (map cadr (named-let-bindings exp)))))
      (cons (list 'lambda
                  (map car (bindings exp))
                  (sequence->exp (body exp)))
            (map cadr (bindings exp)))))
(let->combination '(let test ((x 1) (y 2)) (+ x y) (print "test")))
;; => (begin (define (test x y) (begin (+ x y) (print "test"))) (test 1 2))
(let->combination '(let ((x 1) (y 2)) (+ x y) (print "test")))


;;   *Exercise 4.9:* Many languages support a variety of iteration
;;   constructs, such as `do', `for', `while', and `until'.  In Scheme,
;;   iterative processes can be expressed in terms of ordinary
;;   procedure calls, so special iteration constructs provide no
;;   essential gain in computational power.  On the other hand, such
;;   constructs are often convenient.  Design some iteration
;;   constructs, give examples of their use, and show how to implement
;;   them as derived expressions.

;; Made a "while" that matches (while <pred> <body>)
(define (while? exp) (tagged-list? exp 'while))
(define (pred exp) (cadr exp))
(define (body exp) (caddr exp))
(define (while->comb exp)
  (cons (list 'define (list 'iter-name '())
              (list 'if
                    (pred exp)
                    (list 'begin
                          (body exp)
                          (list 'iter-name))))
        (list (list 'iter-name))))
(while->comb '(while (true) (body)))
;; => ((define (iter-name ()) (if (true) (begin (body) (iter-name)))) (iter-name))


