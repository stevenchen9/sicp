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
