(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (actual-value (if-predicate exp) env)
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

(define (eval exp env)
  (display "\nEval Call\n")
  (display exp)
  (display "\n")
  (display env)
  (display "\n")
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (actual-value (operator exp) env)
                (operands exp)
                env))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (actual-value exp env)
  (force-it (eval exp env)))

(define (apply procedure arguments env)
  (cond ((primitive-procedure? procedure) ;; evaluate
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env)))  
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           (list-of-args-sometimes-lazy (procedure-parameters-with-lazy procedure)
                                        arguments env) 
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps)
                                env))))

(define (list-of-args-sometimes-lazy procedure-args exps env)
  (if (no-operands? exps)
      '()
      (let [(first-arg (first-operand exps))
            (first-param (first-operand procedure-args))]
        (cons
         (cond ((lazy-arg? first-param) (delay-it first-arg env))
               ((memo-arg? first-param) (delay-it first-arg env))
               (else (actual-value first-arg env)))
         (list-of-args-sometimes-lazy (rest-operands procedure-args)
                                      (rest-operands exps)
                                      env)))))

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))

(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value
                        (thunk-exp obj)
                        (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result)  ; replace `exp' with its value
           (set-cdr! (cdr obj) '())     ; forget unneeded `env'
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))

(define (delay-it exp env)
  (display "\nDelaying:")
  (display exp)
  (display "\n")
  (list 'thunk exp env))

(define (memo-it exp env)
  (list 'thunk exp env))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (thunk-exp thunk) (cadr thunk))

(define (thunk-env thunk) (caddr thunk))
