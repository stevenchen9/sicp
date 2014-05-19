;;   *Exercise 4.25:* Suppose that (in ordinary applicative-order
;;   Scheme) we define `unless' as shown above and then define
;;   `factorial' in terms of `unless' as

;;        (define (factorial n)
;;          (unless (= n 1)
;;                  (* n (factorial (- n 1)))
;;                  1))

;;   What happens if we attempt to evaluate `(factorial 5)'?  Will our
;;   definitions work in a normal-order language?

;; Yes, it should, because it will stop when n = 1


;;   *Exercise 4.26:* Ben Bitdiddle and Alyssa P. Hacker disagree over
;;   the importance of lazy evaluation for implementing things such as
;;   `unless'.  Ben points out that it's possible to implement `unless'
;;   in applicative order as a special form.  Alyssa counters that, if
;;   one did that, `unless' would be merely syntax, not a procedure
;;   that could be used in conjunction with higher-order procedures.
;;   Fill in the details on both sides of the argument.  Show how to
;;   implement `unless' as a derived expression (like `cond' or `let'),
;;   and give an example of a situation where it might be useful to
;;   have `unless' available as a procedure, rather than as a special
;;   form.

;; If we make unless a special form, we cannot use it with list comprehensions,
;; or pass it in to other functions, or return it from functions. It no longer
;; would be "first class".

;; We could make it like cond or let by wrapping functions in lambdas.

(define (unless1 cond left right)
  (if cond
      (right)
      (left)))

(unless1 true (lambda () 1) (lambda () 2))


;;   *Exercise 4.27:* Suppose we type in the following definitions to
;;   the lazy evaluator:

;;        (define count 0)

;;        (define (id x)
;;          (set! count (+ count 1))
;;          x)

;;   Give the missing values in the following sequence of interactions,
;;   and explain your answers.(5)

;;        (define w (id (id 10)))

;;        ;;; L-Eval input:
;;        count
;;        ;;; L-Eval value:
;;        0

;;        ;;; L-Eval input:
;;        w
;;        ;;; L-Eval value:
;;        10

;;        ;;; L-Eval input:
;;        count
;;        ;;; L-Eval value:
;;        2

;; The define will not have been evaluated yet, so until we try to
;; lookup the value of w, the counter will not have been incremented.

