;; 1 - Intro

;; 2 - Overview
;; 2.1 - Values
;; 2.2 - Binding 
;; 2.3 - Procedures
;; 2.4 - Conditionals/Logic

;; 3 - Tricks/Tips
;; 3.1 - Doing all the exercises
;; 3.2 - Streaks
;; 3.3 - Racket documentation
;; 3.4 - Internet for homework





;; Primitives

453

888

(* 2 2)


(/ 40 2)










;; Naming

(define size 2)

(* 5 size)











;; Procedures

;; function times-two(x) {return 2 * x;} 
(define (times-two x)
  (* 2 x))

(define times-two 4)

(define (square x)
  (times-two x))
(square 4)













;; Conditionals

(cond (<p1> <e1>
       <p2> <e2>
       ...
       <pn> <en>))

(define (abs x)
  (cond ((< x 0) (- x))
        (else) x))
(abs 4)


(if <predicate> <then> <else>)

(define (abs x)
  (if (< x 0)
      (- x)
      x))


(and <e1> .... <en>)
(or <e1> .... <en>)
(not <e>)

(eq? #!unspecific (result))

(* 3 [5 3 (foo 2) 2]
   (* 2 4))


;; helpful tooling
paredit
rainbow parens
scheme/racket repl
auto pairs

;; drracket

