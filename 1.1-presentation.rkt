;; 1 - Intro
;; 1.1 - Goals - Everyone complete it!

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

(+ 2 2)


(/ 40 2)










;; Naming

(define size 2)

(* 5 size)











;; Procedures

(define (times-two x)
  (* 2 x))

(times-two 4)












;; Conditionals

(cond (<p1> <e1>
       <p2> <e2>
       ...
       <pn> <en>))

(define (abs x)
  (cond ((< x 0) (- x))
        (else x)))

(if <predicate> <then> <else>)

(define (abs x)
  (if (< x 0)
      (- x)
      x))


(and <e1> .... <en>)
(or <e1> .... <en>)
(not <e>)
