;;  *Exercise 4.55:* Give simple queries that retrieve the following
;;  information from the data base:
;;    1. all people supervised by Ben Bitdiddle;

(supervisor ?x (Bitdiddle Ben))
;;    2. the names and jobs of all people in the accounting division;

(job ?x (accounting . ?y))
;;    3. the names and addresses of all people who live in Slumerville.
(address ?x (Slumerville . ?y))



;;   *Exercise 4.56:* Formulate compound queries that retrieve the
;;   following information:

;;     a. the names of all people who are supervised by Ben Bitdiddle,
;;        together with their addresses;

(and (supervisor ?person (Bitdiddle Ben))
     (address ?person ?where))

;;     b. all people whose salary is less than Ben Bitdiddle's,
;;        together with their salary and Ben Bitdiddle's salary;

(or (salary (Bitdiddle Ben) ?bens)
    (and (salary ?person ?amount)
         (lisp-value > ?amount ?bens)))

;;     c. all people who are supervised by someone who is not in the
;;        computer division, together with the supervisor's name and
;;        job.

(and (supervisor ?person ?super)
     (job ?super ?supertitle)
     (not (job ?super (computer . ?x))))

; (salary ?person ?amount)
; (address ?person ?where)
; (supervisor ?person ?super)
; (job ?person ?title)

;;   *Exercise 4.57:* Define a rule that says that person 1 can replace
;;   person 2 if either person 1 does the same job as person 2 or
;;   someone who does person 1's job can also do person 2's job, and if
;;   person 1 and person 2 are not the same person. Using your rule,
;;   give queries that find the following:
;;     a. all people who can replace Cy D. Fect;
(rule (samejob ?p1 ?p2)
      (and (job ?p1 ?p2job)
           (job ?p2 ?p1job)
           (same ?p1job ?p2job)))
(rule (can-replace ?er ?ee)
      (and (or (samejob ?er ?ee)
               (and (samejob ?er ?other)
                    (can-replace ?other ?ee)))
           (not (same ?er ?ee))))

(can-replace (Cy D. Fect) ?p)

;;     b. all people who can replace someone who is being paid more
;;        than they are, together with the two salaries.
(rule (is-paid-less ?p1 ?p2)
      (and (salary ?p1 ?p1Sal)
           (salary ?p2 ?p2Sal)
           (lisp-value < ?p1Sal ?p2Sal)))

(and (can-replace ?p1 ?p2)
     (is-paid-less ?p1 ?p2)
     (salary ?p1 ?p1Sal)
     (salary ?p2 ?p2Sal))
