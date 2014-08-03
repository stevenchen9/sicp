;;  *Exercise 4.55:* Give simple queries that retrieve the following
;;  information from the data base:
;;    1. all people supervised by Ben Bitdiddle;

(supervisor ?x (Bitdiddle Ben))
;;    2. the names and jobs of all people in the accounting division;

(job ?x (accounting . ?y))
;;    3. the names and addresses of all people who live in Slumerville.
(address ?x (Slumerville . ?y))


; (salary ?person ?amount)
; (address ?person ?where)
; (supervisor ?person ?super)
; (job ?person ?title)

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

