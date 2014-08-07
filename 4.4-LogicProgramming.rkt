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

;;   *Exercise 4.58:* Define a rule that says that a person is a "big
;;   shot" in a division if the person works in the division but does
;;   not have a supervisor who works in the division.

(rule (division ?p ?div)
      (job ?p (?div . ?_)))

(rule (big-shot ?p)
      (and (division ?p ?div)
           (supervisor ?p1 ?s)
           (division ?s ?superDiv)
           (not (same ?superDiv ?div))))


;;   *Exercise 4.59:* Ben Bitdiddle has missed one meeting too many.
;;   Fearing that his habit of forgetting meetings could cost him his
;;   job, Ben decides to do something about it.  He adds all the weekly
;;   meetings of the firm to the Microshaft data base by asserting the
;;   following:

;;        (meeting accounting (Monday 9am))
;;        (meeting administration (Monday 10am))
;;        (meeting computer (Wednesday 3pm))
;;        (meeting administration (Friday 1pm))

;;   Each of the above assertions is for a meeting of an entire
;;   division.  Ben also adds an entry for the company-wide meeting
;;   that spans all the divisions.  All of the company's employees
;;   attend this meeting.

;;        (meeting whole-company (Wednesday 4pm))

;;     a. On Friday morning, Ben wants to query the data base for all
;;        the meetings that occur that day.  What query should he use?

(meeting ?div (Friday ?time))

;;     b. Alyssa P. Hacker is unimpressed.  She thinks it would be much
;;        more useful to be able to ask for her meetings by specifying
;;        her name.  So she designs a rule that says that a person's
;;        meetings include all `whole-company' meetings plus all
;;        meetings of that person's division.  Fill in the body of
;;        Alyssa's rule.

(rule (division ?p ?div)
      (job ?p (?div . ?_)))
(rule (meeting-time ?person ?day-and-time)
      (and (division ?person ?div)
           (meeting ?div ?day-and-time)
           (meeting whole-company ?day-and-time)))

;;     c. Alyssa arrives at work on Wednesday morning and wonders what
;;        meetings she has to attend that day.  Having defined the
;;        above rule, what query should she make to find this out?

(meeting-time (Hacker Alyssa P) (Wednesday ?time))

;; (salary ?person ?amount)
;; (address ?person ?where)
;; (supervisor ?person ?super)
;; (job ?person ?title)
;; (meeting ?div (?day ?time))
