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


;;   *Exercise 4.60:* By giving the query

;;        (lives-near ?person (Hacker Alyssa P))

;;   Alyssa P. Hacker is able to find people who live near her, with
;;   whom she can ride to work.  On the other hand, when she tries to
;;   find all pairs of people who live near each other by querying

;;        (lives-near ?person-1 ?person-2)

;;   she notices that each pair of people who live near each other is
;;   listed twice; for example,

;;        (lives-near (Hacker Alyssa P) (Fect Cy D))
;;        (lives-near (Fect Cy D) (Hacker Alyssa P))

;;   Why does this happen?  Is there a way to find a list of people who
;;   live near each other, in which each pair appears only once?
;;   Explain.

This happens because the pairs are unordered. A way to fix this would
be to order the return and then only take the top half.


;;   *Exercise 4.61:* The following rules implement a `next-to'
;;   relation that finds adjacent elements of a list:

;;        (rule (?x next-to ?y in (?x ?y . ?u)))

;;        (rule (?x next-to ?y in (?v . ?z))
;;              (?x next-to ?y in ?z))

;;   What will the response be to the following queries?

;;        (?x next-to ?y in (1 (2 3) 4))
((2 3) next-to 4 in (1 (2 3) 4))
(1 next-to (2 3) in (1 (2 3) 4))

;;        (?x next-to 1 in (2 1 3 1))
(2 next-to 1 in (2 1 3 1))
(3 next-to 1 in (2 1 3 1))


;;   *Exercise 4.62:* Define rules to implement the `last-pair'
;;   operation of *Note Exercise 2-17::, which returns a list
;;   containing the last element of a nonempty list.  Check your rules
;;   on queries such as `(last-pair (3) ?x)', `(last-pair (1 2 3) ?x)',
;;   and `(last-pair (2 ?x) (3))'.  Do your rules work correctly on
;;   queries such as `(last-pair ?x (3))' ?
(rule (last-pair (?x) (?x)))
(rule (last-pair (?u . ?v) (?x))
      (last-pair ?v (?x)))

(last-pair (3) ?x)
;; => (last-pair (3) 3)
(last-pair (1 2 3) ?x)
;; => (last-pair (1 2 3) 3)
(last-pair (2 ?x) (3))
;; =? (last-pair (2 3) (3))



;;   *Exercise 4.64:* Louis Reasoner mistakenly deletes the
;;   `outranked-by' rule (section *Note 4-4-1::) from the data base.
;;   When he realizes this, he quickly reinstalls it.  Unfortunately,
;;   he makes a slight change in the rule, and types it in as

(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          (and (outranked-by ?middle-manager ?boss)
               (supervisor ?staff-person ?middle-manager))))

;;   Just after Louis types this information into the system, DeWitt
;;   Aull comes by to find out who outranks Ben Bitdiddle. He issues
;;   the query

;;        (outranked-by (Bitdiddle Ben) ?who)

;;   After answering, the system goes into an infinite loop.  Explain
;;   why.

;; The two or queries are evaluated in parallel, which causes the
;; infinite loop
