;; This section is about consequences


;; First, consequences of memory

;; Linear Recursion and Iteration
    ;; Linear Recursive PROCESS
    (define (factorial n)
      (if (= n 1)
          1
          (* n (factorial (- n 1)))))

         (factorial 6)        ------------------------.
         (* 6 (factorial 5))                          |
         (* 6 (* 5 (factorial 4)))                    |
         (* 6 (* 5 (* 4 (factorial 3))))              |
         (* 6 (* 5 (* 4 (* 3 (factorial 2)))))        |
         (* 6 (* 5 (* 4 (* 3 (* 2 (factorial 1))))))  |
         (* 6 (* 5 (* 4 (* 3 (* 2 1)))))              |
         (* 6 (* 5 (* 4 (* 3 2))))                    |
         (* 6 (* 5 (* 4 6)))                          |
         (* 6 (* 5 24))                               |
         (* 6 120)                                    |
         720          <-------------------------------'



    ;; Linear Iterative PROCESS
    (define (factorial n)
      (fact-iter 1 1 n))

    (define (fact-iter product counter max-count)
      (if (> counter max-count)
          product
          (fact-iter (* counter product)
                     (+ counter 1)
                     max-count)))

         (factorial 6)   -----.
         (fact-iter   1 1 6)  |
         (fact-iter   1 2 6)  |
         (fact-iter   2 3 6)  |
         (fact-iter   6 4 6)  |
         (fact-iter  24 5 6)  |
         (fact-iter 120 6 6)  |
         (fact-iter 720 7 6)  V
         720

    ;; They both are recursive PROCEDURES 

    ;; The second is iterative because its state
    ;; is entirely captured by: product counter max-count

    ;; In C, this would consume a lot more memory, it is not tail-recursive.
    ;; Tail-recursive means you can define an iterative process using
    ;; a recursive procedure, and have it not consume more than one iteration
    ;; of memory.



;; Tree Recursion

     (define (fib n)
       (cond ((= n 0) 0)
             ((= n 1) 1)
             (else (+ (fib (- n 1))
                      (fib (- n 2))))))


                           ..<............ fib5   <..........
                        ...     ___________/  \___________   .
                     ...       /       . .....            \    .
                   ..       fib4     .        . . . .     fib3  .
                 ..     ____/. \____  ..             .  __/  \__  .
               ..      /  . .  ..   \    .        ..   /  . .   \   .
             ..     fib3 .       .  fib2 .        . fib2 .   .  fib1 .
           ..      / . \  .     .   /  \  .      .  /  \ ...  .  |  .
         ..       / . . \   .  .   /  . \   .  .   / .  \   .  . 1 .
        .      fib2 . . fib1.  .fib1 .  fib0 . .fib1. . fib0 .  .  .
        .      /  \  . . |  .  . |  .  . |   . . |   . . |   .   ..
        .     /  . \   . 1  .  . 1  .  . 0  .  . 1  .  . 0  ..
        .  fib1 .. fib0..  .   .   .   .   .   .   .   ..  .
        .   |  .  . |  . ..     ...     . .    ....      ..
        .   1 .   . 0  .
         .   .     .  .
          ...       ..


    ;; Steps grows exponentially from the input
    ;; Space grows linearly


    ;; Redefine it to use an iterative process over recursive
     (define (fib n)
       (fib-iter 1 0 n))

     (define (fib-iter a b count)
       (if (= count 0)
           b
           (fib-iter (+ a b) a (- count 1))))

    ;; Tree-recursion is useful in other domains.

    ;; Notice how we can improve our algorithms once we
    ;; know where they fall short. E.G. Once we knew how many
    ;; steps our tree recursive fib was going to take, we could
    ;; redesign it to use iteration.


;; Example: Counting change

;; Orders of Growth

;; Examples: Exponentiation, GCD, Primality, 
