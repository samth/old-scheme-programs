

(define base-object-class
  (lambda ()
    (lambda (msg)
      (error (cons 'Message (cons msg
                                  '(not understood by object))))))))

(define person-class
  (lambda (name age)
    (let ((base-object (base-object-class)))
      (lambda (msg)
        (cond
          [(equal? msg 'name) name]
          [(equal? msg 'age) age]
          [else (base-object msg)])))))

(define picky-person-class
  (lambda (name age)
    (let ((P (person-class name age)))
      (lambda (msg)
        (cond
          [(equal? msg 'likes) '(ice cream candy cake)]
          [(equal? msg 'hungry) 'yes]
          [else (P msg)])))))

(define glutton-class
  (lambda (name age)
    (let ((P (picky-person-class name age)))
      (lambda (msg)
        (cond
          [(equal? msg 'likes) 'everything]
          [else (P msg)])))))

(define classy-person-class
  (lambda (name age female?)
    (let ((p (person-class name age)))
      (lambda (msg)
        (cond
          [(equal? msg 'name)
           (cond
             [female? (cons 'lady (cons name '()))]
             [else (cons 'lord (cons name '()))])]
          [(equal? msg 'gender)
           (cond
             [female? 'female]
             [else 'male])]
          [else (p msg)])))))

(define bucket-class
  (lambda (contents)
    (let ((base-object (base-object-class)))
      (lambda (msg)
        (cond
          [(equal? msg 'get) contents]
          [(equal? msg 'set) (lambda (x)
                               (begin (set! contents x) x))]
          [else (base-object msg)])))))

(define counter-class
  (lambda ()
    (let ((counter (bucket-class 0)))
      (lambda (msg)
        (cond
          [(equal? msg 'inc)
           ((counter 'set) (add1 (counter 'get)))]
          [(equal? msg 'dec)
           ((counter 'set) (sub1 (counter 'get)))]
          [else (counter msg)])))))

(define n-step-counter-class
  (lambda (n)
    (let ((counter (bucket-class 0)))
      (lambda (msg)
        (cond
          [(equal? msg 'inc)
           ((counter 'set) (+ n (counter 'get)))]
          [(equal? msg 'dec)
           ((counter 'set) (- (counter 'get) n))]
          [else (counter msg)])))))

(define boolean-class
  (lambda (init-contents)
    (let ((b (bucket-class init-contents)))
      (lambda (msg)
        (cond
          [(equal? msg 'true) ((b 'set) #t)]
          [(equal? msg 'false) ((b 'set) #f)]
          [(equal? msg 'not) ((b 'set) (not (b 'get)))]
          [(equal? msg 'false?) (not (b 'get))]
          [(equal? msg 'true?) (b 'get)]
          [else (b msg)])))))


(define container-class
  (lambda ()
    (let ((contents (bucket-class '())))
      (lambda (msg)
        (cond
          [(equal? msg 'empty?)
           (null? (contents 'get))]
          [(equal? msg 'flush)
           ((contents 'set) '())]
          [(equal? msg 'member?)
           (lambda (x) (member? x (contents 'get)))]
          [else (contents msg)])))))

(define stack-class
  (lambda ()
    (let ((holder (container-class)))
      (lambda (msg)
        (cond
          [(equal? msg 'push) (lambda (x)
                                ((holder 'set)
                                 (cons x (holder 'get))))]
          [(equal? msg 'pop) (cond
                               [(holder 'empty?)
                                (error '(stack is empty))]
                               [else (let ((pile (holder 'get)))
                                       (begin
                                         ((holder 'set) (rest pile))
                                         (first pile)))])]
          [(equal? msg 'top)
           (cond
             [(holder 'empty?)
              (error '(stack is empty))]
             [else (first (holder 'get))])]
          [else (holder msg)])))))

(define queue-class
  (lambda ()
    (let ((holder (container-class)))
      (lambda (msg)
        (cond
          [(equal? msg 'enqueue)
           (lambda (x)
             (begin
               ((holder 'set) (cons-to-end x (holder 'get)))
               (last (holder 'get))))]
          [(equal? msg 'dequeue)
           (cond
             [(holder 'empty?)
              (error '(queue is empty))]
             [else (let ((pile (holder 'get)))
                     (begin
                       ((holder 'set) (rest pile))
                       (first pile)))])]
          [else (holder msg)])))))