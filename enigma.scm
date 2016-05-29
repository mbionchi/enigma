(define rotor-i (list
   '(#\A #\E) '(#\B #\K) '(#\C #\M) '(#\D #\F) '(#\E #\L) '(#\F #\G) '(#\G #\D)
   '(#\H #\Q) '(#\I #\V) '(#\J #\Z) '(#\K #\N) '(#\L #\T) '(#\M #\O) '(#\N #\W)
   '(#\O #\Y) '(#\P #\H) '(#\Q #\X)   "NOTCH"  '(#\R #\U) '(#\S #\S) '(#\T #\P)
   '(#\U #\A) '(#\V #\I) '(#\W #\B) '(#\X #\R) '(#\Y #\C) '(#\Z #\J)))

(define rotor-ii (list
   '(#\A #\A) '(#\B #\J) '(#\C #\D) '(#\D #\K) '(#\E #\S)   "NOTCH"  '(#\F #\I)
   '(#\G #\R) '(#\H #\U) '(#\I #\X) '(#\J #\B) '(#\K #\L) '(#\L #\H) '(#\M #\W)
   '(#\N #\T) '(#\O #\M) '(#\P #\C) '(#\Q #\Q) '(#\R #\G) '(#\S #\Z) '(#\T #\N)
   '(#\U #\P) '(#\V #\Y) '(#\W #\F) '(#\X #\V) '(#\Y #\O) '(#\Z #\E)))

(define rotor-iii (list
   '(#\A #\B) '(#\B #\D) '(#\C #\F) '(#\D #\H) '(#\E #\J) '(#\F #\L) '(#\G #\C)
   '(#\H #\P) '(#\I #\R) '(#\J #\T) '(#\K #\X) '(#\L #\V) '(#\M #\Z) '(#\N #\N)
   '(#\O #\Y) '(#\P #\E) '(#\Q #\I) '(#\R #\W) '(#\S #\G) '(#\T #\A) '(#\U #\K)
   '(#\V #\M)   "NOTCH"  '(#\W #\U) '(#\X #\S) '(#\Y #\Q) '(#\Z #\O)))

(define reflector-b (list
   '(#\A #\Y) '(#\B #\R) '(#\C #\U) '(#\D #\H) '(#\E #\Q) '(#\F #\S) '(#\G #\L)
   '(#\H #\D) '(#\I #\P) '(#\J #\X) '(#\K #\N) '(#\L #\G) '(#\M #\O) '(#\N #\K)
   '(#\O #\M) '(#\P #\I) '(#\Q #\E) '(#\R #\B) '(#\S #\F) '(#\T #\Z) '(#\U #\C)
   '(#\V #\W) '(#\W #\V) '(#\X #\J) '(#\Y #\A) '(#\Z #\T)))

(define rotate
  (lambda (lst)
    (append (cdr lst) (list (car lst)))))

(define rotate-rotors
  (lambda (rotors)
    rotors))


(define get-val
  (lambda (pairs key)
    (let ((h (car pairs)) (t (cdr pairs)))
      (cond ((list? (car pairs)) (cond ((char=? (car h) key) (car (cdr h)))
                                       (else (get-val t key))))
            (else (get-val t key))))))

(define encrypt-circuit
  (lambda (circuit char)
    (cond ((null? circuit) char)
          (else (encrypt-circuit (cdr circuit) (get-val (car circuit) char))))))

(define encrypt-char
  (lambda (rotors reflector char)
    (encrypt-circuit
      (append
        rotors
        (list reflector)
        (reverse (map (lambda (lst)
                        (map (lambda (elm)
                               (cond ((list? elm) (reverse elm))
                                     (else elm))) lst))
                        rotors)))
      char)))

(define encrypt
  (lambda (rotors reflector input)
    (cond ((null? input) '())
          (else (let ((rotors (rotate-rotors rotors)))
                  (cons (encrypt-char rotors reflector (car input))
                        (encrypt (rotate-rotors rotors) reflector (cdr input))))))))

;(tracing 1)
(display (encrypt (list rotor-iii rotor-ii rotor-i) reflector-b (string->list "HELLOXWORLD")))
