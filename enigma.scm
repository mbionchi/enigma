(define alpha (list
  '(#\A #\A) '(#\B #\B) '(#\C #\C) '(#\D #\D) '(#\E #\E) '(#\F #\F) '(#\G #\G)
  '(#\H #\H) '(#\I #\I) '(#\J #\J) '(#\K #\K) '(#\L #\L) '(#\M #\M) '(#\N #\N)
  '(#\O #\O) '(#\P #\P) '(#\Q #\Q) '(#\R #\R) '(#\S #\S) '(#\T #\T) '(#\U #\U)
  '(#\V #\V) '(#\W #\W) '(#\X #\X) '(#\Y #\Y) '(#\Z #\Z)))

(define rotor-i (list
  '(#\A #\E) '(#\B #\K) '(#\C #\M) '(#\D #\F) '(#\E #\L) '(#\F #\G) '(#\G #\D)
  '(#\H #\Q) '(#\I #\V) '(#\J #\Z) '(#\K #\N) '(#\L #\T) '(#\M #\O) '(#\N #\W)
  '(#\O #\Y) '(#\P #\H) '(#\Q #\X) '()        '(#\R #\U) '(#\S #\S) '(#\T #\P)
  '(#\U #\A) '(#\V #\I) '(#\W #\B) '(#\X #\R) '(#\Y #\C) '(#\Z #\J)))
(define rotor-ii (list
  '(#\A #\A) '(#\B #\J) '(#\C #\D) '(#\D #\K) '(#\E #\S) '()        '(#\F #\I)
  '(#\G #\R) '(#\H #\U) '(#\I #\X) '(#\J #\B) '(#\K #\L) '(#\L #\H) '(#\M #\W)
  '(#\N #\T) '(#\O #\M) '(#\P #\C) '(#\Q #\Q) '(#\R #\G) '(#\S #\Z) '(#\T #\N)
  '(#\U #\P) '(#\V #\Y) '(#\W #\F) '(#\X #\V) '(#\Y #\O) '(#\Z #\E)))
(define rotor-iii (list
  '(#\A #\B) '(#\B #\D) '(#\C #\F) '(#\D #\H) '(#\E #\J) '(#\F #\L) '(#\G #\C)
  '(#\H #\P) '(#\I #\R) '(#\J #\T) '(#\K #\X) '(#\L #\V) '(#\M #\Z) '(#\N #\N)
  '(#\O #\Y) '(#\P #\E) '(#\Q #\I) '(#\R #\W) '(#\S #\G) '(#\T #\A) '(#\U #\K)
  '(#\V #\M) '()        '(#\W #\U) '(#\X #\S) '(#\Y #\Q) '(#\Z #\O)))
(define reflector-b (list
  '(#\A #\Y) '(#\B #\R) '(#\C #\U) '(#\D #\H) '(#\E #\Q) '(#\F #\S) '(#\G #\L)
  '(#\H #\D) '(#\I #\P) '(#\J #\X) '(#\K #\N) '(#\L #\G) '(#\M #\O) '(#\N #\K)
  '(#\O #\M) '(#\P #\I) '(#\Q #\E) '(#\R #\B) '(#\S #\F) '(#\T #\Z) '(#\U #\C)
  '(#\V #\W) '(#\W #\V) '(#\X #\J) '(#\Y #\A) '(#\Z #\T)))

(define filter
  (lambda (predicate lst)
    (let filter ((predicate predicate) (lst lst) (result '()))
      (cond ((null? lst) result)
            (else (filter predicate (cdr lst) (append result (cond ((predicate (car lst)) (list (car lst))) (else '())))))))))

(define rotate
  (lambda (lst)
    (append (cdr lst) (list (car lst)))))

(define rotate-rotors
  (lambda (rotors)
    (let rotate-rotors ((rotors rotors) (fun rotate) (result '()))
      (cond ((null? rotors) result)
            (else (let ((rotor (car rotors)))
                    (cond ((null? (car rotor)) (rotate-rotors (cdr rotors) fun (append result (list (fun (fun rotor))))))
                          (else (rotate-rotors (cdr rotors) (lambda (x) x) (append result (list (fun rotor))))))))))))

(define get-val
  (lambda (keys vals key)
    (let get-val ((keys keys) (vals vals) (key key))
      (cond ((equal? key (car keys)) (car vals))
            (else (get-val (cdr keys) (cdr vals) key))))))

(define encrypt-char
  (lambda (rotors reflector char)
    (let encrypt-char
      ((rotors (cons alpha (append rotors (list reflector) (reverse (map (lambda (x) (map reverse x)) rotors)) (list alpha))))
       (char char))
      (cond ((null? (cdr rotors))
             char)
            (else
             (encrypt-char
               (cdr rotors)
               (get-val
                 (map (lambda (x) (car x)) (filter (lambda (x) (not (null? x))) (car rotors)))
                 (map (lambda (x) (car x)) (filter (lambda (x) (not (null? x))) (car (cdr rotors))))
                 (get-val
                   (map (lambda (x) (car x)) (filter (lambda (x) (not (null? x))) (car rotors)))
                   (map (lambda (x) (car (cdr x))) (filter (lambda (x) (not (null? x))) (car rotors)))
                   char))))))))

(define encrypt
  (lambda (rotors reflector input)
    (let encrypt ((rotors (rotate-rotors rotors)) (reflector reflector) (input input) (result '()))
      (cond ((null? input) result)
            (else (encrypt (rotate-rotors rotors) reflector (cdr input)
                           (append result (list (encrypt-char rotors reflector (car input))))))))))

;(tracing 1)
(display (list->string (encrypt (list rotor-iii rotor-ii rotor-i) reflector-b (string->list "HELLOXWORLD"))))
(display "\n")
