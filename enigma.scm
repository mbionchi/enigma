(define alpha       (string->list "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
(define rotor-i     (string->list "EKMFLGDQVZNTOWYHX|USPAIBRCJ"))
(define rotor-ii    (string->list "AJDKS|IRUXBLHWTMCQGZNPYFVOE"))
(define rotor-iii   (string->list "BDFHJLCPRTXVZNYEIWGAKM|USQO"))
(define rotor-iv    (string->list "ESOVPZJAYQ|UIRHXLNFTGKDCMWB"))
(define rotor-v     (string->list "VZBRGITYUPSDNHLXAWMJQOFECK|"))
(define reflector-a (string->list "EJMZALYXVBWFCRQUONTSPIKHGD"))
(define reflector-b (string->list "YRUHQSLDPXNGOKMIEBFZCWVJAT"))
(define reflector-c (string->list "FVPJIAOYEDRZXWGCTKUQSBNMHL"))

; TODO
(define rotate-rotors
  (lambda (rotors)
    rotors))

(define filter
  (lambda (predicate lst)
    (let filter ((predicate predicate) (lst lst) (result '()))
      (cond ((null? lst) result)
            (else (filter predicate (cdr lst) (append result (cond ((predicate (car lst)) (list (car lst))) (else '())))))))))

(define get-val
  (lambda (keys vals key)
    (let get-val ((keys keys) (vals vals) (key key))
      (cond ((equal? key (car keys)) (car vals))
            (else (get-val (cdr keys) (cdr vals) key))))))

(define encrypt-char
  (lambda (rotors reflector char)
    (let encrypt-char ((rotors (append rotors (list reflector))) (inv-rotors (reverse rotors)) (char char))
      (cond ((null? inv-rotors) char)
            ((null? rotors) (encrypt-char rotors (cdr inv-rotors) (get-val (filter (lambda (x) (not (equal? x #\|))) (car inv-rotors)) alpha char)))
            (else (encrypt-char (cdr rotors) inv-rotors (get-val alpha (filter (lambda (x) (not (equal? x #\|))) (car rotors)) char)))))))

(define encrypt
  (lambda (rotors reflector input)
    (let encrypt ((rotors (rotate-rotors rotors)) (reflector reflector) (input input) (result '()))
      (cond ((null? input) result)
            (else (encrypt (rotate-rotors rotors) reflector (cdr input)
                           (append result (list (encrypt-char rotors reflector (car input))))))))))

;(tracing 1)
(display (list->string (encrypt (list rotor-iii rotor-ii rotor-i) reflector-b (string->list "HELLOXWORLD"))))
