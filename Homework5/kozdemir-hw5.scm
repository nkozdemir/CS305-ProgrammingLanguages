(define get-operator (lambda (op-symbol env)
  (cond
    ((eq? op-symbol '+) +)
    ((eq? op-symbol '*) *)
    ((eq? op-symbol '-) -)
    ((eq? op-symbol '/) /)
    (else (display "cs305: ERROR\n\n" ) (repl env)))))

(define get-value (lambda (var env)
    (cond 
       ( (null? env) (display "cs305: ERROR\n\n") (repl env)) 
       ( (eq? var (caar env)) (cdar env))
       ( else (get-value var (cdr env))))))

(define extend-env (lambda (var val old-env)
        (cons (cons var val) old-env)))

(define define-expr? (lambda (e)
         (and (list? e) (= (length e) 3) (eq? (car e) 'define) (symbol? (cadr e)))))

(define if-expr? (lambda (e)
   (and (list? e) (equal? (car e) 'if) (= (length e) 4))))

(define cond-param? (lambda (e)
   (if (null? e) #f
      (if (and (list? (car e)) (= (length (car e)) 2))
         (if (equal? (car (car e)) 'else) 
               (if (null? (cdr e)) #t #f) 
            (cond-param? (cdr e)) 
         )
         #f ))))

(define cond-expr? (lambda (e)
   (and (list? e) (equal? (car e) 'cond) (> (length e) 2) (cond-param? (cdr e)))))

(define let-param? (lambda (e)
   (if (list? e)
      (if (null? e) #t
         (if (= (length (car e)) 2) (let-param? (cdr e)) #f)))))

(define let-expr? (lambda (e)
   (and (list? e) (equal? (car e) 'let) (= (length e) 3) (let-param? (cadr e)))))

(define letst-expr? (lambda (e)
   (and (list? e) (equal? (car e) 'let*) (= (length e) 3) (let-param? (cadr e)))))

(define last-elem (lambda (e)
      (last-elem (cdr e))))

(define s7 (lambda (e env)
   (cond
      ( (number? e) e)
      ( (symbol? e) (get-value e env))
      ( (not (list? e)) (display "cs305: ERROR\n\n") (repl env))
      ( (not (> (length e) 1)) (display "cs305: ERROR\n\n") (repl env))

      ((if-expr? e) 
         (if (= (s7 (cadr e) env) 0)
            (s7 (cadddr e) env)
         (s7 (caddr e) env))) 
      
      ((cond-expr? e) 
	      (if (= (length e) 3) 
		      (if (= (s7 (caadr e) env) 0) 
               (s7 (car (cdaddr e)) env)
		      (s7 (cadadr e) env)) 
		   (let ((if-c (caadr e)) (then-c (cadadr e)) (else-c (cons 'cond (cddr e)))) 
			   (let ((new-expr (list 'if if-c then-c else-c))) (s7 new-expr env)))))

      ((let-expr? e)
         (let ((var (map car (cadr e))) (init (map cadr (cadr e))))
            (let ((val (map (lambda (x) (s7 x env)) init)))
               (let ((nenv (append (map cons var val) env))) (s7 (caddr e) nenv)))))
      
      ((letst-expr? e)
         (if (or (= (length (cadr e)) 0) (= (length (cadr e)) 1))
            (s7 (list 'let (cadr e) (caddr e)) env)
            (let* ((var (s7 (car (cdaadr e)) env)) (nenv (cons (cons (caaadr e) var) env)))
                  (s7 (list 'let* (cdadr e) (caddr e)) nenv))))

      ( else 
         (let (
                (operator (get-operator (car e) env))
                (operands (map s7 (cdr e) (make-list (length (cdr e) ) env )))
              )
              (apply operator operands))))))

(define repl (lambda (env)
   (let* (
           (dummy1 (display "cs305> "))
           (expr (read))
           (new-env (if (define-expr? expr) 
                        (extend-env (cadr expr) (s7 (caddr expr) env) env)
                        env
                    ))
           (val (if (define-expr? expr)
                    (cadr expr)
                    (s7 expr env)
                ))
           (dummy2 (display "cs305: "))
           (dummy3 (display val))
           (dummy4 (newline))
           (dummy5 (newline))
          )
          (repl new-env))))

(define cs305 (lambda () (repl '())))