; Hello
(define LAMBDA (string->symbol "\u03BB"))
; Ok! This project seems to compare expressions and return a scheme expression that, when evaluated,
; gives us x when % is true, and y when (not %) is true. If there is one special lambda, both do it

(define (bind x y) (string->symbol (string-append (symbol->string x) "!" (symbol->string y))))

(define (index lst i x) (if (equal? x (car lst)) i (index (cdr lst) (+ i 1) x)))

(define (lambda_type_check x y) (if (equal? x y) x LAMBDA))

(define (list_maker x y a b c) (cond
  [(and (equal? x '()) (equal? y '())) (list a b c)]
  [(equal? (car x) (car y)) (list_maker (cdr x) (cdr y) a b c)]
  [else (list_maker (cdr x) (cdr y) (cons (bind (car x) (car y)) a) (cons (car x) b) (cons (car y) c))]))
  
(define (bind_x_exp x y a b) (cond
  [(equal? x '()) '()]
  [(or (and (list? (car x)) (not (list? (car y)))) 
       (and (not (list? (car x))) (list? (car y)))
       (and (list? (car x)) (list? (car y)) (not (equal? (length (car x)) (length (car y))))))
   (cons (const_check (car x) (car y)) (bind_x_exp (cdr x) (cdr y) a b))]
  [(and (list? (car x)) (or (equal? (car (car x)) 'lambda) (equal? (car (car y)) 'lambda)
                            (equal? (car (car x)) LAMBDA) (equal? (car (car y)) LAMBDA)))
   (cons (expr-compare (car x) (car y)) (bind_x_exp (cdr x) (cdr y) a b))]
  [(list? (car x)) (cons (bind_x_exp (car x) (car y) a b) (bind_x_exp (cdr x) (cdr y) a b))]
  [(member (car x) b) (cons (let ((i (index b 0 (car x)))) (list-ref a i)) (bind_x_exp (cdr x) (cdr y) a b))]
  [else (cons (car x) (bind_x_exp (cdr x) (cdr y) a b))]))

(define (bind_y_exp y x a b) (cond
  [(equal? y '()) '()]
  [(or (and (list? (car y)) (not (list? (car x)))) 
       (and (not (list? (car y))) (list? (car x)))
       (and (list? (car y)) (list? (car x)) (not (equal? (length (car y)) (length (car x))))))
   (cons (const_check (car x) (car y)) (bind_y_exp (cdr y) (cdr x) a b))]
  [(and (list? (car y)) (or (equal? (car (car y)) 'lambda) (equal? (car (car x)) 'lambda)
                            (equal? (car (car y)) LAMBDA) (equal? (car (car x)) LAMBDA)))
   (cons (expr-compare (car x) (car y)) (bind_y_exp (cdr y) (cdr x) a b))]
  [(list? (car y)) (cons (bind_y_exp (car y) (car x) a b) (bind_y_exp (cdr y) (cdr x) a b))]
  [(member (car y) b) (cons (let ((i (index b 0 (car y)))) (list-ref a i)) (bind_y_exp (cdr y) (cdr x) a b))]
  [else (cons (car y) (bind_y_exp (cdr y) (cdr x) a b))]))

; checking cases with lambda expressions, 
(define (lambda_check x y)
  (let ((xC (list-ref x 1)) (yC (list-ref y 1)))
    (let ((r 
	   (if (and (list? xC) (list? yC) (equal? (length xC) (length yC))) (list_maker xC yC '() '() '())
      (if (and (not (list? xC)) (not (list? yC))) (list_maker (list xC) (list yC) '() '() '())
	 #f))))
        (if r
          (cons (lambda_type_check (car x) (car y))
            (expr-compare
              (bind_x_exp (cdr x) (cdr y) (car r) (list-ref r 1))
              (bind_y_exp (cdr y) (cdr x) (car r) (list-ref r 2))))
	    (const_check x y)))))

; This is the most basic expression checker: if we're dealing with constants!
(define (const_check x y)
  (cond
   [(equal? x y) x]
   [(and (equal? x #f) (equal? y #t)) '(not %)]
   [(and (equal? x #t) (equal? y #f)) '%]
   [else (list 'if '% x y)]))

; This normal check is what will allow for more complicated checks, such as nested expressions
(define (norm_check x y)
  (if (or (equal? x '()) (equal? y '())) '()
    (cons (expr-compare (car x) (car y)) (norm_check (cdr x) (cdr y)))))

;Here, we check the first list to see if there's anything we can do preemptively at this point
(define (fst_check x y) 
  (cond
   [(equal? (car x) 'quote) (const_check x y)]
   [(equal? (car x) 'lambda) (lambda_check x y)]
   [(equal? (car x) LAMBDA) (lambda_check x y)]
   [else (norm_check x y)]))

; Here is where we do more refined checks (I couldn't think of a way to do in one so split into 2: fst and snd
(define (snd_check x y)
  (cond
   [(or (equal? (car x) 'quote) (equal? (car y) 'quote) (equal? (car x) 'if) (equal? (car y) 'if)) (const_check x y)]
   [(or (and (equal? (car x) 'lambda) (equal? (car y) LAMBDA)) (and (equal? (car y) 'lambda) (equal? (car x) LAMBDA))) (lambda_check x y)]
   [else (norm_check x y)]))

(define (expr-compare x y)
  (if (and (list? x) (list? y))
    (if (equal? (length x) (length y))
      (if (equal? (car x) (car y))
        (fst_check x y)
        (snd_check x y))
      (const_check x y))
    (const_check x y)))

(define (test-expr-compare x y)
  (and (equal? (eval x) (eval (list 'let '((% #t)) (expr-compare x y)))) (equal? (eval y) (eval (list 'let '((% #f)) (expr-compare x y))))))

(define test-expr-x '(list "tism" (lambda (a b) (if (equal? (a b)) 1 2))))
(define test-expr-y '(list "bism" (λ (a c) (if (eqv? (a c)) 1 2))))

(test-expr-compare test-expr-x test-expr-y)
