#lang racket


;; ----- data structures -----
(struct Closure (fun env))
(struct Value (data rator rand [grad #:mutable]) #:transparent)

;; ----- main code -----
(define forward
  (lambda (exp)
    (forward-inner exp env0)))

(define forward-inner
  (lambda (exp env)
    (match exp
      [(? symbol? x)
       (let ([v (lookup x env)])
         (cond
          [(not v)
           x]
          [else v]))]
      [(? number? x) (Value x '() '() '())]
      [`(lambda (,x) ,e)
       (Closure exp env)]
      [`(let ([,x ,e1]) ,e2)
       (let ([v1 (interp e1 env)])
         (interp e2 (ext-env x v1 env)))]
      [`(,e1 ,e2)
       (let ([v1 (interp e1 env)]
             [v2 (interp e2 env)])
         (match v1
           [(Closure `(lambda (,x) ,e) env-save)
            (interp e (ext-env x v2 env-save))]))]
      [`(,op ,e1 ,e2)
       (let ([v1 (forward-inner e1 env)]
             [v2 (forward-inner e2 env)])
         (let ([v (match op
                    ['+ (+ (Value-data v1) (Value-data v2))]
                    ['- (- (Value-data v1) (Value-data v2))]
                    ['* (* (Value-data v1) (Value-data v2))]
                    ['/ (/ (Value-data v1) (Value-data v2))])])
           (Value v op (list v1 v2) '())))])))

(define backward
  (lambda (exp out)
    (match exp
      [(Value v op '() _)
       (void)]
      [(Value v op (list v1 v2) _)
       (let* ([back-op (get-backward op)]
              [grads (back-op out (Value-data v1) (Value-data v2))])

         (set-Value-grad! exp grads)
         (backward v1 (car grads))
         (backward v2 (cadr grads))
         exp)])))

(define prepare-ops
  (lambda ()
    (register-op '+ (lambda (out v1 v2) (list out out)))
    (register-op '* (lambda (out v1 v2) (list (*  out v2) (* out v1))))))


(prepare-ops)

(define v1 (forward '(* 2 3)))
(backward v1 1)

(define v2 (forward '(+ 2 (* 3 5))))
(backward v2 2)


;; ----- environment -----
(define env0 '())

(define ext-env
  (lambda (x v env)
    (cons `(,x . ,v) env)))

(define lookup
  (lambda (x env)
    (let ([p (assq x env)])
      (cond
       [(not p) #f]
       [else (cdr p)]))))

(define *op-map* '())

(define register-op
  (lambda (forward backward)
    (set! *op-map* (cons `(,forward . ,backward) *op-map*))))

(define get-backward
  (lambda (op)
    (lookup op *op-map*)))


;; ----- examples -----
(r2 '(+ 1 2))
;; => 3

(r2 '(* 2 3))
;; => 6

(r2 '(* 2 (+ 3 4)))
;; => 14

(r2 '(* (+ 1 2) (+ 3 4)))
;; => 21

(r2 '((lambda (x) (* 2 x)) 3))
;; => 6

(r2
'(let ([x 2])
   (let ([f (lambda (y) (* x y))])
     (f 3))))
;; => 6

(r2
'(let ([x 2])
   (let ([f (lambda (y) (* x y))])
     (let ([x 4])
       (f 3)))))
;; => 6
