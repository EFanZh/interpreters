#lang racket


;; ----- data structures -----
(struct Closure (fun env))
(struct Value ([data #:mutable] rator rand [grad #:mutable]) #:transparent)

;; ----- main code -----
(define forward
  (lambda (exp)
    (forward1 exp env0)))

(define forward1
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
       (let ([v1 (forward1 e1 env)])
         (forward1 e2 (ext-env x v1 env)))]
      [`(,e1 ,e2)
       (let ([v1 (forward1 e1 env)]
             [v2 (forward1 e2 env)])
         (match v1
           [(Closure `(lambda (,x) ,e) env-save)
            (forward1 e (ext-env x v2 env-save))]))]
      [`(,op ,e1 ,e2)
       (let ([v1 (forward1 e1 env)]
             [v2 (forward1 e2 env)])
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

(define SGD
  (lambda (exp lr)
    (match exp
      [(Value v op '() _)
       exp]
      [(Value v op (list v1 v2) (list g1 g2))
       [set-Value-data! v1 (- (Value-data v1) (* g1 lr))]
       [set-Value-data! v2 (- (Value-data v2) (* g2 lr))]
       (SGD v1 lr)
       (SGD v2 lr)
       exp])))


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


;; ----- main -----
(prepare-ops)


;; ----- examples -----
(define v1 (forward '(* 2 3)))
(backward v1 1)
(SGD v1 0.1)

(define v2 (forward '(+ 2 (* 3 5))))
(backward v2 2)
(SGD v2 0.1)

(define v3 (forward '((lambda (x) (* 2 x)) 3)))
(backward v3 1)
(SGD v3 0.1)

(define v4
 (forward
  '(let ([x 2])
     (let ([f (lambda (y) (* x y))])
       (f 3)))))

(backward v4 1)
(SGD v4 0.1)

(define v5
  (forward
   '(let ([x 2])
      (let ([f (lambda (y) (* x y))])
        (let ([x 4])
          (f 3))))))

(backward v5 1)
(SGD v5 0.1)
