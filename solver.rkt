#lang typed/racket

(require typed/rackunit)

(define-type expr (U binop Symbol Integer))
(struct binop ([op : Symbol] [left : expr] [right : expr])#:transparent)
(struct constraint ([e : expr] [num : Integer])#:transparent)
(define-type mapping (Pairof Symbol Integer))

(define (top-interp [s : Sexp] [env : Sexp] [mod : Integer]) : Integer
  (interp (parse s) (parse-mappings env) mod))

(define (parse [s : Sexp]) : expr
  (match s
    [(list (? symbol? sym) left right) (binop sym (parse left) (parse right))]
    [(? symbol? sym) sym]))

(define (parse-mappings [s : Sexp]) : (Listof mapping)
  (match s
    [(list (list (? symbol? var) (? integer? i)) rest ...)
     (cons (cons var (cast i Integer)) (parse-mappings rest))]
    ['() '()]))

(define (get-op [op : Symbol]) : (-> Integer Integer Integer)
  (match op
    ['+ +]
    ['* *]))

(define (subst [sym : Symbol] [env : (Listof mapping)]) : Integer
  (match env
    ['() (error 'subst "not good")]
    [(cons (cons varname int) rest) (if (equal? varname sym) int (subst sym rest))]))

(define (interp [e : expr] [env : (Listof mapping)] [mod : Integer]) : Integer
  (match e
    [(binop op l r) (modulo ((get-op op) (interp l env mod) (interp r env mod)) mod)]
    [(? symbol? sym) (subst sym env)]))

(define (parse-constraint [s : Sexp]) : constraint
  (match s
    [(list ex '= (? integer? i)) (constraint (parse ex) (cast i Integer))]))

(define (cart-prod-n [nums : (Listof Integer)] [how-many : Integer]) : (Listof Any)
  (if (equal? how-many 1)
      nums
      (cartesian-product (cart-prod-n nums (- how-many 1)) nums)))

(define (glue-lists
         [a : (Listof Symbol)]
         [b : (Listof Integer)]) : (Listof (Listof (U Symbol Integer)))
  (match* (a b)
    [((cons firsta resta) (cons firstb restb))
     (cons (list firsta firstb) (glue-lists resta restb))]
    [('() '()) '()]))

(define (satisfy
         [cs : (Listof Sexp)]
         [vars : (Listof Symbol)]
         [mod : Integer]) : (Listof (Listof Integer))
  (define constraints
    (map
     (lambda ([s : Sexp]) : constraint
       (parse-constraint s))
     cs))
  (filter
   (lambda ([valuation : (Listof Integer)]) : Boolean
     (define mappings (parse-mappings (glue-lists vars valuation)))
     (: done? (Boxof Boolean))
     (define done? (box #f))
     (andmap
      (lambda ([c : constraint]) : Boolean
        (and
         (not (unbox done?))
         (begin
           (if (equal? (interp (constraint-e c) mappings mod) (constraint-num c))
               #t
               (begin
                 (set-box! done? #t)
                 #f)))))
      constraints))
   (cast
    (map
     (lambda ([l : Any]) : (Listof Any)
       (flatten l))
     (cart-prod-n (range mod) (length vars)))
    (Listof (Listof Integer)))))

(check-equal? (top-interp '(+ a d) '((a 5) (d 6)) 100) 11)
(check-equal? (top-interp '(* a d) '((a 5) (d 6)) 100) 30)
(check-equal? (top-interp '(* a (+ b c)) '((a 6) (b 3) (c 2)) 100) 30)
(check-equal?
 (map
  (lambda ([l : Any]) : (Listof Any)
    (flatten l))
  (cart-prod-n '(1 2) 4))
 (cartesian-product '(1 2) '(1 2) '(1 2) '(1 2)))
(check-equal? (glue-lists (list 'a 'b 'c) (list 1 2 3)) '((a 1) (b 2) (c 3)))
(check-equal?
 (satisfy
  '(((+ a d) = 0))
  '(a d)
  3)
 '((0 0) (1 2) (2 1)))
(check-equal?
 (satisfy
  '(((+ a d) = 0)
    ((+ b (* a d)) = 0)
    ((+ c (* b d)) = 0)
    ((* c d) = 1))
  '(a b c d)
  3)
 '())
(check-equal?
 (satisfy
  '(((+ a c) = 0)
    ((+ (+ b d) (* a c)) = 0)
    ((+ (* a d) (* b c)) = 0)
    ((* b d) = 1))
  '(a b c d)
  3)
 '((1 2 2 2) (2 2 1 2)))
