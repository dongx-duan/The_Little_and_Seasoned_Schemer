#lang scheme
(define rember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car lat) (rember a (cdr lat)))))))

(define first
  (lambda (l)
    (cond
      ((null? l) '())
      (else
       (cons (car (car l)) (first (cdr l)))))))

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons (car lat) 
                                 (cons new (cdr lat))))
      (else (cons (car lat) (insertR new old (cdr lat)))))))

(define subt
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new (cdr lat)))
      (else (cons (car lat) (subt new old (cdr lat)))))))

(define subt2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) '())
      ((or (eq? (car lat) o1) (eq? (car lat) o2)) (cons new
                                                        (cdr lat)))
      (else (cons (car lat) (subt2 new o1 o2 (cdr lat)))))))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a) (multirember a (cdr lat)))
      (else (cons (car lat)
                  (multirember a (cdr lat)))))))

(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons (car lat)
                                 (cons new (multiinsertR new old (cdr lat)))))
      (else (cons (car lat)
                  (multiinsertR new old (cdr lat)))))))

(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new (cons (car lat)
                                           (multiinsertL new old (cdr lat)))))
      (else (cons (car lat)
                  (multiinsertL new old (cdr lat)))))))
 
(define multisubt
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new
                                 (multisubt new old (cdr lat))))
      (else (cons (car lat)
                  (multisubt new old (cdr lat)))))))
