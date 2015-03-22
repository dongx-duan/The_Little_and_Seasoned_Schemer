#lang scheme
;(define rember-f
;  (lambda (test? a l)
;    (cond
;      ((null? l) '())
;      ((test? (car l) a) (cdr l))
;      (else (cons (car l) 
;                  (rember-f test? a (cdr l)))))))

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) '())
        ((test? (car l) a) (cdr l))
        (else (cons (car l)
                    ((rember-f test?) a (cdr l))))))))

(define rember-eq? (rember-f eq?))

(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((test? (car l) old) (cons new (cons old (cdr l))))
        (else (cons (car l)
                    ((insertL-f test?) new old (cdr l))))))))
(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((test? (car l) old) (cons old (cons new (cdr l))))
        (else (cons (car l)
                    ((insertR-f test?) new old (cdr l))))))))

(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((eq? (car l) old) (seq new old (cdr l)))
        (else (cons (car l) 
                  ((insert-g seq) new old (cdr l))))))))

(define insertL (insert-g seqL))
(define insertR (insert-g seqR))

(define seqS
  (lambda (new old l)
    (cons new l)))
(define subt (insert-g seqS))


(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        ((null? lat) '())
        ((test? (car lat) a) ((multirember-f test?) a (cdr lat)))
        (else (cons (car lat) ((multirember-f test?) a (cdr lat))))))))

; Given a test? like  (lambda (k) (eq? k 'tuna))
(define multiremberT
  (lambda (test? lat)
    (cond
      ((null? lat) '())
      ((test? (car lat)) (multiremberT test? (cdr lat)))
      (else (cons (car lat) (multiremberT test? (cdr lat)))))))

;
(define multirember&co
  (lambda (a lat col)
    (cond
      ((null? lat) (col '() '()))
      ((eq? (car lat) a) (multirember&co a (cdr lat)
                                         (lambda (newlat seen)
                                           (col newlat 
                                                (cons (car lat) seen)))))
      (else (multirember&co a (cdr lat)
                            (lambda (newlat seen)
                              (col (cons (car lat) newlat)
                                   seen)))))))

(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
      ((null? lat) (col '() 0 0))
      ((eq? (car lat) oldL) (multiinsertLR&co new oldL oldR (cdr lat) 
                                             (lambda (newlat L R)
                                               (col(seqL new oldL newlat)
                                                   (add1 L)
                                                   R))))
      ((eq? (car lat) oldR) (multiinsertLR&co new oldL oldR (cdr lat)
                                              (lambda (newlat L R)
                                                (col(seqR new oldR newlat)
                                                     L
                                                     (add1 R)))))
      (else (multiinsertLR&co new oldL oldR (cdr lat)
                              (lambda (newlat L R)
                                (col (cons (car lat) newlat)
                                     L
                                     R)))))))

(define even?
  (lambda (n)
    (= (remainder n 2) 0)))

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define even-only*
  (lambda (l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((even? (car l)) (cons (car l) (even-only* (cdr l))))
         (else
          (even-only* (cdr l)))))
      (else
       (cons (even-only* (car l))
             (even-only* (cdr l)))))))

(define even-only*&co
  (lambda (l col)
    (cond
      ((null? l) (col '() 1 0))
      ((atom? (car l))
       (cond
         ((even? (car l)) (even-only*&co (cdr l) 
                                      (lambda (newlat p s)
                                        (col 
                                          (cons (car l) newlat)
                                          (* p (car l))
                                          s))))
         (else (even-only*&co (cdr l)
                              (lambda (newlat p s)
                                (col 
                                 newlat p (+ s (car l))))))))
      (else
       (even-only*&co (car l)
                      (lambda (al ap as)
                        (even-only*&co (cdr l)
                                       (lambda (dl dp ds)
                                         (col
                                          (cons al dl)
                                          (* ap dp)
                                          (+ as ds))))))))))
       