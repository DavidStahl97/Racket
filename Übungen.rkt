#lang racket

; 1. Programmieraufgaben

(require rackunit)

; 1.1 Einfügesort

;; fügt ein Element in eine schon sortierte Liste ein
(define (insert-element lst element)
  (if (null? lst)
      (list element)
      (if (< element (first lst))
          (cons element lst)
          (cons (first lst) (insert-element (rest lst) element)))))

(check-equal? (insert-element (list 1 2 4 5) 3) (list 1 2 3 4 5))
(check-equal? (insert-element '() 5) (list 5))
(check-equal? (insert-element (list 3 5 6) 7) (list 3 5 6 7))

;; sortiert eine Liste nach dem Einfügesort-Algorithmus:
;; nacheinander werden die Elemente aus der unsortierten Liste in eine sortierte eingefügt
(define (insert-sort unsorted-list)
  (define (insert-sort-iter lst sorted-list)
    (if (null? lst)
        sorted-list
        (insert-sort-iter (rest lst) (insert-element sorted-list (first lst)))))

  (insert-sort-iter unsorted-list '()))

(check-equal? (insert-sort (list 2 4 1 6 4)) (list 1 2 4 4 6))
(check-equal? (insert-sort '()) '())


; 1.2 Flatten

;; Rekursiv wird der Baum untersucht.
;; Die Blätter, die keine Liste sind, werden als Liste zurückgegeben
;; und dann werden die Blätter durch append zusammengeführt.
(define (flatten tree)
  (if (list? tree)
      (foldr append '()
             (map (λ (child) (flatten child)) tree))
      (list tree)))


(check-equal? (flatten '((a) b (c (d) e) ())) '(a b c d e))
(check-equal? (flatten '()) '())
(check-equal? (flatten '(a (b c (d)) e (f))) '(a b c d e f))


; 1.3 Ströme

;; cons-stream
(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream x y)
     (cons x (delay y)))))

(define head car)
(define (tail s) (force (cdr s)))

(define the-empty-stream '())
(define empty-stream? null?)

;; Datenabstraktion zur Vereinfachung
(define (make-fib-pair n fib) (list n fib))
(define (part-fib fib-pair) (first (rest fib-pair)))
(define (part-n fib-pair) (first fib-pair))

(define (calc-fib-pair fib1 fib2)
  (make-fib-pair (add1 (part-n fib2))
                 (+ (part-fib fib1) (part-fib fib2))))

;; Zusammenführung der zwei Ströme
(define (add-fib-pair s1 s2)
  (cons-stream (calc-fib-pair (head s1) (head s2))
               (add-fib-pair (tail s1) (tail s2))))

;; Der eigentliche Fib-Stream
(define fib-stream2
    (cons-stream (make-fib-pair 0 0)
               (cons-stream (make-fib-pair 1 1)
                            (add-fib-pair fib-stream2 (tail fib-stream2)))))

(define fib-stream (λ () fib-stream2))

;; Tests
(define a (fib-stream))

(check-equal? (head a) '(0 0))
(check-equal? (head (tail a)) '(1 1))
(check-equal? (head (tail (tail a))) '(2 1))
(check-equal? (head (tail (tail (tail a)))) '(3 2))
(check-equal? (head (tail (tail (tail (tail a))))) '(4 3))
(check-equal? (head (tail (tail (tail (tail (tail a)))))) '(5 5))
(check-equal? (head (tail (tail (tail (tail (tail (tail a))))))) '(6 8))



; 2. Theoriefragen

;; 2.1 Was ist funktionale Programmierung?
;;
;; Die funktionale Programmierung ist ein Programmierstil, der sich an die Funktionen aus der Mathematik anlegt.
;; Mit der gleichen Eingabe muss die Funktion immer die gleiche Ausgabe zurückgegeben, weshalb eine Funktion im Gegensatz zu einem Objekt aus OOP zustandslos ist.
;; Ausßerdem dürfen Funktionen als Parameter einer anderen Funktion übergeben werden.


;; 2.2 Sind (1 2 . 3), (#t #t #f), () Listen? Begründen Sie Ihre Antwort.
;;
;; Liste ist rekursiv mithilfe eines Paars definiert:
;; Eine Liste ist entweder eine leere Liste oder sie ist ein Paar bestehend aus einem Wert und einer Liste.
;;
;; 1. (1 2 . 3): keine Liste
(check-equal? (list? '(1 2 . 3)) false)
;; Mit der Klammer und dem Punkt wird ein Paar definiert.
;; Jedoch darf ein Paar nur aus zwei Werten bestehen.
;; Außerdem müsste der zweite Wert eine leere Liste sein, damit ein Paar eine gültige Liste ist:
(check-equal? (list? (cons 1 '())) true)
;;
;; 2. (#t #t #f): ist eine Liste
(check-equal? (list? '(#t #t #f)) true)
;;
;; 3. (): eine gültige, leere Liste
(check-equal? (list? '()) true)


;; 2.3 Was ist eine Umgebung (Environment) und wie wird darüber das statische Binden umgesetzt?
;;
;; Es gibt eine globale Umgebung, mithilfe des Symbols "define" ein Symbol mit einem Wert oder mit einem Lambda-Ausdruck gebunden werden kann:
(define konstante 1)
(define mal-2 (λ (a) (* a 2)))
;;
;; Dadurch kann eine Prozedur Symbole aus der globalen Umgebung verwenden, da sie dort definiert ist:
(define mal-4 (λ (a) (* (mal-2 a) 2)))
;; In einer anderen Umgebung kann das Symbol mal-4 eine andere Bedeutung haben.
;;
;; Bei einem Prozeduraufruf wird eine weitere Umgebung erzeugt, wo die Parameter mit den übergebenen Werten gebunden werden:
(mal-4 3)
;; Die neu erzeugte Umgebung bindet das Symbol a mit dem Wert 3.
;; Die neu erzeugte Umgebung besitzt ebenfalls ein Verweis auf die globale Umgebung, sodass das Symbol mal-2 gefunden werden kann.


;; 2.4 Was ist ein Abschlussobjekt (Closure) und wann entsteht es?
;;
;; Ein Abschlussobjekt besteht aus einem Lambda-Ausdruck und einer Umgebung, wo der Lambda-Ausdruck definiert ist.
;; Ein Beispiel ist das Abschlussobjekt mal-2 aus der Frage 3. Mit define wird das Abschlussobjekt erzeugt. Die Umgebung des Abschlussobjekts ist die globale Umgebung.
;;
;; Ein Beispiel mit einem Abschlussobjekt, dessen Umgebung nicht die globale ist:
(define (outer-func a)
  (λ (b) (+ a b)))

(define w1 (outer-func 2))
;; Das Abschlussobjekt w1 besteht aus dem erzeugten Lambda-Ausdruck vom Prozeduraufruf outer-func
;; und der Umgebung beim Aufruf von outer-func. Diese Umgebung hat das Symbol a mit dem Wert 2 gebunden und da diese Umgebung die Umgebung von w1 ist, kennt w1 den Wert 2 von a.


;; 2.5 Beschreiben Sie die Funktionsweise von compose und geben ein Beispiel für seine Nutzung an.
;;
;; Mit compose werden Prozeduren hintereinander verschachtelt und es wird die verschachtelte Prozedur zurückgegeben.
;; Beispiel: f(x)=sqrt(x + 1)
(define f (compose sqrt add1))
(check-equal? (f 3) 2)


;; 2.6 Warum kann man delay nicht als Funktion schreiben?
;;
;; Da Racket nach dem applicative-order vorgeht, werden zunächt die Argumente einer Applikation ausgewertet.
;; Doch die Ausführungen der Argumente sollen ja gerade durch delay verzögert werden.


;; 2.7 Welchen Vorteil bieten endrekursive Funktionen in Racket?
;;
;; Bei einer endrekursiven Funktion wird das Zwischenergebnis bei einem rekursiven Aufruf mitgegeben.
;; Das letzte Zwischenergebnis ist das Endergebnis und kann zurückgegeben werden.
;; Bei einer rekursiven Funktion werden zunächst die verschachtelten Operationen rekursiv aufgebaut
;; und erst ab dem letzten rekursiven Aufruf werden die Operationen ausgeführt.
;; Dadurch müssen die Operationen mit deren Operanden gespeichert werden, da sie nicht direkt wie bei einer endrekursiven Funktion ausgeführt werden.
;; Eine endrekursive Funktion verbraucht demnach weniger Speicherplatz.


;; 2.8 Was ist der Unterschied zwischen foldl und foldr? Wie unterscheidet sich das Laufzeitverhalten?
;;
;; foldr ist die rekusive Version:
(define (my-foldr func start lst)
  (if (null? lst)
      start
      (func (first lst) (my-foldr func start (rest lst)))))

(check-equal? (my-foldr append '() '((1) (2) (3))) '(1 2 3))



;; Verhalten von my-foldr mit append:
;; -> (append (1) (my-foldr))
;; -> (append (1) (append (2) (my-foldr)))
;; -> (append (1) (append (2) (append (3) (my-foldr))))
;; -> (append (1) (append (2) (append (3) ()))))
;; -> (1 2 3)

;; Generell arbeitet foldr mit einer beliebigen Funktion f(x, y) mit einer Liste (a b c) wie folgt:
;; f(a, f(b, f(c, start)))

;; foldl ist die endrekusive Version:
(define (my-foldl func acc lst)
  (if (null? lst)
      acc
      (my-foldl func (func (first lst) acc) (rest lst))))

(check-equal? (my-foldl append '() '((1) (2) (3))) '(3 2 1))

;; Verhalten von my-foldl mit append:
;; -> (append (1) ())
;; -> (append (2) (1))
;; -> (append (3) (2 1))
;; -> (3 2 1)

;; Generell arbeitet foldl mit einer beliebigen Funktion f(x, y) mit einer Liste (a b c) wie folgt:
;; f(c, f(b, f(a, start)))



; 3. Entwurf von Aufgaben

;; 3.1 Schreiben Sie eine map-Funktion.

(define (my-map func lst)
  (if (null? lst)
      '()
      (cons (func (first lst)) (map func (rest lst)))))

(check-equal? (my-map sqr '(1 2 3)) '(1 4 9))

;; 3.2 Gegeben sei der folgende λ-Term: ((λx.y (λy.y)) a).
;;     Geben Sie die Menge der freien und die Menge der gebundenen Variablen an.
;;
;; free(...) = {y, a}
;; bound(...) = {y, x}


;; 3.3 Schreiben Sie eine Oder-Funktion als λ-Term. Die Werte wahr und falsch sind als λ-Term wie folgt definiert:
;;     true := λx.λy.x
;;     false := λx.λy.y
;;     Als Beispiel ist die Und-Funktion wie folgt definiert:
;;     and := λx.λy.(x y false)
;;
;; or := λx.λy.(x true y)


;; 3.4 Mit welcher Idee kann die botschaftenorientierte Programmierung in Racket umgesetzt werden?
;;     Erklären Sie anhand des folgenden Beispiels was genau hinter dem Symbol "konto" steckt.
;;     ((konto auszahlen)