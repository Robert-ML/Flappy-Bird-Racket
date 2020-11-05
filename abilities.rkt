#lang racket

(provide fill-abilities)
(provide compose-abilities)
(provide hourglass)
(provide get-ability-image)
(provide get-ability-time)
(provide get-ability-pos)
(provide get-ability-next)
(provide (struct-out ability))

(require "random.rkt")
(require lang/posn)
(require 2htdp/image)

;; structura mea de abilitati
(define-struct ability (image time posn function) #:transparent)

; Imaginea si range-ul în care vor aparea abilitațile
; Nu modificați
(define POSITION_RANGE '((300 2000) (30 550)))
(define (hourglass color) (underlay
 (rectangle 40 40 "solid" color)
 (polygon
  (list (make-posn 0 0)
        (make-posn 25 0)
        (make-posn 0 25)
        (make-posn 25 25))
  "outline"
  (make-pen "darkslategray" 5 "solid" "round" "round"))))


; Fiecare funcție returneaza o componenta a unei abilități.
(define get-ability-image (λ (_ability) (ability-image    _ability)))
(define get-ability-time  (λ (_ability) (ability-time     _ability)))
(define get-ability-pos   (λ (_ability) (ability-posn     _ability)))
(define get-ability-next  (λ (_ability) (ability-function _ability)))

; Returneaza o poziție aleatorie în POSITION_RANGE.
(define (random-position range)
	(apply make-posn (map ((curry apply) random) range)))

; Returnează o listă de n elemente alese aleatoriu din lista L.
(define (choice-abilities n L)
	(sample (discrete-dist L) n))

; Va parcurge abitatile și pentru cele care au poziția null va asigna
; una aletorie.
; Folosiți random-position
(define (position-abilities abilities)
	(map (λ (x) (if (null? (get-ability-pos x))
                        (struct-copy ability x [posn (random-position POSITION_RANGE)])
                        x))
             abilities))

; Fiecare abilitate are o funcție next care modifica stare jocului
; Compuneti toate funcțiile next în una singură
; Hint: compose
(define (compose-abilities L)
	(apply compose (map get-ability-next L)))

; Primiște o listă de abilități inițiale, un număr n
; și o listă cu toate abilități posibile.
; Va adauga elemente aleatoare la lista inițială pană aceasta are lungimea n
; Atentie n poate fi chiar si 0 cand vrem sa jucam fara nicio abilitate.
; Folosiți choice-abilities.
(define (fill-abilities initial n abilities)
	(position-abilities (append initial (choice-abilities (- n (length initial)) abilities))))
