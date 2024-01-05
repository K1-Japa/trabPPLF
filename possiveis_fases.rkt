#lang racket
(require rackunit)
(require rackunit/text-ui)

;;(define (nivel)
;;  (displayln '"Voce deve selecionar os aliados que tenham uma média de atributos maior ou iual a ")
;  (resp (read (current-input-port)) x) 
;  )




(define verifica-solu-nivel
  (test-suite
   "verifica-solu-nivel"
   (check-equal? (nivel-solucao 1 1 4) #f)
   (check-equal? (nivel-solucao 0 1 8) #f)
   (check-equal? (nivel-solucao 0 0 3) #f)
   (check-equal? (nivel-solucao 10 7 7) #t)
   ))

(define (nivel-solucao a b c)
  (>= (/ (+ a b c) 3) 7))

(define (executa-testes . testes)
(run-tests (test-suite "Todos os testes" testes))
(void))


;; Chama a função para executar os testes.
(executa-testes verifica-solu-nivel)



;;(define (nivel)
;;  (displayln '"Voce deve selecionar os aliados que tenham uma média de atributos maior ou iual a ")
;  (resp (read (current-input-port)) x) 
;  )
(define (nivel-solucao5 lista)
  (map string-length lista))

(define verifica-solu-nivel5
  (test-suite
   "verifica-comprimentos"
   (check-equal? (nivel-solucao5 '("banana" "laranja" "maçã")) '(6 7 4))
   (check-equal? (nivel-solucao5 '("um" "dois" "três" "quatro")) '(2 4 4 6))
   (check-equal? (nivel-solucao5 '("")) '(0))
   ))




;; Chama a função para executar os testes.
(executa-testes verifica-solu-nivel5)