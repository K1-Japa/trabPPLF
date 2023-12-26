#lang racket
(require rackunit)
(require rackunit/text-ui)


(define ns (make-base-namespace))

(define (func-to-lambda lista)
  (define etc (drop lista 2))
  (define args (rest (second lista)))
  (cons 'lambda (cons args etc))
  )

(define (nivel1)
  (displayln '"Você precisa SOMAR danos ao inimigo. Crie uma função que SOME o dano de DOIS feitiços e retorne o resultado da soma.")
  (display '"Insira a resposta aqui: ")
  (resp (read (current-input-port)) 1)
  )

(define(nivel1-solucao parametroA parametroB)
  (+ parametroA parametroB)
  )



(define (nivel2)
  (displayln '"Você precisa identificar os ataques inimigos. Sua função deve retornar:")
  (displayln (string-append '"#t: quando receber ATAQUE"))
  (displayln (string-append '"#f: quando receber qualquer outra coisa"))
  (resp (read (current-input-port)) 2) 
  )

(define (nivel2-solucao parametro)
  (cond
    [(equal? parametro "ATAQUE") #t]
    [else #f])
  )

(define (nivel3)
  (displayln '"Você precisa separar em 2 listas os buffs e os debuffs que você ira receber para ajudar a derrotar o taltal tal, para isso separe a lista em numeros pares e impares")
  (display '"Insira a sua função aqui: ")
  (resp (read (current-input-port)) 3)
  )


(define (nivel3-solucao lista pares impares)
  (cond
    [(empty? lista) (list pares impares)]
    [(and (number? (first lista)) (integer? (first lista)))
     (if (= (remainder (first lista) 2) 0)
         (nivel3-solucao (rest lista) (cons (first lista) pares) impares)
         (nivel3-solucao (rest lista) pares (cons (first lista) impares)))]
    [else
     (nivel3-solucao (rest lista) pares impares)])
  )

;; Chamar a função com uma lista para separar pares e ímpares
;;(display (separar '(1 2 3 4 5 6 11 12 13 15 2 2) '() '())) ; Substitua a lista pelo que desejar testar
;;(newline)



(define (nivel4-solucao vida golpes)
  (cond
    [(equal? vida 0) golpes]
    [else (nivel4-solucao (sub1 vida) (add1 golpes))])
  )

(define (errou-nivel nv)
  (cond
    [(= nv 1) (displayln "Você errou, tente novamente")]
    [(= nv 2) (displayln "Você errou, tente novamente")]
    [(= nv 3) (displayln "Você errou, tente novamente")]
    [(= nv 4) (displayln "Você errou, tente novamente")]
    )
  )

(define (resp resposta nivel)
  (with-handlers ([exn:fail? (lambda(e) (errou-nivel nivel))])
    (let ([exp (eval (func-to-lambda resposta) ns)])
      (verifica-resp exp nivel))
    )
  )

(define (verifica-resp func nivel)
  (if
   (cond
  [(= nivel 1) (verifica-resp-nivel1 func)]
  [(= nivel 2) (verifica-resp-nivel2 func)]
  [(= nivel 3) (verifica-resp-nivel3 func)]
    ;;[(= nivel 4) (teste-nivel4 func)]
    ) (displayln "Voce acertou") (errou-nivel nivel))
  )

(define (verifica-resp-nivel1 func)
  (and
   (if (equal? (func 1 1) (nivel1-solucao 1 1)) #t #f)
   (if (equal? (func 0 1) (nivel1-solucao 0 1)) #t #f)
   (if (equal? (func 0 0) (nivel1-solucao 0 0)) #t #f)
   (if (equal? (func -1 1) (nivel1-solucao -1 1)) #t #f)
   (if (equal? (func -1 -1) (nivel1-solucao -1 -1)) #t #f)
   (if (equal? (func 15.5 4) (nivel1-solucao 15.5 4)) #t #f)
   )
  )

(define (verifica-resp-nivel2 func)
  (and
   (if (equal? (func "ATAQUE") (nivel2-solucao "ATAQUE")) #t #f)
   (if (equal? (func "qualquer coisa") (nivel2-solucao 2)) #t #f)
   )
  )


(define (verifica-resp-nivel3 func)
  (and
   (if(equal? (func '(1 2 3 4 5 6) '()'()) (nivel3-solucao '(1 2 3 4 5 6) '() '()))#t #f)
   (if(equal? (func '(4 5 11 13 12 11 100 )'()'() ) (nivel3-solucao '(4 5 11 13 12 11 100) '() '()))#t #t)
   (if(equal? (func '(9 8 7 6 5 4 3 2 1)'()'()) (nivel3-solucao '(9 8 7 6 5 4 3 2 1) '() '()))#t #f)
   (if(equal? (func '(0)'()'()) (nivel3-solucao '(0) '() '()))#t #f)
  (if(equal? (func '(1)'()'()) (nivel3-solucao '(1) '() '()))#t #f)
   )
  )

;;XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXx

(define (printWithEnter text)
  (for ([section (in-list text)])
    (display section)
    (newline)
    (displayln "Press Enter to continue...")
    (flush-output)
    (read-char) ; Wait for Enter key
    (newline)
    ))

(displayln "Insira seu nome: ")
(define nome (read-line (current-input-port)))

(define text1
  (quasiquote( (unquote (string-append "Em um reino distante, existia um mundo paralelo onde os destinos eram tecidos pela linguagem mágica da programação. Neste universo, cada indivíduo nascia com um código único, determinando seus poderes e habilidades.
      Certo dia, um jovem chamado " nome " foi transportado para esse mundo mágico através de um portal inesperado. Ao despertar, descobriu que havia reencarnado como um Herói, destinado a derrotar o temível Rei Demônio que ameaçava a existência de todas as criaturas."))
     (unquote (string-append "Contudo, as habilidades do Herói só poderiam ser desbloqueadas através do conhecimento e domínio da linguagem de programação Racket, que era a base de todos os poderes mágicos daquele mundo. " nome " precisava aprender a utilizar esses códigos mágicos para manifestar seus poderes e salvar o reino."))
     (unquote (string-append "Em sua jornada, " nome " encontrou sábios e mestres que guardavam conhecimentos ancestrais sobre Racket. Para liberar seus poderes, ele precisava passar por testes elaborados pelos seres divinos que regiam o uso da magia. Cada vez que " nome " enfrentava um desafio divino, era questionado sobre conceitos complexos de Racket, desde definição de funções até manipulação de listas e recursões.")) ;;recursão até manipulação avançada de listas."
     (unquote (string-append "Com determinação e estudo árduo, " nome " começou a dominar a linguagem de programação, desvendando segredos e desbloqueando novos poderes. Ele aprendeu a conjurar escudos protetores com 'if-else', lançar feitiços de ataque com 'define'."))
     ;; Colocar algumas quests aqui para testar o usuário
     ;; Caso o usuário falhe, falar que ele ainda não era apto para suceder a todos os poderes do Heroi
     )))

(define text2
  (quasiquote((unquote (string-append "À medida que progredia, " nome " reunia aliados, cada um com sua própria habilidade única, todos unidos pelo objetivo comum de derrotar o Rei Demônio e salvar o mundo da perdição iminente."))
     (unquote (string-append "No confronto final, diante do Rei Demônio e suas legiões demoníacas, " nome " utilizou seu conhecimento em Racket para conjurar um poderoso algoritmo, capaz de desativar as defesas do vilão. Com coragem e determinação, " nome " se preparou para lançar o código final, para invocar um feitiço supremo que selaria o Rei Demônio."))
     ;; Quest final aqui
     ;; Caso o usuário falhe, falar que ainda o Rei Demônio era forte demais para o estado atual do usuário e que ele deveria refazer a jornada dele para se aprimorar
     (unquote (string-append "Após concluir o feitiço final, " nome " selou o Rei Demônio, restaurando a paz e a harmonia ao reino. "))
     (unquote (string-append nome " agora reconhecido como o Herói da Programação, tornou-se uma lenda, inspirando outros a dominar a linguagem mágica de Racket para proteger e preservar a ordem daquele universo encantado."))
     )))

(printWithEnter text1)
;;(nivel1)
;;(nivel2)
(nivel3)

(printWithEnter text2)
