#lang racket
(require rackunit)
(require rackunit/text-ui)
(require racket/list)

;;Um jogo lúdico para o ensino de programação da linguagem racket de modo que ele seja educativo.
;;O jogo se passará por uma história fictícia, aonde o jogador deve
;;ler as perguntas/quests e respondê-las de forma correta para progredir na história.

;; =================

;;Funções


(define ns (make-base-namespace))

(parameterize ([current-namespace ns])
  (namespace-require 'racket))

;;Lista -> Lista
;;Função que recebe uma lista como parametro e retorna
;;A lista com determinados elementos trocados por 'x, mantendo a estrutura da lista original
(define (build elemento lista listafinal)
  (cond
    [(empty? lista) listafinal]
    [(or (symbol? (first lista)) (not (list? (first lista))))
     (if (equal? elemento (first lista))
         (build elemento (rest lista) (append listafinal (list 'x)))
         (build elemento (rest lista) (append listafinal (list (first lista))))
         )
     ]
    [(list? (first lista))
     (build elemento (rest lista) (append listafinal (list (build elemento (first lista) '()))))
     ]
    )
  )


;; Lista -> Lista
;;Função serve para substituir a palavra "define" no começo de uma função por "lambda".
(define (func-to-lambda lista)
  ;;Lista -> Lista
  ;; Função que pega o nome da função que foi inserida no argumento
  (define funcname (first (second lista)))
  ;;Lista -> Lista
  ;; Função que pega os argumentos da lista que foi inserida como argumento
  (define args (rest (second lista)))
  ;;Lista -> Lista
  ;;Função que obtém o resto da lista, com exceção dos 2 primeiros elementos da lista
  (define etc (drop lista 2))
  ;;(cons 'lambda(cons args (build funcname etc '())))

  ;;Lista -> Lista
  ;; Cria uma expressão 'letrec' para definir a função 'x' como uma lambda com os argumentos 'args' e o corpo 'etc'
  (list 'letrec (list (list 'x (append (list 'lambda args) (build funcname etc '())))) 'x)
  )


;;Void -> String
;;Função para mostrar a 1a fase para o jogador e pedir a resposta dele
(define (nivel1)
  (displayln '"Você precisa SOMAR danos ao inimigo. Crie uma função que SOME o dano de DOIS feitiços e retorne o resultado da soma.")
  (display '"Insira a resposta aqui: ")
  (resp (read (current-input-port)) 1)
  )

;; Numero -> Numero
;; Função que faz os testes unitarios na nossa solucao do nivel 1
(define testes-solucao-nivel1
  (test-suite
   "verifica-solu-nivel1"
   (check-equal? (nivel1-solucao 1 1) 2)
   (check-equal? (nivel1-solucao 0 1) 1)
   (check-equal? (nivel1-solucao 0 0)0)
   (check-equal? (nivel1-solucao -1 1)0)
   (check-equal? (nivel1-solucao -1 -1) -2)
   (check-equal? (nivel1-solucao 15.5 4 ) 19.5)
   
  ))

;; Numeros -> Numeros
;; Função resolução da fase 1
;; Função para somar 2 Numeros e retorna a sua somatória
(define(nivel1-solucao parametroA parametroB)
  (+ parametroA parametroB)
  )

;;Teste -> Void
;;Executa um conjunto de testes
(define (executa-testes . testes)
(run-tests (test-suite "Todos os testes" testes))
(void))


;; Funcao que executa a rodada de testes da funcao solucao
(executa-testes testes-solucao-nivel1)

;; Lista -> Boolean
;; Função que verifica se a função inserida pelo jogador retorna o valor desejado no nivel 1,
;; Com base nas nossos testes e solucoes
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

;;Void -> String
;;Função para mostrar a 2a fase para o jogador e pedir a resposta dele
(define (nivel2)
  (displayln '"Você precisa identificar os ataques inimigos. Sua função deve retornar:")
  (displayln (string-append '"#t: quando receber ATAQUE"))
  (displayln (string-append '"#f: quando receber qualquer outra coisa"))
  (resp (read (current-input-port)) 2) 
  )

;;String -> Boolean
;; Função que faz os testes unitarios na nossa solucao do nivel 2
(define testes-solucao-nivel2
  (test-suite
   "testes-solucao-nivel2"
   (check-equal? (nivel2-solucao "ATAQUE") #t )
   (check-equal? (nivel2-solucao 2)#f  )
   )
  )

;; String -> Boolean
;; Função resolução da fase 2
;; Função que verifica se o jogador foi atacado, retornando #t caso o jogador receba um ataque
;; e retornando #f caso seja algo diferente de um ataque
(define (nivel2-solucao parametro)
  (cond
    [(equal? parametro "ATAQUE") #t]
    [else #f])
  )


;; Funcao que executa a rodada de testes da funcao solucao
(executa-testes testes-solucao-nivel2)

;; Lista -> Boolean
;; Função que verifica se a função inserida pelo jogador retorna o valor desejado no nivel 2
;; Com base nas nossos testes e solucoes
(define (verifica-resp-nivel2 func)
  (and
   (if (equal? (func "ATAQUE") (nivel2-solucao "ATAQUE")) #t #f)
   (if (equal? (func "qualquer coisa") (nivel2-solucao 2)) #t #f)
   )
  )

;; ??? -> ???
;; Função resolução da fase 3
;; Função que ???
(define (nivel3-solucao vida golpes)
  (letrec ([x (lambda (vida golpes)
  (cond
    [(equal? vida 0) golpes]
    [else (x (sub1 vida) (add1 golpes))]))])
  x))



;; Lista -> Boolean
;; Função que verifica se a função inserida pelo jogador retorna o valor desejado no nivel 3
;; Com base nas nossos testes e solucoes
(define (verifica-resp-nivel3 func)
    (func 10 0)
    )


;; Void -> String
;; Função para mostrar a 4a fase para o jogador e pedir a resposta da resolução dele
(define (nivel4)
  (displayln '"Você precisa separar em 2 listas os buffs e os debuffs que você ira receber para ajudar a derrotar o taltal tal, para isso separe a lista em numeros pares e impares")
  (display '"Insira a sua função aqui: ")
  (resp (read (current-input-port)) 4)
  )

;; Lista -> Lista
;; Função resolução da fase 4
;; Função que recebe uma lista de numeros e
;; Separa eles em outras 2 listas, de numeros pares e numeros impares
(define (nivel4-solucao lista pares impares)
  (cond
    [(empty? lista) (list pares impares)]
    [(and (number? (first lista)) (integer? (first lista)))
     (if (= (remainder (first lista) 2) 0)
         (nivel4-solucao (rest lista) (cons (first lista) pares) impares)
         (nivel4-solucao (rest lista) pares (cons (first lista) impares)))]
    [else
     (nivel4-solucao (rest lista) pares impares)])
  )

;; Lista -> Lista
;; Função que faz os testes unitarios na nossa solucao do nivel 4
;; Com base nas nossos testes e solucoes
(define testes-solucao-nivel4
  (test-suite
   "testes-solucao-nivel4"
   (check-equal? (nivel4-solucao '(1 2 3 4 5 6) '() '()) '((6 4 2) (5 3 1))  )
   (check-equal? (nivel4-solucao '(4 5 11 13 12 11 100) '() '()) '((100 12 4)(11 13 11 5)))
   (check-equal? (nivel4-solucao '(9 8 7 6 5 4 3 2 1) '() '()) '((2 4 6 8)(1 3 5 7 9) ))
   (check-equal? (nivel4-solucao '(0) '() '()) '((0) ())  )
   (check-equal? (nivel4-solucao '(1) '() '()) '(() (1))  ) 
   )
  )


;; Funcao que executa a rodada de testes da funcao solucao
(executa-testes testes-solucao-nivel4)


;; Lista -> Boolean
;; Função que verifica se a função inserida pelo jogador retorna o valor desejado no nivel 4
(define (verifica-resp-nivel4 func)
  (and
   (if(equal? (func '(1 2 3 4 5 6) '()'()) (nivel4-solucao '(1 2 3 4 5 6) '() '())) #t #f)
   (if(equal? (func '(4 5 11 13 12 11 100 )'()'() ) (nivel4-solucao '(4 5 11 13 12 11 100) '() '()))#t #f)
   (if(equal? (func '(9 8 7 6 5 4 3 2 1)'()'()) (nivel4-solucao '(9 8 7 6 5 4 3 2 1) '() '()))#t #f)
   (if(equal? (func '(0)'()'()) (nivel4-solucao '(0) '() '()))#t #f)
   (if(equal? (func '(1)'()'()) (nivel4-solucao '(1) '() '()))#t #f)
   )
  )

;; Numero -> String/Função anonima??
;; Função que recebe em qual nivel o jogador cometeu um erro na resposta e
;; Retorna a mensagem de erro, permitindo a tentativa do usuario novamente
;; Neste mesmo nivel
(define (errou-nivel nv)
  (cond
    [(= nv 1)
     (let ([funcao (lambda ()
                     (displayln "\nVocê errou, tente novamente\n")
                     (nivel1))])
           (funcao))]
    [(= nv 2) (let ([funcao (lambda ()
                     (displayln "\nVocê errou, tente novamente\n")
                     (nivel2))])
           (funcao))]
    ;;[(= nv 3) (let ([funcao (lambda ()
      ;;               (displayln "\nVocê errou, tente novamente\n")
        ;;             (nivel3))])
          ;; (funcao))]
    [(= nv 4) (let ([funcao (lambda ()
                     (displayln "\nVocê errou, tente novamente\n")
                     (nivel4))])
           (funcao))]
    )
  )

;; String + Numero -> Funçao anonima
;; Função que avalia a resposta do usuario como uma função e
;; Retorna a funçao de 'errou nivel nivel' caso a resposta do usuario esteja errada(exceção)
(define (resp resposta nivel)
  (with-handlers ([exn:fail? (lambda(e) (errou-nivel nivel))])
    (let ([exp (eval (func-to-lambda resposta) ns)])
      (verifica-resp exp nivel))
    )
)

;; Funçao + Numero -> String ou Funçao
;; Função que verifica se o jogador inseriu uma função que retorna a resposta correta
;; Caso o jogador acerte o resultado, a função retorna a String
;; Caso contrario retorna a função 'errou-nivel'
(define (verifica-resp func nivel)
  (if
   (cond
  [(= nivel 1) (verifica-resp-nivel1 func)]
  [(= nivel 2) (verifica-resp-nivel2 func)]
  [(= nivel 3) (verifica-resp-nivel3 func)]
  ;;[(= nivel 4) (verifica-resp-nivel4 func)]
    ) (displayln "\nVoce acertou!\n") (errou-nivel nivel))
  )



;;XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

;; Jogo

;; Void -> String
;; Funcao que pede para o jogador apertar 'enter' para continuar contando a historia do jogo
(define (printWithEnter text)
  (for ([section (in-list text)])
    (display section)
    (newline)
    (displayln "Press Enter to continue...")
    (flush-output)
    (read-char) ; Wait for Enter key
    (newline)
    ))

;; String -> String
;; Funcao que armazena o nome do jogador e retorna ela quando o nome do jogador é citado na historia
(displayln "Insira seu nome: ")
(define nome (read-line (current-input-port)))

;; String -> String
;; Funcao que mostra a 1a parte da historia jogo
(define text1
  (quasiquote( (unquote (string-append "Em um reino distante, existia um mundo paralelo onde os destinos eram tecidos pela linguagem mágica da programação. Neste universo, cada indivíduo nascia com um código único, determinando seus poderes e habilidades.
      Certo dia, um jovem chamado " nome " foi transportado para esse mundo mágico através de um portal inesperado. Ao despertar, descobriu que havia reencarnado como um Herói, destinado a derrotar o temível Rei Demônio que ameaçava a existência de todas as criaturas."))
     (unquote (string-append "Contudo, as habilidades do Herói só poderiam ser desbloqueadas através do conhecimento e domínio da linguagem de programação Racket, que era a base de todos os poderes mágicos daquele mundo. " nome " precisava aprender a utilizar esses códigos mágicos para manifestar seus poderes e salvar o reino."))
     (unquote (string-append "Em sua jornada, " nome " encontrou sábios e mestres que guardavam conhecimentos ancestrais sobre Racket. Para liberar seus poderes, ele precisava passar por testes elaborados pelos seres divinos que regiam o uso da magia. Cada vez que " nome " enfrentava um desafio divino, era questionado sobre conceitos complexos de Racket, desde definição de funções até manipulação de listas e recursões.")) ;;recursão até manipulação avançada de listas."
     (unquote (string-append "Com determinação e estudo árduo, " nome " começou a dominar a linguagem de programação, desvendando segredos e desbloqueando novos poderes. Ele aprendeu a conjurar escudos protetores com 'if-else', lançar feitiços de ataque com 'define'."))
     ;; Colocar algumas quests aqui para testar o usuário
     ;; Caso o usuário falhe, falar que ele ainda não era apto para suceder a todos os poderes do Heroi
     )))


;; String -> String
;; Funcao que mostra a 1a parte da historia jogo
(define text2
  (quasiquote((unquote (string-append "À medida que progredia, " nome " reunia aliados, cada um com sua própria habilidade única, todos unidos pelo objetivo comum de derrotar o Rei Demônio e salvar o mundo da perdição iminente."))
     (unquote (string-append "No confronto final, diante do Rei Demônio e suas legiões demoníacas, " nome " utilizou seu conhecimento em Racket para conjurar um poderoso algoritmo, capaz de desativar as defesas do vilão. Com coragem e determinação, " nome " se preparou para lançar o código final, para invocar um feitiço supremo que selaria o Rei Demônio."))
     ;; Quest final aqui
     ;; Caso o usuário falhe, falar que ainda o Rei Demônio era forte demais para o estado atual do usuário e que ele deveria refazer a jornada dele para se aprimorar
     (unquote (string-append "Após concluir o feitiço final, " nome " selou o Rei Demônio, restaurando a paz e a harmonia ao reino. "))
     (unquote (string-append nome " agora reconhecido como o Herói da Programação, tornou-se uma lenda, inspirando outros a dominar a linguagem mágica de Racket para proteger e preservar a ordem daquele universo encantado."))
     )))

(printWithEnter text1)
(nivel1)
(nivel2)
;;(nivel3)
(nivel4)

(printWithEnter text2)
