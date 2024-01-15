#lang racket
(require rackunit)
(require rackunit/text-ui)
(require racket/list)

; ;Um jogo lúdico para o ensino de programação da linguagem racket de modo que ele seja educativo.
;; O jogo se passará por uma história fictícia, aonde o jogador deve
;; ler as perguntas/quests e respondê-las de forma correta para progredir na história.

;; =================

;; Funções

; ;Define ns com o namespace base do racket e possibilitar a utilização da função eval
(define ns (make-base-namespace))

; ;Atualiza o namespace para que funções como empty? possam ser utilizadas pelo eval
(parameterize ([current-namespace ns])
  (namespace-require 'racket))

;; Lista + Symbol -> Lista
;; Função que recebe uma lista e um símbolo que representa o nome da função inserida
;; Pelo usuário como parametro e retorna a lista que representa a função com as recursões
;; (se houver) trocados por 'x, mantendo a estrutura original da lista
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
;; Função serve para converter uma lista que representa uma função nomeada, para uma lista que representa uma função anônima.
(define (func-to-lambda lista)
  ;; Lista -> Lista
  ;; Função que pega o nome da função que foi inserida no argumento
  (define funcname (first (second lista)))
  ;; Lista -> Lista
  ;; Função que pega os argumentos da lista que foi inserida como argumento
  (define args (rest (second lista)))
  ;; Lista -> Lista
  ;; Função que obtém o resto da lista, com exceção dos 2 primeiros elementos da lista
  (define etc (drop lista 2))

  ;; Lista -> Lista
  ;; Cria uma expressão 'letrec' para definir a função 'x' como uma lambda com os argumentos 'args' e o corpo 'etc'
  (list 'letrec (list (list 'x (append (list 'lambda args) (build funcname etc '())))) 'x)
  )


;; Numero -> Funcao
;; Função para mostrar a 1a fase para o jogador e pedir a resposta dele
;; Retornando o resultado da execucao da funçao de verificar a resposta
;; Que retorna o score do usuario no nivel
(define (nivel1 tentativa)
  (displayln '"Para aprender a derrotar seus inimigos, você precisa SOMAR danos ao inimigo. Crie uma função que SOME o dano de DOIS feitiços e retorne o resultado da soma.")
  (if (= tentativa 0) (displayln "Dica: a função para subtração é\n(define (sub x y)\n    (- x y))\n") (void))
  (display '"Insira a resposta aqui: ")
  (resp (read (current-input-port)) 1 tentativa)
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

;; Teste -> Void
;; Executa um conjunto de testes
(define (executa-testes . testes)
  (run-tests (test-suite "Todos os testes" testes))
  (void))


;; Funcao que executa a rodada de testes da funcao solucao do nivel 1
(displayln "Verificando o funcionamento correto do jogo...")
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

;; Numero -> Funcao
;; Função para mostrar a 2a fase para o jogador e pedir a resposta dele
;; Retornando o resultado da execucao da funçao de verificar a resposta
;; Que retorna o score do usuario no nivel
(define (nivel2 tentativa)
  (displayln '"Para aprender a se defender, você precisa identificar os ataques inimigos. Sua função deve retornar:")
  (displayln (string-append '"#t: quando receber a string ATAQUE"))
  (displayln (string-append '"#f: quando receber qualquer outra coisa"))
  (if (= tentativa 0) (displayln "Dica: Utilize a forma cond\n (cond\n    [comparacao #t]\n    [else #f]\n )\n") (void))
  (display '"Insira a resposta aqui: ")
  (resp (read (current-input-port)) 2 tentativa) 
  )

;; String -> Boolean
;; Função que faz os testes unitarios na nossa solucao do nivel 2
(define testes-solucao-nivel2
  (test-suite
   "testes-solucao-nivel2"
   (check-equal? (nivel2-solucao "ATAQUE") #t )
   (check-equal? (nivel2-solucao 2) #f)
   (check-equal? (nivel2-solucao 'ATAQUE) #f)
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


;; Funcao que executa a rodada de testes da funcao solucao do nivel 2
(executa-testes testes-solucao-nivel2)

;; Lista -> Boolean
;; Função que verifica se a função inserida pelo jogador retorna o valor desejado no nivel 2
;; Com base nas nossos testes e solucoes
(define (verifica-resp-nivel2 func)
  (and
   (if (equal? (func "ATAQUE") (nivel2-solucao "ATAQUE")) #t #f)
   (if (equal? (func "DEFESA") (nivel2-solucao "DEFESA")) #t #f)
   (if (equal? (func "qualquer coisa") (nivel2-solucao "qualquer coisa")) #t #f)
   )
  )

;; Numero -> Funcao
;; Função para mostrar a 3a fase para o jogador e pedir a resposta dele
;; Retornando o resultado da execucao da funçao de verificar a resposta
;; Que retorna o score do usuario no nivel
(define (nivel3 tentativa)
  (displayln '"Você precisa causa mais dano de uma vez, lance um feitiço que o dano seja exponencial.Sua função deve retornar a o poder do feitiço elevado ao tempo de conjuração do mesmo (poder^tempo)")
  (if (= tentativa 0) (displayln "Dica: utilize recursão com o caso base\n[(equal? tempo 0) 1]") (void))
  (display '"Insira a resposta aqui: ")
  (resp (read (current-input-port)) 3 tentativa)
  )

;; Numeros -> Numeros
;; Função que faz os testes unitarios na nossa solucao do nivel 3
(define testes-solucao-nivel3
  (test-suite
   "testes-solucao-nivel3"
   (check-equal? (nivel3-solucao 0 1) 0)
   (check-equal? (nivel3-solucao 1 1) 1)
   (check-equal? (nivel3-solucao 3 2) 9)
   (check-equal? (nivel3-solucao 10 3) 1000)
   )
  )

;; Numeros -> Numeros
;; Função resolução da fase 3
;; Função que calcula o dano do feitiço lançado pelo jogador, retorna a potência
;; Do poder do feitiço e do tempo de conjuração do feitiço, ou seja,
;; O poder do feitiço elevado ao tempo de conjuração
(define (nivel3-solucao poder tempo)
  (cond
    [(equal? tempo 0) 1]
    [else (* poder (nivel3-solucao  poder (sub1 tempo)))]
    )
  )

;; Funcao que executa a rodada de testes da funcao solucao do nivel 3
(executa-testes testes-solucao-nivel3)

;; Lista -> Boolean
;; Função que verifica se a função inserida pelo jogador retorna o valor desejado no nivel 3
;; Com base nas nossos testes e solucoes
(define (verifica-resp-nivel3 func)
  (and
   (if (equal? (func 1 1) (nivel3-solucao 1 1)) #t #f)
   (if (equal? (func 3 2) (nivel3-solucao 3 2)) #t #f)
   (if (equal? (func 2 6) (nivel3-solucao 2 6)) #t #f)
   (if (equal? (func 9 1) (nivel3-solucao 9 1)) #t #f)
   (if (equal? (func 10 3) (nivel3-solucao 10 3)) #t #f)
   (if (equal? (func 4 3) (nivel3-solucao 4 3)) #t #f)
   )
  )


;; Numero -> Funcao
;; Função para mostrar a 3a fase para o jogador e pedir a resposta dele
;; Retornando o resultado da execucao da funçao de verificar a resposta
;; Que retorna o score do usuario no nivel
(define (nivel4 tentativa)
  (displayln '"Você precisa separar em 2 listas os buffs e os debuffs que você ira receber para ajudar a derrotar o taltal tal, para isso separe a lista em numeros pares e impares")
  (if (= tentativa 0) (displayln "Dica: utilize recursao com um dos casos recursivos sendo (nivel4-solucao (rest lista) (cons (first lista) pares) impares)") (void))
  (display '"Insira a resposta aqui: ")
  (resp (read (current-input-port)) 4 tentativa)
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


;; Funcao que executa a rodada de testes da funcao solucao do nivel 4
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

;; Numeros -> Numero ou Funcao 
;; Função que recebe em qual nivel o jogador cometeu um erro na resposta e
;; Retorna a mensagem de erro, executando a função do nivel novamente, assim
;; Permitindo a tentativa do usuario no mesmo nível.
;; Caso ele nao tenha mais tentativas, a função retorna -1
(define (errou-nivel nv tentativa)
  (cond
    [(= nv 1)
     (if (= 0 tentativa)
         -1
         ((lambda ()
           (displayln "\nVocê errou, tente novamente\n")
           (nivel1 (sub1 tentativa))))
         )]
    
    [(= nv 2)
    (if (= 0 tentativa)
         -1
         ((lambda ()
           (displayln "\nVocê errou, tente novamente\n")
           (nivel2 (sub1 tentativa)))))]
    
   [(= nv 3)
    (if (= 0 tentativa)
         -1
         ((lambda ()
           (displayln "\nVocê errou, tente novamente\n")
           (nivel3 (sub1 tentativa))))
         )]
   
    [(= nv 4)
    (if (= 0 tentativa)
         -1
         ((lambda ()
           (displayln "\nVocê errou, tente novamente\n")
           (nivel4 (sub1 tentativa))))
         )]
    )
  )

;; Lista + String + Numero -> Numero
;; Função que avalia a resposta do usuario em forma de lista como uma função e
;; Retorna a execução da funcao 'verifica-resp' verifica a resposta do usuario
;; Caso a resposta do usuario gere um erro, ela é tratada como uma
;; tentativa errada e executa a funcao 'errou-nivel'
(define (resp resposta nivel tentativa)
  (with-handlers ([exn:fail? (lambda(e) (errou-nivel nivel tentativa))])
    (let ([exp (eval (func-to-lambda resposta) ns)])
      (verifica-resp exp nivel tentativa))
    )
)

;; Funçao + Numeros -> Numero ou Funcao
;; Função que verifica se o jogador inseriu uma função que retorna a resposta correta
;; Caso o jogador acerte o resultado, a função retorna apresenta uma String e retorna o score do jogador no nivel
;; Caso contrario retorna a execuçãpo da função 'errou-nivel'
(define (verifica-resp func nivel tentativa)
  (if
   (cond
  [(= nivel 1) (verifica-resp-nivel1 func)]
  [(= nivel 2) (verifica-resp-nivel2 func)]
  [(= nivel 3) (verifica-resp-nivel3 func)]
  [(= nivel 4) (verifica-resp-nivel4 func)]
    ) ((lambda ()
        (display "\nVoce acertou!\n\n")
        (+ 1 tentativa)))
        (errou-nivel nivel tentativa))
  )



;;XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

;; Jogo

;; Lista -> Void
;; Funcao que pede para o jogador apertar 'enter' para continuar contando a historia do jogo
(define (printWithEnter text)
  (for ([section (in-list text)])
    (display section)
    (newline)
    (displayln "Press Enter to continue...")
    (flush-output)
    (let ([bash (read-line)])
      (void)); Wait for Enter key
    (newline)
    )
  )

;; String -> String
;; Funcao que armazena o nome do jogador e retorna ela quando o nome do jogador é citado na historia
(displayln "\nInsira seu nome: ")
(define nome (read-line (current-input-port)))

;; Void -> Lista
;; Funcao que define uma lista de strings do tutorial do jogo
(define tutorial
  (quasiquote( (unquote (string-append "\nOlá " nome "! antes do início do jogo, vamos a um breve tutorial do funcionamento do jogo."))
               (unquote "Durante sua envolvente jornada, você enfrentará vários desafios! Assim, para cada uma das adversidades haverá um número máximo de tentativas. Caso seja necessário, a partir da terceira tentativa, uma dica será apresentada.")
               (unquote "Você terá que se provar ao elaborar funções na linguagem Racket. Assim, segue algumas orientações da forma como as funções devem ser escritas: ")
               (unquote "Exemplo de como escrever uma função que retorne o maior elemento:\n(define (funcao x y)\n    (if (< x y) x y))")
               (unquote "Agora tudo que tenho a fazer é desejar sorte a sua jornada. BOA SORTE!!!")
               )))

;; Void -> Lista
;; Funcao que define uma lista de strings da 1a parte da história do jogo
(define text1
  (quasiquote( (unquote (string-append "Em um reino distante, existia um mundo paralelo onde os destinos eram tecidos pela linguagem mágica da programação. Neste universo, cada indivíduo nascia com um código único, determinando seus poderes e habilidades."))
               (unquote (string-append "Certo dia, um jovem chamado " nome " foi transportado para esse mundo mágico através de um portal inesperado. Ao despertar, descobriu que havia reencarnado como um Herói, destinado a derrotar o temível Rei Demônio que ameaçava a existência de todas as criaturas."))
               (unquote (string-append "Contudo, as habilidades do Herói só poderiam ser desbloqueadas através do conhecimento e domínio da linguagem de programação Racket, que era a base de todos os poderes mágicos daquele mundo. " nome " precisava aprender a utilizar esses códigos mágicos para manifestar seus poderes e salvar o reino."))
               (unquote (string-append "Em sua jornada, " nome " encontrou sábios e mestres que guardavam conhecimentos ancestrais sobre Racket. Para liberar seus poderes, ele precisava passar por testes elaborados pelos seres divinos que regiam o uso da magia. Cada vez que " nome " enfrentava um desafio divino, era questionado sobre conceitos complexos de Racket ele realizava diversos ataque utilizando funções 'define'."))
     )
     ))

;; Void -> Lista
;; Funcao que define uma lista de strings da 2a parte da história do jogo
(define text2
  (quasiquote ( (unquote (string-append "Com determinação e estudo árduo, " nome " começou a dominar a linguagem de programação, desvendando segredos e desbloqueando novos poderes."))
                (unquote (string-append "Em seguida, " nome " aprendeu que se defender é tão importante quanto atacar. Assim se dedicou a aprender como conjurar escudos protetores com 'if-else'"))
                )
              ))

;; Void -> Lista
;; Funcao que define uma lista de strings da 3a parte da história do jogo
(define text3
  (quasiquote ( (unquote (string-append "Durante sua jornada para enfrentar o Rei demônio " nome " se deparou com um enorme golem de pedra como inimigo."))
                (unquote (string-append "Sua resistência era incomparável! E além possui habilidades regenerativas."))
                (unquote (string-append "Era necessário uma explosão de dano."))
                )
              ))

;; Void -> Lista
;; Funcao que define uma lista de strings da 4a parte da história do jogo
(define text4
  (quasiquote((unquote (string-append "À medida que progredia, " nome " reunia aliados, cada um com sua própria habilidade única, todos unidos pelo objetivo comum de derrotar o Rei Demônio e salvar o mundo da perdição iminente."))
     (unquote (string-append "No confronto final, diante do Rei Demônio e suas legiões demoníacas, " nome " utilizou seu conhecimento em Racket para conjurar um poderoso algoritmo, capaz de desativar as defesas do vilão."
                             )))))

(define textfim
  (quasiquote ((unquote (string-append "Com coragem e determinação, " nome " se preparou para lançar o código final, para invocar um feitiço supremo que selaria o Rei Demônio."))
     (unquote (string-append "Após concluir o feitiço final, " nome " selou o Rei Demônio, restaurando a paz e a harmonia ao reino. "))
     (unquote (string-append nome " agora reconhecido como o Herói da Programação, tornou-se uma lenda, inspirando outros a dominar a linguagem mágica de Racket para proteger e preservar a ordem daquele universo encantado."))
     )))

;; Numero -> Funcao
;; Função que executa o jogo
(define (jogo niveis historia)
  (printWithEnter tutorial)

  (define tentativas 2)

  ;; Listas -> Numero
  ;; Funcao que imprime a história e executa os niveis realizando a soma
  ;; Do score do jogador, caso o jogador falhe durante um nivel
  ;; O jogo é interrompido e encerrado
  (define (soma-score nivel historia)
    (if (empty? historia) (void) (printWithEnter (first historia)))
    (cond
      [(empty? nivel) 0]
      [else
       (with-handlers ([exn:fail? (lambda(e) #f)])
       ((lambda ()
          (define score ((first nivel) tentativas))
          (cond
            [(equal? score -1) #f]
            [else (+ 0 score (soma-score (rest nivel) (rest historia)))]))))
          ]
      )
    )


  ;; Numero -> Boolean
  ;; Função que com base na soma feita pelo 'soma-score'
  ;; Retorna se o jogador conseguiu ganhar o jogo ou se
  ;; o jogador perdeu o jogo
  (define score (soma-score niveis historia))
  (if (equal? score #f) (fimderrota) (fimvitoria score))
  )

;; Void -> String
;; Função que mostra na tela do jogador que ele perdeu o jogo
(define (fimderrota)
  (displayln "\nVocê foi derrotado.....\nTe desejo mais sorte da próxima vez.....")
  )

;; Void -> String
;; Função que mostra na tela do jogador que ele ganhou o jogo
(define (fimvitoria pontuacao)
  (printWithEnter textfim)
  (display "Você ganhou!\nSeu score final foi: ")
  (display pontuacao)
  (displayln "\nMeus parabéns!")
  )

(define niveis (list nivel1 nivel2 nivel3 nivel4))

(define historia (list text1 text2 text3 text4))



(jogo niveis historia)
;; Terminar score, terminar as dicas, revisar comentarios, fazer as funcoes do nivel 3, fazer funcao de derrota, fazer função de vitoria
