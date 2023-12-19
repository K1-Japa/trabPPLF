#lang racket

(define ns (make-base-namespace))

(define (func-to-lambda lista)
  (define etc (drop lista 2))
  (define args (rest (second lista)))
  (cons 'lambda (cons args etc))
  )

(define (nivel2-solucao parametro)
  (cond
    [(equal? parametro "ataque") #t]
    [else #f])
  )

(define (nivel4-solucao vida golpes)
  (cond
    [(equal? vida 0) golpes]
    [else (nivel4-solucao (sub1 vida) (add1 golpes))])
  )

(define (errou-nivel nv)
  (cond
    [(= nv 1) (displayln "Você errou, tente novamente")]
    )
  )

(define (resp resposta nivel)
  (with-handlers ([exn:fail? (lambda(e) (errou-nivel nivel))])
    (eval (func-to-lambda resposta) ns)
    )
  )

(define (teste nivel)
  (resp (read (current-input-port)) nivel)
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

(define text-to-print
  (quasiquote( (unquote (string-append "Em um reino distante, existia um mundo paralelo onde os destinos eram tecidos pela linguagem mágica da programação. Neste universo, cada indivíduo nascia com um código único, determinando seus poderes e habilidades.
      Certo dia, um jovem chamado " nome " foi transportado para esse mundo mágico através de um portal inesperado. Ao despertar, descobriu que havia reencarnado como um Herói, destinado a derrotar o temível Rei Demônio que ameaçava a existência de todas as criaturas."))
     (unquote (string-append "Contudo, as habilidades do Herói só poderiam ser desbloqueadas através do conhecimento e domínio da linguagem de programação Racket, que era a base de todos os poderes mágicos daquele mundo. " nome " precisava aprender a utilizar esses códigos mágicos para manifestar seus poderes e salvar o reino."))
     (unquote (string-append "Em sua jornada, " nome " encontrou sábios e mestres que guardavam conhecimentos ancestrais sobre Racket. Para liberar seus poderes, ele precisava passar por testes elaborados pelos seres divinos que regiam o uso da magia. Cada vez que " nome " enfrentava um desafio divino, era questionado sobre conceitos complexos de Racket, desde definição de funções até manipulação de listas e recursões.")) ;;recursão até manipulação avançada de listas."
     (unquote (string-append "Com determinação e estudo árduo, " nome " começou a dominar a linguagem de programação, desvendando segredos e desbloqueando novos poderes. Ele aprendeu a conjurar escudos protetores com 'if-else', lançar feitiços de ataque com 'define' e até manipular o espaço com 'list'."))
     ;; Colocar algumas quests aqui para testar o usuário
     ;; Caso o usuário falhe, falar que ele ainda não era apto para suceder a todos os poderes do Heroi
     (unquote (string-append "À medida que progredia, " nome " reunia aliados, cada um com sua própria habilidade única, todos unidos pelo objetivo comum de derrotar o Rei Demônio e salvar o mundo da perdição iminente."))
     (unquote (string-append "No confronto final, diante do Rei Demônio e suas legiões demoníacas, " nome " utilizou seu conhecimento em Racket para conjurar um poderoso algoritmo, capaz de desativar as defesas do vilão. Com coragem e determinação, " nome " se preparou para lançar o código final, para invocar um feitiço supremo que selaria o Rei Demônio."))
     ;; Quest final aqui
     ;; Caso o usuário falhe, falar que ainda o Rei Demônio era forte demais para o estado atual do usuário e que ele deveria refazer a jornada dele para se aprimorar
     (unquote (string-append "Após concluir o feitiço final, " nome " selou o Rei Demônio, restaurando a paz e a harmonia ao reino. "))
     (unquote (string-append nome " agora reconhecido como o Herói da Programação, tornou-se uma lenda, inspirando outros a dominar a linguagem mágica de Racket para proteger e preservar a ordem daquele universo encantado."))

     )))

(printWithEnter text-to-print)
