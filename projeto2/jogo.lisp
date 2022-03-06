(defvar *base-path* "TiagoBranco_201500575/projeto2/")

(load (compile-file (concatenate 'string *base-path* "puzzle.lisp")))
(load (compile-file (concatenate 'string *base-path* "algoritmo.lisp")))

(defvar *jogador1* 1)
(defvar *jogador2* 2)
(defvar *minimax-depth* 20)

(defun start()
  "Le input do utilizador e comeca o programa"
  (format t "~%Select option:~%")
  (format t "1- Player vs AI~%")
  (format t "2- AI vs AI~%>")
  (let ((players (read)))
    (format t "Time in seconds for AI to play:~%>")
    (let ((time (read))) 
      (cond ((= players 1) (progn 
                            (format t "1- Play first~%")
                            (format t "2- Play second~%>") 
                            (let ((order (read-play-order)))
                              (cond ((= order 1) (first-play-human (no-inicial) *jogador1* *jogador2* time))
                                    ((= order 2) (first-play-pc (no-inicial) *jogador1* *jogador2* time))
                              )
                             )
                            )
             )
            ((= players 2) (jogada-pc-pc (no-inicial) time))
            (t (progn (format t "Wrong input~%~%") (start)))
      )
    ) 
  )
)

(defun read-play-order ()
  "Le e verifica se input para ordem de jogo é valida"
  (let ((resposta (read)))
      (cond ((and (> resposta 0) (< resposta 3)) resposta)
            (t (progn (format t "~%input invalido~%") (read-play-order)))
      )
   )  
)

(defun first-play-pc(no player opponent time)
  "Funcao para iniciar o jogo caso o computador seja o jogador 1"
  (first-play-human (print-and-return-play(minimax-start no 2 infinity- infinity+ t player opponent time 'sucessores 'heuristica 'no-pai)) opponent player time) 
)

(defun jogada-pc (no player opponent time &aux (estado (no-estado no)))
  "Funcao para jogada do computador quando defrontado com jogador humano"
     (cond ((game-over no player opponent) (print-victory no))
        ((not (check-jogada-valida estado player)) (jogada-humano no opponent player time))
        (t (jogada-humano (print-and-return-play (minimax-start no *minimax-depth* infinity- infinity+ t player opponent time 'sucessores 'heuristica 'no-pai)) opponent player time))
        )  
)

(defun first-play-human (no player opponent time)
  "Funcao para a primeira jogada do jogador humano"
  (let ((tabuleiro (get-tab no)))
    (progn (print-tab tabuleiro)      
         (format t "~%Your turn, select piece: ~%~%" player)
         (print-pieces (no-estado no) player)
         (let ((peca (ler-peca (no-estado no) player)))
                 (jogada-pc (list (jogar-inicial peca (no-estado no) player) 1) opponent player time)
          )
    )
  )
)

(defun print-victory(no)
  "Imprime dados de um jogo acabado"
  (let ((casas-j1 (apply '+ (count-casas-left (get-pieces-left (no-estado no) *jogador1*))))
       (casas-j2 (apply '+ (count-casas-left (get-pieces-left (no-estado no) *jogador2*))))
       (depth (no-profundidade no)))
    
       (cond ((< casas-j1 casas-j2) (progn (print-no no) (format t "Player ~a WINS with ~a vs ~a points, after ~a plays~%" *jogador1* casas-j1 casas-j2 depth)))
             ((> casas-j1 casas-j2) (progn (print-no no) (format t "Player ~a WINS with ~a vs ~a points, after ~a plays~%" *jogador2* casas-j2 casas-j1 depth)))
             (t (progn (print-no no) (format t "DRAW with ~a points after ~a plays~%" casas-j1 depth)))
      )
       
  )
)

(defun print-pieces(no player)
  "Imprime as pecas para o ecra"
  (let ((pecas (get-pieces-left no player)))
    (progn (format t "Peca A:~%x~%~a left~%~%" (car pecas))
      (format t "Peca B:~%xx~%xx~%~a left~%~%" (cadr pecas))
      (format t "Peca C1:~% xx~%xx~%~a left~%~%" (caddr pecas))
      (format t "Peca C2:~%x~%xx~% x~%~a left~%~%" (caddr pecas))    
      )
  )
)

(defun ler-peca(estado player) 
  "Le e verfica input relativo a escolha de uma peca"
  (let ((disponiveis (pieces-left estado player)))
  (progn (format t ">")
  (let ((resposta (read)))
      (cond ((and (eq resposta 'a) (member 'peca-a disponiveis)) 'peca-a) 
              ((and (eq resposta 'b) (member 'peca-b disponiveis)) 'peca-b)
              ((and (eq resposta 'c1) (member 'peca-c-1 disponiveis)) 'peca-c-1)
              ((and (eq resposta 'c2) (member 'peca-c-2 disponiveis)) 'peca-c-2)
            (t (progn (format t "~%input invalido~%") (ler-peca estado player)))
      )
   ))  
  ) 
)

(defun ler-pos() 
  "Le e verfica input relativo a escolha de uma coordenada de posicao"  
  (let ((resposta (read)))
      (cond ((and (>= resposta 0) (<= resposta 13)) resposta)
            (t (progn (format t "~%input invalido~%") (ler-pos)))
      )
   ) 
)

(defun jogada-humano (no jogador opponent time &aux (estado (no-estado no)))
  "Funcao para ler input e aplicar jogada de jogador humano"
  (let ((tabuleiro (get-tab no)))
    (cond ((game-over no jogador opponent) (print-victory no))
          ((not (check-jogada-valida (no-estado no) jogador)) (jogada-pc no opponent jogador time))
    (t 
    (progn (print-tab tabuleiro)      
         (format t "Your turn: ~a~%Escolha peça: ~%~%" jogador)
         (print-pieces estado jogador)
         (let ((peca (ler-peca estado jogador)))
           (format t "line? (0-13) ~%>" jogador)
           (let ((y (ler-pos)))
             (format t "column? (0-13)~%>")
             (let ((x (ler-pos)))
               (cond ((not (null (funcall peca y x (no-estado no) jogador))) 
                        (jogada-pc (list (funcall peca y x (no-estado no) jogador) (no-profundidade no)) opponent jogador time))
                     (t (progn (format t "Posicao invalida, tente outra posicao ou peca ~%")
                          (jogada-humano no jogador opponent time))
                        )        
                     )
               )
             )
           )
    )
    )
    )
  )
)



(defun jogada-pc-pc (no time &optional (player *jogador1*) (opponent *jogador2*))
  "Funcao para jogada do computador quando defrontado com outro jogador artificial"
  (cond ((game-over no player opponent) (print-victory no))
        ((not (check-jogada-valida (no-estado no) player)) (jogada-pc-pc no time opponent player))
        (t (jogada-pc-pc (print-and-return-play (minimax-start no *minimax-depth* infinity- infinity+ t player opponent time 'sucessores 'heuristica 'no-pai)) time opponent player))
        )
)

(defun print-and-return-play(play)
  "Imprime e escreve dados de uma jogada do computador num ficheiro dat"
  (let ((jogada (car play)))
  (progn
  (format t "Play: ~a | Value: ~a | Found in: ~a seconds | Nodes visited: ~a | Alpha-Beta cuts: ~a | Player: ~a~%"
                 (get-operator jogada) (second play) (third play) (fourth play) (fifth play) (sixth play))            
                 (print-no jogada)

  (with-open-file (str (concatenate 'string *base-path* "resultados.dat")
                     :direction :output
                     :if-exists :append
                     :if-does-not-exist :create)
    (format str "Play: ~a | Value: ~a | Found in: ~a seconds | Nodes visited: ~a | Alpha-Beta cuts: ~a | Player: ~a~%"
                 (get-operator jogada) (second play) (third play) (fourth play) (fifth play) (sixth play))            
                 (print-no jogada str)
  )
  )
  jogada

  )
)






