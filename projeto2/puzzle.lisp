(defun tabuleiro-inicial (&optional (dimensao 14))
  "Retorna um tabuleiro 14x14 (default) com as casas vazias"
  (make-list dimensao :initial-element (make-list dimensao :initial-element '0))
)

(defun no-inicial ()
  "Constroi no inicial"
  (list (list (tabuleiro-inicial) '((10 10 15) (10 10 15))) 0 nil)
)

(defun no-estado(no)
  "Devolve estado do no"
  (car no)
)

(defun get-tab(no)
  "Devolve tabuleiro"
  (car (no-estado no))
)
(defun print-no (no &optional (form t))
  "Imprime no"
  (progn (print-tab (car (car no)) form) (format form "Pecas: ~a~%~%" (cadr (car no))))
)

(defun get-operator(no)
  "Retorna ultimo operador executado"
  (third no)
)

(defun no-pai(no)
  "Retorna no pai"
  (fourth no)
)

(defun print-tab (tab &optional (form t))
  "Imprime tabuleiro"
  (format form "    0 1 2 3 4 5 6 7 8 9 0 1 2 3~%")
  (print-tab-line tab form)
)

(defun print-tab-line (tab &optional (form t) (index 0))
  "Auxiliar a print-tab"
  (cond ((null tab) nil)        
        (t (progn 
             (cond ((< index 10) (format form "~2,'0d ~a~%" index (car tab)))
                   (t (format form "~a ~a~%" index (car tab)))
              )
             (print-tab-line (cdr tab) form (1+ index))))
  )
)

(defun game-over (no player opponent &aux (estado (no-estado no)))
  "Verifica se o jogo acabou"
  (cond ((or (check-jogada-valida estado player)(check-jogada-valida estado opponent)) nil)        
        (t t)        
  )
)

(defun get-pieces-left(estado player)
  "Retorna numero de pecas disponiveis dos jogadores"
  (cond ((equal nil player) (cadr estado))
        ((= player 1) (car (cadr estado)))
        ((= player 2) (cadr (cadr estado)))        
  )
)

(defun count-casas-left(pecas)
  "Retorna numero total de casas na lista de pecas no formato (A B C)"
  (list (car pecas) (* 4 (cadr pecas)) (* 4 (caddr pecas)))
)

(defun heuristica (no maxPlayer minPlayer &aux (tab (get-tab no)))
  "Retorna valor de um estado para os jogadores considerando numero de pecas no tabuleiro"
  (let ((count-max (count-casas maxPlayer tab))
        (count-min (count-casas minPlayer tab)))
        
    (- count-max count-min)
  )  
)

(defun heuristica-alt (no maxPlayer minPlayer &aux (tab (get-tab no)))
  "Retorna valor de um estado para os jogadores considerando numero de pecas no tabuleiro e numero de linhas e colunas ocupadas pelo jogador respetivo"
  "Pouco eficiente com profundidades maximas minimax maiores"
  (let ((count-max (count-casas maxPlayer tab))
        (count-min (count-casas minPlayer tab))
        (range-max (get-range tab maxPlayer))
        (range-min (get-range tab minPlayer))
        )
        
    (- (+ count-max range-max) (+ count-min range-min))
   
  )  
)

(defun get-range (tab player)
  "Devolve numero de linhas ocupadas mais numero de colunas ocupadas por jogador"
   (cond ((= player 1) (+ (get-index-first-linha-vazia tab player) (get-index-first-coluna-vazia tab player)))
         ((= player 2) (+ (- 13 (get-index-first-linha-vazia tab player)) (- 13 (get-index-first-coluna-vazia tab player))))
   )                   
)


(defun count-casas (player tab)
  "Conta casas ocupadas por jogador"
  (cond ((null tab) 0)
        (t (+ (count-casas-line (car tab) player) (count-casas player (cdr tab))))
  )
)

(defun count-casas-line (l player)
  "Auxiliar de count-casas"
  (count player l)   
)



(defun linha (nrlinha lista)
  "Devolve linha do tabuleiro"
  (cond ((>= nrlinha (length lista)) nil)
        (t (nth nrlinha lista))
  )
)

(defun coluna (nrcoluna lista)
  "Devolve coluna do tabuleiro"
  (cond ((>= nrcoluna (length (car lista))) nil)
        (t (mapcar (lambda (lista) 
                     (nth nrcoluna lista)) lista))
  )
)

(defun celula (y x lista)
  "Devolva celula de tabuleiro"
  (cond ((casa-no-tabuleiro y x lista) (nth x (linha y lista)))
        (t nil)
  )
)

(defun celula-casa (casa lista)
  "Devolve celula do tabuleiro dada uma casa"
  (celula (car casa) (cadr casa) lista)
)

(defun casa-vaziap (y x lista)
  "Devolve t se casa está vazia"
  (cond ((zerop (nth x (linha y lista))) t)
        (t nil)
  )
)

(defun verifica-casas-vazias(tabuleiro posicoes)
  "Devolve t se casas no tabuleiro estão vazias"
  (mapcar (lambda(p) (casa-vaziap (car p) (cadr p) tabuleiro)) posicoes)
)

(defun verifica-linha-vazia(linha player)
  "Verifica se linha nao contem nenhuma peca do jogador"
  (cond ((null linha) t)
        ((= (car linha) player) nil)
        (t (verifica-linha-vazia (cdr linha) player))        
  )
)

(defun no-profundidade (no)
  "Devolve profundidade do no"
  (cadr no)
)

(defun casa-no-tabuleiro(y x tabuleiro)
  "Verifica se coordenadas estão dentro do tabuleiro"
  (cond ((or (< y 0) (>= y (length tabuleiro))) nil)
        ((or (< x 0) (>= x (length (car tabuleiro)))) nil)
        (t t)
  )
)

(defun verifica-casas-no-tabuleiro(tabuleiro posicoes)
  "Verifica se conjunto de casas está dentro do tabuleiro"
  (mapcar (lambda(p) (casa-no-tabuleiro (car p) (cadr p) tabuleiro)) posicoes)
)

(defun substituir (y x tabuleiro valor)
  "Substitui posicao do tabuleiro com valor em parametro"
    (cond ((null tabuleiro) nil)
          ((= y 0) (cons (substituir-posicao x (car tabuleiro) valor) (cdr tabuleiro)))
          (t (cons (car tabuleiro) (substituir (- y 1) x (cdr tabuleiro) valor)))
    ) 
)

(defun substituir-posicao (index lista valor)
  "Auxiliar de substituir"
    (cond ((null lista) nil)
          ((= index 0) (cons valor (cdr lista)))
          (t (cons (car lista) (substituir-posicao (- index 1) (cdr lista) valor)))
    )
)

(defun verificar-posicoes(tabuleiro casas-peca)
  "Verifica se posicionamento de peca no tabuleiro é valido"
  (cond ((equal (eval (cons 'and (verifica-casas-no-tabuleiro tabuleiro casas-peca))) nil) nil)
        ((equal (eval (cons 'and (verifica-casas-vazias tabuleiro casas-peca))) nil) nil)        
        (t t)
  )
)

(defun verificar-pos-rel (casas-ocupadas tabuleiro player)
  "Retorna t se a posicao de peca relativamente a outras pecas é valida"
  (cond ((zerop (car (car tabuleiro))) t)
        (t
           (cond ((validar-adjacencia tabuleiro (peca-casas-adjacentes casas-ocupadas) player)
              (cond ((validar-angulos tabuleiro (peca-casas-angulo casas-ocupadas (peca-casas-adjacentes casas-ocupadas)) player) t)
                (t nil))
           )
           (t nil)
          )
         )
  )  
)

(defun validar-adjacencia (tabuleiro casas-adjacentes player)
  "Verifica que nao existem casas no tabuleiro que coincidam com as laterais de peca a ser colocada" 
  (cond ((null casas-adjacentes) t)
        ((equal (celula-casa (car casas-adjacentes) tabuleiro) nil) (validar-adjacencia tabuleiro (cdr casas-adjacentes) player))
        ((= (celula-casa (car casas-adjacentes) tabuleiro) player) nil)
        (t (validar-adjacencia tabuleiro (cdr casas-adjacentes) player))    
  )
)

(defun validar-angulos (tabuleiro casas-angulo player)
   "Verifica que existe pelo menos uma casa que coincide com um canto de peca a ser colocada"
   (cond ((null casas-angulo) nil)
         ((equal (celula-casa (car casas-angulo) tabuleiro) nil) (validar-angulos tabuleiro (cdr casas-angulo) player))
         ((= (celula-casa (car casas-angulo) tabuleiro) player) t)
         (t (validar-angulos tabuleiro (cdr casas-angulo) player))  
   )
)

(defun peca-casas-ocupadas (y x tipo)
  "Retorna lista de coordenadas ocupadas por peca relativamente a uma posicao"
  (cond ((equal tipo 'peca-a) (list(list y x)))
        ((string= tipo 'peca-b) (list (list y x) (list (+ 1 y) x) (list y (+ 1 x)) (list (+ 1 y)(+ 1 x))))
        ((string= tipo 'peca-c-1) (list (list y x) (list y (+ 1 x)) (list (- y 1) (+ x 1)) (list (- y 1) (+ x 2))))
        ((string= tipo 'peca-c-2) (list (list y x) (list (+ y 1) x) (list (+ y 1) (+ x 1)) (list (+ y 2)(+ x 1))))
        (t nil)
  )
)

(defun peca-casas-adjacentes (casas-ocupadas)
  "Retorna coordenadas de casas adjacentes a casas de peca"
  (cond ((null casas-ocupadas) nil)
        (t (labels ((iter (casas)
                      (let ((casa (car casas)))
                        (cond ((null casas) nil)
                              (t (append (subtract-lists (get-adjacentes-casa casa) casas-ocupadas) (iter (cdr casas)))) 
                         )
                      )                      
                      )
                      ) (iter casas-ocupadas)))
  )      
)


(defun peca-casas-angulo (casas-ocupadas casas-adjacentes)  
  "Retorna coordenadas das casas nos cantos de uma peca"
  (cond ((null casas-ocupadas) nil)
        (t (labels ((iter (casas)
                      (let ((casa (car casas)))
                        (cond ((null casas) nil)                              
                              (t (append (subtract-lists (get-angle-pieces-casa casa) (append casas-ocupadas casas-adjacentes)) (iter (cdr casas)))) 
                         )
                      )                      
                      )
                      ) (iter casas-ocupadas)))
  )      
)

(defun get-angle-pieces-casa(casa)
  "Retorna coordenadas das casas nos cantos de uma casa"
  (let ((y (car casa))
        (x (cadr casa)))
  (list (list (+ y 1) (+ x 1)) (list (- y 1) (+ x 1)) (list (+ y 1) (- x 1)) (list (- y 1) (- x 1)))
  )
)


(defun get-adjacentes-casa (casa)
  "Retorna coordenadas de casas adjacentes a casas de uma casa"
  (let ((y (car casa))
        (x (cadr casa)))
    (list (list (+ y 1) x) (list (- y 1) x) (list y (+ x 1)) (list y (- x 1)))
  )
)

(defun member-custom(e l)
  "Retorna se elemento existe na lista"
  (cond ((null l) nil)
        ((equal e (car l)) t)
        (t (member-custom e (cdr l)))
  )
)

(defun subtract-lists (l1 l2)
  "Retorna lista 1 sem o conteudo da lista 2"
  (cond ((or (null l1) (null l2)) nil)
        ((member-custom (car l1) l2) (subtract-lists (cdr l1) l2))
        (t (cons (car l1) (subtract-lists (cdr l1) l2)))
  )  
)

(defun peca-a (y x estado player)
  "Coloca peca A no tabuleiro"
 (colocar-peca y x 'peca-a estado player)
)
(defun peca-b (y x estado player)
  "Coloca peca B no tabuleiro"
  (colocar-peca y x 'peca-b estado player)
)
(defun peca-c-1 (y x estado player)
  "Coloca peca C1 no tabuleiro"
  (colocar-peca y x 'peca-c-1 estado player)
)
(defun peca-c-2 (y x estado player)
  "Coloca peca C2 no tabuleiro"
  (colocar-peca y x 'peca-c-2 estado player)
)

(defun peca-a-inicial(y x estado jogador)
  "Coloca peca A no tabuleiro sem verificar posicao relativa"
   (colocar-primeira-peca y x 'peca-a estado jogador)       
)
(defun peca-b-inicial(y x estado jogador)
  "Coloca peca B no tabuleiro sem verificar posicao relativa"
   (colocar-primeira-peca y x 'peca-b estado jogador)       
)
(defun peca-c-2-inicial(y x estado jogador)
  "Coloca peca C2 no tabuleiro sem verificar posicao relativa"
   (colocar-primeira-peca y x 'peca-c-2 estado jogador)      
)

(defun get-casas-livres-range(tabuleiro player)
  "Devolve casas livres no tabuleiro a partir da posicao inicial do jogador ate uma linha e coluna maxima"
  (cond ((= player *jogador1*) 
             (get-casas-livres-range-exe-1 0 (get-index-first-linha-vazia tabuleiro player) (get-index-first-coluna-vazia tabuleiro player) tabuleiro))
         ((= player *jogador2*) 
             (get-casas-livres-range-exe-2 13 (get-index-first-linha-vazia tabuleiro player) (get-index-first-coluna-vazia tabuleiro player) tabuleiro))
  )     
)

(defun get-casas-livres-range-exe-1(begin-l end-l end-c tabuleiro)
  "Auxiliar de get-casas-livres-range para o jogador 1"
  (cond ((or (> (- begin-l end-l) 1)(> begin-l 13)) nil) 
        (t 
         (labels ((go-line (column)
                    (cond ((or (> (- column end-c) 0) (> column 13)) (get-casas-livres-range-exe-1 (+ begin-l 1) end-l end-c tabuleiro))
                          ((casa-vaziap begin-l column tabuleiro) (cons (list begin-l column) (go-line (+ column 1))))
                          (t (go-line (+ column 1)))
                    )
                  )) (go-line 0))
         )
   )     
)

(defun get-casas-livres-range-exe-2(begin-l end-l end-c tabuleiro)
  "Auxiliar de get-casas-livres-range para o jogador 2"
  (cond ((or (< (- begin-l end-l) -2) (< begin-l 0)) nil)
        (t 
         (labels ((go-line (column)
                    (cond ((or (< (- column end-c) -2) (< column 0)) (get-casas-livres-range-exe-2 (- begin-l 1) end-l end-c tabuleiro))
                          ((casa-vaziap begin-l column tabuleiro) (cons (list begin-l column) (go-line (- column 1))))
                          (t (go-line (- column 1)))
                    )
                  )) (go-line 13))
         )
   )     
)

(defun get-index-first-linha-vazia (tabuleiro player)
  "Devolve index da primeira linha sem pecas do jogador a partir da posicao inicial do jogador"
  (cond ((= player 1) (get-index-first-linha-vazia-exe-1 tabuleiro 0 player))
        (t (get-index-first-linha-vazia-exe-2 tabuleiro 13 player))

  )
)


(defun get-index-first-linha-vazia-exe-1 (tabuleiro index player)
  "Auxiliar de get-index-first-linha-vazia para jogador 1"
  (cond ((>= index (length (car tabuleiro))) index)
        ((verifica-linha-vazia (linha index tabuleiro) player) index) 
        (t (get-index-first-linha-vazia-exe-1 tabuleiro (+ 1 index) player))
  )
)

(defun get-index-first-linha-vazia-exe-2 (tabuleiro index player)
  "Auxiliar de get-index-first-linha-vazia para jogador 2"
  (cond ((<= index 0) 0)
        ((verifica-linha-vazia (linha index tabuleiro) player) index) 
        (t (get-index-first-linha-vazia-exe-2 tabuleiro (- index 1) player))
  )
)


(defun get-index-first-coluna-vazia (tabuleiro player)
  "Devolve index da primeira coluna sem pecas do jogador a partir da posicao inicial do jogador"
  (cond ((= player *jogador1*) (get-index-first-coluna-vazia-exe-1 tabuleiro 0 player))
        (t (get-index-first-coluna-vazia-exe-2 tabuleiro 13 player))
  )
)

(defun get-index-first-coluna-vazia-exe-1 (tabuleiro index player)
  "Auxiliar de get-index-first-coluna-vazia para jogador 1"
  (cond ((>= index (length (car tabuleiro))) index)
        ((verifica-linha-vazia (coluna index tabuleiro) player) index) 
        (t (get-index-first-coluna-vazia-exe-1 tabuleiro (+ 1 index) player))
  )
)

(defun get-index-first-coluna-vazia-exe-2 (tabuleiro index player)
  "Auxiliar de get-index-first-coluna-vazia para jogador 2"
  (cond ((<= index 0) 0)
        ((verifica-linha-vazia (coluna index tabuleiro) player) index) 
        (t (get-index-first-coluna-vazia-exe-2 tabuleiro (- index 1) player))
  )
)

(defun pecas()
  "Devolve pecas do jogo"
  (list 'peca-a 'peca-b 'peca-c-1 'peca-c-2)
)


(defun pieces-left(estado player)
  "Devolve as pecas disponiveis do jogador"
  (let ((pieces-nr (append (get-pieces-left estado player) (last (get-pieces-left estado player)))))
    (remove nil (mapcar (lambda (x y) (cond ((> x 0) y))) pieces-nr (pecas)))
  )
)

(defun pecas-1st-play()
  "Devolve pecas jogaveis no primeiro turno"
  (list 'peca-a 'peca-b 'peca-c-2)
)

(defun get-jogadas-validas(estado player)
  "Devolve todos os operadores possiveis dado um no"
  (cond ((zerop (car (car (car estado)))) (operadoresIniciais estado *jogador1*))
        ((zerop (nth 13 (nth 13 (car estado)))) (operadoresIniciais estado *jogador2*))
        (t (get-jogadas-validas-exe (reverse (pieces-left estado player)) (get-casas-livres-range (car estado) player) estado player))
  )
)

(defun get-jogadas-validas-exe(operadores casas estado player)  
  "Auxiliar de get-jogadas-validas"
   (cond ((null operadores) nil)
         (t (labels ((append-casas (operador casas-2)
                       (cond ((null casas-2) (get-jogadas-validas-exe (cdr operadores) casas estado player))
                             ((equal (funcall operador (car (car casas-2)) (cadr(car casas-2)) estado player) nil) (append-casas operador (cdr casas-2)))
                             (t (cons (list operador (car (car casas-2)) (cadr(car casas-2)) estado player) 
                                      (append-casas operador (cdr casas-2)))
                             )
                        )
                     )) (append-casas (car operadores) casas)))
   )     
)

(defun check-jogada-valida(estado player)
  "Verifica se existe pelo menos 1 jogada valida"
  (cond ((or (zerop (car (car (car estado)))) (zerop (nth 13 (nth 13 (car estado))))) t)        
        (t (check-jogada-valida-exe (pieces-left estado player) (get-casas-livres-range (car estado) player) estado player))
  )
)

(defun check-jogada-valida-exe(operadores casas estado player) 
  "Auxiliar de check-jogada-valida"
   (cond ((null operadores) nil)
         (t (labels ((append-casas (operador casas-2)
                       (cond ((null casas-2) (get-jogadas-validas-exe (cdr operadores) casas estado player))
                             ((equal (funcall operador (car (car casas-2)) (cadr(car casas-2)) estado player) nil) (append-casas operador (cdr casas-2)))
                             (t t)
                        )
                     )) (append-casas (car operadores) casas)))
   )     
)


(defun colocar-peca(y x peca estado player)
  "Coloca peca indicada no tabuleiro, subtrai-a da lista de pecas disponÃ¯Â¿Â½veis e retorna novo estado."
  (let ((tabuleiro (car estado))
        (casas-peca (peca-casas-ocupadas y x peca)))
    (cond ((not (verificar-posicoes tabuleiro casas-peca)) nil)
          ((not (verificar-pos-rel casas-peca tabuleiro player)) nil)
          (t (list (colocar casas-peca tabuleiro player) (subtract-peca estado peca player)))
              
    )
   )
)

(defun colocar-primeira-peca(y x peca estado player)
  "Igual a colocar-peca mas nao verifica posicao relativa a outras pecas"
  (let ((tabuleiro (car estado))
        (casas-peca (peca-casas-ocupadas y x peca)))
    (cond ((not (verificar-posicoes tabuleiro casas-peca)) nil)        
          (t (list (colocar casas-peca tabuleiro player) (subtract-peca estado peca player)))
    )
  )
)

(defun colocar (casas-peca tabuleiro player)
  "Auxiliar de colocar-peca e colocar-primeira-peca"
  (cond ((null casas-peca) tabuleiro)
        (t (substituir (car(car casas-peca)) (cadr(car casas-peca)) (colocar (cdr casas-peca) tabuleiro player) player))
  )               
)                    

(defun subtract-peca(estado peca player) 
  "Subtrai peca de pecas disponiveis do jogador"
  (let ((pecas-left (get-pieces-left estado nil))
       (pecas (list 'peca-a 'peca-b 'peca-c)))
    (progn
      (cond ((or (equal peca 'peca-c-1) (equal peca 'peca-c-2)) (setf peca 'peca-c)))
      (cond ((= player *jogador1*) (list (subtract peca (car pecas-left) pecas) (cadr pecas-left)))
            ((= player *jogador2*) (list (car pecas-left) (subtract peca (cadr pecas-left) pecas)))
      )  
    )        
  )
)

(defun subtract(peca pecas-left lista-pecas)  
  "Auxiliar de subtract-peca"
  (cond ((null pecas-left) nil)
        ((equal peca (car lista-pecas)) (cons (1- (car pecas-left)) (cdr pecas-left)))
        (t (cons (car pecas-left) (subtract peca (cdr pecas-left) (cdr lista-pecas))))
  )
)

(defun jogar-inicial(peca estado jogador &rest bla)
  "(funcall peca 0 0 tabuleiro)" 
  (cond ((= jogador *jogador1*) (colocar-primeira-peca 0 0 peca estado jogador))
        (t (cond ((eq peca 'peca-a) (colocar-primeira-peca 13 13 peca estado jogador))
                 ((eq peca 'peca-b) (colocar-primeira-peca 12 12 peca estado jogador))  
                 ((eq peca 'peca-c-2) (colocar-primeira-peca 11 12 peca estado jogador))
           )
        )       
  )  
)

(defun operadoresIniciais (estado player)
  "Devolve operadores possiveis na primeira jogada consoante o jogador"
  (cond ((= player *jogador1*) 
         (reverse (list (list 'peca-a-inicial 0 0 estado player) (list 'peca-b-inicial 0 0 estado player) (list 'peca-c-2-inicial 0 0 estado player))
         ))
        (t 
            (reverse (list (list 'peca-a-inicial 13 13 estado player) (list 'peca-b-inicial 12 12 estado player) (list 'peca-c-2-inicial 11 12 estado player)))
            )
  )  
)

(defun novo-sucessor (pai operador)     
  "Devolve novo no apos aplicado operador a um no pai"
   (let ((prof-filho (+ 1 (cadr pai)))        
         (estado-filho (funcall (car operador) (second operador) (third operador) (fourth operador) (fifth operador))))       
         (list estado-filho prof-filho (subseq operador 0 3) pai)     
   )
)

(defun sucessores (pai player)
  "Retorna os resultados da aplicacao de todos os operadores possiveis dado um estado"
  (let ((operadores (get-jogadas-validas (no-estado pai) player)))
    (labels ((iter (pai ops)
               (cond ((null ops) nil)        
                     (t (cons (novo-sucessor pai (car ops)) (iter pai (cdr ops))))   
               ) 
               
               )) (iter pai operadores)
     )   
  )
)

