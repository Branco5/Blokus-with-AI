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
  (cond (( = (nth x (linha y lista)) 0) t)
        (t nil)
  )
)

(defun verifica-casas-vazias(tabuleiro posicoes)
  "Devolve t se casas no tabuleiro estão vazias"
  (mapcar (lambda(p) (casa-vaziap (car p) (cadr p) tabuleiro)) posicoes)
)

(defun verifica-linha-vazia(linha)
  "Verifica se linha nao contem nenhuma peca do jogador"
  (cond ((null linha) t)
        ((not (= (car linha) 1)) (verifica-linha-vazia (cdr linha)))
        (t nil)
  )
)

(defun get-index-first-linha-vazia (tabuleiro)
  "Devolve index da primeira linha vazia a partir da posicao inicial do jogador"
  (get-index-first-linha-vazia-exe tabuleiro 0)
)

(defun get-index-first-linha-vazia-exe (tabuleiro index)
  "Auxiliar de get-index-first-linha-vazia"
   (cond ((>= index (length (car tabuleiro))) index)
        ((verifica-linha-vazia (linha index tabuleiro)) index) 
        (t (get-index-first-linha-vazia-exe tabuleiro (+ 1 index)))
   )  
)

(defun get-index-first-coluna-vazia (tabuleiro)
  "Devolve index da primeira coluna vazia a partir da posicao inicial do jogador"
  (get-index-first-coluna-vazia-exe tabuleiro 0)
)

(defun get-index-first-coluna-vazia-exe (tabuleiro index)
  "Auxiliar de get-index-first-coluna-vazia"
  (cond ((>= index (length (car tabuleiro))) index)
        ((verifica-linha-vazia (coluna index tabuleiro)) index) 
        (t (get-index-first-coluna-vazia-exe tabuleiro (+ 1 index)))
  )
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

(defun substituir (y x tabuleiro &optional (valor 1))
  "Substitui posicao do tabuleiro com valor em parametro"
    (cond ((null tabuleiro) nil)
          ((= y 0) (cons (substituir-posicao x (car tabuleiro) valor) (cdr tabuleiro)))
          (t (cons (car tabuleiro) (substituir (- y 1) x (cdr tabuleiro))))
    )
)

(defun substituir-posicao (index lista &optional (valor 1))  
  "Auxiliar de substituir"
    (cond ((null lista) nil)
          ((= index 0) (cons valor (cdr lista)))
          (t (cons (car lista) (substituir-posicao (- index 1) (cdr lista))))
    )
)


(defun verificar-posicoes(tabuleiro casas-peca)
  "Verifica se posicionamento de peca no tabuleiro é valido"
  (cond ((equal (eval (cons 'and (verifica-casas-no-tabuleiro tabuleiro casas-peca))) nil) nil)
        ((equal (eval (cons 'and (verifica-casas-vazias tabuleiro casas-peca))) nil) nil)        
        (t t)
  )
)


(defun colocar-peca(y x peca estado)
  "Coloca peca indicada no tabuleiro, subtrai-a da lista de pecas disponiveis e retorna novo estado"
  (let ((tabuleiro (car estado))
        (casas-peca (peca-casas-ocupadas y x peca)))
    (cond ((not (verificar-posicoes tabuleiro casas-peca)) nil) ; (format t "Posicao invalida"))
          ((not (verificar-pos-rel casas-peca tabuleiro)) nil) ;(format t "Posicao invalida"))
          (t (list (colocar casas-peca tabuleiro) (subtract-peca estado peca)))
              
    )
   )
)

(defun colocar-primeira-peca(y x peca estado)
  "Igual a colocar-peca mas nao verifica posicao relativa a outras pecas"
  (let ((tabuleiro (car estado))
        (casas-peca (peca-casas-ocupadas y x peca)))
    (cond ((not (verificar-posicoes tabuleiro casas-peca)) nil) ;;(format t "Posicao invalida"))        
          (t (list (colocar casas-peca tabuleiro) (subtract-peca estado peca)))
    )
  )
)

(defun colocar (casas-peca tabuleiro)
  "Auxiliar de colocar-peca e colocar-primeira-peca"
  (cond ((null casas-peca) tabuleiro)
        (t (substituir (car(car casas-peca)) (cadr(car casas-peca)) (colocar (cdr casas-peca) tabuleiro)))
  )               
)  

(defun subtract-peca(estado peca)  
  "Subtrai peca de pecas disponiveis do jogador"
  (let ((pecas-left (get-pieces-left estado))
       (pecas (list 'peca-a 'peca-b 'peca-c)))
    (progn
      (cond ((or (equal peca 'peca-c-1) (equal peca 'peca-c-2)) (setf peca 'peca-c)))
      (subtract peca pecas-left pecas)                  
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

(defun pecas()
  "Devolve pecas do jogo"
  (list 'peca-a 'peca-b 'peca-c-1 'peca-c-2)
)

(defun pieces-left(estado)
  "Devolve as pecas disponiveis do jogador"
  (let ((pieces-nr (append (get-pieces-left estado) (last (get-pieces-left estado)))))
    (remove nil (mapcar (lambda (x y) (cond ((> x 0) y))) pieces-nr (pecas)))
  )
)

(defun get-pieces-left(estado)
  "Devolve numero de pecas disponiveis do jogador"
  (cadr estado)
)

(defun verificar-pos-rel (casas-ocupadas tabuleiro)
  "Retorna t se a posicao de peca relativamente a outras pecas é valida"
  (cond ((zerop (car (car tabuleiro))) t)
        (t
           (cond ((validar-adjacencia tabuleiro (peca-casas-adjacentes casas-ocupadas))
              (cond ((validar-angulos tabuleiro (peca-casas-angulo casas-ocupadas (peca-casas-adjacentes casas-ocupadas))) t)
                (t nil))
           )
           (t nil)
          )
         )
  )  
)

(defun validar-adjacencia (tabuleiro casas-adjacentes)
  "Verifica que nao existem casas no tabuleiro que coincidam com as laterais de peca a ser colocada"
  (cond ((null casas-adjacentes) t)
        ((equal (celula-casa (car casas-adjacentes) tabuleiro) nil) (validar-adjacencia tabuleiro (cdr casas-adjacentes)))
        ((= (celula-casa (car casas-adjacentes) tabuleiro) 1) nil)
        (t (validar-adjacencia tabuleiro (cdr casas-adjacentes)))    
  )
)

(defun validar-angulos (tabuleiro casas-angulo)
  "Verifica que existe pelo menos uma casa que coincide com um canto de peca a ser colocada"
   (cond ((null casas-angulo) nil)
         ((equal (celula-casa (car casas-angulo) tabuleiro) nil) (validar-angulos tabuleiro (cdr casas-angulo)))
         ((= (celula-casa (car casas-angulo) tabuleiro) 1) t)
         (t (validar-angulos tabuleiro (cdr casas-angulo)))  
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

(defun peca-a (y x estado)
  "Coloca peca A no tabuleiro"
 (colocar-peca y x 'peca-a estado)
)

(defun peca-b (y x estado)
  "Coloca peca B no tabuleiro"
  (colocar-peca y x 'peca-b estado)
)

(defun peca-c-1 (y x estado)
  "Coloca peca C1 no tabuleiro"
  (colocar-peca y x 'peca-c-1 estado)
)

(defun peca-c-2 (y x estado)
  "Coloca peca C2 no tabuleiro"
  (colocar-peca y x 'peca-c-2 estado)
)

(defun get-casas-livres-range(tabuleiro)
   "Devolve casas livres no tabuleiro a partir da posicao inicial do jogador ate uma linha e coluna maxima"
   (get-casas-livres-range-exe 0 (get-index-first-linha-vazia tabuleiro) (get-index-first-coluna-vazia tabuleiro) tabuleiro)
)

(defun get-casas-livres-range-exe(begin-l end-l end-c tabuleiro)
  "Auxiliar de get-casas-livres-range"
  (cond ((or (> (- begin-l end-l) 1) (> begin-l 13)) nil) 
        (t 
         (labels ((go-line (column)
                    (cond ((or (>(- column end-c) 0) (> column 13)) (get-casas-livres-range-exe (+ begin-l 1) end-l end-c tabuleiro))
                          ((casa-vaziap begin-l column tabuleiro) (cons (list begin-l column) (go-line (+ column 1))))
                          (t (go-line (+ column 1)))
                    )
                  )) (go-line 0))
         )
   )     
)

(defun operadores-1st-play()
  "Devolve pecas jogaveis no primeiro turno"
  (list 'peca-a 'peca-b 'peca-c-2)
)

(defun get-jogadas-validas(no)
  "Devolve todos os operadores possiveis dado um no"
  (let ((estado (no-estado no)))
    (cond ((= (car (car (get-tab no))) 0) (get-jogadas-validas-exe (operadores-1st-play) (list '(0 0)) estado))
          (t (get-jogadas-validas-exe (pieces-left estado) (get-casas-livres-range (get-tab no)) estado))
    )
  )
)

(defun get-jogadas-validas-exe(operadores casas estado) 
  "Auxiliar de get-jogadas-validas"
   (cond ((null operadores) nil)
         (t (labels ((append-casas (operador casas-2)
                       (cond ((null casas-2) (get-jogadas-validas-exe (cdr operadores) casas estado))
                             ((equal (funcall operador (car (car casas-2)) (cadr(car casas-2)) estado) nil) (append-casas operador (cdr casas-2)))
                             (t (cons (list operador (car (car casas-2)) (cadr(car casas-2))) 
                                      (append-casas operador (cdr casas-2)))
                             )
                        )
                     )) (append-casas (car operadores) casas)))
   )     
)


(defun no-solucaop (no target) 
  "Devolve t se no satisfaz condicao de vitoria"
  (cond ((>= (length (get-casas-ocupadas (get-tab no))) target) t)
        ;(t (print-no no))
        (t nil)
  )
)

(defun get-tab (no)
  "Devolve tabuleiro de um no"
  (car (no-estado no))
)

(defun print-no (no alg &optional (form t))
  "Imprime no para o ecra"
  (progn (print-tab (car (car no)) form) (format form "Pecas: ~a~%Profundidade: ~a~%" (cadr (car no)) (no-profundidade no)))
         (cond ((equal alg 'a*) (format form "Heuristica: ~a ~%Custo: ~a~%" (no-heuristica no) (no-custo no))))
         (format form "~%")
)

(defun print-tab (tab &optional (form t))
  "Imprime tabuleiro para o ecra"
  (cond ((null tab) nil)
        (t (progn (format form "~a~%" (car tab)) (print-tab (cdr tab) form)))
  )
)

(defun count-casas-ocupadas(tabuleiro)
  "Devolve quantidade de casas ocupadas num tabuleiro"
  (length (get-casas-ocupadas tabuleiro))
)

(defun get-casas-ocupadas(tabuleiro)
  "Devolve casas ocupadas no tabuleiro"
  (get-casas-ocupadas-line 0 tabuleiro)
)

(defun get-casas-ocupadas-line(line tabuleiro)
  "Auxiliar a get-casas-ocupadas"
  (cond ((> line 13) nil)
        (t 
         (labels ((go-line (column)
                    (cond ((> column 13) (get-casas-ocupadas-line (+ line 1) tabuleiro))
                          ((= (celula line column tabuleiro) 1) (cons (list line column) (go-line (+ column 1))))
                          (t (go-line (+ column 1)))
                    )
                  )) (go-line 0))
         )
   )     
)

(defun novo-sucessor (pai operador)       
  "Devolve novo no apos aplicado operador a um no pai"
   (let ((prof-filho (+ 1 (no-profundidade pai)))         
         (estado-filho (funcall (car operador) (second operador) (third operador) (no-estado pai))))     
           (list estado-filho prof-filho pai)     
   )
)

(defun novo-sucessor-a* (pai operador heuristica target)
  "Devolve novo no, adaptado ao algoritmo a* apos aplicado operador a um no pai"
   (let ((prof-filho (+ 1 (no-profundidade pai)))         
         (estado-filho (funcall (car operador) (second operador) (third operador) (no-estado pai))))     
           (list estado-filho prof-filho (funcall heuristica estado-filho target) pai)     
   )
)

(defun sucessores (pai funcoes alg &optional profundidade heuristica target)
  "Retorna os resultados da aplicacao de todos os operadores possiveis dado um estado"
  (cond ((null funcoes) nil)
        ((equal alg 'dfs) 
          (cond ((= (no-profundidade pai) profundidade) nil)
           (t (cons (novo-sucessor pai (car funcoes)) (sucessores pai (cdr funcoes) alg profundidade)))))
        ((equal alg 'bfs)         
          (cons (novo-sucessor pai (car funcoes)) (sucessores pai (cdr funcoes) alg)))
        ((equal alg 'a*)
           (cons (novo-sucessor-a* pai (car funcoes) heuristica target) (sucessores pai (cdr funcoes) alg nil heuristica target)))        
        (t nil)
  )        
)

(defun jogar-inicial(peca estado)
  "Coloca peca na posicao inicial do tabuleiro"
  (colocar-primeira-peca 0 0 peca estado)
)

(defun operadoresIniciais (tabuleiro)
  "Retorna jogadas possiveis no primeiro turno"
  (list '(jogar-inicial peca-a tabuleiro) '(jogar-inicial peca-b tabuleiro) '(jogar-inicial peca-c-1 tabuleiro) '(jogar-inicial peca-c-2 tabuleiro))
)

(defun heuristica (estado target)
  "Calcula heuristica de um no"
   (- target (count-casas-ocupadas (car estado)))
)

(defun penetrancia (lengthToSolution totalNrNodes)
  "Devolve penetrancia dada a profundidade e o numero total de nos gerados"
  (coerce (/ lengthToSolution totalNrNodes) 'short-float)
)


(defun ramificacaoMedia (nrNos nosExpandidos)
  "Devolve ramificacao media dado os nos gerados e nos expandidos"
  (coerce (/ nrNos nosExpandidos) 'short-float)
)

(defun mostrar-nos (no alg &optional (form t))
  "Imprime cadeia de nos do ultimo para o primeiro"
  (cond ((null no) nil)
        (t (progn (print-no no alg form) (mostrar-nos (no-pai no alg) alg form)))
  )
)

(defun get-first-no (no alg)
  "Devolve primeiro no de uma cadeia de nos"
  (cond ((null (no-pai no alg)) no)
        (t (get-first-no (no-pai no alg) alg))
  )
)


(defun no-estado(no)
  "Devolve estado de um no"
  (car no)
)

(defun no-pai (no alg)
  "Devolve o no pai"
  (cond ((equal alg 'a*) (fourth no))
        (t (third no))
  )
)

(defun no-profundidade (no)
  "Devolve profundidade do no"
  (cadr no)
)

(defun no-heuristica(no)
  "Devolve heuristica do no"
  (third no)
)

(defun no-custo (no)
  "Devolve custo de um no"
  (+ (no-profundidade no) (no-heuristica no))
)
