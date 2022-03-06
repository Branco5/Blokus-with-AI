
(defun bfs (no solucao sucessores operadores target registar &optional (abertos nil) (fechados nil) (beginTime (get-universal-time)))
  "Implementacao do algoritmo bfs"
  (cond ((and (null abertos) (not (member no fechados))) 
            (let* ((fechado (append fechados (list no)))
                  (suces (sucessoresSemRepetidos no fechado sucessores (funcall operadores no) 'bfs)))
                (bfs no solucao sucessores operadores target registar (abertos-bfs abertos suces) fechado)
            )           
         )     
        (t (labels ((iter (no-iter open open-global closed)                      
                      (cond  ((null no-iter)  
                                (let ((open-next (bfs-open-next closed open-global sucessores operadores)))
                               (iter (car open-next) (cdr open-next) (cdr (abertos-bfs open-global open-next)) (append closed (list(car open-global)))))
                               )
                             ((funcall solucao no-iter target) (funcall registar no-iter abertos closed (- (get-universal-time) beginTime) 'bfs))

                             ;;se nos sucessores (open) forem todos avaliados, expande primeiro no aberto (open-global)
                             ((null open) 
                                (let ((open-next (bfs-open-next closed open-global sucessores operadores)))
                               (iter (car open-next) (cdr open-next) (cdr (abertos-bfs open-global open-next)) (append closed (list(car open-global)))))
                             )
                             
                             ;;se houver nos abertos por avaliar                           
                            (t (iter (car open) (cdr open) open-global closed))

                      ))) (iter no abertos abertos fechados))
       ) 
  )       
)

(defun bfs-open-next(closed open-global sucessores operadores)
  "Devolve nos sucessores excluindo nos ja explorados"
  (let* ((existentes (append closed open-global)))
    (sucessoresSemRepetidos (car open-global) existentes sucessores (funcall operadores (car open-global)) 'bfs)
  )
)


(defun dfs (no solucao sucessores operadores target profundidade registar &optional (abertos nil) (fechados nil) (beginTime (get-universal-time)))
  "Implementacao do algoritmo dfs"
        (cond ((and (null abertos) (not (member no fechados))) 
            (let* ((fechado (append fechados (list no)))
                  (suces (sucessoresSemRepetidos no fechado sucessores (funcall operadores no) 'dfs profundidade)))
                (dfs (car suces) solucao sucessores operadores target profundidade registar (cdr suces) (append fechado (list (car suces))))
            )           
         )
        (t
           (labels ((iter (no-iter open closed expandidos)                         
                      (cond ((and (null no-iter) (null open)) (format t "Solução não encontrada~%"))                            
                            ((funcall solucao no-iter target) (funcall registar no-iter abertos expandidos (- (get-universal-time) beginTime) 'dfs))                            
                            (t (let ((open-next (sucessoresSemRepetidos no-iter closed sucessores (funcall operadores no-iter) 'dfs profundidade)))

                             ;;profundidade maxima atingida
                             (cond ((null open-next) (iter (car open) (cdr open) (adicionar-no-fechados (car open) closed 'dfs) (append expandidos (list (car open)))))
                                   (t (iter (car open-next) (abertos-dfs (cdr open-next) open) (adicionar-no-fechados (car open-next) closed 'dfs) (append expandidos (list (car open-next)))))
                            )
                            )
                   )))) (iter no abertos fechados fechados)
             )
           )
           )           
) 

(defun a* (no solucao sucessores operadores target heuristica no-custo registar &optional (abertos nil) (fechados nil) (beginTime (get-universal-time)))
  "Implementacao do algoritmo a*"
  (cond ((and (null abertos) (not (member no fechados))) 
            (let* ((fechado (list no))
                  (suces (sucessoresSemRepetidos no fechado sucessores (funcall operadores no) 'a* nil heuristica target))
                  (open (put-sucessores-abertos no-custo abertos suces)))
                (a* (car open) solucao sucessores operadores target heuristica no-custo registar (cdr open) (adicionar-no-fechados (car open) fechado 'a*) (get-universal-time))
            )           
         )   
        (t (labels ((iter (no-iter abertos fechados expandidos)                      
                      (cond  ((funcall solucao no-iter target) (funcall registar no-iter abertos expandidos (- (get-universal-time) beginTime) 'a*))
                             (t                            
                             (let* ((suces (sucessoresSemRepetidos no-iter (append abertos fechados) sucessores (funcall operadores no-iter) 'a* nil heuristica target))
                                    
                                   (open (put-sucessores-abertos no-custo abertos suces)))
                               
                              (iter (car open) (cdr open) (adicionar-no-fechados (car open) fechados 'a*) (append expandidos (list (car open))))
                              )
                             )
                    ))) (iter no abertos fechados fechados))
       ) 
  )       
)

(defun order-nos (list no-custo)
  "Ordena os nos de acordo com o custo"
  (cond ((null list) nil)        
        (t 
         (let ((min-no (find-min-no list no-custo)))
         (cons min-no (order-nos (remove min-no list) no-custo)))
        )
  )
)

(defun find-min-no (list no-custo &optional (no (car list)) (index 0) (min-index 0))
  "Devolve o no na lista de menor custo"
  (cond ((null list) no)
        ((< (funcall no-custo (car list)) (funcall no-custo no)) (find-min-no (cdr list) no-custo (car list) (+ index 1) index))
        (t (find-min-no (cdr list) no-custo no (+ index 1) min-index))
  )
)

(defun put-sucessores-abertos (no-custo abertos sucessores)
  "Coloca no em abertos de acordo com o custo"
  (order-nos (append sucessores abertos) no-custo)
)


(defun adicionar-no-fechados (no fechados alg)
  "Adiciona no a fechados caso este ainda nao exista em fechados ou caao este exista mas com profundidade ou custo maior"
  (cond ((null fechados) (list no))       
        (t
        (let ((result (no-existep no fechados alg)))
         (cond ((equal result t) fechados)
               ((equal result nil) (append fechados (list no)))
               (t (replace-no result no fechados))
         )
        )
        )
  )
)

(defun replace-no (replaced replacer list)
  "Substitui um no por outro numa lista"
  (cond ((null list) nil)
        ((equal (car list) replaced) (append (list replacer) (cdr list)))
        (t (cons (car list) (replace-no replaced replacer (cdr list))))
  )
)

(defun no-existep (no lista alg)
  "Devolve t se no existe na lista. Devolve o no na lista caso este seja igual ao no no parametro mas com profundidade/custo maior"  
  (cond ((null lista) nil)
            ((equal (car no) (car (car lista)))
               (cond ((equal alg 'bfs) t) 
                     ((equal alg 'dfs)
                       (cond ((< (no-profundidade no) (no-profundidade (car lista))) (car lista))
                              (t t))                      
                     )
                     ((equal alg 'a*)
                      (cond ((< (no-custo no) (no-custo (car lista))) (car lista))
                              (t t))  
                      )
                     (t nil)
               ))
              (t (no-existep no (cdr lista) alg))         
  )
)

(defun sucessoresSemRepetidos(no lista sucessores operadores alg &optional profundidade heuristica target)
  "Devolve sucessores sem nos existentes numa lista"
  (cond ((null no) nil)
        ((equal alg 'bfs) (excluirRepetidos (funcall sucessores no operadores alg) lista alg))
        ((equal alg 'dfs) (excluirRepetidos (funcall sucessores no operadores alg profundidade) lista alg))
        ((equal alg 'a*) (excluirRepetidos (funcall sucessores no operadores alg nil heuristica target) lista alg))
        (t nil)
  )
)

(defun excluirRepetidos (sucessores lista alg)
  "Exclui nos que ja existem numa lista da lista de sucessores"
  (cond ((null lista) nil)
        ((null sucessores) nil)
        ((no-existep (car sucessores) lista alg) (excluirRepetidos (cdr sucessores) lista alg))
        (t (cons (car sucessores) (excluirRepetidos (cdr sucessores) lista alg)))
  )
)

(defun abertos-bfs (abertos sucessores)
  "Introduz sucessoras na lista de abertos de acordo com procura em largura"
  (append abertos sucessores)
)

(defun abertos-dfs (sucessores abertos)
  "Introduz sucessoras na lista de abertos de acordo com procura em profundidade"
  (append sucessores abertos)
)