(defvar *jogada* nil)

(defvar infinity+ most-positive-fixnum)
(defvar infinity- most-negative-fixnum)

(defvar *nos-explorados* 0)
(defvar *cortes-a-b* 0)

(defun minimax-start (no depth alfa beta maxTurn maxPlayer minPlayer time-limit sucessores heuristica no-pai)
  "Reinicializa as varáveis globais e inicia o algoritmo minimax. Retorna uma lista com a jogada e informacao estatistica."
  (progn (setf *nos-explorados* 0)
         (setf *cortes-a-b* 0) 
	 (setf *jogada* nil)        
         (let ((start-time (get-universal-time))
               (val (minimax no depth alfa beta maxTurn maxPlayer minPlayer time-limit sucessores heuristica))
               (jogada (set-jogada-1st-node *jogada* no no-pai)))         
              (list jogada val (- (get-universal-time) start-time) *nos-explorados* *cortes-a-b* maxPlayer)
                 
           )
  )
)

(defun minimax (no depth alfa beta maxTurn maxPlayer minPlayer time-limit sucessores heuristica &optional (start-time (get-universal-time)))
  "Devolve o valor da melhor jogada e altera as variaveis de estatistica"
  (cond ((or (= depth 0) (> (- (get-universal-time) start-time) time-limit)) (funcall heuristica no maxPlayer minPlayer)) 
        (t 
        (cond (maxTurn                
               (let* ((children (funcall sucessores no maxPlayer)))
                   (cond ((null children) (funcall heuristica no maxPlayer minPlayer))
                         (t
                   (labels ((iter (children alfa play) 
                         (cond ((null children) alfa)      
                               (t (let* ((child (car children))
                                         (val (minimax child (1- depth) alfa beta nil maxPlayer minPlayer time-limit sucessores heuristica)))        
                                      (progn (setf *nos-explorados* (1+ *nos-explorados*))                 
                                      (cond ((>= val beta) (progn(setf *cortes-a-b* (1+ *cortes-a-b*))
                                                                  alfa))                                            
                                            (t (cond ((>= (- (get-universal-time) start-time) time-limit) (progn (update-play-alfa val alfa child) (max alfa val)))
                                                       (t (iter (cdr children) (max alfa val) (update-play-alfa val alfa child)))
                                               )                       
                                      )
                                      ))
                                    )
                                  )
                               ))) (iter children alfa nil)))))
              )             
        (t 
           (let* ((children (funcall sucessores no minPlayer)))
              (cond ((null children) (funcall heuristica no maxPlayer minPlayer))
                    (t
              (labels ((iter (children beta play) 
              (cond ((null children) beta)      
                    (t (let* ((child (car children))
                              (val (minimax child (1- depth) alfa beta t maxPlayer minPlayer time-limit sucessores heuristica)))                         
                           (progn (setf *nos-explorados* (1+ *nos-explorados*))
                           (cond ((<= val alfa) (progn  (setf *cortes-a-b* (1+ *cortes-a-b*))
                                                         beta))
                                 (t (cond ((>= (- (get-universal-time) start-time) time-limit) (progn (update-play-beta val beta child) (min beta val)))
                                          (t (iter (cdr children) (min beta val) (update-play-beta val beta child)))                               
                                 )
                           )))
                         )
                       )
               ))) (iter children beta nil)))))   
            )        
        ))
  )
)

(defun update-play-alfa (val alfa child)
  "Atualiza a melhor jogada se o alfa for atualizado"
  (if (> val alfa) (setf *jogada* child))
)

(defun update-play-beta (val beta child)
  "Atualiza a melhor jogada se o beta for atualizado"
  (if (< val beta) (setf *jogada* child))
)


(defun set-jogada-1st-node(no pai no-pai)
  "Devolve a primeira jogada de uma serie de jogadas"
  (cond ((null no) nil)
        ((equal (funcall no-pai no) pai) no)
        (t (set-jogada-1st-node (fourth no) pai no-pai))
  )
)






#|
;;minimax alternativo
(defun minimax (no depth alfa beta maxTurn maxPlayer minPlayer time-limit sucessores heuristica &optional (start-time (get-universal-time)))
  (cond 
        ((or (= depth 0) (> (- (get-universal-time) start-time) time-limit)) (funcall heuristica no maxPlayer minPlayer)) 
        (t 
        (cond (maxTurn                
               (let* ((maxEval infinity-)
                   (children (funcall sucessores no maxPlayer)))
                   (cond ((null children) (funcall heuristica no maxPlayer minPlayer))
                         (t
                   (labels ((iter (children) 
                         (cond ((null children) maxEval)      
                               (t (let* ((child (car children))
                                         (val (minimax child (1- depth) alfa beta nil maxPlayer minPlayer time-limit sucessores heuristica)))
                                     
                                      
                                      (progn
                                      (setf *nos-explorados* (1+ *nos-explorados*))
                                      ;(if (> val alfa) (progn (setf maxEval (set-max maxEval val)) (set-jogada child) (print-best-play child val maxPlayer 'Max alfa beta)))
                                
                                      (cond ((>= val beta) (progn ;(format t "CUTTING at alfa: ~a | beta: ~a ~%" alfa beta)
                                                                  ; (print-no child)
                                                                   (setf *cortes-a-b* (1+ *cortes-a-b*))
                                                                    maxEval))
                                            
                                            (t (progn 
                                                 (if (> val alfa) (progn (setf alfa val) (set-jogada child))) ;(print-best-play child val maxPlayer 'Max alfa beta)))
                                                 
                                                 (setf maxEval (max maxEval val))
                                                 (cond ((>= (- (get-universal-time) start-time) time-limit) maxEval)
                                                       (t (iter (cdr children)))
                                               )
                                            )
                                      
                                      )
                                      ))
                                    )
                                  )
                               ))) (iter children)))))
              )             
        (t 
           (let* ((minEval infinity+)
              (children (funcall sucessores no minPlayer)))
              (cond ((null children) (funcall heuristica no maxPlayer minPlayer))
                    (t
              (labels ((iter (children) 
              (cond ((null children) minEval)      
                    (t (let* ((child (car children))
                              (val (minimax child (1- depth) alfa beta t maxPlayer minPlayer time-limit sucessores heuristica)))
                         
                           
                           (progn
                           (setf *nos-explorados* (1+ *nos-explorados*))
                           ;(if (< val minEval) (progn (setf minEval val) (set-jogada child)));(print-best-play child val maxPlayer 'Min alfa beta)))                           
                            (setf beta (min beta val))
                           (cond ((<= val alfa) (progn ;(format t "CUTTING at alfa: ~a | beta: ~a ~%" alfa beta)
                                                        ;(print-no child) 
                                                        (setf *cortes-a-b* (1+ *cortes-a-b*))
                                                         minEval))
                                 (t (progn 
                                      (if (< val beta) (progn (setf beta val) (set-jogada child))); (print-best-play child val maxPlayer 'Min alfa beta)))
                                                 
                                      (setf minEval (min minEval val))
                                      (cond ((>= (- (get-universal-time) start-time) time-limit) minEval)
                                            (t (iter (cdr children)))
                                    )
                                 )
                           )))
                         )
                       )
               ))) (iter children)))))   
            )        
        ))
  )
)


(defun print-best-play (no val player turn alfa beta)
  (progn (format t "Max Player: ~a | Turn: ~a~%" player turn)
         (format t "Current play value: ~a | Alfa: ~a | Beta: ~a~%" val alfa beta)
         (print-no no))
)

(defun set-jogada (child)
    (setf *jogada* child)
)

(defun set-max(old-val new-val)
  (max old-val new-val)
)
|#



