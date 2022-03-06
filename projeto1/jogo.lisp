(defvar *base-path* "TiagoBranco_201500575/projeto1/")

(load (compile-file (concatenate 'string *base-path* "puzzle.lisp")))
(load (compile-file (concatenate 'string *base-path* "algoritmo.lisp")))

(defun start ()
"Permite iniciar o programa, fazendo a leitura do estado inicial e do algoritmo a utilizar para procurar a solucao"
  (let* ((file-data (read-input-file (concatenate 'string *base-path* "problemas.dat") (select-problem)))
         (algoritmo (ler-algoritmo))
         (target (get-target-from-file-str file-data))
         (no (cria-no (get-tab-from-file-str file-data) algoritmo target))         
         (profundidade (cond ((eql algoritmo 'dfs) (ler-profundidade))))          
         )
      (progn
	(cond
		((equal algoritmo 'bfs) (funcall algoritmo no 'no-solucaop 'sucessores 'get-jogadas-validas target 'registar-solucao))
		((equal algoritmo 'dfs) (funcall algoritmo no 'no-solucaop 'sucessores 'get-jogadas-validas target profundidade 'registar-solucao))
                ((equal algoritmo 'a*) (funcall algoritmo no 'no-solucaop 'sucessores 'get-jogadas-validas target 'heuristica 'no-custo 'registar-solucao))              
	)
        (start)
       ) 
  )
)


(defun cria-no (tab alg target)
  "Cria um no com base num tabuleiro e algoritmo"
  (cond ((equal alg 'a*) (list (list tab '(10 10 15)) 0 target nil))
        (t (list (list tab '(10 10 15)) 0 nil))
  )
)

(defun get-tab-from-file-str(str)
  "Devolve tabuleiro a partir dos dados do ficheiro"
  (cdr str)
)
(defun get-target-from-file-str(str)
  "Devolve pontuacao objetivo a partir dos dados do ficheiro"
  (car (car str))
)

(defun read-input-file(name problem)
  "Le um ficheiro"
  (with-open-file (file name :direction :input)
                  (find-problem file problem) 
  )
)

(defun find-problem (f1 p)
  "Le os dados do ficheiro quando encontra problema requerido"
  (let ((linha (read-line f1 nil :fim)))
    (cond ((not (eq linha :fim))            
              (cond ((equal linha p) (read-board f1))
                    (t (find-problem f1 p))))
          (t (format t "Problem doesn't exist"))
    )
  )
)

(defun read-board (f1)
  "Permite ler um tabuleiro a partir de um ficheiro"
  (let ((linha (read-line f1 nil :fim)))
    (cond ((not (eq linha :fim))            
              (cond ((equal linha "") nil)
                    (t (cons (read-from-string linha) (read-board f1)))))
          (t (progn (close f1) nil))
    )
  )
)

(defun select-problem()
  "Permite fazer a leitura do problema a resolver"
  (format t "Selecione problema~%>")
  (let ((a (read)))
    (prin1-to-string a)
  )
)


(defun ler-algoritmo ()
"Permite fazer a leitura do algoritmo a utilizar."
  (progn
    (format t "Que algoritmo quer usar para procurar? ~%")
    (format t "1- Procura na largura ~%")
    (format t "2- Procura na profundidade ~%")
    (format t "3- Procura A* ~%")
    (let ((resposta (read)))
      (cond ((= resposta 1) 'bfs)
            ((= resposta 2) 'dfs)
            ((= resposta 3) 'a*)
      ))
    )
  )

(defun ler-profundidade()
"Permite fazer a leitura da profundidade limite para o algoritmo dfs."
    (progn
    (format t "Qual a profundidade limite? ~%")
    (read)
    )
)


(defun registar-solucao (no abertos fechados tempo alg)
  "Imprime e escreve resultados num ficheiro dat"
  (progn (escrever-resultado no abertos fechados tempo alg)
         (mostrar-solucao no abertos fechados tempo alg)
  )
)


(defun mostrar-solucao (no abertos fechados tempo alg)
  "Imprime resultados na consola"
  (let ((totalNosGerados (- (length (append abertos fechados)) 1)) 
        (totalExpandidos (length fechados)))
  (progn (format t "~%Solucao:~%") (print-no no alg)
	 (format t "Algoritmo: ~a~%" alg)
         (format t "Nos gerados: ~a~%" totalNosGerados)
         (format t "Nos expandidos: ~a~%" (length fechados))
         (format t "Solucao encontrada em ~a segundos~%" tempo)
         (format t "Penetrancia: ~a~%" (penetrancia (no-profundidade no) totalNosGerados))
         (format t "Ramificacao media: ~a~%" (ramificacaoMedia totalNosGerados totalExpandidos))
         (format t "~%Caminho percorrido:~%")
         (mostrar-nos no alg)         
  )  
  )
)

(defun escrever-resultado (no abertos fechados tempo alg)
  "Escreve resultados num ficheiro dat"
  (with-open-file (str (concatenate 'string *base-path* "resultados.dat")
                     :direction :output
                     :if-exists :append
                     :if-does-not-exist :create)
    (let ((totalNosGerados (- (length (append abertos fechados)) 1)) ;;sem contar com raiz
        (totalExpandidos (length fechados)))
  (progn (format str "~%Raiz:~%") (print-no (get-first-no no alg) alg str)
         (format str "Solucao:~%") (print-no no alg str)
	 (format str "Algoritmo: ~a~%" alg)
         (format str "Nos gerados: ~a~%" totalNosGerados)
         (format str "Nos expandidos: ~a~%" (length fechados))
         (format str "Solucao encontrada em ~a segundos~%" tempo)
         (format str "Penetrancia: ~a~%" (penetrancia (no-profundidade no) totalNosGerados))
         (format str "Ramificacao media: ~a~%~%" (ramificacaoMedia totalNosGerados totalExpandidos))  
	 (format str "~%Caminho percorrido:~%")
         (mostrar-nos no alg str)
  )  
  )
  (close str)
  )
)