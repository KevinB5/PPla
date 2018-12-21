;(in-package :user)

; Estrutura de dados do estado
(defstruct state
  shifts
  unusedTasks
  )

; Estrutura de dados do turno
(defstruct shift
  tasks
  duration
  lastPoint
  mealBrake
  )


; Updates the duration of the shift to 6h if it is less than 6h
(defun less-than-6h (shift)
    (let ((aux shift)
          (duration (shiftDuration shift)))
            (if (< duration 360)
                (setf (shift-duration aux) 360)
            )
    aux)
)

; Função de que retorna a duração total do turno em inserido como argumento
(defun shiftDuration (shift)
  (let ((duration 0))
  (if (equal (list-length (shift-tasks shift)) 1)
    (setf duration (- (nth 3 (first (shift-tasks shift))) (nth 2 (first (shift-tasks shift)))))
    (setf duration (- (nth 3 (nth (- (list-length (shift-tasks shift)) 1) (shift-tasks shift))) (nth 2 (first (shift-tasks shift)))))
    )
    duration))

; Função que adiciona 40 minutos ao turno se não iniciou na primeira posição
(defun check-start-location-not-L1 (shift)
    (let ((aux shift)
          (start-location (nth 0(nth 0 (shift-tasks shift)))))
                (if (not (eql start-location (car '(L1))))
                       (setf (shift-duration aux) (+ (shift-duration aux) 40))
                )
    aux)
)

; Cria um turno
(defun makeShift (tasks)
  (let ((ini (make-shift :tasks '()
                        :duration 0
                        :lastPoint "L"
                        :mealBrake nil)))
        (setf (shift-tasks ini) tasks)
        (setf (shift-duration ini) (shiftDuration ini))
        (setf (shift-lastPoint ini) (nth 1 (nth (- (list-length (shift-tasks ini)) 1) (shift-tasks ini))))
        ;(setf (shift-mealBrake ini) (hadMealBreak ini))
  ini))

; Atualiza a duração de um turno e a ultima localizacao
(defun updateShift (shift)
  (let ((aux shift))
    (setf (shift-duration aux) (shiftDuration aux))
    (setf (shift-lastPoint aux) (nth 1 (nth (- (list-length (shift-tasks aux)) 1) (shift-tasks aux))))
  aux)
  )

; Adiciona uma tarefa ao turno
(defun addTask (shift task)

  (cond ((not (equal nil shift)) (let ((aux (first shift)))
            (setf (shift-tasks aux) (append (shift-tasks aux) (list task)))
            (updateShift aux)))

        ((equal nil shift) (makeShift (list task)))
        )
  )

; Adiciona uma tarefa ao estado
(defun addShift (shifts task)
    (cond ((and (not (equal nil shifts)) (listp shifts) ) (let ((aux shifts))
            (setf aux (append aux (list (makeShift (list task)))))
          aux))

          ((and (not (equal nil shifts)) (not (listp shifts)) ) (let ((aux shifts))
            (setf aux (append (list aux) (list (makeShift (list task)))))
          aux))

          ((equal nil shifts) (list (makeShift (list task))))
          )
    )

; Cria o estado inicial
(defun makeInitialState (tasks)
  (let ((ini (make-state :shifts '() :unusedTasks tasks)))
  ini))

; Função que verifica se foi realizado a pausa para o almoço
; (defun hadMealBreak (shift)
;   (loop for task in (shift-tasks shift)
;         do(
;             if (and (equal (nth 0 task) (nth 1 task)) (equal 40 (- (nth 3 task) (nth 2 task))))
;                 (and (equal (nth 0 task) (nth 1 task)) (equal 40 (- (nth 3 task) (nth 2 task)))))
;         )
;    )

; Função que devolve a posiçao de um turno, numa lista de turnos
(defun index (shifts shift)
  (let ((pos 0))
    (loop for aux in shifts
      do(
          cond ((and (equal (shift-tasks aux) (shift-tasks shift))
                     (equal (shift-duration aux) (shift-duration shift))
                     (equal (shift-lastPoint aux) (shift-lastPoint shift))
                     (equal (shift-mealBrake aux) (shift-mealBrake shift))
                     (/= (position aux shifts) 0))
                (setf pos (+ pos 1)))
         )
      )
    pos)
  )

; Função custo
(defun custo (state)
  (let ((auxShifts (state-shifts state))
        (custoTotal 0))
                (loop for shift in auxShifts do
                  (setf custoTotal (+ custoTotal (shiftDuration shift)))
                )
    custoTotal)
)

; Função operadores
(defun operator (state)
  (let ((auxState state)
        states)
    (let ((auxShifts (state-shifts auxState))
          (auxTasks (state-unusedTasks auxState)))
      (cond

        ((equal nil auxShifts) (setf auxShifts (addTask (state-shifts auxState) (first auxTasks))
                                     auxTasks (rest auxTasks)))



         ((and (not (equal nil auxShifts)) (not (listp auxShifts)))
                 (loop for shift in (list auxShifts)
                   do(

                       cond ((not (equal (nth 0(first auxTasks)) (shift-lastPoint shift)))
                            (setf auxShifts (addShift auxShifts (first auxTasks))
                                  auxTasks (rest auxTasks)))

                            ((equal (nth 0(first auxTasks)) (shift-lastPoint shift))
                            (setf (nth (index auxShifts shift) auxShifts) (addTask (list shift) (first auxTasks))
                                  auxTasks (rest auxTasks)))
                      )
                   )
                 )

        ((and (not (equal nil auxShifts)) (listp auxShifts))
              (loop for shift in auxShifts
                do(
                    cond    ((not (equal (nth 0(first auxTasks)) (shift-lastPoint shift)))
                            (setf auxShifts (addShift auxShifts (first auxTasks))
                                  auxTasks (rest auxTasks)))

                            ((equal (nth 0(first auxTasks)) (shift-lastPoint shift))
                            (setf (nth (index auxShifts shift) auxShifts) (addTask (list shift) (first auxTasks))
                                  auxTasks (rest auxTasks)))
                  )
                )
              )
      )
      (push (make-state :shifts  auxShifts
				  :unusedTasks auxTasks)
		  states)
      )

   (values states))
)

; Função que verifica se o estado atingiu o objetivo
(defun objective? (state)
  (equal nil (state-unusedTasks state)))


; Função que executa a solução do problema
(defun faz-afectacao (tasks strategy)

  (procura (cria-problema (makeInitialState tasks) (list #'operator) :objectivo? #'objective?) strategy)

  ; tempo
  ; (round (/ (- (get-internal-run-time) tempo-inicio) internal-time-units-per-second))
  )

; (cria-problema <initial state>
;                <list of operators>
;                :objectivo? <function that tests the final state>
;                :estado= <equality function for states>
;                :hash <function that gives the hashing value for a state>
;                :custo <function that gives the generation cost of a state>
;                :heuristica <function that evaluates a state>)

; (procura <problem>
;          <search type>
;          :profundidade-maxima <integer representing the maximun depth>
;          :espaco-em-arvore? <tree search space: t or nil>)

; (load(compile-file "G18.lisp"))
(setf tt '((L1 L2 34 60) (L5 L1 408 447) (L1 L1 448 551) (L1 L1 474 565)))
; (setf sh (makeShift tt))
(setf s1 (makeInitialState tt))
; (print "s1")
; (print s1)
; (setf s2 (operator s1))
; (print "s2")
; (print s2)
; (setf s3 (operator (first s2)))
; (print "s3")
; (print s3)
; (setf s4 (operator (first s3)))
; (print "s4")
; (print s4)
;(setf s2 (addShift (state-shifts s1) (first tt)))
; (print s2)
; (print s1)
; (equal (nth 0 (first tt)) (nth 1 (nth (- (list-length (shift-tasks sh)) 1) (shift-tasks sh))))
; (print tt)
; (print (nth 0 (first tt)))
; (print (nth 1 (nth (- (list-length (shift-tasks sh)) 1) (shift-tasks sh))))
; (print s1)
; (print "s2 (operator s1)")
; (setf s2 (operator s1))
; (print s2)
; (print "s3 (operator (first s2))")
; (setf s3 (operator (first s2)))
; (print (state-shifts s3))
(print (procura (cria-problema s1 (list #'operator) :objectivo? #'objective? :estado= #'equal) "profundidade"))

;(objective? (makeInitialState '()))
; (setf t1 '(L1 L2 3 4))
; (setf t2 '(L3 L4 78 90))
; (setf t3 '((L2 L1 1 25) (L1 L2 34 60)))
; (setf s1 (makeInitialState t3))
; ; (print s1)

; ; (print s2)
; (setf s3 (makeShift t3))
; (print s3)
; (setf s2 (addShift s3 t2))
; (print s2)
; ; (print s3)
; (operator s1)
; -----------------------------------------------------------------------------

; ; Algoritmos

; Algoritmo IDA*
; (defun ida* (problem)
;   (setf (problem-iterative? problem) t)
;   (let* ((root (create-start-node problem))
; 	 (f-limit (node-f-cost root))
; 	 (solution nil))
;     (loop (multiple-value-setq (solution f-limit)
; 	    (DFS-contour root problem f-limit))
;         (dprint "DFS-contour returned" solution "at" f-limit)
; 	(if (not (null solution)) (RETURN solution))
; 	(if (= f-limit infinity) (RETURN nil)))))
;
; ; DFS-contour para IDA*
; (defun DFS-contour (node problem f-limit)
;   "Return a solution and a new f-cost limit."
;   (let ((next-f infinity))
;     (cond ((> (node-f-cost node) f-limit)
; 	   (values nil (node-f-cost node)))
; 	  ((goal-test problem (node-state node))
; 	   (values node f-limit))
; 	  (t (for each s in (expand node problem) do
; 		  (multiple-value-bind (solution new-f)
; 		      (DFS-contour s problem f-limit)
; 		    (if (not (null solution))
; 			(RETURN-FROM DFS-contour (values solution f-limit)))
; 		    (setq next-f (min next-f new-f))))
; 	     (values nil next-f)))))

; Algoritmo de Sondagem Iterativa
; (defun sondagem-iterativa (problema)
;   (let* ((*nos-gerados* 0)
; 		 (*nos-expandidos* 0)
; 		 (objectivo? (problema-objectivo? problema))
; 		 ;(estado= (problema-estado= problema))
; 		 (result nil))
;
;     (labels ((lanca-sonda (estado)
;               (cond ((funcall objectivo? estado) (list estado))
;                     ((null estado) nil)
;                     (t
;                      (let* ((sucessores (problema-gera-sucessores problema estado))
;                             (num-elem (length sucessores)))
;                        (if(equal num-elem 0)
;                            nil
;                          (lanca-sonda (nth (random num-elem) sucessores))))))))
;              (loop while (null result) do
;                (setf result (lanca-sonda (problema-estado-inicial problema))))
;
;              (return-from sondagem-iterativa (list result *nos-expandidos* *nos-gerados*)))))


; Algoritmo de ILDS
; (defun ilds (problema maxDepth)
;   (let ((*nos-gerados* 0)
; 		(*nos-expandidos* 0)
; 		(max-runtime 300)
; 		(objectivo? (problema-objectivo? problema))
;         ;(estado= (problema-estado= problema))
;         (numMaxDiscrepancia maxDepth)
;         (out-result nil))
;
;     (labels ((ildsProbe (estado maxDiscrepancia rProfundidade start-time)
;                 (let* ((sucessores-nao-ordenados (problema-gera-sucessores problema estado))
; 		       		   (sucessores (funcall (ilds-sorter (problema-heuristica problema)) sucessores-nao-ordenados))
;                        (num-elem (list-length sucessores))
;                        (result nil))
;                      (cond 	((funcall objectivo? estado) (list estado))
;                      		((or (= 0 num-elem) (<= (- max-runtime (/ (- (get-internal-run-time) tempo-inicio) internal-time-units-per-second)) 2.5)) nil)
;                      		(t
;                      			(setf result nil)
;                      			(if (> rProfundidade maxDiscrepancia)
;                      				(setf result (ildsProbe (car sucessores) maxDiscrepancia (- rProfundidade 1) start-time)))
;                      			(if (and (> maxDiscrepancia 0) (null result))
;                      				(progn
; 	                     				(dolist (suc (cdr sucessores))
; 	                     					(setf result (ildsProbe suc (- maxDiscrepancia 1 ) (- rProfundidade 1) start-time))
; 	                     					(when (not (null result))
; 				                     			(return-from ildsProbe result)))))
;                  				(return-from ildsProbe result))))))
;
; 			(loop for maxDiscrepancia from 0 to numMaxDiscrepancia do
; 				(setf out-result (ildsProbe (problema-estado-inicial problema) maxDiscrepancia maxDepth tempo-inicio))
; 				(when (not (null out-result))
; 					(return-from ilds (list out-result *nos-expandidos* *nos-gerados*)))))))
;

; Algoritmo de 1-samp
; (defun 1-samp (problema profundidade-maxima)
;   "Algoritmo de procura em profundidade primeiro."
;
;   (let ((estado= (problema-estado= problema))
;         (objectivo? (problema-objectivo? problema)))
;
;     (labels ((esta-no-caminho? (estado caminho)
;                                (member estado caminho :test estado=))
;
;              (procura-prof (estado caminho prof-actual)
;                            (cond ((funcall objectivo? estado) (list estado))
;                                  ((= prof-actual profundidade-maxima) nil)
;                                  ((esta-no-caminho? estado caminho) nil)
;                                  (t
;                                    ;; avancamos recursivamente, em profundidade,
;                                    ;; para cada sucessor
;                                    (let* ((sucs (problema-gera-sucessores problema estado))
;                                           (sucs-number (list-length sucs))
;                                           (suc nil)
;                                           (solucao nil))
;                                      (when (eql sucs-number 0)
;                                        (return-from procura-prof nil))
;                                      (setf suc (nth (random sucs-number) sucs))
;                                      (setf solucao (procura-prof suc
;                                                                  (cons estado caminho)
;                                                                  (1+ prof-actual)))
;                                      (when solucao
;                                        (cons estado solucao)))))))
;
;                         (procura-prof (problema-estado-inicial problema) nil 0))))

; Algoritmo de ILDS melhorada
; (defun ilds-best-solution (problema maxDepth)
;   (let ((*nos-gerados* 0)
; 		(*nos-expandidos* 0)
; 		(max-runtime 300)
; 		(objectivo? (problema-objectivo? problema))
;         ;(estado= (problema-estado= problema))
;         (numMaxDiscrepancia maxDepth)
;         (current-result nil)
;         (out-result nil))
;
;     (labels ((ildsProbe (estado maxDiscrepancia rProfundidade start-time)
;                 (let* ((sucessores-nao-ordenados (problema-gera-sucessores problema estado))
; 		       		   (sucessores (funcall (ilds-sorter (problema-heuristica problema)) sucessores-nao-ordenados))
;                        (num-elem (list-length sucessores))
;                        (result nil)
;                        (result-temp nil))
;                      (cond 	((funcall objectivo? estado) (list estado))
;                      		((or (= 0 num-elem) (<= (- max-runtime (/ (- (get-internal-run-time) tempo-inicio) internal-time-units-per-second)) 2.5)) nil)
;                      		(t
;                      			(setf result-temp nil)
;                      			(if (> rProfundidade maxDiscrepancia)
;                      				(setf result-temp (ildsProbe (car sucessores) maxDiscrepancia (- rProfundidade 1) start-time)))
;                      			(if (and (> maxDiscrepancia 0) (null result))
;                      				(progn
; 	                     				(dolist (suc (cdr sucessores))
; 	                     					(if (> (- max-runtime (/ (- (get-internal-run-time) tempo-inicio) internal-time-units-per-second)) 2.5)
; 	                     						(progn
; 			                     					(setf result-temp (ildsProbe suc (- maxDiscrepancia 1 ) (- rProfundidade 1) start-time))
; 			                     					(when (not (null result-temp))
; 			                     						(if (null result)
; 			                     							(setf result result-temp)
; 			                     							(if (is-better-solution-job-shop result result-temp)
; 			                     								(setf result result-temp)))))
; 	             					 		(return-from ildsProbe result)))))
;                      			(setf result result-temp)
;                  				(return-from ildsProbe result))))))
;
; 			(loop for maxDiscrepancia from 0 to numMaxDiscrepancia do
; 				(if (> (- max-runtime (/ (- (get-internal-run-time) tempo-inicio) internal-time-units-per-second)) 5)
; 					(progn
; 						(setf current-result (ildsProbe (problema-estado-inicial problema) maxDiscrepancia maxDepth tempo-inicio))
; 						(when (not (null current-result))
; 							(if (null out-result)
; 								(setf out-result current-result)
; 								(if (is-better-solution-job-shop out-result current-result)
; 									(setf out-result current-result)))))
; 					(return-from ilds-best-solution (list out-result *nos-expandidos* *nos-gerados*))))
;
; 			(return-from ilds-best-solution (list out-result *nos-expandidos* *nos-gerados*)))))


; ; Heurísticas

; Heurística que retorna a quantidade de turnos que contém o estado
(defun heuristic-shifts-quantity (state)
    (let ((auxState state)
          (quantity 0))
      (let((auxShifts (state-shifts auxState)))
        (setq quantity (list-length auxShifts))
    )quantity)

)

; Heurística que retorna a quantidade turnos que não iniciam na localização "L1"
(defun heuristic-shifts-notL1 (state)
    (let ((auxState state)
          (counter 0))
      (let((auxShifts (state-shifts auxState)))
        (loop for shift in auxShifts do
            (let ((start-location (nth 0(nth 0 (shift-tasks shift)))))
                (if (not (eql start-location (car '(L1))))
                    (setq counter (+ counter 1))

                )
            )
        )
    )counter)
)

; Heurística que retorna o tempo que sobrou do turno (em minutos)
(defun heuristic-remaining-shifts-time (state)
    (let ((auxState state)
          (total 0))
      (let((auxShifts (state-shifts auxState)))
        (loop for shift in auxShifts do
            (setq total (+ total (- 480 (shiftDuration shift))))
        )
    )total)
)

; Heurística que retorna o número de serviços inferior a 6h
(defun heuristic-shifts-less-than-6h (state)

    (let ((auxState state)
          (counter 0))
      (let((auxShifts (state-shifts auxState)))
        (loop for shift in auxShifts do
            (if (< 360 (shiftDuration shift))
                (setq counter (+ counter 1))
             )
        )
    )counter)
)


;  (setf problem1 '((L2 L1 1 25) (L10 L1 4 33) (L1 L2 14 40) (L1 L11 14 55) (L4 L1 16 37) (L2 L1 21 45) (L1 L10 26 55) (L1 L9 28 72) (L1 L2 34 60) (L1 L10 46 75) (L10 L1 364 393) (L1 L1 394 465) (L5 L1 408 447) (L1 L1 448 551) (L1 L1 474 565) (L1 L1 512 607) (L1 L1 552 647) (L1 L1 566 633) (L1 L1 594 665) (L1 L1 608 711) (L1 L1 666 733) (L1 L1 684 735) (L1 L1 698 761) (L1 L1 722 777) (L1 L1 736 823) (L1 L1 752 847) (L1 L1 762 817) (L1 L1 776 863) (L1 L1 778 841) (L1 L1 802 857) (L1 L1 808 911) (L1 L1 814 905) (L1 L1 848 951) (L1 L1 856 943) (L1 L1 864 915) (L1 L1 866 933) (L1 L1 886 973) (L1 L1 906 953) (L1 L1 906 973) (L1 L1 934 1005) (L1 L1 944 995) (L1 L1 946 1013) (L1 L1 954 1045) (L1 L1 974 1045) (L1 L1 994 1085) (L1 L1 996 1083) (L1 L1 1018 1081) (L1 L1 1046 1113) (L1 L1 1058 1121) (L1 L1 1078 1141) (L1 L1 1082 1137) (L1 L1 1084 1135) (L1 L1 1086 1153) (L1 L1 1114 1185) (L1 L1 1122 1177) (L1 L1 1144 1195) (L1 L1 1148 1251) (L1 L1 1152 1247) (L1 L1 1168 1271) (L1 L1 1178 1241) (L1 L1 1194 1265) (L1 L1 1196 1283) (L1 L1 1224 1275) (L1 L1 1232 1327) (L1 L1 1234 1325) (L1 L1 1248 1351) (L1 L1 1272 1367) (L1 L1 1276 1363) (L1 L1 1284 1335) (L1 L1 1286 1353) (L1 L1 1318 1381) (L1 L1 1328 1431) (L1 L1 1336 1423) (L1 L1 1358 1421) (L1 L1 1382 1437) (L1 L1 1386 1453) (L1 L1 1386 1453) (L1 L2 1394 1420) (L1 L10 1406 1435) (L1 L2 1414 1440) (L1 L4 1422 1444) (L1 L5 1432 1472)))
;
; (faz-afectacao problem1 "profundidade" )
