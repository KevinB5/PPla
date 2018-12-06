;(in-package :user)

(defstruct state
  shifts
  unusedTasks
  )

(defstruct shift
  tasks
  duration
  lastPoint
  mealBrake
  )

(defun shiftDuration (shift)
  (let ((duration 0))
  (if (equal (list-length (shift-tasks shift)) 1)
    (setf duration (- (nth 3 (first (shift-tasks shift))) (nth 2 (first (shift-tasks shift)))))
    (setf duration (- (nth 3 (nth (- (list-length (shift-tasks shift)) 1) (shift-tasks shift))) (nth 2 (first (shift-tasks shift)))))
    )
    duration))

(defun makeShift (tasks)
  (let ((ini (make-shift :tasks '()
                        :duration 0
                        :lastPoint "L"
                        :mealBrake nil)))
        (setf (shift-tasks ini) tasks)
        (setf (shift-duration ini) (shiftDuration ini))
        (setf (shift-lastPoint ini) (nth 1 (nth (- (list-length (shift-tasks ini)) 1) (shift-tasks ini))))
        ;(setf (shift-mealBrake ini) (hadMealBreak (shift-tasks ini)))
  ini))

(defun addTask (shift task)
  (let ((aux shift))
        (cons (shift-tasks aux) (list task))
  (print aux)
    aux)
  )

(defun addShift (state task)
  (let ((aux state))
        (append (state-shifts aux) (makeShift (list task)))
  aux)
  )
(defun makeInitialState (tasks)
  (let ((ini (make-state :shifts (list ()) :unusedTasks tasks)))
  ini))


; (defun hadMealBreak (tasks)
;   ;for all in tasks
;   ;   if endingTime - startingTime = 40 && startingPoint == endingPoint
;   ;       true
;   ;   false
;   )

(defun operator (state)
  (let ((auxState state))
      (let((auxTasks (state-unusedTasks auxState))
      (auxShift  (last (state-shifts auxState))))

  (push (list (first auxTask)) auxShift)
  (setf (state-shifts auxState) auxShift)
  (setf (state-unusedTasks auxState) (rest auxTask))
    (list auxState))))

; (loop for x in '(a b c d e)




(defun objective? (state)
  (equal '() (state-unusedTasks state)))


(defun faz-afectacao (tasks strategy)

  (print (objective? (makeInitialState tasks)))
  (procura (cria-problema (makeInitialState tasks) (list #'operator) :objectivo? #'objective?) strategy)
  ; (procura problema estrategia)
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

;(load(compile-file "G18.lisp"))
;(faz-afectacao '((L2 L1 1 25) (L1 L2 34 60) (L5 L1 408 447) (L1 L1 448 551)(L1 L1 474 565)) "profundidade" )
;(objective? (makeInitialState '()))
(setf t1 '(L1 L2 3 4))
(setf t2 '(L3 L4 78 90))
(setf t3 '((L2 L1 1 25) (L1 L2 34 60)))
(print "s1")
(setf s1 (makeShift (list t1)))
(print s1)
(print "s3")
(setf s3 (makeShift t3))
(print s3)
(addTask s3 t2)
(print s3)

; ; Algoritmos

; Algoritmo IDA*
(defun ida* (problem)
  (setf (problem-iterative? problem) t)
  (let* ((root (create-start-node problem))
	 (f-limit (node-f-cost root))
	 (solution nil))
    (loop (multiple-value-setq (solution f-limit)
	    (DFS-contour root problem f-limit))
        (dprint "DFS-contour returned" solution "at" f-limit)
	(if (not (null solution)) (RETURN solution))
	(if (= f-limit infinity) (RETURN nil)))))
    
; DFS-contour para IDA*
(defun DFS-contour (node problem f-limit)
  "Return a solution and a new f-cost limit."
  (let ((next-f infinity))
    (cond ((> (node-f-cost node) f-limit)
	   (values nil (node-f-cost node)))
	  ((goal-test problem (node-state node))
	   (values node f-limit))
	  (t (for each s in (expand node problem) do
		  (multiple-value-bind (solution new-f)
		      (DFS-contour s problem f-limit)
		    (if (not (null solution))
			(RETURN-FROM DFS-contour (values solution f-limit)))
		    (setq next-f (min next-f new-f))))
	     (values nil next-f)))))

; Algoritmo de Sondagem Iterativa
(defun sondagem-iterativa (problema)
  (let* ((*nos-gerados* 0)
		 (*nos-expandidos* 0)
		 (tempo-inicio (get-internal-run-time))
		 (objectivo? (problema-objectivo? problema))
		 ;(estado= (problema-estado= problema))
		 (result nil))

    (labels ((lanca-sonda (estado)
              (cond ((funcall objectivo? estado) (list estado))
                    ((null estado) nil)
                    (t
                     (let* ((sucessores (problema-gera-sucessores problema estado))
                            (num-elem (length sucessores)))
                       (if(equal num-elem 0)
                           nil
                         (lanca-sonda (nth (random num-elem) sucessores))))))))
             (loop while (null result) do
               (setf result (lanca-sonda (problema-estado-inicial problema))))

             (return-from sondagem-iterativa (list result (round (/ (- (get-internal-run-time) tempo-inicio) internal-time-units-per-second)) *nos-expandidos* *nos-gerados*)))))


; Algoritmo de ILDS
(defun ilds (problema maxDepth)
  (let ((*nos-gerados* 0)
		(*nos-expandidos* 0)
		(tempo-inicio (get-internal-run-time))
		(max-runtime 300)
		(objectivo? (problema-objectivo? problema))
        ;(estado= (problema-estado= problema))
        (numMaxDiscrepancia maxDepth)
        (out-result nil))

    (labels ((ildsProbe (estado maxDiscrepancia rProfundidade start-time)
                (let* ((sucessores-nao-ordenados (problema-gera-sucessores problema estado))
		       		   (sucessores (funcall (ilds-sorter (problema-heuristica problema)) sucessores-nao-ordenados))
                       (num-elem (list-length sucessores))
                       (result nil))
                     (cond 	((funcall objectivo? estado) (list estado))
                     		((or (= 0 num-elem) (<= (- max-runtime (/ (- (get-internal-run-time) tempo-inicio) internal-time-units-per-second)) 2.5)) nil)
                     		(t
                     			(setf result nil)
                     			(if (> rProfundidade maxDiscrepancia)
                     				(setf result (ildsProbe (car sucessores) maxDiscrepancia (- rProfundidade 1) start-time)))
                     			(if (and (> maxDiscrepancia 0) (null result))
                     				(progn
	                     				(dolist (suc (cdr sucessores))
	                     					(setf result (ildsProbe suc (- maxDiscrepancia 1 ) (- rProfundidade 1) start-time))
	                     					(when (not (null result))
				                     			(return-from ildsProbe result)))))
                 				(return-from ildsProbe result))))))

			(loop for maxDiscrepancia from 0 to numMaxDiscrepancia do
				(setf out-result (ildsProbe (problema-estado-inicial problema) maxDiscrepancia maxDepth tempo-inicio))
				(when (not (null out-result))
					(return-from ilds (list out-result (round (/ (- (get-internal-run-time) tempo-inicio) internal-time-units-per-second)) *nos-expandidos* *nos-gerados*)))))))


; Algoritmo de 1-samp
(defun 1-samp (problema profundidade-maxima)
  "Algoritmo de procura em profundidade primeiro."

  (let ((estado= (problema-estado= problema))
        (objectivo? (problema-objectivo? problema)))

    (labels ((esta-no-caminho? (estado caminho)
                               (member estado caminho :test estado=))

             (procura-prof (estado caminho prof-actual)
                           (cond ((funcall objectivo? estado) (list estado))
                                 ((= prof-actual profundidade-maxima) nil)
                                 ((esta-no-caminho? estado caminho) nil)
                                 (t
                                   ;; avancamos recursivamente, em profundidade,
                                   ;; para cada sucessor
                                   (let* ((sucs (problema-gera-sucessores problema estado))
                                          (sucs-number (list-length sucs))
                                          (suc nil)
                                          (solucao nil))
                                     (when (eql sucs-number 0)
                                       (return-from procura-prof nil))
                                     (setf suc (nth (random sucs-number) sucs))
                                     (setf solucao (procura-prof suc
                                                                 (cons estado caminho)
                                                                 (1+ prof-actual)))
                                     (when solucao
                                       (cons estado solucao)))))))

                        (procura-prof (problema-estado-inicial problema) nil 0))))

; Algoritmo de ILDS melhorada
(defun ilds-best-solution (problema maxDepth) 
  (let ((*nos-gerados* 0)
		(*nos-expandidos* 0)
		(tempo-inicio (get-internal-run-time))
		(max-runtime 300)
		(objectivo? (problema-objectivo? problema))
        ;(estado= (problema-estado= problema))
        (numMaxDiscrepancia maxDepth)
        (current-result nil)
        (out-result nil))
    
    (labels ((ildsProbe (estado maxDiscrepancia rProfundidade start-time)
                (let* ((sucessores-nao-ordenados (problema-gera-sucessores problema estado))
		       		   (sucessores (funcall (ilds-sorter (problema-heuristica problema)) sucessores-nao-ordenados))
                       (num-elem (list-length sucessores))
                       (result nil)
                       (result-temp nil))
                     (cond 	((funcall objectivo? estado) (list estado))
                     		((or (= 0 num-elem) (<= (- max-runtime (/ (- (get-internal-run-time) tempo-inicio) internal-time-units-per-second)) 2.5)) nil)
                     		(t 
                     			(setf result-temp nil)
                     			(if (> rProfundidade maxDiscrepancia)
                     				(setf result-temp (ildsProbe (car sucessores) maxDiscrepancia (- rProfundidade 1) start-time)))
                     			(if (and (> maxDiscrepancia 0) (null result))
                     				(progn
	                     				(dolist (suc (cdr sucessores))
	                     					(if (> (- max-runtime (/ (- (get-internal-run-time) tempo-inicio) internal-time-units-per-second)) 2.5)
	                     						(progn
			                     					(setf result-temp (ildsProbe suc (- maxDiscrepancia 1 ) (- rProfundidade 1) start-time))
			                     					(when (not (null result-temp))
			                     						(if (null result)
			                     							(setf result result-temp)
			                     							(if (is-better-solution-job-shop result result-temp)
			                     								(setf result result-temp)))))
	             					 		(return-from ildsProbe result)))))
                     			(setf result result-temp)
                 				(return-from ildsProbe result))))))

			(loop for maxDiscrepancia from 0 to numMaxDiscrepancia do
				(if (> (- max-runtime (/ (- (get-internal-run-time) tempo-inicio) internal-time-units-per-second)) 5)
					(progn
						(setf current-result (ildsProbe (problema-estado-inicial problema) maxDiscrepancia maxDepth tempo-inicio))
						(when (not (null current-result))
							(if (null out-result)
								(setf out-result current-result)
								(if (is-better-solution-job-shop out-result current-result)
									(setf out-result current-result)))))
					(return-from ilds-best-solution (list out-result (round (/ (- (get-internal-run-time) tempo-inicio) internal-time-units-per-second)) *nos-expandidos* *nos-gerados*))))

			(return-from ilds-best-solution (list out-result (round (/ (- (get-internal-run-time) tempo-inicio) internal-time-units-per-second)) *nos-expandidos* *nos-gerados*)))))


; ; Heurísticas

; Heurística que retorna a quantidade de turnos que contém o estado
(defun heuristic-shifts-quantity (state)
    (let ((auxState state))
      (let((auxShifts (state-shifts auxState)))
        (return (list-length auxShifts))
    ))
)

; Heurística que retorna a quantidade turnos que não iniciam na localização "L1"
(defun heuristic-shifts-notL1 (state)
    (setq counter 0)
    (let ((auxState state))
      (let((auxShifts (state-shifts auxState)))
        (loop for shift in auxShifts do
            (let (start-location (nth 0 (nth 0 shift)))
                (if (not (eql start-location "L1"))
                    (setq counter (+ counter 1))
                )
            )
        )
    )) counter
)

; Heurística que retorna o tempo que sobrou do turno (em minutos)
(defun heuristic-remaining-shifts-time (state)
    (setq total 0)
    (let ((auxState state))
      (let((auxShifts (state-shifts auxState)))
        (loop for shift in auxShifts do
            (setq total (+ total (- 480 (shiftDuration(shift)))))
        )
    )) total
)

; Heurística que retorna o número de serviços inferior a 6h
(defun heuristic-shifts-less-than-6h (state)
    (setq counter 0)
    (let ((auxState state))
      (let((auxShifts (state-shifts auxState)))
        (loop for shift in auxShifts do
            (if (< 360 (shiftDuration(shift)))
                (setq counter (+ counter 1))
             )
        )
    )) counter
)


