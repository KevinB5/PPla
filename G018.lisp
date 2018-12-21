; Grupo 18
; Rafael Koener ist176475
; Kevin Batista Corrales, ist194131
;(in-package :user)


; Estrutura de dados do estado
(defstruct state
  nShifts
  shifts
  unusedTasks
  )

; Cria o estado inicial
(defun makeInitialState (tasks)
  (let ((ini (make-state :nShifts 0 :shifts '() :unusedTasks tasks)))
    ini
    )
  )

; Função que verifica se o estado atingiu o objetivo
(defun objective? (state)
  (equal nil (state-unusedTasks state))
  )

(defun addShift (state)
  (let ((auxShifts (state-shifts state))
        (auxTasks (state-unusedTasks state)))
    (cond ((and (not (equal nil auxShifts)) (> (list-length auxShifts) 1)) (setf auxShifts (append auxShifts (list (list (first auxTasks))))
                                                                                 auxTasks (rest auxTasks)))
          ((and (not (equal nil auxShifts)) (equal (list-length auxShifts) 1)) (setf auxShifts (cons auxShifts (list (list (first auxTasks))))
                                                                     auxTasks (rest auxTasks)))
          ((equal nil auxShifts) (setf auxShifts (list (first auxTasks))
                                       auxTasks (rest auxTasks)))
      )
    (make-state :nShifts (+ (state-nShifts state) 1) :shifts auxShifts :unusedTasks auxTasks))
  )

(defun addNthList (task index shifts)
  (let ((auxShifts shifts))
    (cond
      ((zerop index)
       (cons (append (nth index auxShifts)  task) (rest auxShifts)))

      ((and (> index 0)(< index (list-length auxShifts)))
       (nreconc (reverse (subseq auxShifts 0 index))
                (cons (append (nth index auxShifts) task) (last auxShifts (- (list-length auxShifts) (+ index 1))))))

      ((equal index (- (list-length auxShifts) 1))
       (nreconc (reverse (subseq auxShifts 0 index))
                (list (append (nth index auxShifts) task))))

      )
    )
  )

(defun addTask (state n)
  (let ((auxShifts (state-shifts state))
        (auxTasks (state-unusedTasks state)))
    (cond
      ((not (equal (list-length auxShifts) 1)) (setf auxShifts (addNthList (list (first auxTasks)) n auxShifts)
                                                      auxTasks (rest auxTasks)))

          ((equal (list-length auxShifts) 1) (setf auxShifts (append auxShifts  (list (first auxTasks)))
                                                   auxTasks (rest auxTasks)))
      )
    (make-state :nShifts (state-nShifts state) :shifts auxShifts :unusedTasks auxTasks))
  )

(defun lastPlace (shift)
  (if (listp (nth 2 shift))
    (nth 1 (nth (- (list-length shift) 1) shift))
    (nth 1  shift))
  )

(defun lastTime (shift)
  (if (listp (nth 2 shift))
    (nth 3 (nth (- (list-length shift) 1) shift))
    (nth 3  shift))
  )

(defun shiftDuration (shift)
  (let ((duration 0))
  (if (not (listp (nth 1 shift)))
    (setf duration (- (nth 3 (first shift)) (nth 2 (first shift))))
    (setf duration (- (nth 3 (nth (- (list-length shift) 1) shift)) (nth 2 (first shift))))
    )
    duration))

(defun add? (state)
  (let ((auxState state)
        (match 0))
    (loop for shift in (state-shifts auxState)
      do(
        if (and (equal (first (first (state-unusedTasks auxState))) (lastPlace shift))
                (> (nth 2 (first (state-unusedTasks auxState))) (lastTime shift))
                (equal match 0)
                (< (- (nth 3 (first (state-unusedTasks auxState))) (nth 2 (first shift))) 480))
            (setf auxState (addTask auxState (position shift (state-shifts auxState) :test #'equal))
                  match 1)
         )
      )
    (if (equal match 0) (setf auxState (addShift auxState)))
    auxState)
 )

; Função que verifica se uma tarefa com localidade não contínua é possível ser adicionada a um turno
(defun check-time-continuity-different (shift task)
    (if (listp (nth 0 shift))
      (let* ((waiting-time (- (nth 2 task) (nth 3 (nth (- (list-length shift) 1) shift))))
              (actual-shift-time (shiftDuration shift))
              (task-time (- (nth 3 task) (nth 2 task))))
              (and (> waiting-time 40)
                (or (and (> (+ actual-shift-time waiting-time) 240)
                   (> waiting-time 80))
                       (< (- actual-shift-time (+ waiting-time task-time)) 240)
                       (> actual-shift-time 320))
              )
           )
       (let* ((waiting-time (- (nth 2 task) (nth 3 shift)))
               (actual-shift-time (shiftDuration (list shift)))
               (task-time (- (nth 3 task) (nth 2 task))))
               (and (> waiting-time 40)
                    (or (and (> (+ actual-shift-time waiting-time) 240)
                         (> waiting-time 80))
                        (< (- actual-shift-time (+ waiting-time task-time)) 240)
                        (> actual-shift-time 320))
               )
            )
      )
)

; Função que verifica se uma tarefa com localidade contínua é possível ser adicionada a um turno
(defun check-time-continuity-equal (shift task)
    (if (listp (nth 0 shift))
      (let* ((waiting-time (- (nth 2 task) (nth 3 (nth (- (list-length shift) 1) shift))))
              (actual-shift-time (shiftDuration shift))
              (task-time (- (nth 3 task) (nth 2 task))))
              (and (> waiting-time 40)
                (or (> (+ actual-shift-time waiting-time) 240)
                       (< (- actual-shift-time (+ waiting-time task-time)) 240)
                       (> actual-shift-time 280))
              )
           )
       (let* ((waiting-time (- (nth 2 task) (nth 3 shift)))
               (actual-shift-time (shiftDuration (list shift)))
               (task-time (- (nth 3 task) (nth 2 task))))
               (and (> waiting-time 40)
                    (or (> (+ actual-shift-time waiting-time) 240)
                        (< (- actual-shift-time (+ waiting-time task-time)) 240)
                        (> actual-shift-time 280))
               )
            )
      )
)

; Função operadores
(defun operator-old (state)
  (let ((match 0)
        auxState
        states)
    (cond
      ((equal nil (state-shifts state)) (setq auxState (addShift state)
                                              states (cons auxState states)))

      ((not (equal nil (state-shifts state)))
      (loop for shift in (state-shifts state)
        do( if (and (equal (first (first (state-unusedTasks state))) (lastPlace shift))
                  (> (nth 2 (first (state-unusedTasks state))) (lastTime shift))
                  (< (- (nth 3 (first (state-unusedTasks state))) (nth 2 (first shift))) 480))
                (setq auxState (addTask state (position shift (state-shifts state) :test #'equal))
                      states (cons auxState states)
                      match (+ match 1))

                ; (if (check-time-continuity shift (first (state-unusedTasks state)))
                ;       (setq auxState (addTask state (position shift (state-shifts state) :test #'equal))
                ;             states (cons auxState states)
                ;             match (+ match 1)))
          )
        )
      )
      )
    (if (equal match 0) (setq auxState (addShift state)
                              states (cons auxState states)))
    (values states))
)

(defun operator (state)
	(let ((match 0) auxState states)
		(cond 
			((equal nil (state-shifts state)) 
				(setq auxState (addShift state)
				states (cons auxState states)))
			
			((not (equal nil (state-shifts state)))
			(loop for shift in (state-shifts state)
				do   
					(if (and (> (nth 2 (first (state-unusedTasks state))) (lastTime shift))
						(< (- (nth 3 (first (state-unusedTasks state))) (nth 2 (first shift))) 480))
							(if (and (equal (first (first (state-unusedTasks state))) (lastPlace shift))
								(check-time-continuity-equal shift (first (state-unusedTasks state))))
									(setq auxState (addTask state (position shift (state-shifts state) :test #'equal))
										states (cons auxState states)
										match (+ match 1))
									(if (check-time-continuity-different shift (first (state-unusedtasks state)))
										(setq auxstate (addtask state (position shift (state-shifts state) :test #'equal))
											states (cons auxstate states)
											match (+ match 1))
									)
							)
					)
				
			))
		)
	(if (equal match 0) 
		(setq auxState (addShift state)
            states (cons auxState states))
	)
    (values states)
	)
)

(defun cost (state)
  (let ((custoTotal 0))
    (loop for shift in (state-shifts state) do
      (if (listp (nth 1 shift))
        (setf custoTotal (+ custoTotal (shiftDuration shift)))
        (setf custoTotal (+ custoTotal (shiftDuration (list shift)))))
    )
  custoTotal)
)

(defun compare-3rd (a b)
  (< (nth 3 a) (nth 3 b)))

; Função que executa a solução do problema
(defun faz-afectacao (tasks strategy)

  ;(sort tasks 'compare-3rd)

  ; (print (sondagem-iterativa (cria-problema (makeInitialState tasks)
  ;                                   (list #'operator)
  ;                                   :objectivo? #'objective?
  ;                                   :custo #'cost)
  ;                  ))
  (print (procura (cria-problema (makeInitialState tasks)
                                    (list #'operator)
                                    :objectivo? #'objective?
                                    :heuristica #'heuristic-shifts-quantity
                                    :custo #'cost
                                    ) strategy))
  ; (setf s1 (makeInitialState tasks))
  ; (setf s2 (addShift s1))
  ; (print "s2")
  ; (print s2)
  ; (setf s3 (addShift s2))
  ; (print "s3")
  ; (print s3)
  ; (setf s4 (addShift s3))
  ; (print "s4")
  ; (print s4)
  ; (setf auxShifts (state-shifts s4))
  ; (setf s5 (operator s4))
  ; (print "s5")
  ; (print s5)
  ; (setf s5 (add? s4))
  ; (print "s5")
  ; (print s5)
  ; (setf s6 (add? s5))
  ; (print "s6")
  ; (print s6)
  ; (setf s7 (add? s6))
  ; (print "s7")
  ; (print s7)
  ; (setf s8 (add? s7))
  ; (print "s8")
  ; (print s8)
  ; (loop for shift in (state-shifts s8 )
  ;   do(print (shiftDuration shift))
  ; )
  ; (print (heuristic-remaining-shifts-time s8))
  ; (print "operator")
  ; (print (operator s8))
)

; Updates the duration of the shift to 6h if it is less than 6h
(defun less-than-6h (shift)
    (let ((duration (shiftDuration shift)))
            (if (< duration 360)
                360
            )
    )
)

;;;;;;;;;;; Heurísticas ;;;;;;;;;;;;;;;;;;;;;

; Heurística que retorna a quantidade de turnos que contém o estado
(defun heuristic-shifts-quantity (state)
  (state-nShifts state))


; Heurística que retorna a quantidade turnos que não iniciam na localização "L1"
(defun heuristic-shifts-notL1 (state)
    (let ((counter 0))
      (loop for shift in (state-shifts state) do
          (if (listp (nth 1 shift))
            (if (not (eql (first (first shift)) (car '(L1))))
              (setq counter (+ counter 1))
             )
            (if (not (eql (first shift) (car '(L1))))
              (setq counter (+ counter 1))
            ))
          )
    counter)
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
   (let ((counter 0))
     (loop for shift in (state-shifts state) do
       (if (listp (nth 1 shift))
         (if (> 360 (shiftDuration shift))
           (setq counter (+ counter 1))
          )
        (if (> 360 (shiftDuration (list shift)))
          (setq counter (+ counter 1))
         ))
       )
   counter))


; ; Algoritmo
; Algoritmo de Sondagem Iterativa
(defun sondagem-iterativa (problema)
  (let* ((*nos-gerados* 0)
		 (*nos-expandidos* 0)
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

             (return-from sondagem-iterativa (list result *nos-expandidos* *nos-gerados*)))))

; Algoritmo de ILDS

(defun ilds-sorter (heur)
 "Return a combiner function that sorts according to heuristic."
 #'(lambda (all-states)
	(stable-sort all-states #'< :key heur)))

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


; (load(compile-file "G018.lisp"))
(load(compile-file "problems.lisp"))
(faz-afectacao problem2 "largura")
