; Grupo 18
; Rafael Koener ist176475
; Kevin Batista Corrales, ist194131
;(in-package :user)

; Estrutura de dados do state
(defstruct state
  nShifts
  shifts
  unusedTasks
  )

; Cria o state inicial
(defun makeInitialState (tasks)
  (let ((ini (make-state :nShifts 0 :shifts '() :unusedTasks tasks)))
    ini
    )
  )

; Função que verifica se o state atingiu o objetivo
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
  (if (listp (nth 0 shift))
    (nth 3 (nth (- (list-length shift) 1) shift))
    (nth 3  shift))
  )

(defun shiftDuration (shift)
  (let ((duration 0))
  (if (listp (nth 1 shift))
    (if (not (equal (first (first shift)) (car '(L1))))
      (setf duration (+ 40 (- (nth 3 (nth (- (list-length shift) 1) shift)) (nth 2 (first shift)))))
      (setf duration (- (nth 3 (nth (- (list-length shift) 1) shift)) (nth 2 (first shift)))))
    (if (not (equal (first shift) (car '(L1))))
      (setf duration (+ 40 (- (nth 3 shift) (nth 2 shift))))
      (setf duration (- (nth 3 shift) (nth 2 shift))))
    )


  (if (listp (nth 1 shift))
    (if (not (equal (nth 1 (nth (- (list-length shift) 1) shift)) (car '(L1))))
      (setf duration (+ 40 (- (nth 3 (nth (- (list-length shift) 1) shift)) (nth 2 (first shift)))))
      (setf duration (- (nth 3 (nth (- (list-length shift) 1) shift)) (nth 2 (first shift)))))
    (if (not (equal (nth 1 shift) (car '(L1))))
      (setf duration (+ 40 (- (nth 3 shift) (nth 2 shift))))
      (setf duration (- (nth 3 shift) (nth 2 shift))))
    )


      ; (if (< duration 360) (setf duration 360))
    duration)
)

(defun shiftDurationReal (shift)
  (let ((duration 0))
  (if (not (equal (first (first shift)) (car '(L1))))
      (setf duration (+ 40 (- (nth 3 (nth (- (list-length shift) 1) shift)) (nth 2 (first shift)))))
      (setf duration (- (nth 3 (nth (- (list-length shift) 1) shift)) (nth 2 (first shift)))))
  (if (not (equal (nth 1 (nth (- (list-length shift) 1) shift)) (car '(L1))))
      (setf duration (+ 40 (- (nth 3 (nth (- (list-length shift) 1) shift)) (nth 2 (first shift)))))
      (setf duration (- (nth 3 (nth (- (list-length shift) 1) shift)) (nth 2 (first shift))))
	 )

	(if (not (equal (nth 3 (car (last shift))) (car '(L1))))
		(setf duration (+ duration 40 ))
	)
	duration
	)
)

(defun drivingTime (shift)
	(let ((duration 0))
	(loop for task in shift do
		(setf duration (+ duration (- (nth 3 task) (nth 2 task) ) ) )
	)
	duration
	)
)

(defun drivingTimeAfterLunch (shift)
	(let ((duration 0)
			(pause 0))
	(loop for task in shift do
		(if (> (- (nth 3 task) (nth 2 task) ) 40)
			(setf pause 1)
		)
		(if (= pause 1)
			(setf duration (+ duration (- (nth 3 task) (nth 2 task) ) ) )
		)
	)
	(if (not (equal (nth 3 (car (last shift))) (car '(L1))))
		(setf duration (+ duration 40 ))
	)
	duration
	)
)

; Função que verifica se uma tarefa com localidade não contínua é possível ser adicionada a um turno
(defun check-time-continuity-different (shift task)
	(if (not(equal nil task))
		(if (listp (nth 0 shift))
		  (let* ((waiting-time (- (nth 2 task) (nth 3 (nth (- (list-length shift) 1) shift))))
				  (actual-shift-time (shiftDurationReal shift))
				  (task-time (- (nth 3 task) (nth 2 task))))
				  (and (> waiting-time 40)
					(or (and (> (+ actual-shift-time waiting-time) 240)
					   (> waiting-time 80)
					   (< (drivingTimeAfterLunch shift) 240))
						   (< (- actual-shift-time (+ waiting-time task-time)) 240)
						  ; (> actual-shift-time 320)
						   )
				  )
			   )
		   (let* ((waiting-time (- (nth 2 task) (nth 3 shift)))
				   (actual-shift-time (shiftDurationReal (list shift)))
				   (task-time (- (nth 3 task) (nth 2 task))))
				   (and (> waiting-time 40)
						(or (and (> (+ actual-shift-time waiting-time) 240)
							 (> waiting-time 80)
							 (< (drivingTimeAfterLunch (list shift)) 240))
							(< (- actual-shift-time (+ waiting-time task-time)) 240)
							;(> actual-shift-time 320)
							)
				   )
				)
		  )
		  (not t)
	  )
)

; Função que verifica se uma tarefa com localidade contínua é possível ser adicionada a um turno
(defun check-time-continuity-equal (shift task)
	(if (not(equal nil task))
		(if (listp (nth 0 shift))
		  (let* ((waiting-time (- (nth 2 task) (nth 3 (nth (- (list-length shift) 1) shift))))
				  (actual-shift-time (shiftDurationReal shift))
				  (task-time (- (nth 3 task) (nth 2 task))))
				  (and (> waiting-time 0)
					 (or (and (> (+ (+ actual-shift-time waiting-time) task-time) 240)
						   (> waiting-time 40)
							(< (drivingTimeAfterLunch shift) 240))
						   (< (+ actual-shift-time (+ waiting-time task-time)) 240)
						 ;  (> actual-shift-time 280)
						 )
				  )
			   )
		   (let* ((waiting-time (- (nth 2 task) (nth 3 shift)))
				   (actual-shift-time (shiftDurationReal (list shift)))
				   (task-time (- (nth 3 task) (nth 2 task))))
				   (and (> waiting-time 0)
						(or (and (> (+ (+ actual-shift-time waiting-time) task-time) 240)
							(> waiting-time 40)
							(< (drivingTimeAfterLunch (list shift)) 240))
							(< (+ actual-shift-time (+ waiting-time task-time)) 240)
						;	(> actual-shift-time 280)
							)
				   )
				)
		  )
		  (not t)
	)
)


(defun checkTask (shift task)
  (let ((testShift shift))
        (setf testShift (append testShift (list task)))
        (< (shiftDuration testShift) 480)
    )
  )

; Função operadores
(defun operator (state)
	(let ((match 0) auxState states)
		(cond
			((equal nil (state-shifts state))
				(setq auxState (addShift state)
				states (cons auxState states)))

			((not (equal nil (state-shifts state)))
			(loop for shift in (state-shifts state)
					when (and (< match 1) (not (equal nil (state-unusedTasks state))))
				do
					(if (and (> (nth 2 (first (state-unusedTasks state))) (lastTime shift))
						(checkTask shift (first (state-unusedTasks state))))
							(if
         (and
                        (equal (first (first (state-unusedTasks state))) (lastPlace shift))
                       (check-time-continuity-equal shift (first (state-unusedtasks state)))
                )
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

(defun operatorAlt (state)
	(let ((match 0) auxState states)
		(cond
			((equal nil (state-shifts state))
				(setq auxState (addShift state)
				states (cons auxState states)))

			((not (equal nil (state-shifts state)))
			(loop for shift in (state-shifts state)
				do
					(if (and (> (nth 2 (first (state-unusedTasks state))) (lastTime shift))
						       (checkTask shift (first (state-unusedTasks state)))
							     (equal (first (first (state-unusedTasks state))) (lastPlace shift))
								 (check-time-continuity-equal shift (first (state-unusedTasks state)))
                   (< match 1))
									(setq auxState (addTask state (position shift (state-shifts state) :test #'equal))
										states (cons auxState states)
										match (+ match 1))
									)
							)
					)
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
        (if (< (shiftDuration shift) 360)
          (setf custoTotal (+ custoTotal 360))
          (setf custoTotal (+ custoTotal (shiftDuration shift))))

          )
  custoTotal)
)

(defun compare-3rd (a b)
  (< (nth 3 a) (nth 3 b)))

; Função que executa a solução do problem
(defun faz-afectacao (tasks strategy)

  (let ((test t)
        sol
        solShifts
        (tasks (sort tasks 'compare-3rd))
        (problem (cria-problema (makeInitialState tasks)
                                          (list #'operator)
                                          :objectivo? #'objective?
                                          :heuristica #'heuristic-shifts-quantity
                                          :custo #'cost
                                          ))
        )

  (cond ((equal strategy "ILDS") (setf sol (ilds problem 1000)
                                  test nil))

        ((equal strategy "sondagem.iterativa") (setf sol (sondagem-iterativa problem)
                                        test nil))

        ((equal strategy "a*.melhor.heuristica") (setf sol (procura  (cria-problema (makeInitialState tasks)
                                          (list #'operator)
                                          :objectivo? #'objective?
                                          :heuristica #'heuristic-shifts-quantity
                                          :custo #'cost
                                          ) "a*" :espaco-em-arvore? T)
                                                                          test nil))

        ((equal strategy "a*.melhor.heuristica.alternativa") (setf sol (procura  (cria-problema (makeInitialState tasks)
                                          (list #'operator)
                                          :objectivo? #'objective?
                                          :heuristica #'heuristic-shifts-notL1
                                          :custo #'cost
                                          ) "a*" :espaco-em-arvore? T)
                                                                          test nil))

        ((equal strategy "melhor.abordagem") (setf sol (procura problem "a*" :espaco-em-arvore? T)
                                        test nil))

        ((equal strategy "abordagem.alternativa") (setf sol (procura (cria-problema (makeInitialState tasks)
                                          (list #'operatorAlt)
                                          :objectivo? #'objective?
                                          :heuristica #'heuristic-shifts-quantity
                                          :custo #'cost
                                          ) "profundidade" :espaco-em-arvore? T)
                                                                          test nil))



    )
    (if test (setf sol (procura problem strategy :espaco-em-arvore? T)))


    (setf solShifts (state-shifts (nth (- (list-length (nth 0 sol)) 1) (nth 0 sol))))
    ; (print solShifts)
	)
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

; Heurística que retorna a quantidade de turnos que contém o state
(defun heuristic-shifts-quantity (state)
  (values (state-nShifts state)))


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
    (values counter))
)

; Heurística que retorna o tempo que sobrou do turno (em minutos)
(defun heuristic-remaining-shifts-time (state)
    (let ((auxState state)
          (total 0))
      (let((auxShifts (state-shifts auxState)))
        (loop for shift in auxShifts do
          (if (listp (nth 1 shift))
            (setq total (+ total (- 480 (shiftDuration shift))))
            (setq total (+ total (- 480 (shiftDuration (list shift))))))

        )
    )(values total))
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
   (values counter)))


; ; Algoritmo
; Algoritmo de Sondagem Iterativa
(defun sondagem-iterativa (problema)
  (let* ((*nos-gerados* 0)
		 (*nos-expandidos* 0)
		 (tempo-inicio (get-internal-run-time))
		 (objectivo? (problema-objectivo? problema))
		 (result nil))
    (labels ((beam (state)
              (cond ((funcall objectivo? state) (list state))
                     ((null state) nil)
                    (t
                     (let* ((sucessores (problema-gera-sucessores problema state))
                            (num-elem (length sucessores)))
                       (if(equal num-elem 0)
                           nil
                         (beam (nth (random num-elem) sucessores))))))))
             (loop while (null result) do
               (setf result (beam (problema-estado-inicial problema))))

             (return-from sondagem-iterativa (list result (round (/ (- (get-internal-run-time) tempo-inicio)internal-time-units-per-second)) *nos-expandidos* *nos-gerados*)))))

; Algoritmo de ILDS

(defun ilds-sorter (heuristic)
 #'(lambda (all-states)
	(stable-sort all-states #'< :key heuristic)))

(defun ilds (problema profundidade-maxima)
 (let ((*nos-gerados* 0)
		(*nos-expandidos* 0)
		(tempo-inicio (get-internal-run-time))
		(max-runtime 300)
		(objectivo? (problema-objectivo? problema))
       (numMaxDiscrepancia profundidade-maxima)
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
				(setf out-result (ildsProbe (problema-estado-inicial problema) maxDiscrepancia profundidade-maxima tempo-inicio))
				(when (not (null out-result))
					(return-from ilds (list out-result (round (/ (- (get-internal-run-time) tempo-inicio) internal-time-units-per-second)) *nos-expandidos* *nos-gerados*)))))))
