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
  (print (shift-tasks shift))
  (print (nth (- (list-length (shift-tasks shift)) 1) (shift-tasks shift)))
  (- (nth 3  (last (nth (- (list-length (shift-tasks shift)) 1) (shift-tasks shift)))) (nth 2 (first (shift-tasks shift))))
  )

(defun makeShift (tasks)
  (let ((ini (make-shift :tasks '()
                        :duration 0
                        :lastPoint "L"
                        :mealBrake nil)))
        (setf (shift-tasks ini) (list tasks))
        (setf (shift-duration ini) (shiftDuration ini))
        (print 1)
        (setf (shift-lastPoint ini) (nth 1 (last (shift-tasks ini))))
        (print 2)
        ;(setf (shift-mealBrake ini) (hadMealBreak (shift-tasks ini)))
  ini))


(defun makeState (shifts tasks)
  (let ((ini (make-state :shifts shifts :unusedTasks tasks)))
  ini))

(defun makeInitialState (tasks)
  (let ((ini (make-state :shifts (list ()) :unusedTasks tasks)))
  ini))


; (defun hadMealBreak (tasks)
;   ;for all in tasks
;   ;   if endingTime - startingTime = 40
;   ;       true
;   ;   false
;   )

(defun operator (state)
  (let ((auxState state))
      (let((auxTask (state-unusedTasks auxState))
      (auxShift  (last (state-shifts auxState))))

  (push (list (first auxTask)) auxShift)
  (setf (state-shifts auxState) auxShift)
  (setf (state-unusedTasks auxState) (rest auxTask))
    (print "1")
    (print auxState)
    (list auxState))))





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
;(setf t1 '(L1 L2 3 4))
;(setf t3 '((L2 L1 1 25) (L1 L2 34 60)))

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
