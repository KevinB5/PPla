;(in-package :user)

(defstruct state
  shifts
  unusedTasks
  )

(defun addTask (state)
  (let ((auxState state))
      (let((auxTask (state-unusedTasks auxState))
      (auxShift  (last (state-shifts auxState))))

  (push (list (first auxTask)) auxShift)
  (setf (state-shifts auxState) auxShift)
  (setf (state-unusedTasks auxState) (rest auxTask))
    (print "1")
    (print auxState)
    (list auxState))))

; (defun checkTask (state)
;   (let ((auxState state)
;         (auxShift (nth nshift state-shifts))
;         (lastTask (last auxShift))))
;   if ((first task))
;   )
;
(defun addShift (state)
  (let ((auxState state))
  (push () (state-shifts auxState))
  (print "2")
  (print auxState)
  (list auxState)))



(defun shiftDuration (shift)
  (- (nth 4 (last shift)) (nth 3 (first shift)))
  )

(defun makeState (shifts tasks)
  (let ((ini (make-state :shifts shifts :unusedTasks tasks)))
  ini))

(defun makeInitialState (tasks)
  (let ((ini (make-state :shifts (list ()) :unusedTasks tasks)))
  ini))

(defun objetivo? (state)
  (null (state-unusedTasks state)))


(defun faz-afectacao (tasks strategy)


  (procura (cria-problema (makeInitialState tasks) (list #'addTask #'addShift) :objectivo? #'objetivo?) strategy)
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
