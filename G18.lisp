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
        ;(setf (shift-mealBrake ini) (hadMealBreak ini))
  ini))

(defun updateShift (shift)
  (let ((aux shift))
    (setf (shift-duration aux) (shiftDuration aux))
    (setf (shift-lastPoint aux) (nth 1 (nth (- (list-length (shift-tasks aux)) 1) (shift-tasks aux))))
  aux)
  )

(defun addTask (shift task)

  (cond ((equal nil shift) (makeShift (list task)))
        ((not (equal nil shift)) (let ((aux (first shift)))
                                  (setf (shift-tasks aux) (append (shift-tasks aux) (list task)))
                                  (updateShift aux))))
  )

(defun addShift (state task)
  (let ((aux state))
        (setf (state-shifts aux) (append (state-shifts aux) (list (makeShift (list task)))))
  aux)
  )

(defun makeInitialState (tasks)
  (let ((ini (make-state :shifts '() :unusedTasks tasks)))
  ini))

; (defun hadMealBreak (shift)
;   (loop for task in (shift-tasks shift)
;         do(
;             if (and (equal (nth 0 task) (nth 1 task)) (equal 40 (- (nth 3 task) (nth 2 task))))
;                 (and (equal (nth 0 task) (nth 1 task)) (equal 40 (- (nth 3 task) (nth 2 task)))))
;         )
;    )

(defun operator (state)
  (let ((auxState state)
        states)
    (let ((auxTasks (state-unusedTasks state))
          auxShifts)
      (setf auxShifts (addTask (state-shifts auxState) (first auxTasks)))
      ; (print auxShifts)
      (setf auxTasks (rest auxTasks))
      (push (make-state :shifts (list auxShifts)
				  :unusedTasks auxTasks)
		  states)
      )

   (values states))
)


(defun objective? (state)
  (equal nil (state-unusedTasks state)))


(defun faz-afectacao (tasks strategy)

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

; (load(compile-file "G18.lisp"))
(setf tt '((L2 L1 1 25) (L1 L2 34 60) (L5 L1 408 447) (L1 L1 448 551)(L1 L1 474 565)))
(setf s1 (makeInitialState tt))
; (print s1)
; ; (print s1)
; (setf s2 (operator s1))
; (print s2)
; (print (operator (first s2)))
; (print (procura (cria-problema s1 (list #'operator) :objectivo? #'objective? :estado= #'equal) "largura"))

;(objective? (makeInitialState '()))
; (setf t1 '(L1 L2 3 4))
; (setf t2 '(L3 L4 78 90))
; (setf t3 '((L2 L1 1 25) (L1 L2 34 60)))
; (setf s1 (makeInitialState t3))
; ; (print s1)
; ; (setf s2 (addShift s1 t2))
; ; (print s2)
; ; (setf s3 (makeShift t3))
; ; (print s3)
; (operator s1)
; -----------------------------------------------------------------------------

; (defun sondagem-iterativa (problema)
;   (let* ((*nos-gerados* 0)
; 		 (*nos-expandidos* 0)
; 		 (tempo-inicio (get-internal-run-time))
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
;              (return-from sondagem-iterativa (list result (round (/ (- (get-internal-run-time) tempo-inicio) internal-time-units-per-second)) *nos-expandidos* *nos-gerados*)))))

; (defun ilds (problema maxDepth)
;   (let ((*nos-gerados* 0)
; 		(*nos-expandidos* 0)
; 		(tempo-inicio (get-internal-run-time))
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
; 					(return-from ilds (list out-result (round (/ (- (get-internal-run-time) tempo-inicio) internal-time-units-per-second)) *nos-expandidos* *nos-gerados*)))))))
;
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
; (procura-prof (problema-estado-inicial problema) nil 0))))
;
; ; ; heuristicas
;
; ; Heurística que retorna a quantidade de turnos que contém o estado
; (defun heuristic-shifts-quantity (state)
;     (let ((auxState state))
;       (let((auxShifts (state-shifts auxState)))
;         (return (list-length auxShifts))
;     ))
; )
;
; ; Heurística que retorna a quantidade turnos que não iniciam na localização "L1"
; (defun heuristic-shifts-notL1 (state)
;     (setq counter 0)
;     (let ((auxState state))
;       (let((auxShifts (state-shifts auxState)))
;         (loop for shift in auxShifts do
;             (let (start-location (nth 0 (nth 0 shift)))
;                 (if (not (eql start-location "L1"))
;                     (setq counter (+ counter 1))
;                 )
;             )
;         )
;     )) counter
; )

;  (setf problem1 '((L2 L1 1 25) (L10 L1 4 33) (L1 L2 14 40) (L1 L11 14 55) (L4 L1 16 37) (L2 L1 21 45) (L1 L10 26 55) (L1 L9 28 72) (L1 L2 34 60) (L1 L10 46 75) (L10 L1 364 393) (L1 L1 394 465) (L5 L1 408 447) (L1 L1 448 551) (L1 L1 474 565) (L1 L1 512 607) (L1 L1 552 647) (L1 L1 566 633) (L1 L1 594 665) (L1 L1 608 711) (L1 L1 666 733) (L1 L1 684 735) (L1 L1 698 761) (L1 L1 722 777) (L1 L1 736 823) (L1 L1 752 847) (L1 L1 762 817) (L1 L1 776 863) (L1 L1 778 841) (L1 L1 802 857) (L1 L1 808 911) (L1 L1 814 905) (L1 L1 848 951) (L1 L1 856 943) (L1 L1 864 915) (L1 L1 866 933) (L1 L1 886 973) (L1 L1 906 953) (L1 L1 906 973) (L1 L1 934 1005) (L1 L1 944 995) (L1 L1 946 1013) (L1 L1 954 1045) (L1 L1 974 1045) (L1 L1 994 1085) (L1 L1 996 1083) (L1 L1 1018 1081) (L1 L1 1046 1113) (L1 L1 1058 1121) (L1 L1 1078 1141) (L1 L1 1082 1137) (L1 L1 1084 1135) (L1 L1 1086 1153) (L1 L1 1114 1185) (L1 L1 1122 1177) (L1 L1 1144 1195) (L1 L1 1148 1251) (L1 L1 1152 1247) (L1 L1 1168 1271) (L1 L1 1178 1241) (L1 L1 1194 1265) (L1 L1 1196 1283) (L1 L1 1224 1275) (L1 L1 1232 1327) (L1 L1 1234 1325) (L1 L1 1248 1351) (L1 L1 1272 1367) (L1 L1 1276 1363) (L1 L1 1284 1335) (L1 L1 1286 1353) (L1 L1 1318 1381) (L1 L1 1328 1431) (L1 L1 1336 1423) (L1 L1 1358 1421) (L1 L1 1382 1437) (L1 L1 1386 1453) (L1 L1 1386 1453) (L1 L2 1394 1420) (L1 L10 1406 1435) (L1 L2 1414 1440) (L1 L4 1422 1444) (L1 L5 1432 1472)))
;
; (print (faz-afectacao problem1 "profundidade" ))
