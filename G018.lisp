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


(defun addTask (state n)
  (let ((auxShifts (state-shifts state))
        (auxTasks (state-unusedTasks state)))
    (cond ((not (equal (list-length auxShifts) 1)) (setf (nth n auxShifts) (append (nth n auxShifts)  (list (first auxTasks)))
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
  (if (not (listp (nth 2 shift)))
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

; Função operadores
(defun operator (state)
  (let ((auxState state)
        states)
    (cond
      ((equal nil (state-shifts state)) (setf auxState (addShift state)))

      ((not (equal nil (state-shifts state))) (setf auxState (add? auxState)))

      )

    (push auxState states)

    (values states))
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

; Função que executa a solução do problema
(defun faz-afectacao (tasks strategy)

  (setf res (procura (cria-problema (makeInitialState tasks)
                                    (list #'operator)
                                    :objectivo? #'objective?
                                    :heuristica #'heuristic-shifts-notL1
                                    :custo #'cost
                                    ) strategy))
  (print res)
  ; (setf s1 (makeInitialState tasks))
  ; (setf s2 (addShift s1))
  ; (print "s2")
  ; (print s2)
  ; (setf s3 (add? s2))
  ; (print "s3")
  ; (print s3)
  ; (setf s4 (add? s3))
  ; (print "s4")
  ; (print s4)
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
)

  ; Updates the duration of the shift to 6h if it is less than 6h
  (defun less-than-6h (shift)
      (let ((aux shift)
            (duration (shiftDuration shift)))
              (if (< duration 360)
                  360
              )
      )
  )

  ; ; Heurísticas

  ; Heurística que retorna a quantidade de turnos que contém o estado
  (defun heuristic-shifts-quantity (state)
      (state-nShifts state)
  )

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

; SMA*

(defun tree-sma (problem &optional (memory-size 20)
			 &aux n
			      (start (create-start-node problem))
			      (q (make-search-tree start (node-f-cost start)))
			      (memory-used 1))

  (loop 
   (when (empty-tree q) (return nil))
   (setq n (deepest-least-leaf q))
   (when (goal-test problem n)
     (return n))
   (when (= (node-f-cost n) infinity) (return nil))
   (let ((s (tree-get-next-successor n q memory-size problem)))
     (when s
       (unless (node-unexpanded n)  ;;; n exhausted, drop from queue
	 (delete-element n q (node-f-cost n)))
       (incf memory-used)
       (insert-element s q (node-f-cost s))
       (when (> memory-used memory-size)
	 (tree-prune-open q)
	 (decf memory-used)))))
  )


;;; tree-get-next-successor returns the next successor of n, if any (else nil)
(defun tree-get-next-successor (n q memory-size problem &aux (next nil))
  (unless (node-expanded? n) 
    (setf (node-unexpanded n)
	  (if  (= (1+ (node-depth n)) memory-size)
	      (list 'done)
	    (nconc (expand n problem) (list 'done))))
    (setf (node-expanded? n) t))
  (unless (eq (car (node-unexpanded n)) 'done)
    (setq next (pop (node-unexpanded n)))
    (push next (node-successors n)))
  (unless (node-completed? n)
    (when (eq (car (node-unexpanded n)) 'done)  ;;; all successors examined 
      (pop (node-unexpanded n))
      (setf (node-completed? n) t)
      (tree-backup-f-cost n q t)))
  next)

;;; tree-backup-f-cost updates the f-cost for a node's ancestors as needed
(defun tree-backup-f-cost (node q &optional (was-open? nil) 
                                  &aux (current (node-f-cost node))
				       (least infinity)) 
  (when (node-completed? node)
    (dolist (s (node-successors node))
      (let ((v (node-f-cost s)))
        (when (< v least) (setq least v))))
    (dolist (s (node-unexpanded node))
      (let ((v (node-f-cost s)))
        (when (< v least) (setq least v))))
    (when (> least current)
      (when (or was-open? (openp node))  ;;; changing f value - re-order
        (delete-element node q current)
        (insert-element node q least))
      (setf (node-f-cost node) least)
      (let ((parent (node-parent node)))
        (when parent (tree-backup-f-cost parent q))))))


(defun tree-prune-open (q &aux (worstnode (shallowest-largest-leaf q))
                               (parent (node-parent worstnode)))
  (delete-element worstnode q (node-f-cost worstnode))
  (setf (node-successors worstnode) nil) ;;;actually free up memory
  (setf (node-expanded? worstnode) nil)

  (unless (node-unexpanded parent)   ;;;parent was closed - need to re-open
    (insert-element parent q (node-f-cost parent)))
  (tree-unexpand-successor worstnode parent))

(defun tree-unexpand-successor (successor parent)  
  (setf (node-unexpanded parent) 
	(nconc (node-unexpanded parent) (list successor)))
  (setf (node-successors parent)
	(delete successor (node-successors parent) :test #'eq)) 
  (when (node-completed? parent)
    (unless (node-successors parent)
      (setf (node-unexpanded parent) nil) ;;; reclaim space
      (setf (node-expanded? parent) nil)
      (setf (node-completed? parent) nil))))




(defun deepest-least-leaf (q)
  (the-biggest #'(lambda (n) (node-depth n)) (search-tree-node-value
					       (leftmost q)))) 

(defun shallowest-largest-leaf (q)
  (the-smallest-that 
    #'(lambda (n) (node-depth n))
    #'leafp
    (search-tree-node-value (rightmost q))))


(defun find-leaf (node &aux (s (node-successors node)))
  (if s (find-leaf (car s))
      node))

(defun leafp (n)
  (null (node-successors n)))

(defun openp (n)
  (or (not (node-expanded? n))
      (node-unexpanded n))


; (load(compile-file "G018.lisp"))
(print (sma (cria-problema (makeInitialState problem1)
                                    (list #'operator)
                                    :objectivo? #'objective?
                                    :heuristica #'heuristic-shifts-notL1
                                    :custo #'cost
                                    ) 100))
