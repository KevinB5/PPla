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
