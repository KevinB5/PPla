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


; (load(compile-file "G018.lisp"))
(faz-afectacao problem3 "profundidade")
