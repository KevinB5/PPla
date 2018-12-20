; Grupo 18
; Rafael Koener ist176475
; Kevin Batista Corrales, ist194131
;(in-package :user)


; Estrutura de dados do estado
(defstruct state
  shifts
  unusedTasks
  )

; Cria o estado inicial
(defun makeInitialState (tasks)
  (let ((ini (make-state :shifts '() :unusedTasks tasks)))
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
    (make-state :shifts auxShifts :unusedTasks auxTasks))
  )


(defun addTask (state n)
  (let ((auxShifts (state-shifts state))
        (auxTasks (state-unusedTasks state)))
    (cond ((not (equal (list-length auxShifts) 1)) (setf (nth n auxShifts) (append (nth n auxShifts)  (list (first auxTasks)))
                                                                     auxTasks (rest auxTasks)))
          ((equal (list-length auxShifts) 1) (setf auxShifts (append auxShifts  (list (first auxTasks)))
                                                                          auxTasks (rest auxTasks)))
      )
    (make-state :shifts auxShifts :unusedTasks auxTasks))
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
  (if (equal (list-length shift) 1)
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

; Função que executa a solução do problema
(defun faz-afectacao (tasks strategy)

  (print (procura (cria-problema (makeInitialState tasks) (list #'operator) :objectivo? #'objective?) strategy))
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
  ; (print (nth 3 (first (state-unusedTasks s4))))
  ; (print (lastTime (first (state-shifts s4))))
  ; (print (> (nth 3 (first (state-unusedTasks s4))) (lastTime (first (state-shifts s4)))))
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
  ; (loop for shift in (state-shifts s6 )
  ;   do(print (lastTime shift))
  ;   )
  )

  ; Updates the duration of the shift to 6h if it is less than 6h
(defun less-than-6h (shift)
    (setq duration (shiftDuration shift))
    (let ((aux shift))
            (if (< duration 360)
                (setq duration 360)
            )
    )
    duration
)

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
              (let ((start-location (nth 0(nth 0 shift))))
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

; (load(compile-file "G018.lisp"))
(setf problem1 '((L2 L1 1 25) (L10 L1 4 33) (L1 L2 14 40) (L1 L11 14 55) (L4 L1 16 37) (L2 L1 21 45) (L1 L10 26 55) (L1 L9 28 72) (L1 L2 34 60) (L1 L10 46 75) (L10 L1 364 393) (L1 L1 394 465) (L5 L1 408 447) (L1 L1 448 551) (L1 L1 474 565) (L1 L1 512 607) (L1 L1 552 647) (L1 L1 566 633) (L1 L1 594 665) (L1 L1 608 711) (L1 L1 666 733) (L1 L1 684 735) (L1 L1 698 761) (L1 L1 722 777) (L1 L1 736 823) (L1 L1 752 847) (L1 L1 762 817) (L1 L1 776 863) (L1 L1 778 841) (L1 L1 802 857) (L1 L1 808 911) (L1 L1 814 905) (L1 L1 848 951) (L1 L1 856 943) (L1 L1 864 915) (L1 L1 866 933) (L1 L1 886 973) (L1 L1 906 953) (L1 L1 906 973) (L1 L1 934 1005) (L1 L1 944 995) (L1 L1 946 1013) (L1 L1 954 1045) (L1 L1 974 1045) (L1 L1 994 1085) (L1 L1 996 1083) (L1 L1 1018 1081) (L1 L1 1046 1113) (L1 L1 1058 1121) (L1 L1 1078 1141) (L1 L1 1082 1137) (L1 L1 1084 1135) (L1 L1 1086 1153) (L1 L1 1114 1185) (L1 L1 1122 1177) (L1 L1 1144 1195) (L1 L1 1148 1251) (L1 L1 1152 1247) (L1 L1 1168 1271) (L1 L1 1178 1241) (L1 L1 1194 1265) (L1 L1 1196 1283) (L1 L1 1224 1275) (L1 L1 1232 1327) (L1 L1 1234 1325) (L1 L1 1248 1351) (L1 L1 1272 1367) (L1 L1 1276 1363) (L1 L1 1284 1335) (L1 L1 1286 1353) (L1 L1 1318 1381) (L1 L1 1328 1431) (L1 L1 1336 1423) (L1 L1 1358 1421) (L1 L1 1382 1437) (L1 L1 1386 1453) (L1 L1 1386 1453) (L1 L2 1394 1420) (L1 L10 1406 1435) (L1 L2 1414 1440) (L1 L4 1422 1444) (L1 L5 1432 1472)))

(faz-afectacao problem1 "profundidade" )
