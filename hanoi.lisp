;;
;; Hanoi Towers (@author: krpec), made for fun
;;

(defclass hanoi-towers ()
  ((tower1 :initform '())
   (tower2 :initform '())
   (tower3 :initform '())
   (disc-count :initform 0)))

(defmethod tower1 ((hanoi-towers hanoi-towers))
  (slot-value hanoi-towers 'tower1))

(defmethod set-tower1 ((h hanoi-towers) list)
  (unless (typep list 'list)
    (error "tower1 value should be a list."))
  (setf (slot-value h 'tower1) list)
  h)

(defmethod tower2 ((hanoi-towers hanoi-towers))
  (slot-value hanoi-towers 'tower2))

(defmethod set-tower2 ((h hanoi-towers) list)
  (unless (typep list 'list)
    (error "tower2 value should be a list."))
  (setf (slot-value h 'tower2) list)
  h)

(defmethod tower3 ((hanoi-towers hanoi-towers))
  (slot-value hanoi-towers 'tower3))

(defmethod set-tower3 ((h hanoi-towers) list)
  (unless (typep list 'list)
    (error "tower3 value should be a list."))
  (setf (slot-value h 'tower3) list)
  h)

(defmethod get-tower ((h hanoi-towers) tower)
  (cond ((= tower 1) (tower1 h))
        ((= tower 2) (tower2 h))
        ((= tower 3) (tower3 h))))

(defmethod set-tower ((h hanoi-towers) tower list)
  (cond ((= tower 1) (set-tower1 h list))
        ((= tower 2) (set-tower2 h list))
        ((= tower 3) (set-tower3 h list))))

(defmethod disc-count ((h hanoi-towers))
  (slot-value h 'disc-count))

(defmethod set-disc-count ((h hanoi-towers) count)
  (unless (typep count 'number)
    (error "Disc count should be a number."))
  (if (< count 3)
      (error "Minimal disc count should be 3.")
    (setf (slot-value h 'disc-count) count))
    h)

;Prepares game "board" with chosen "difficulty" - number of discs
(defmethod prepare-game ((h hanoi-towers) discs)
  (set-disc-count h discs)
  (let ((tower '()))
    (dotimes (x discs)
      (setf tower (append tower
                          (list (+ x 1)))))
    (set-tower1 h tower)
    (set-tower2 h nil)
    (set-tower3 h nil))
  (show-board h))

;prints out game "board"
(defmethod show-board ((h hanoi-towers))
  (let ((t1 (tower1 h))
        (t2 (tower2 h))
        (t3 (tower3 h)))
    (format t 
            "~%[1] : ~s~%[2] : ~s~%[3] : ~s~%"
            (if (null t1)
                "-"
              t1)
            (if (null t2)
                "-"
              t2)
            (if (null t3)
                "-"
              t3)))
    h)

;made moves
(defmethod move ((h hanoi-towers) from to)
  (unless (typep from 'number)
    (error "'From' should be a number of a source tower."))
  (unless (typep to 'number)
    (error "'To' should be a number of a destination tower."))
  (let ((source (get-tower h from))
        (dest (get-tower h to)))
    (if (or (null dest)
            (< (car source) 
               (car dest)))
        (and (set-tower h to (append (list (car source))
                                     dest))
          (set-tower h from (cdr source)))
      (format t "~%Illegal move.")))
  (show-board h)
  (check-state h))

;checks end of the game
(defmethod check-state ((h hanoi-towers))
  (if (and (= (list-length (tower1 h)) 0)
           (or (= (list-length (tower2 h)) 
                  (disc-count h))
               (= (list-length (tower3 h))
                  (disc-count h))))
      (format t "~%Game over, congratulations!~%"))
  h)

#| 
Usage:
(setf g (make-instance 'hanoi-towers))
(prepare-game g 4)
(move g 1 2)
|#