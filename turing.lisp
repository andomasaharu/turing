; turing machine

(let ((blank #\_))
  (defclass tape () ((left :accessor tape-left :initarg :left)
					 (middle :accessor tape-middle :initarg :middle)
					 (right :accessor tape-right :initarg :right))
	)
  (defclass tmrule () ((state :accessor tmrule-state :initarg :state)
					   (character :accessor tmrule-character :initarg :character)
					   (next_state :accessor tmrule-next_state :initarg :next_state)
					   (write_character :accessor tmrule-write_character :initarg :write_character)
					   (direction :accessor tmrule-direction :initarg :direction))
	)
  (defclass tmconfiguration () ((state :accessor tmconfiguration-state :initarg :state)
							(tape :accessor tmconfiguration-tape :initarg :tape))
	)
  (defmethod tape-inspect((ta tape))
	(format t "~{~A ~}[~A]~{ ~A~}~%" (reverse (tape-left ta)) (tape-middle ta) (tape-right ta))
	)
  (defun tape-new(l m r)
	(let ((ta (make-instance 'tape :left (if (null l) (cons blank nil) (reverse l)) :middle m :right (if (null r) (cons blank nil) r)))
		  )
	(format t "tape-new ~A [~A] ~A~%" l m r)
	(tape-inspect ta)
	ta)
	)
  (defmethod tape-write((ta tape) c)
	(format t "do tape-write ~A~%" c)
	(setf (tape-middle ta) c)
	(tape-inspect ta)
	)
  (defun move-head-left((ta tape))
	(format t "do move-head-left~%")
	(setf (tape-right ta) (cons (tape-middle ta) (tape-right ta)))
	(setf (tape-middle ta) (if (null (tape-left ta)) blank (car (tape-left ta))))
	(setf (tape-left ta) (if (null (cdr (tape-left ta))) (cons blank nil) (cdr (tape-left ta))))
	(tape-inspect ta)
	)
  (defun move-head-right((ta tape))
	(format t "do move-head-right~%")
	(setf (tape-left ta) (cons (tape-middle ta) (tape-left ta)))
	(setf (tape-middle ta) (if (null (tape-right ta)) blank (car (tape-right ta))))
	(setf (tape-right ta) (if (null (cdr (tape-right ta))) (cons blank nil) (cdr (tape-right ta))))
	(tape-inspect ta)
	)
  (defun set-state(a)
	(setf state a)
	)
  (defmethod tmrule-inspect((rule tmrule))
	(format t "state:~A character:~A next_state:~A write_character:~A direction:~A~%" (tmrule-state rule) (tmrule-character rule) (tmrule-next_state rule) (tmrule-write_character rule) (tmrule-direction rule))
	)
  (defmethod applies_top((rule tmrule) (configuration tmconfiguration))
	(and (eql (tmrule-state rule) (tmconfiguration-state configuration))
		 (eql (tmrule-character rule) (tape-middle (tmconfiguration-tape configuration))))
	)
  )
(defun test()
  (let* (
		(ta (tape-new nil 1 nil))
		(rule (make-instance 'tmrule :state 1 :character 0 :next_state 2 :write_character 1 :direction 'right))
		(configuration (make-instance 'tmconfiguration :state 1 :tape ta))
		)
	(tmrule-inspect rule)
	(format t "~A~%" (applies_top rule configuration))

  (setf ta (tape-new nil 1 nil))
  (move-head-left ta)
  (move-head-left ta)
  (tape-write ta 1)

  (setf ta (tape-new nil 0 nil))
  (move-head-left ta)
  (tape-write ta 1)
  (move-head-right ta)
  (tape-write ta 1)
  (move-head-left ta)
  (tape-write ta 0)
  (move-head-left ta)
  (tape-write ta 1)

  (setf ta (tape-new '(1 0 1 1) 1 nil))
  (move-head-left ta)
  (move-head-right ta)

  (setf ta (tape-new nil 0 nil))

  (tape-write ta 1)

  (tape-write ta 0)
  (move-head-left ta)
  (tape-write ta 1)
  (move-head-right ta)

  (tape-write ta 1)

  (tape-write ta 0)
  (move-head-left ta)
  (tape-write ta 0)
  (move-head-left ta)
  (tape-write ta 1)
  (move-head-right ta)
  (move-head-right ta)
  )
  )
(test)
