(ql:quickload 'lispbuilder-sdl)

(defparameter world (make-array '(100 100) :element-type 'fixnum))

(defun cp-arr (array &key (element-type (array-element-type array))
					 (fill-pointer (and (array-has-fill-pointer-p array)
										(fill-pointer array)))
					 (adjustable (adjustable-array-p array)))
  (let* ((dimensions (array-dimensions array))
		 (new-array (make-array dimensions
								:element-type element-type
								:adjustable adjustable
								:fill-pointer fill-pointer)))
	(dotimes (i (array-total-size array))
	  (setf (row-major-aref new-array i)
			(row-major-aref array i)))
	new-array)
  )

;; initialize
(defun init-world! (world)
  (loop for i from 0 to (1- (array-dimension world 0)) do
		(loop for j from 0 to (1- (array-dimension world 1)) do
			  (setf (aref world i j) (if (zerop (random 7)) 1 0)))))

(defun count-neighboring-individual (i j world)
  (let ((next-i (if (= i (1- (array-dimension world 0))) 0 (1+ i)))
		(prev-i (if (= i 0) (1- (array-dimension world 0)) (1- i)))
		(next-j (if (= j (1- (array-dimension world 1))) 0 (1+ j)))
		(prev-j (if (= j 0) (1- (array-dimension world 1)) (1- j))))
	(+ (aref world prev-i prev-j) (aref world prev-i j) (aref world prev-i next-j)
	   (aref world i prev-j) (aref world i next-j)
	   (aref world next-i prev-j) (aref world next-i j) (aref world next-i next-j))))

;; return next generation world
(defun update-next-generation (world)
  (let ((next-world (cp-arr  world)))
	(loop for i from 0 to (1- (array-dimension world 0)) do
		  (loop for j from 0 to (1- (array-dimension world 1)) do
				(cond ((and (zerop (aref world i j)) ; birth
							(= (count-neighboring-individual i j world) 3))
					   (setf (aref next-world i j) 1))
					  ((and (= (aref world i j) 1)   ; die by under-population or overcrowding
							(or (<= (count-neighboring-individual i j world) 1)
								(>= (count-neighboring-individual i j world) 4)))
					   (setf (aref next-world i j) 0))
					  )
				)
		  )
	next-world)
  )

(defun life (argv)
  (sdl:with-init ()
				 (sdl:window 400 400)
				 (setf (sdl:frame-rate) 60)
				 (init-world! world)
				 (sdl:with-events ()
								  (:quit-event () (exit))
									(key-down-event (:key key :mod mod)
										(case key
										 (:sdl-key-escape (sdl:push-quit-event))
										 )
									)
								  (:idle ()
										 (setf world (update-next-generation world))
										 (loop for i from 0 to (1- (array-dimension world 0)) do
											   (loop for j from 0 to (1- (array-dimension world 1)) do
													 (if (= (aref world i j) 0)
													   (sdl:draw-box (sdl:rectangle :x (* i 4) :y (* j 4) :w 4 :h 4) :color sdl:*black*)
													   (sdl:draw-box (sdl:rectangle :x (* i 4) :y (* j 4) :w 4 :h 4) :color sdl:*white*)
													   )
													 )
											   )
										 (sdl:update-display)
										 )
								  )
				 )
  )

(sb-int:with-float-traps-masked (:invalid :inexact :overflow) (life *posix-argv*))
