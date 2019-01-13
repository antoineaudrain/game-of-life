; **************************************************************************** ;
;                                                           LE - /             ;
;                                                               /              ;
;    game_of_life.lisp                                .::    .:/ .      .::    ;
;                                                  +:+:+   +:    +:  +:+:+     ;
;    By: aaudrain <aaudrain@student.le-101.fr>      +:+   +:    +:    +:+      ;
;                                                  #+#   #+    #+    #+#       ;
;    Created: 2019/01/09 12:00:32 by aaudrain     #+#   ##    ##    #+#        ;
;    Updated: 2019/01/09 12:00:37 by aaudrain    ###    #+. /#+    ###.fr      ;
;                                                          /                   ;
;                                                         /                    ;
; **************************************************************************** ;

(ql:quickload 'lispbuilder-sdl)

(defparameter *world* nil)
(defparameter *world-h* 50)
(defparameter *world-w* 100)
(defparameter *world-x* 0)
(defparameter *world-y* 0)
(defparameter *win-w* 1600)
(defparameter *win-h* 900)
(defparameter *square* 15)
(defparameter *running* nil)
(defparameter *counter* 0)
(defparameter *slowdown* 0)
(defparameter *grid* 1)

(defun init-world ()
  (setq *world* (make-array (list *world-h* *world-w*))))

(defun draw-life (i j)
  (sdl:draw-box
	(sdl:rectangle
	  :x (floor (+ *world-y* (+ (* j *grid*) (* j *square*))))
	  :y (floor (+ *world-x* (+ (* i *grid*) (* i *square*))))
	  :w (floor *square*)
	  :h (floor *square*))
	:color (sdl:color
			 :r (if (= 0 (aref *world* i j)) 0 255)
			 :g (if (= 0 (aref *world* i j)) 0 255)
			 :b (if (= 0 (aref *world* i j)) 0 255))))

(defun loop-life ()
  (dotimes (i *world-h*)
	(dotimes (j *world-w*)
	  (draw-life i j))))

(defun resetgrid ()
  (dotimes (i *world-h*)
	(dotimes (j *world-w*)
	  (setf *world-x* 0)
	  (setf *world-y* 0)
	  (setf *square* 10)
	  (setf *running* nil)
	  (setf (aref *world* i j) 0))))

(defun get-val (i j)
  (if (or (< i 0) (< j 0) (>= i *world-h*) (>= j *world-w*))
	'0
	(rem (aref *world* i j) 2 )))

(defun life-rules (life sum)
  (if (= (rem life 2) 1)
	(if (or (= 2 sum) (= 3 sum))
	  '1
	  '3)
	(if (= sum 3)
	  '2
	  '0)))

(defun get-cells-neighbour (i j)
  (let ((sum 0))
	(setf sum(+ sum (get-val (- i 1) j)))
	(setf sum(+ sum (get-val (+ i 1) j)))
	(setf sum(+ sum (get-val (- i 1) (- j 1))))
	(setf sum(+ sum (get-val (+ i 1) (- j 1))))
	(setf sum(+ sum (get-val (- i 1) (+ j 1))))
	(setf sum(+ sum (get-val (+ i 1) (+ j 1))))
	(setf sum(+ sum (get-val i (+ j 1))))
	(setf sum(+ sum (get-val i (- j 1))))
	sum))

(defun get-state(value)
  (if (= value 3)
	'0
	'1))

(defun god ()
  (dotimes (i *world-h*)
	(dotimes (j *world-w*)
	  (if (or (= 3 (aref *world* i j)) (= 2 (aref *world* i j)))
		(list (setf (aref *world* i j) (get-state (aref *world* i j))) (draw-life i j))))))

(defun walk ()
  (dotimes (i *world-h*)
	(dotimes (j *world-w*)
	  (setf (aref *world* i j) (life-rules (aref *world* i j) (get-cells-neighbour i j))))) *world*)

(defun cycle ()
  (sdl:update-display)
  (if (>= *counter* *slowdown*)
	(progn
	  (setf *counter* 0)
	  (if *running*
		(progn
		  (walk)
		  (god))))
	)
  (incf *counter*)
)

(defun size-x ()
  (+ (* *square* *world-h*) (* *grid* (- *world-h* 1))))

(defun size-y ()
  (+ (* *square* *world-w*) (* *grid* (- *world-w* 1))))

(defun get-i (x size)
  (floor (* (/ (- x *world-x*) size) *world-h*)))

(defun get-j (y size)
  (floor (* (/ (- y *world-y*) size) *world-w*)))

(defun touch (x y)
  (let ((i (get-i x (size-x)))(j (get-j y (size-y))))
	(if (and (< x (+ (size-x) *world-x*)) (> x *world-x*) (< y (+ (size-y) *world-y*)) (> y *world-y*))
	  (setf (aref *world* i j) (if (= 1 (aref *world* i j))
								  '0
								  '1))
	  'nil)))

(defun zoom()
  (if (>= *square* 40)
	'0
	(setf *square* (+ *square* 1))))

(defun unzoom()
  (if (<= *square* 2)
	'0
	(setf *square* (- *square* 1))))

(defun speedup()
  (if (< 0 *slowdown*)
	(decf *slowdown*)))

(defun slowdown()
  (if (> 150 *slowdown*)
	(incf *slowdown*)))

(defun zoom_mouse(mx my)
  (let ((sx (size-x))(sy (size-y)))
	(zoom)
	(let ((nsx (size-x))(nsy (size-y)))
	  (setf *world-y* (floor(- mx (* nsx (/ sx (- mx *world-y*))))))
	  (setf *world-x* (floor(- my (* nsy (/ sy (- my *world-x*))))))))
  (sdl:clear-display sdl:*black*)
  (loop-life))

(defun unzoom_mouse(mx my)
  (let ((sx (size-x))(sy (size-y)))
	(unzoom)
	(let ((nsx (size-x))(nsy (size-y)))
	  (setf *world-y* (floor(- mx (* nsx (/ sx (- mx *world-y*))))))
	  (setf *world-x* (floor(- my (* nsy (/ sy (- my *world-x*))))))))
  (sdl:clear-display sdl:*black*)
  (loop-life)
)

(defun world-generation ()
  	(sdl:with-init ()
		(sdl:window *win-w* *win-h* :title-caption "Sarciflette")
		(setf (sdl:frame-rate) 60)
		(sdl:clear-display sdl:*black*)
		(loop-life)
		(sdl:with-events ()
			(:quit-event () (exit))
			(:key-down-event ()
				(when (sdl:key-down-p :sdl-key-escape)
					(sdl:push-quit-event))
				(when (or (sdl:key-down-p :sdl-key-up) (sdl:key-down-p :sdl-key-w))
					(setf *world-x* (+ *world-x* *square*)))
				(when (or (sdl:key-down-p :sdl-key-down) (sdl:key-down-p :sdl-key-s))
					(setf *world-x* (- *world-x* *square*)))
				(when (or (sdl:key-down-p :sdl-key-right) (sdl:key-down-p :sdl-key-d))
					(setf *world-y* (- *world-y* *square*)))
				(when (or (sdl:key-down-p :sdl-key-left) (sdl:key-down-p :sdl-key-a))
					(setf *world-y* (+ *world-y* *square*)))
				(when (sdl:key-down-p :sdl-key-p)
					(if *running*
						(setf *running* nil)
						(setf *running* t)))
				(when (sdl:key-down-p :sdl-key-comma)
					(slowdown))
				(when (sdl:key-down-p :sdl-key-period)
					(speedup))
				(when (sdl:key-down-p :sdl-key-kp-plus)
					(progn (zoom_mouse (sdl:mouse-x) (sdl:mouse-y)) (zoom_mouse (sdl:mouse-x) (sdl:mouse-y))))
				(when (sdl:key-down-p :sdl-key-kp-minus)
					(progn (unzoom_mouse (sdl:mouse-x) (sdl:mouse-y)) (unzoom_mouse (sdl:mouse-x) (sdl:mouse-y))))
				(when (sdl:key-down-p :sdl-key-r)
					(resetgrid))
				(when (sdl:key-down-p :sdl-key-g)
					(setf *grid* (- 1 *grid*))
					(if (= *grid* 1) (setf *square*  (- *square* 1)) (setf *square* (+ *square* 1))))
				(sdl:clear-display sdl:*black*)
				(loop-life))
			(:mouse-button-down-event (:button button :x mouse-x :y mouse-y)
				(if (and (= button 1) (not  (sdl:key-down-p :sdl-key-lshift)))
					(touch mouse-y mouse-x))
				(if (= button 4)
					(if (or (sdl:key-down-p :sdl-key-lshift) (sdl:key-down-p :sdl-key-rshift))
						(speedup)
						(progn (unzoom_mouse mouse-x mouse-y) (unzoom_mouse mouse-x mouse-y))))
				(if (= button 5)
					(if (or (sdl:key-down-p :sdl-key-lshift) (sdl:key-down-p :sdl-key-rshift))
						(slowdown)
						(progn (zoom_mouse mouse-x mouse-y) (zoom_mouse mouse-x mouse-y))))
				(sdl:clear-display sdl:*black*)
				(loop-life))
			(:idle ()
				(sdl:clear-display sdl:*black*)
				(loop-life)
				(cycle)
			)
		)
	)
)

(defun trucmuch (argv)
	(if (or (eq (length argv) 1) (eq (nth 2 argv) NIL) (eq (nth 1 argv) "-h") (eq (nth 1 argv) "--help") (not (eq (nth 3 argv) NIL)))
		(progn (format t "usage: sbcl --load game_of_life.lsp [-h] width height
	   
positional arguments:
  width			width of the grid
		 
  height		height of the grid
		 
optianal arguments:
  -h, --help		show this help message and exit~%")
		(exit)
		)
    )
    (progn (setf *world-w* (parse-integer (second argv)))
       (setf *world-h* (parse-integer (third argv)))
    )
    (if (or (< *world-w* 1) (< *world-h* 1) (> *world-h* *win-h*) (> *world-w* *win-w*))
		(progn(format t "Look at the MAC Displays what are you trying to do ?~%")
		(exit)
		)
	)
    (init-world)
    (world-generation)
    (sb-ext:exit)
)

(sb-int:with-float-traps-masked (:invalid :inexact :overflow) (trucmuch *posix-argv*))