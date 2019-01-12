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

;; Screen Dimensions
(defparameter *sw* 50)
(defparameter *sh* 30)

;; Grid Dimensions
(defparameter *gw* nil)
(defparameter *gh* nil)

;; The grid
(defparameter world (make-array '(*sw* *sh*) :element-type 'integer))

(defun print-world (world)
    (dotimes (i *sw*)
    (format t "~1  " i)
    )
    (format t "~%")
    (dotimes (j *sh*)
    (format t "~1  " j)
    )
)

(defun life (argv)
    (progn (setf *gw* (parse-integer (second argv)))
       (setf *gh* (parse-integer (third argv)))
    )
    (init-world world)
    (print-world)
    (sdl:with-init ()
        (sdl:window *gw* *gh* :title-caption "Sarciflette" :icon-caption "Carnifex")
        (setf (sdl:frame-rate) 60)
        (sdl:with-events ()
            (:quit-event () t)
            (:key-down-event ()
                (when (sdl:key-down-p :sdl-key-escape)
                (sdl:push-quit-event)
                (exit)
                )
            )
        )
    )
)

(sb-int:with-float-traps-masked (:invalid :inexact :overflow) (life *posix-argv*))