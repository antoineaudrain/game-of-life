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

; Screen Dimensions
(defparameter *sw* 2880)
(defparameter *sh* 5120)

; Grid Dimensions
(defparameter *gw* nil)
(defparameter *gh* nil)

; The grid
(defparameter *grid* nil)

(defun life (argv)
     (sdl:with-init ()
        (sdl:window 400 400)
        (init-world) ;  <- create blank window
        (sdl:with-events ()
            (:quit-event () t)
            (:idle ()
            (
)

(life )