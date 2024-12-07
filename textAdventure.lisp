; Make exe with => (sb-ext:save-lisp-and-die "binary-name" :executable t :toplevel 'start)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Globals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *player-loc* nil)
(defvar *player-inv* '())
(defvar *loc-list* nil )
(defvar *item-list* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct location
  (name nil)          ; name ie "livingroom"
  (description nil)   ; short desctiption "Living Room"
  (ldescription nil)  ;long description "you are in a brightly lit living room with french windows"
  (exits nil)         ; expects a list of exits '((east bedroom) (west kitchen) (up attic))
  (inventory nil))    ; items you can pick up in the room as a list '(whiskybottle jewlrybox book)

(defstruct item
  (name nil)          ; name ie jewlerybox
  (description nil)   ; short desc Jewlery Box
  (ldescription nil)  ; long description "a silver jelwely box when open it gently plays green sleeves"
  (currentloc nil)    ; where is the time currently located can be a room name or player or in another item
  (inventory nil)     ; list of items in the item a jewlery-box might contain '(necklace earings brasskey)
  (flags nil))         ; a list of flags '(open turnon turnoff wearing)



(defun add-location (name desc ldesc exits inv)
  " ADD-location will add to the global *loc-list*"
  (push (make-location :name name
		       :description desc
		       :ldescription ldesc
		       :exits exits
		       :inventory inv) *loc-list*))

(defun add-item (name desc ldesc curloc inv flag)
  "ADD-item will add and item struct to the global *item-list*"
  (push (make-item :name name
		   :description desc
		   :ldescription ldesc
		   :currentloc curloc
		   :inventory inv
		   :flags flag) *item-list*))

(defun look (&optional object &rest args)
  "returns a description of the players location or a specific object if called with [look object]"
  (if object
      (let ((item (find object *item-list* :test #'string= :key #'item-name))
            (location (find object *loc-list* :test #'string= :key #'location-name)))
        (cond (item
               (format t "~%~A" (item-ldescription item)))
              (location
               (format t "~%~A" (location-ldescription location)))
              (t
               (format t "~%Object not found."))))
      (progn
	(format t "~%~A" (location-name (find-node *player-loc*)))
        (format t "~%~A" (location-description (find-node *player-loc*)))
        (format t "~%~A" (location-ldescription (find-node *player-loc*)))
        (format t "~%This room contains:~%~A " (location-inventory (find-node *player-loc*)))
        (format t "~%Exits:")
        (format t "~%~A" (location-exits (find-node *player-loc*))))))


(defun find-node (name)
  "write find-node which take a node name and returns the node if it appears in *node-LIST*
   (location-ldescription (find-node 'bathroom))"
  (dolist (i *loc-list*)
    (if (equal name (location-name i))
        (return i))))

(defun move (direction &rest args)
  "Move the player in the given direction, if it's a valid exit."
  (let* ((current-location (find-node *player-loc*))
         (exits (location-exits current-location)))
    (dolist (exit exits)
      (when (eq direction (car exit))
        (setf *player-loc* (cadr exit))
        (format t "You moved to ~A." *player-loc*)))))  ; Return no values after moving



(defun take (item &rest args)
  "Get an item from the current location and add it to the player's inventory."
  (let* ((current-location (find-node *player-loc*))
         (room-inventory (location-inventory current-location)))
    (if (member item room-inventory)
        (progn
          (setf (location-inventory current-location) (remove item room-inventory))
          (push item *player-inv*)
          (format t "You picked up ~A." item)) 
        (progn
          (format t "~A is not in this room." item))))) 

(defun inventory ()
  "Print the player's current inventory."
  (format t "Your inventory: ~A" *player-inv*))

(defun where ()
  "Print the player's current location."
  (format t "You are in the ~A" *player-loc*))

(defun help (&optional cmd)
  "print the documentation string for the given command"
  (if (null cmd)
      (format t "~% List of avaialable commands: ~A~%
Use help [command] for speciffic help ie: help look~%~%" *allowed-commands* )
      (format t "~% ~A" (documentation cmd 'function))))

(defun save-game (filename )
  "save the game to disk, put filename in quotes\" save-game \"/path/to/my-game\" "
  (with-open-file (stream filename
                         :direction :output
                         :if-exists :supersede)
    (prin1 (cons *player-loc* (cons *player-inv* (cons *loc-list* (cons *item-list* '()))))
	   stream)))

(defun load-game (filename)
  "load a saved game to disk, put filename in quotes\" load-game \"/path/to/my-game\" "
  (if (probe-file filename)
      (with-open-file (stream filename
                              :direction :input)
	(let ((game-state (read stream)))
	  (setf *player-loc* (first game-state))
	  (setf *player-inv* (second game-state))
	  (setf *loc-list* (nth 2 game-state))
	  (setf *item-list* (nth 3 game-state))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; game REPL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun game-repl ()
  (let ((cmd (progn (format t "> ")
		    (finish-output)  ;; Ensure that the prompt is printed before reading input
		    (game-read))))
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))


(defun game-read ()
  (let ((cmd (read-from-string (concatenate 'string "(" (read-line) ")"))))
         (flet ((quote-it (x)
                    (list 'quote x)))
             (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))



(defun game-eval (sexp)
    (if (member (car sexp) *allowed-commands*)
        (eval sexp)
        (format t "I do not know that command.~%")))

(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
          (rest (cdr lst)))
      (cond ((eql item #\space) (cons item (tweak-text rest caps lit)))
            ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
            ((eql item #\") (tweak-text rest caps (not lit)))
            (lit (cons item (tweak-text rest nil lit)))
            (caps (cons (char-upcase item) (tweak-text rest nil lit)))
            (t (cons (char-downcase item) (tweak-text rest nil nil)))))
      (fresh-line)))

(defun game-print (lst)
  (princ (coerce (tweak-text (coerce (string-trim "() " (prin1-to-string lst)) 'list) t nil) 'string))
  (fresh-line)) 




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *allowed-commands* '(help look walk inventory where move take exit e w s n inv x save-game load-game lo sa))

;; define some aliases, using defun rather than defmacro as it's more readable - at least I think so!
(defun e ()  "alias to move function - moves player east" (move 'east))
(defun w ()  "alias to move function - moves player west" (move 'west))
(defun n ()  "alias to move function - moves player north" (move 'north))
(defun s ()  "alias to move function - moves player south" (move 'south))
(defun inv ()   "alias to inventory function - display player inventory" (inventory))
(defun walk (dir) "alias to move function - takes a direction" (move dir))
(defun x (obj) "examine an object ie: [x book]" (look obj))
(defun sa (filename) "alias to save-file (enter the file name in double quotes)" (save-game filename))
;; cant use LOAD it's a key word
(defun lo (filename) "alias to load-file (enter the file name in double quotes)" (load-game filename))

(load "./game.lisp") ;; set the path to your game definitions here


(defun start ()
  ;this is the start procedure to start the game loop and print the welcome text
  (format t "~%~%~A" *start-text*)
  (help)
  (game-repl))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; using ex 6-41 as inspiration, this will become a text adventure engine using struct
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Keyboard Excercise 6-41
;					
; In this keyboard exercise we will write some routines for moving Robbie the
; robot around in a house. The map of the house appears in Figure 6-2. Robbie
; can move in any of four directions: north, south, east, or west.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|

(defvar loc 'kitchen)
(defvar rooms

  '((living-room        (north front-stairs)
			(south dining-room)
			(east kitchen))

    (upstairs-bedroom   (west library) 
			(south front-stairs))

    (dining-room        (north living-room) 
			(east pantry)
			(west downstairs-bedroom))

    (kitchen            (west living-room)
			(south pantry))

    (pantry             (north kitchen)
			(west dining-room))

    (downstairs-bedroom (north back-stairs)
			(east dining-room))

    (back-stairs        (south downstairs-bedroom)
			(north library))

    (front-stairs       (north upstairs-bedroom)
			(south living-room))

    (library            (east upstairs-bedroom)
			(south back-stairs))))
  

(defun next-room (current-room direction rooms)
    "returns an alist for the room in the direction specified from the current room
ie: if the pantry is south of the kichen it would return (south pantry) or NIL if there
is no room in that direction."
    (assoc direction (rest (assoc current-room rooms))) )

(defun choices (current-room rooms)
  "(a) CHOICES that takes the name of a room as input
and returns the table of permissible directions Robbie may take
from that room. For example, (CHOICES ‘PANTRY) should return
the list ((NORTH KITCHEN) (WEST DINING-ROOM))."
  (rest (assoc current-room rooms) ))

(defun look_old (direction room)
  "(b) Write a function LOOK that takes two inputs, a direction and a
room, and tells where Robbie would end up if he moved in that di‐
rection from that room. For example, (LOOK ‘NORTH ‘PANTRY)
should return KITCHEN."
 (cdr (assoc direction (rest (assoc room rooms)))) )

(defun set-robbie-location (place)
  "Moves Robbie to PLACE by setting the global variable LOC"
  (setf loc place))

(defun how-many-choices ()
  "d. Write a function HOW-MANY-CHOICES that tells how many
choices Robbie has for where to move to next. Your function
should refer to the global variable LOC to find his current location.
If he is in the pantry, (HOW-MANY-CHOICES) should return 2."
  (length (choices loc rooms))
  )

(defun upstairsp ()
"e. Write a predicate UPSTAIRSP that returns T if its input is an up‐
stairs location. (The library and the upstairs bedroom are the only
two locations upstairs.) Write a predicate ONSTAIRSP that returns
T if its input is either FRONT-STAIRS or BACK-STAIRS."
  (if (or (equal loc 'library) (equal loc 'upstairs-bedroom)) T
      nil) )

(defun ONSTAIRSP ()
"Write a predicate ONSTAIRSP that returns
T if its input is either FRONT-STAIRS or BACK-STAIRS."
 (if (or (equal loc 'FRONT-STAIRS) (equal loc 'BACK-STAIRS)) T
      nil) )
  
(defun where ()   
"f. Where’s Robbie? Write a function of no inputs called WHERE that
tells where Robbie is. If he is in the library, (WHERE) should say
(ROBBIE IS UPSTAIRS IN THE LIBRARY). If he is in the
kitchen, it should say (ROBBIE IS DOWNSTAIRS IN THE
KITCHEN). If he is on the front stairs, it should say (ROBBIE IS
ON THE FRONT-STAIRS)."
; (format nil "world : ~a" x)
  (cond ((onstairsp) (format nil "Robbie is on the ~a" loc))
	 ((upstairsp) (format nil "Robbie is upstairs in ~a" loc))
	 ((format nil "Robbie is in the ~a" loc)) ))


(defun move (direction) 
"g. Write a function MOVE that takes one input, a direction, and
 moves Robbie in that direction. MOVE should make use of the
 LOOK function you wrote previously, and should call SET-ROBBIE-LOCATION
 to move him. If Robbie can’t move in the speci‐
 fied direction an appropriate message should be returned. For ex‐
 ample, if Robbie is in the pantry, (MOVE ‘SOUTH) should return
 something like (OUCH! ROBBIE HIT A WALL). (MOVE
 ‘NORTH) should change Robbie’s location and return (ROBBIE IS
 DOWNSTAIRS IN THE KITCHEN)."
  (if (look direction loc) (setf loc (car (look direction loc)))
      (format nil "OUCH! Robbie hit a wall")) )

; h. Starting from the pantry, take Robbie to the library via the back
; stairs. Then take him to the kitchen, but do not lead him through the
; downstairs bedroom on the way.

|#
