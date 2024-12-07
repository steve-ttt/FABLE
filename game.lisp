;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; this file defines the game environment   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf *player-loc* 'bedroom)   ; Players starting location

(defparameter *start-text*
  "
Ladies and gentlemen, boys and girls, programmers of all ages! Prepare to embark on a journey back in time, to the golden age of text-adventure games. Remember Zork? The game that had you trembling in fear of the Grue in dark places? Well, hold on to your parentheses, because we’re about to introduce you to the next big thing in text-adventure gaming: F.A.B.L.E., the Fiction and Adventure Building Lisp Engine!

F.A.B.L.E. is not just another game engine. It’s a love letter to the classics, a nostalgic trip down memory lane, and a tribute to the power of LISP programming. Remember when you first encountered a (lambda (x) (* x x)) in your Scheme interpreter and realized you could create magic with code? F.A.B.L.E. brings back that feeling, but this time, you’re not just squaring numbers—you’re creating worlds!

With F.A.B.L.E., you can build your own text-adventure games, complete with intricate puzzles, witty NPCs, and treacherous dungeons. And the best part? It’s all powered by LISP, the language that’s as timeless as the games it inspired. So, whether you’re a seasoned LISP veteran or a curious newbie, F.A.B.L.E. has something for you.

So, dust off your old LISP manuals, put on your adventurer’s fedora hat, and get ready to step into the world of F.A.B.L.E. Who knows? You might just find yourself in a maze of twisty little passages, all alike. But don’t worry, there’s no Grue here—just the thrill of creating your own text-adventure masterpieces. Happy coding, and may your parentheses always be balanced!
 ")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; room definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-location 'bedroom
	      "Main Bedroom"
	      "A brightly lit bedroom with french windows that frame a picturesque view of a traditional English garden. The room is tastefully decorated, with a comfortable bed and a writing desk, creating a peaceful sanctuary for rest and reflection."
	      '((east bathroom) (west garden) (north kitchen) (south sitting-room))
	      '(laptop jewlrybox book))

(add-location 'bathroom
		"Bathroom"
		"A rather small ensuite bathroom located east of the main bedroom. Despite its size, it's well-equipped with modern amenities, including a shower and a vanity. The white tiles and chrome fixtures give it a clean, minimalist aesthetic."
	      '((west bedroom))
	      '(shavingkit sewingkit))

(add-location 'garden
		"English Garden"
		"A beautiful traditional English formal garden lies to the west, boasting a variety of vibrant flowers and manicured hedges. A small pond sits in the middle, reflecting the sky above. On a table next to the pond, a small brass key glints in the sunlight."
	      '((east bedroom))
	      '(brass-key)) 

(add-location 'kitchen
		"Small kitchen"
		"A small kitchen, typical of an Edwardian era house, greets you to the north. The room is dominated by a wood-fired stove and a newfangled electric ice box. The smell of freshly baked bread fills the air, adding to the room's warm and inviting atmosphere."
	      '((south bedroom))
	      '())

(add-location 'sitting-room
		"Sitting room"
		"A cosy art deco sitting room awaits to the south, equipped with the latest vacuum tube gramophone and turntable. The room is filled with comfortable furniture and tasteful decor, creating a perfect space for relaxation. To the south, a strange arch beckons with mystery."
	      '((north bedroom) (south arch))
		'())

(add-location 'arch
		"Passage way"
		"A futuristic passage way with LED lighting and chrome fixtures lies beyond the arch. Signs point east to '10-forward' and west to 'engineering', hinting at the adventures that await. The hum of distant machinery can be heard, adding to the intrigue."
	      '((north sitting-room) (west engineering) (east 10-forward))
	      '())

(add-location '10-forward
		"10 Forward"
		"Stepping into '10 Forward', you're greeted by a neon sign above the bar that reads 'Welcome to Quark's bar est 2369'. The room buzzes with activity, with patrons from all corners of the galaxy enjoying their drinks and sharing stories."
	      '((west arch))
	      '())

(add-location 'engineering
		"Engineering"
		"This is a place holder this room is not complete"
	      '((east arch))
	      '())
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; item definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-item 'book
	  "Old book"
	  "An old leather bound book with gold lettering saying 'Wizards guide to Common Lisp'"
	  'bedrom1
	  nil
	  '('open 'read))

(add-item 'laptop
	  "Modern laptop"
	  "A sleek designed laptop, of a brand and style you have never seen"
	  'bedrom1
	  nil
	  '('open 'read))

(add-item 'jewlrybox
	  "Wodden Jewlry box"
	  "beautiful jewlery box with celtic carvings."
	  'bedrom1
	  nil
	  '('open))

(add-item 'sewingkit
	  "small sewing kit"
	  "the sewing kit contains thread, needle, and buttons."
	  'bathroom
	  nil
	  '('open))


(add-item 'shavingkit
	  "small shaving kit"
	  "the shaving kit contains the usuall toiletries, I dont think you will need it."
	  'bathroom
	  nil
	  '('open))

(add-item 'brass-key
	  "small shiny brass key."
	  "brass key, strange it seems to have some kind of microchip connections on it."
	  'bathroomgarden
	  nil
	  '())
