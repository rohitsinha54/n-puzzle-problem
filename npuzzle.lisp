;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Title    :   nxn puzzle solver using a* algorithm.
;; Problem  :   Implement the A* search for searching trees (in Lisp). Do not 
;;          :   use Russell’s code or other code from the web. Implement a 
;;          :   counter that counts the number of nodes expanded and prints this
;;          :   number at the end of the search. Use your code to solve the 
;;          :   8-puzzle problem with heuristic being the number of misplaced 
;;          :   tiles and start state ((E, 1, 3),(4, 2, 5),(7, 8,6)). The goal 
;;          :   state is: ((1, 2, 3),(4, 5, 6),(7, 8, E)). Print the number of 
;;          :   nodes expanded. You only need to show the states generated 
;;          :   during the search process. Your code should detect infeasible 
;;          :   puzzles.
;; Date     :   Dec 01, 2012
;; Author   :   Rohit Sinha
;; Assignment:  Artificial Intelligence CSCI 5511 - Lisp Assignment
;; Due Date :   Dec 11, 2012
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Declaring Variables

;; Stores all the states or nodes
(setf state_tree (make-array 0 :fill-pointer t :adjustable t))
;; Stores all the traversed states
(setf traversed_states (make-array 0 :fill-pointer t :adjustable t))
;; size of the puzzle or size of n on n X n board.
(setf *size_n* 0)

;; structure of the state node	
(defstruct state_node 
    state 
    id 
    h 
    g 
    f
)
;; stores all the unsolvable states
(setf unsolvable_states (make-array 0 :fill-pointer t :adjustable t))
;; the unsolvable puzzle flag
(setq unsolvable_flag 0)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function Name    :   solve-npuzzle
;; Purpose          :   The main function to check the puzzle validity,
;;                  :   initialize and call the solver on the puzzle.
;; Parameters       :   start-state: the unsolved puzzle state
;; Returns          :   void
;; Calls to         :   check-puzzle-validity, initialize-puzzle,
;;                  :   determine-heuristic, puzzle-solver
;; Called from      :   this program or command line
;; Method           :   1. Print a welcome message to user
;;                  :   2. Check for a valid nxn puzzle
;;                  :       2a. If puzzle is valid then initialize
;;                  :       2b. call the puzzle solver to solve the puzzle
;;                  :       2c. if puzzle is not valid display error message
;;                  :   3. print end marker
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(defun solve-npuzzle(start_state)
    ;; print the welcome messgae
    (format t "~%~%~%~%~%")
    (format t "================== Welcome to nxn Puzzle Solver ===============")
    (format t "===~%~%")
    ;; check for valid n x n puzzle
    (format t "Solver: Checking for n x n puzzle ...~%")
    (if (check-puzzle-validity start_state)
        (progn      ;; if valid n x n puzzle
            ;; initialize
            (format t "Solver: Intializing ...~%")
	        (setf size_n (length start_state))	
	        (initialize-puzzle start_state size_n)		
	        (determine-heuristic this_state)
            ;; call the solver function to solve the puzzle		
	        (format t "Solver: Now trying to solve the puzzle ...~%~%")
	        (puzzle-solver)
	        (setf nodes_expanded (- (length traversed_states) 1))	
	        (format t "~%Total nodes expanded = ~S" nodes_expanded)
        )
        (progn
            ;; print error message if not a valid nxn puzzle
            (format t "Solver: Invalid puzzle size. ") 
            (format t "Please enter a puzzle of nxn size.~%~%")
        )
    )
    ;; print end marker
    (format t "~%~%=========================================================")
    (format t "=========")
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function Name    :   check-puzzle-validity
;; Purpose          :   check for a valid nxn puzzle
;; Parameters       :   start start state
;; Returns          :   start state or nil
;; Calls to         :   none
;; Called from      :   solve-npuzzle
;; Method           :   1. if square root of length of start state is integer
;;                  :      return start state else nil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(defun check-puzzle-validity(start_state)
    ;; check for the square root of the length of start state for integer
    (if (integerp (sqrt (length start_state)))
        start_state     ;; if integer return non nil
        nil             ;; if not an integer return nil
    )
) 



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function Name    :   initialize-puzzle
;; Purpose          :   creates the starting state, goal and unsolvable_states
;; Parameters       :   starting state: the starting state of puzzle,
;;                  :   puzzle_size: the size of nxn puzzle
;; Returns          :   none
;; Calls to         :   add-to-unsolvable
;; Called from      :   solve-npuzzle
;; Method           :   1. initialize the starting state
;;                  :   2. push the starting state to state tree
;;                  :   3. set the goal array
;;                  :   4. determing all possible unsolvable states by possible
;;                  :      left, right, up and down swaps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(defun initialize-puzzle(starting_state puzzle_size)

    ;; Initialize the starting state
    (setf this_state starting_state)
    ;; create and push the state to state tree array
    (vector-push-extend (make-state_node 
                            :state this_state 
                            :id (length state_tree)
                            :h (determine-heuristic this_state) 
                            :g 0 
                            :f (determine-heuristic this_state)) 
                            state_tree) 

    ;; set the goal
    (setf *goal_state* (make-array puzzle_size))
	(loop for i from  0 to (1- puzzle_size) do
		(setf (aref *goal_state* i) (rem (1+ i) puzzle_size))
    )

    ;; Detemrine and initialize unsolvable_states
	(setf sqrt_size (sqrt puzzle_size))
	(loop for i from 0 to (- puzzle_size 2) do
		(let((right (+ i 1)) (left (- i 1)) (down (+ i sqrt_size)) 
                                                    (up (- i sqrt_size)))
		    (if (and (> right -1) (< right puzzle_size) (/= (1- sqrt_size) 
                                    (rem i sqrt_size)) (/= right (1- size_n)))	
			    (add-to-unsolvable *goal_state* i right)
            )
		    (if (and (> left -1) (< left puzzle_size) (/= left (1- size_n)) 
                                                        (/= 0(rem i sqrt_size)))	
			    (add-to-unsolvable *goal_state* i left)
            )
		    (if (and (> down -1) (< down puzzle_size) (/= down (1- size_n)))			
			    (add-to-unsolvable *goal_state* i down)
            )
		    (if (and (> up -1) (< up puzzle_size) (/= up (1- size_n)))			
			    (add-to-unsolvable *goal_state* i up)
            )
        )
    )
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function Name    :   puzzle-solver
;; Purpose          :   solves the puzzle by checking states for unsovable,
;;                  :   popping them from state_tree, traverse them, checks
;;                  :   them with goal state and recursively call itself.
;; Parameters       :   none
;; Returns          :   calls puzzle solver recursively
;; Calls to         :   print-puzzle-state, is-unsolvable, search-empty-slot,
;;                  :   puzzle-solver
;; Called from      :   solve-npuzzle
;; Method           :   1. check for unsolvable flag
;;                  :       1a. if unsolvable flag is set then print message
;;                  :           and end
;;                  :       1b. if unsolvable flag is not set pop from state
;;                  :           tree
;;                  :       1c. copy the node value of the state
;;                  :       1d. print the state
;;                  :       1e. check for unsolvability
;;                  :       1f. push to traversed states
;;                  :       1g. check if this is goal state
;;                  :           1g.a. if goal state then print message and end
;;                  :           1g.b. if this is not goal state find empty slot
;;                  :           1g.c. do recurssion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(defun puzzle-solver ()
    ;; check unsolvable flag to set
	(if (= unsolvable_flag 1)			
        (progn
            ;; if set print meesage and end
		    (format t "~%~%Solver: This puzzle is UNSOLVABLE!!!~%~%")		
        )
		(progn
            ;; if not set pop next state from state tree
		    (setq eval_state (vector-pop state_tree))	
            ;; copy state_node value of the state		
		    (let ((copy_state (copy-seq (state_node-state eval_state))))
                ;; print the value
			    (print-puzzle-state copy_state)
                ;; check for unsolvability
			    (is-unsolvable copy_state)
                ;; add to to traversed states			
			    (vector-push-extend copy_state traversed_states)
                ;; check if this is goal state
			    (if (equalp *goal_state* copy_state)
				    (progn 
                        ;; if goal state print message and end
                        (format t "~%~%Solver: Goal State Reached~%~%")
                    )	
				    (progn
                        ;; if not goal state searh empty slot and perform
                        ;; recurssion 
                        (search-empty-slot eval_state)
                        (puzzle-solver)
                    )
                )
            ) ;; end of let
        )
    ) ;; end of if
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function Name    :   print-puzzle-state
;; Purpose          :   prints the current puzzle state
;; Parameters       :   the state to be printed
;; Returns          :   none
;; Calls to         :   none
;; Called from      :   puzzle-solver
;; Method           :   1. find the square root of puzzle_size
;;                  :   2. print every tile in formatted manner
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(defun print-puzzle-state(print_state)
    ;; find the square root of the puzzle size
    (setf sqrt_size (sqrt (length print_state)))
    ;; print every tile in formatted way
    (loop for i from 0 to (- (length print_state) 1) do
        (if (and (/= 0 i) (= 0 (mod i sqrt_size)))
            (format t "~%")
        )
        (if (= 0 (mod i sqrt_size))
            (format t "|")
        )
        (if (= 0 (aref print_state i))
            (format t " |")
            (format t "~a|" (aref print_state i))
        )
    )
)
            


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function Name    :   add-to-unsolvable
;; Purpose          :   swaps the tiles are add to unsolvable states
;; Parameters       :   state_array: the in which swap has to be perfomed
;;                  :   tile1, tile2: two tiles which are to be swapped
;; Returns          :   none
;; Calls to         :   none
;; Called from      :   initialize-puzzle
;; Method           :   1. copy the state
;;                  :   2. perform the tile exchange
;;                  :   3. push to unsolvable states
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(defun add-to-unsolvable(state_array tile1 tile2)
    ;; copy the state
	(setf copy_array (copy-seq state_array))
    ;; exchange the tiles 
	(rotatef (svref copy_array tile1) (svref copy_array tile2))
    ;; push to unsolvable_states	
	(vector-push-extend copy_array unsolvable_states)
)		

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function Name    :   search-empty-slot
;; Purpose          :   finds the empty slot location in the puzzle
;; Parameters       :   to_find: state in which empty slot has to be found
;; Returns          :   none
;; Calls to         :   find-all-moves
;; Called from      :   puzzle-solver
;; Method           :   1. for every tile in the state
;;                  :   2. check if this is empty tile
;;                  :       2a. if this is the empty slot display message
;;                  :       2b. find all moves with this empty slot 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 	
(defun search-empty-slot(to_find)
    ;; check every tile for empty tile				
    (loop for i from 0 to (1- size_n) do
		(if (= 0 (aref (state_node-state to_find) i))	
		    (progn
                ;; if this is the empty slot
                (format t "~%Solver: Empty Slot at position ~S~%~%" i) 
                (find-all-moves to_find i)
            )
        )
    )
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function Name    :   find-all-moves
;; Purpose          :   find all the possible moves (new states) possible with 
;;                  :   the current position of the empty slot
;; Parameters       :   state: the current state, empty_slot: position
;; Returns          :   none
;; Calls to         :   tile-mover, sort-state-tree
;; Called from      :   search-empty-slot 
;; Method           :   1. find the square root of puzzle size
;;                  :   2. find right, left, up and down positions
;;                  :   3. make possible right, left, up and down moves
;;                  :   4. sort the state tree 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(defun find-all-moves(state empty_slot)
    ;; find the square root of puzzle size 
	(setf sqrt_n (sqrt size_n))
    ;; find the index of right, left, up and down tiles 
	(let((right (+ empty_slot 1)) (left (- empty_slot 1)) 
        (down (+ empty_slot sqrt_n)) (up (- empty_slot sqrt_n)))	
	    ;; perform right move if possible
        (if (and (> right -1) (< right size_n) (/= (1- sqrt_n) 
                                            (rem empty_slot sqrt_n)))	
		    (tile-mover state empty_slot right)
        )
        ;; perform left move is possible		
	    (if (and (> left -1) (< left size_n) (/= 0(rem empty_slot sqrt_n)))		
		    (tile-mover state empty_slot left)
        )
        ;; perform down move if possible
	    (if (and (> down -1) (< down size_n))
		    (tile-mover state empty_slot down)
        )
        ;; perform up move is possible
	    (if (and (> up -1) (< up size_n))
		    (tile-mover state empty_slot up)
        )
    )
    ;; sort the state tree
    (sort-state-tree state_tree) 
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function Name    :   tile-mover
;; Purpose          :   moves the empty slot and pushes to state_tree if not 
;;                  :   traversed
;; Parameters       :   to_move: the state in which move has to be performed
;;                  :   empty: empty slot
;;                  :   tile: the tile which replaces empty slot
;; Returns          :   none
;; Calls to         :   is-traversed
;; Called from      :   find-all-moves 
;; Method           :   1. copy the state's node value
;;                  :   2. move the empty slot
;;                  :   3. intialize traversed flag with 0
;;                  :   4. check for the state not traversed
;;                  :   5. if not traversed
;;                  :       5a. create and push node to state_tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(defun tile-mover(to_move empty tile)
    ;; copy the state_node value of the state
	(setf copy_state (copy-seq (state_node-state to_move)))
    ;; exchnage the values		
	(rotatef (svref copy_state empty) (svref copy_state tile))	
    ;; initialize the traversed flag	
	(setf traversed_flag 0)
    ;; check if state is traversed
	(is-traversed copy_state)
    ;; if this is not a traversed state push it to state tree					
	(if (= traversed_flag 0)						
		(vector-push-extend (make-state_node 
                                :state copy_state 
                                :id (length state_tree) 
		                        :g (+ 1 (state_node-g to_move)) 
                                :h (determine-heuristic copy_state) 
		                        :f (+ (+ 1 (state_node-g to_move)) 
                                    (determine-heuristic copy_state))) 
                                    state_tree
        )
    )
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function Name    :   is-traversed
;; Purpose          :   to check if this is a traversed state
;; Parameters       :   to_check: state to be checked
;; Returns          :   none
;; Calls to         :   none
;; Called from      :   tile-mover 
;; Method           :   1. for every state in traversed states
;;                  :       1a. check the state to be equal 
;;                  :           1a.a. if equal then set the flag
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(defun is-traversed (to_check)
    ;; loop for every traversed state
    (loop for i from 0 to (1- (length traversed_states)) do
        ;; if this sate to be checked is in traversed state
		(if (equalp to_check (aref traversed_states i))	
            ;; set the traversed flag
		    (setf traversed_flag 1)
        )
    )
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function Name    :   is-unsolvable
;; Purpose          :   to check a state to be in unsolvable states list
;; Parameters       :   state to be checked
;; Returns          :   none
;; Calls to         :   none
;; Called from      :   puzzle-solver 
;; Method           :   1. for every state in unsolvable states
;;                  :       1a. check with the state to be checked
;;                  :           1a.a. if equal set the flag
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(defun is-unsolvable (to_check)
    ;; loop for every unsolvable state
    (loop for i from 0 to (1- (length unsolvable_states)) do
        ;; if the state to be checked is in unsolvable state
	    (if (equalp to_check (aref unsolvable_states i))	
            ;; set the unsolvable flag	
		    (setf unsolvable_flag 1)
        )
    )
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function Name    :   determine-heuristic
;; Purpose          :   calculates the heuristic
;; Parameters       :   to_calculate: the state whose heuristic has to be
;;                  :                 calculated
;; Returns          :   hr: the heuristic
;; Calls to         :   
;; Called from      :   initialize_puzzle, tile_mover, solve-npuzzle
;; Method           :   1. intialize heuristic to 0
;;                  :   2. loop for 0 to size of puzzle - 1
;;                  :       2a. if this is not the empty slot
;;                  :           2a.a. if misplaced tile then increment heuristic
;;                  :   3. return the heuristic value
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(defun determine-heuristic (to_calculate)				
    ;; initialize the heurisitic value to zero 
	(setq hr 0)
    (loop for i from 0 to (1- size_n) do
        ;; if not the empty slot
		(if(/= 0 (aref to_calculate i))
            (progn
                ;; if the tile is not at its correct position increment the
                ;; value of heuristic. A misplaced tile is one whose value
                ;; is not equal to its position+1. 
		        (if(/= (+ 1 i) (aref to_calculate i))
			        (incf hr)
                )
            )
        )   
    )	
    hr  ;; return the heuristic
)						



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function Name    :   sort-state-tree
;; Purpose          :   to sort the given state tree on value of f as in A* algo
;; Parameters       :   unsorted array of states
;; Returns          :   sorted array of states
;; Calls to         :   none
;; Called from      :   find-all-moves 
;; Method           :   1. sort the given state array on the basis of f value
;;                  :   2. return the sorted array
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(defun sort-state-tree (unsorted_array) 
    ;; temp array
    (setq sorted_array unsorted_array)
    ;; loop from the end to the begining of the array
    (loop for i from (1- (length state_tree)) downto 0 do
        ;; loop from 0 to this current position
        (loop for j from 0 to i
            when (> (state_node-f (aref sorted_array i)) (state_node-f 
                                            (aref sorted_array j))
                 ) do 
                 ;; swap the elements
                 (rotatef (aref sorted_array i) (aref sorted_array j))
        )
    )
    ;; return the sorted array
    sorted_array
)