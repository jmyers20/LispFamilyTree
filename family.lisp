;;;; -*- Mode: Lisp; -*- 
;;;; Team Members: Justin 

(DEFSTRUCT (person
            (:print-function print-person))
  (parent1 NIL)  ; a symbol or string or NIL
  (parent2 NIL)  ; a symbol or string or NIL
  (children NIL) ; a list or NIL
  (name NIL))    ; a symbol or string or NIL


(DEFUN print-person (item stream depth)
  "A helper function for Lispworks to be able to show you what is
in a person structure concisely."
    (DECLARE (IGNORE depth))
    (FORMAT stream "#<P name:~S p1:~S p2:~S ch:~S>"
            (person-name item) (person-parent1 item) (person-parent2 item) (person-children item))
    item)


;;;NOTE: This function is complete. No need to change it.
(DEFUN lookup-person (name tree)
  "Returns a PERSON structure corresponding to the key NAME in the hashtable TREE.
NAME must be a STRING or a SYMBOL. If there is no one in the tree with the name
in NAME, returns NIL."
  (GETHASH name tree nil))


;;;NOTE: This function is complete. No need to change it.
(DEFUN person-exists (name tree)
  "Returns T when the key NAME has an actual person struct stored in TREE.
Returns NIL (false) otherwise."
  (WHEN (lookup-person name tree)
    t))


;;;NOTE: This function is complete. No need to change it.
(DEFUN ancestors (name tree)
  "Returns a list of names (strings or symbols) of all the ancestors of NAME in TREE. 
Does not remove any duplicated names! Does not sort names! Does dynamic type checking
to see whether all the arguments are of the correct types."
  (WHEN (NOT (OR (SYMBOLP name) (STRINGP name)))
    (ERROR "ANCESTORS called with NAME (~A) that is not a SYMBOL or STRING." name))
  (WHEN (NOT (HASH-TABLE-P tree))
    (ERROR "ANCESTORS called with TREE (~A) that is not a HASH-TABLE." tree))
  (WHEN (person-exists name tree)
    (ancestorsb name tree)))


;;;------------------------------------------------
;;; TEAM SHOULD PUT ALL NEW HELPER FUNCTION
;;; DEFINITIONS BELOW THIS COMMENT
;;;------------------------------------------------ 

(DEFUN add-person (name struct tree)
  "This should enter the person structure in STRUCT into
the hashtable in TREE with the key in NAME."
  (WHEN (NOT (HASH-TABLE-P tree))
    (ERROR "STORE-PERSON called with TREE (~A) that is not a HASH-TABLE." tree))
  (WHEN (NOT (person-p struct))
    (ERROR "STORE-PERSON called with STRUCT (~A) that is not a PERSON structure." struct))
  (WHEN (NOT (OR (SYMBOLP name) (STRINGP name)))
    (ERROR "STORE-PERSON called with NAME (~A) that is not a SYMBOL or a STRING." name))

    (setf (gethash name tree) struct)
       name)


;;This function needs to be defined by your team.
(DEFUN ancestorsb (name tree)
  "A helper function for the ANCESTORS function. 
Returns a list of names (strings or symbols) of all the ancestors of NAME in TREE. 
Does not remove any duplicated names! Does not sort names! Does not check if NAME 
exists as a person in the TREE!"

;if person is not in the tree, it returns NIL

  (LET* ((p (lookup-person name tree))
         (parent1 (person-parent1 p))
         (parent2 (person-parent2 p)))

    (when parent1
      (append (list parent1 parent2)
              (ancestorsb parent1 tree)
              (ancestorsb parent2 tree)))
  ))


;;NOTE: This function needs to be defined by team   
(DEFUN handle-E (linelist tree)
  "LINELIST is a LIST of strings. TREE is a hash-table."
  (LET (
         (parent1 (nth 0 linelist))
         (parent2 (nth 1 linelist))
         (child (nth 2 linelist))
        )

    ;if the parents do not exist, create them
    (if (not (lookup-person parent1 tree))
      (add-person parent1 (make-person :name parent1 :parent1 nil :parent2 nil :children nil) tree))
    (if (not (lookup-person parent2 tree))
      (add-person parent2 (make-person :name parent2 :parent1 nil :parent2 nil :children nil) tree)) 

    ;check to see if there is a <name3> in E <name1> <name2> <name3>
    (when (eql (list-length linelist) 3)
      ;if the child does not exist, create the child
      (if (not (lookup-person child tree))
          (add-person child (make-person :name child :parent1 nil :parent2 nil :children nil) tree)) 

       ;add parent1 and parent2 to children's parents
      (setf (person-parent1 (lookup-person child tree)) parent1)
      (setf (person-parent2 (lookup-person child tree)) parent2)

      ;add child to parent1 and parent2's children
      (setf (person-children (lookup-person parent1 tree)) (remove-duplicates(append (person-children (lookup-person parent1 tree)) (list child))))
      (setf (person-children (lookup-person parent2 tree)) (remove-duplicates(append (person-children (lookup-person parent2 tree)) (list child)))) 
    )

  ))


;;NOTE: This function needs to be defined by team
(DEFUN handle-X (linelist tree)
  "LINELIST is a LIST of strings. TREE is a hash-table."
  (LET (
        (name1 (lookup-person (nth 0 linelist) tree))
        (relation (nth 1 linelist))
        (name2 (if (string= (nth 1 linelist) "cousin")
                  (lookup-person (nth 3 linelist) tree)
               (lookup-person (nth 2 linelist) tree)))
        )

    (if (or (eql name1 nil) (eql name2 nil) ) 
         (RETURN-FROM handle-X nil)
       )
    
    (when (string= relation "child")
        (setf list (person-children name2) )
        (if (member (nth 0 linelist) list :test #'STRING= )
            (format t "~%~a" "Yes")
            (format t "~%~a" "No") 
        )
    )

    (when (string= relation "sibling")
       (print "Here")

    )

    (when (string= relation "ancestor")
        (setf list (sort (ancestors (nth 2 linelist) tree)#'string-lessp))
        (if (member (nth 0 linelist) list :test #'STRING= )
            (format t "~%~a" "Yes")
            (format t "~%~a" "No") 
        )
     ) 

    ;(when (string= relation "cousin")
    ;    (print "Here")
    ;    )
    ;(when (string= relation "unrelated")
    ;    (print "Here")
    ;    )


   ))


;;NOTE: This function needs to be defined by team
(DEFUN handle-W (linelist tree)
  "LINELIST is a LIST of strings. TREE is a hash-table."
  (LET ()
    ;;body of function goes here

    ))

;;;------------------------------------------------
;;; TEAM SHOULD PUT ALL NEW HELPER FUNCTION
;;; DEFINITIONS ABOVE THIS COMMENT
;;;------------------------------------------------ 




;;;THE TOP LEVEL FUNCTION OF THE WHOLE PROGRAM
;;NOTE: This function is complete.
(DEFUN family (stream)
  "This is the top-level function for the whole Lisp program. Reads
each line from the file opened in STREAM."
  
  (LET ((tree (MAKE-HASH-TABLE :size 1000 :test #'equal))
        (line-items (SPLIT-SEQUENCE " " (READ-LINE stream nil "") :test #'equal)))
 
  (LOOP
   (CASE (FIRST line-items)
     ("E" (handle-E (REST line-items) tree))
     ("W" (handle-W (REST line-items) tree))
     ("X" (handle-X (REST line-items) tree))
     (t (RETURN NIL))) ; end of file reached
   (SETF line-items (SPLIT-SEQUENCE " " (READ-LINE stream nil "") :test #'equal)))

   *(loop for value being the hash-values of tree
        do (print value))
))


;;How Dr. Klassner and Jenish will test your code in the Listener:
;;
;;(family (open "~/Documents/School/CSC\ 1800-002/Projects/Project3/tests/test.txt"))
;;
;; NOTE: The FilePath for OPEN is just an example. 
;; Use your own laptop directory to where you keep
;; your project's test files.


;;;A helpful tester function for debugging your tree.
(DEFUN test-tree ()
  (LET ((tree (MAKE-HASH-TABLE :size 1000 :test #'equal)))
    
    ;this is how you sort a response 
    ;(sort(ancestors "Alex" tree)#'string-lessp)
    ;(add-children "ZZZ" (gethash "J" tree))
    ;(print (person-children (gethash "J" tree)))

    ;(SETF p (make-person :name "Barbara" :parent1 "Fred" :parent2 "Carol"))
    ;(print (person-parent1 p))

    ;(linelist tree)

    (setf input "Jamie Beth Will")
    (setf line-items (SPLIT-SEQUENCE " " input))
    (handle-E line-items tree)

    (setf input "Will Barb Mark")
    (setf line-items (SPLIT-SEQUENCE " " input))
    (handle-E line-items tree)
   
    (setf input "Mark Margaret")
    (setf line-items (SPLIT-SEQUENCE " " input))
    (handle-E line-items tree)

    (setf input2 "Mary Mark Justin")
    (setf line-items2 (SPLIT-SEQUENCE " " input2))
    (handle-E line-items2 tree)

    (setf query "Margaret ancestor Justin")
    (setf line-items (SPLIT-SEQUENCE " " query))
    (handle-X line-items tree)
    
    (setf query "Mark child Will")
    (setf line-items (SPLIT-SEQUENCE " " query))
    (handle-X line-items tree)
    
    ;print the keys of a hash table
    ;*(loop for key being the hash-keys of tree
    ;    do (print key))

    ;*(loop for value being the hash-values of tree
    ;    do (print value))

))
