;;;; -*- Mode: Lisp; -*- 
;;;; Team Members: Solomon, Rina, and Justin 

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
    (remove-duplicates (ancestorsb name tree))))

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

(DEFUN ancestorsb (name tree)
  (LET* ((p (lookup-person name tree))
         (parent1 (person-parent1 p))
         (parent2 (person-parent2 p)))

    (when parent1
       (append (list parent1 parent2)
              (ancestorsb parent1 tree)
              (ancestorsb parent2 tree) ))
  ))


(DEFUN ancestorsOnLevel (name tree number)
  "A helper function for the COUSINS function. 
Returns a list of names (strings or symbols) of the ancestors of NAME in TREE on a certain level. Does not sort names! Does not check if NAME exists as a person in the TREE!"

  (when (>= number -1)
      (LET* ((p (lookup-person name tree))
             (parent1 (person-parent1 p))
             (parent2 (person-parent2 p))
             (ancestorList (list)) )    

      (if (= number -1)
        (push name ancestorList))

      (when parent1
        (setf ancestorList 
              (append ancestorList 
                      (ancestorsOnlevel parent1 tree (- number 1))
                      (ancestorsOnlevel parent2 tree (- number 1)) )))
      (sort ancestorList #'string-lessp )
    )
 ))

(DEFUN ancestorsUpToLevel (name tree number)
  "Returns a list of names of all the ancestors 
up to a level of NAME in TREE. Does not sort names! 
Does not check if NAME exists as a person in the TREE!"

  (when (>= number 0)
      (LET* ((p (lookup-person name tree))
             (parent1 (person-parent1 p))
             (parent2 (person-parent2 p))
             (ancestorList (list)) )    
     
      (push name ancestorList)

      (when parent1
        (setf ancestorList 
              (append ancestorList 
                      (ancestorsUpTolevel parent1 tree (- number 1))
                      (ancestorsUpTolevel parent2 tree (- number 1)) )))
        (sort ancestorList #'string-lessp )
      )
 ))

(DEFUN descendants (name tree)
  "Returns a list of names of all the ancestors of NAME in TREE. 
Does not remove any duplicated names! Does not sort names! Does dynamic type checking
to see whether all the arguments are of the correct types."

  (WHEN (NOT (OR (SYMBOLP name) (STRINGP name)))
    (ERROR "DESCENDANTS called with NAME (~A) that is not a SYMBOL or STRING." name))
  (WHEN (NOT (HASH-TABLE-P tree))
    (ERROR "DESCENDANTS called with TREE (~A) that is not a HASH-TABLE." tree))
  (WHEN (person-exists name tree)
    (descendantsb name tree)))

(DEFUN descendantsb (name tree)
  "A helper function for the DESCENDANTS function. 
Returns a list of names of all the descendants of NAME in TREE. 
Does not sort names! Does not check if NAME exists as a person in the TREE!"

  (LET* ((p (lookup-person name tree)) 
         (children (person-children p))
         (desc (list)) )

    ;children is a list
    ;parse through list and get children of each person in the list

    (when children
      (setf desc (append desc children))
      (loop for x in children 
            do
            (setf desc (append desc (descendantsb x tree)))
      )
    )
    (remove-duplicates desc :test #'equal)

  ))

(DEFUN descendantsOnLevel (name tree number)
 "Returns a list of names  of all the descendants on 
a certain level of NAME in TREE.
Does not check if NAME exists as a person in the TREE!"

  (when (>= number -1)
    (LET* ((p (lookup-person name tree)) 
         (children (person-children p))
         (desc (list)) )

      (if (= number -1)
          (push name desc))

      (when children
        (loop for x in children 
            do
            (setf desc (append desc (descendantsOnLevel x tree (- number 1))))
         )
      )
      (sort desc #'string-lessp )
    )
  )

)

(DEFUN getSibling (name tree)
  
  (LET ( 
        (person (lookup-person name tree))
        )

    ;check if name exists in tree
    (if (not (person-exists name tree))
        (RETURN-FROM getSibling NIL)
      )

    (if (string= (person-parent1 person) NIL)
        (RETURN-FROM getSibling NIL)
      (progn
        (setf result (union
          (loop for child in (person-children (lookup-person (person-parent1 person) tree))
            collect child)
          (loop for child in (person-children (lookup-person (person-parent2 person) tree))
            collect child)))
        (remove name result :test #'STRING= )
      )
    )
  )
)

(DEFUN getCousins (name tree level)
  (LET ((ancestors (ancestorsOnLevel name tree level))
        (cousins (list))
        (siblings (getSibling name tree))
       )

    (loop for x in ancestors 
          do
          (setq cousins (append cousins (descendantsOnLevel x tree level)))
       )

    ;need to remove duplicates, siblings, and name from cousins list
    (setf cousins (remove-duplicates cousins :test #'equal))
    (setf cousins (remove name cousins :test #'STRING= ))
    (loop for x in siblings
          do
          (setf cousins (remove x cousins :test #'STRING= ))
       )
    cousins

    )
  )

(DEFUN isUnrelated (name1 name2 tree)
  (setf result "Yes")

        ;check if they are siblings
        (setf siblings (sort (getsibling name2 tree)#'string-lessp))
        (if (member name1 siblings :test #'STRING=)
            (setf result "No"))

        ;check if name1 and name2 share a common ancestor
        (setf ancestors1 (ancestors name1 tree))
        (setf ancestors2 (ancestors name2 tree))
        (loop for person in ancestors1
             do
            (if (member person ancestors2)
              (setf result "No")
           )
         )
        
        ;get decendants of name1 and check for name2
        (setf descendants1 (sort (descendants name1 tree)#'string-lessp))
        (if (member name2 descendants1 :test #'STRING=)
            (setf result "No"))

        ;get decendants of name2 and check for name1
        (setf descendants2 (sort (descendants name2 tree)#'string-lessp))
        (if (member name1 descendants2 :test #'STRING=)
            (setf result "No"))

        ;print result
        (if (string= result "No")
            (RETURN-FROM isUnrelated "No")
            (RETURN-FROM isUnrelated "Yes")
         )
)
 
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

  (format t "~%X ~{~a ~}" linelist)

  (LET (
        (name1 (lookup-person (nth 0 linelist) tree))
        (relation (nth 1 linelist))
        (name2 (if (string= (nth 1 linelist) "cousin")
                  (lookup-person (nth 3 linelist) tree)
               (lookup-person (nth 2 linelist) tree)))
        )
   
    (when (string= relation "child")
        (setf list (person-children name2) )
        (if (member (nth 0 linelist) list :test #'STRING= )
            (format t "~%~a~%" "Yes")
            (format t "~%~a~%" "No") 
        )
     )

    (when (string= relation "sibling")
       (setf list (getsibling (nth 2 linelist) tree))
       (if (member (nth 0 linelist) list :test #'STRING= )
            (format t "~%~a~%" "Yes")
            (format t "~%~a~%" "No") 
       )
     )

    (when (string= relation "ancestor")
        (setf list (sort (ancestors (nth 2 linelist) tree)#'string-lessp))
        (if (member (nth 0 linelist) list :test #'STRING= )
            (format t "~%~a~%" "Yes")
            (format t "~%~a~%" "No") 
        )
     ) 

    (when (string= relation "unrelated")
        (setf result (isUnrelated (nth 0 linelist) (nth 2 linelist) tree))
        (if (string= result "No" )
            (format t "~%~a~%" "No")
            (format t "~%~a~%" "Yes") 
        )
     )

     ;(when (string= relation "cousin")
     ;    (print "Here")
     ;    )


   ))


;;NOTE: This function needs to be defined by team
(DEFUN handle-W (linelist tree)
  "LINELIST is a LIST of strings. TREE is a hash-table."

  (format t "~%W ~{~a ~}" linelist)

  (LET (
        (relation (nth 0 linelist))
        (name1 (if (string= (nth 0 linelist) "cousin")
                  (lookup-person (nth 2 linelist) tree)
               (lookup-person (nth 1 linelist) tree)))
        )

    (when (string= relation "child")
        (setf list (person-children name1))
        (if (eql list nil)
            (format t "~%None ~%")
            (format t "~%~{~a~%~} " (remove name1 (sort list #'string-lessp)))
         )
     )

     (when (string= relation "sibling")
        (setf list (getsibling (nth 1 linelist) tree))
        (if (eql list nil)
            (format t "~%None ~%")
            (format t "~%~{~a~%~} " (remove name1 (sort list #'string-lessp)))
         )
      )

     (when (string= relation "ancestor")
        (setf list (ancestors (nth 1 linelist) tree))
        (if (eql list nil)
            (format t "~%None ~%")
            (format t "~%~{~a~%~} " (remove name1 (sort list #'string-lessp)))
         )
      )

     (when (string= relation "cousin")
         (print "Here")
         ;(getCousins (nth 2 linelist) (nth 1 linelist) tree)
     )
     

      (when (string= relation "unrelated")
          (setf unrelated (list))
          (loop for key being the hash-keys of tree
                        do
                        (setf result (isUnrelated (nth 1 linelist) key tree))
                        (if (string= result "Yes")
                            (push key unrelated)
                         )
            )

            (if (eql unrelated nil)
                (format t "~%None ~%")
                (format t "~%~{~a~%~} " (remove name1 (sort unrelated #'string-lessp)))
             )          
       )
            
    ))

;;;THE TOP LEVEL FUNCTION OF THE WHOLE PROGRAM
(DEFUN family (stream)
  "This is the top-level function for the whole Lisp program. Reads
each line from the file opened in STREAM."
  
  (LET ((tree (MAKE-HASH-TABLE :size 1000 :test #'equal))
        (line-items (SPLIT-SEQUENCE " " (READ-LINE stream nil "") :test #'equal)))

  (LOOP
   (COND 
    ((string= (first line-items) "E")
     (handle-E (REST line-items) tree))
    ((string= (first line-items) "X")
     (handle-X (REST line-items) tree))
    ((string= (first line-items) "W")
     (handle-W (REST line-items) tree))
    (t (return nil)) ) ;end of file reached
   (SETF line-items (SPLIT-SEQUENCE " " (READ-LINE stream nil "") :test #'equal)))

  ;*(loop for value being the hash-values of tree
  ;      do (print value))
))

;;;A helpful tester function for debugging your tree.
(DEFUN test-tree ()
  (LET ((tree (MAKE-HASH-TABLE :size 1000 :test #'equal)))
    
    ;this is how you sort a response 
    ;(sort(ancestors "Alex" tree)#'string-lessp)
    ;(add-children "ZZZ" (gethash "J" tree))
    ;(print (person-children (gethash "J" tree)))

    ;(SETF p (make-person :name "Barbara" :parent1 "Fred" :parent2 "Carol"))
    ;(print (person-parent1 p))

    (setf input "Jamie Beth Will")
    (setf line-items (SPLIT-SEQUENCE " " input))
    (handle-E line-items tree)

    (setf input "Will Barb Mark")
    (setf line-items (SPLIT-SEQUENCE " " input))
    (handle-E line-items tree)

    (setf input "Will Barb Jason")
    (setf line-items (SPLIT-SEQUENCE " " input))
    (handle-E line-items tree)

    (setf input "Jason Angie Zach")
    (setf line-items (SPLIT-SEQUENCE " " input))
    (handle-E line-items tree)

    (setf input "Jason Angie Cole")
    (setf line-items (SPLIT-SEQUENCE " " input))
    (handle-E line-items tree)
   
    (setf input "Mark Margaret")
    (setf line-items (SPLIT-SEQUENCE " " input))
    (handle-E line-items tree)

    (setf input "Mark Margaret Paul")
    (setf line-items (SPLIT-SEQUENCE " " input))
    (handle-E line-items tree)

    (setf input2 "Mary Mark Justin")
    (setf line-items2 (SPLIT-SEQUENCE " " input2))
    (handle-E line-items2 tree)

    (setf input2 "Mary Mark Jennifer")
    (setf line-items2 (SPLIT-SEQUENCE " " input2))
    (handle-E line-items2 tree)

    (setf input2 "Mary Mark Matthew")
    (setf line-items2 (SPLIT-SEQUENCE " " input2))
    (handle-E line-items2 tree)

    (setf input2 "Matthew Peyton Lily")
    (setf line-items2 (SPLIT-SEQUENCE " " input2))
    (handle-E line-items2 tree)

    (setf query "Mary ancestor Justin")
    (setf line-items (SPLIT-SEQUENCE " " query))
    (handle-X line-items tree)
    
    (setf query "Justin sibling Matthew")
    (setf line-items (SPLIT-SEQUENCE " " query))
    (handle-X line-items tree)

    (setf query "child Justin")
    (setf line-items (SPLIT-SEQUENCE " " query))
    (handle-W line-items tree)

    (setf query "child Mark")
    (setf line-items (SPLIT-SEQUENCE " " query))
    (handle-W line-items tree)

    (setf query "sibling Matthew")
    (setf line-items (SPLIT-SEQUENCE " " query))
    (handle-W line-items tree)

    (setf query "ancestor Matthew")
    (setf line-items (SPLIT-SEQUENCE " " query))
    (handle-W line-items tree)

    (setf query "Justin sibling Justin")
    (setf line-items (SPLIT-SEQUENCE " " query))
    (handle-X line-items tree)
    
    (setf query "unrelated Justin")
    (setf line-items (SPLIT-SEQUENCE " " query))
    (handle-W line-items tree)

    (setf query "Justin unrelated Lily")
    (setf line-items (SPLIT-SEQUENCE " " query))
    (handle-X line-items tree)

    (setf query "Mark unrelated Will")
    (setf line-items (SPLIT-SEQUENCE " " query))
    (handle-X line-items tree)
    
    ;(setf ancestors1  (sort (ancestorsOnLevel "Mark" tree 1) #'string-lessp ))
    ;(print ancestors1)

    ;(print (ancestorsOnLevel "Lily" tree 3))
    ;(print (ancestorsUpToLevel "Lily" tree 3))

    ;(descendantsOnLevel "Jamie" tree 1)

    (getCousins "Zach" tree 1)

    ;(print "here")

    ;(setf descendants1 (sort (descendants "Mark" tree)#'string-lessp))

    ;print the keys of a hash table
    ;*(loop for key being the hash-keys of tree
    ;    do (print key))

    ;*(loop for value being the hash-values of tree
    ;    do (print value))

))
