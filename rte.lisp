;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Time-stamp: <2009-11-05 11:51:19 paul>
;;
;;  TODO:
;;    - 
;;    - 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-pairs (&optional 
		      (dataset-filename "data/RTE3_pairs_dev-set-final.xml"))

  ;; Formatting based on data/RTE_dev.dtd
  ; id                 ;; CDATA #REQUIRED
  ; entailment   ;; (YES|NO) #REQUIRED
  ; task   ;; (IR|IE|QA|SUM) #REQUIRED
  ; length  ;; (LONG|SHORT) #REQUIRED 
  ; t        ;; (#PCDATA)
  ; h ;; (#PCDATA)
	
  )

(defun main ()

  (let (t-h-pairs (extract-pairs))

    (loop for t-h-pair in t-h-pairs
	  do

	  ;; Formatting based on data/RTE_dev.dtd
	  (let ((id (car t-h-pair))                 ;; CDATA #REQUIRED
		(entailment (car (cdr t-h-pair)))   ;; (YES|NO) #REQUIRED
		(task (car (cdr (cdr t-h-pair))))   ;; (IR|IE|QA|SUM) #REQUIRED
		(length 
		 (car (cdr (cdr (cdr t-h-pair)))))  ;; (LONG|SHORT) #REQUIRED 
		(t 
		 (car (cdr (cdr (cdr (cdr t-h-pair))))))         ;; (#PCDATA)
		(h (car (cdr (cdr (cdr (cdr (cdr t-h-pair))))))) ;; (#PCDATA)
		)

	    ;; Deal with the text
	    (let* ((lkb-frames (sfy::parse t 'lkb))
		   (native-lkb-frames (sfy::nativize lkb-frames)))

		(loop for native-lkb-frame in native-lkb-frames
		      do

		      (sneps::tell (format nil "~A!" native-lkb-frame))

		      )
		) ;; Done with the text


	    ;; Deal with the hypothesis
	    (let* ((lkb-frames (sfy::parse h 'lkb))
		   (native-lkb-frames (sfy::nativize lkb-frames)))

		(loop for native-lkb-frame in native-lkb-frames
		      do

		      (sneps::??? (format nil "~A" native-lkb-frame))

		      )
		
		) ;; Done with the hypothesis


	    ) ;; Done with a particular t-h pair's LET 

	  );; Done looping through the t-h pairs
    
    ))
