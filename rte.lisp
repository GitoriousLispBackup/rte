;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Time-stamp: <2009-11-05 11:51:19 paul>
;;
;;  TODO:
;;    - 
;;    - 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;
;;  Define some variables we may need later
;;;;
(defvar *sfy-grammar-script* 
  "/home/lingrad/pmheider/research/snalps/grammar_scripts/general_script.lisp")

;;;;
;;  Pre-load Sfy
;;;;
(if (not (find-package 'sfy))
    (load *sfy-grammar-script*))

;; Sax XML parser from Allegro.

(in-package :cl-user)
(require :sax)
(use-package :net.xml.sax)

(defclass rte-sax-parser (sax-parser)
  ((pairs :accessor pairs :initform '())
   (pair-id :accessor current-pair-id)
   (ground-truth :accessor current-ground-truth)
   (task :accessor current-task)
   (length :accessor current-length)
   (text :accessor current-text)
   (hypothesis :accessor current-hyp)
   (do-append :accessor current-append :initform nil)))

(defun extract-pairs (&optional 
		      (dataset-filename "data/RTE3_pairs_dev-set-final.xml"))
  ;; Formatting based on data/RTE_dev.dtd
  ; id                 ;; CDATA #REQUIRED
  ; entailment   ;; (YES|NO) #REQUIRED
  ; task   ;; (IR|IE|QA|SUM) #REQUIRED
  ; length  ;; (LONG|SHORT) #REQUIRED 
  ; t        ;; (#PCDATA)
  ; h ;; (#PCDATA)

  (multiple-value-bind (success parser)
		       (sax-parse-file dataset-filename :class 'rte-sax-parser)
		       (pairs parser)))

(defun aval (key lst)
  "Get the value associated with the string KEY in association list LST."
  (cdr (assoc key lst :test #'string=)))

(defmethod start-element ((parser rte-sax-parser) iri localname qname attrs)
  (declare (ignore iri localname))
  (cond
   ((string= qname "pair")
    (let ((id (parse-integer (aval "id" attrs)))
          (truth (aval "entailment" attrs))
          (task (aval "task" attrs))
          (length (aval "length" attrs)))
      (setf (current-pair-id parser) id
            (current-ground-truth parser) truth
            (current-task parser) task
            (current-length parser) length
            (current-text parser) ""
            (current-hyp parser) ""
            (current-append parser) nil)))
   ((string= qname "t") (setf (current-append parser) 'text))
   ((string= qname "h") (setf (current-append parser) 'hyp))))

(defmethod end-element ((parser rte-sax-parser) iri localname qname)
  (declare (ignore iri localname))
  (cond
   ((string= qname "pair")
    (with-slots (pair-id ground-truth task length text hypothesis) parser
                (let ((pair-data `(,pair-id
                                   ,ground-truth
                                   ,task
                                   ,length
                                   ,text
                                   ,hypothesis)))
                  (push pair-data (pairs parser)))))
   ((string= qname "t") (setf (current-append parser) nil))
   ((string= qname "h") (setf (current-append parser) nil))))

(defun main ()

  (let (t-h-pairs (extract-pairs))

    (loop for t-h-pair in t-h-pairs
 	  do

	  ;;  Formatting based on data/RTE_dev.dtd
	  ;;;; TODO - turn this into destructuring-bind
 	  (let ((id (car t-h-pair))                 ;; CDATA #REQUIRED
 		(entailment (car (cdr t-h-pair)))   ;; (YES|NO) #REQUIRED
 		(task (car (cdr (cdr t-h-pair))))   ;; (IR|IE|QA|SUM) #REQUIRED
 		(length 
 		 (car (cdr (cdr (cdr t-h-pair)))))  ;; (LONG|SHORT) #REQUIRED 
 		(text 
 		 (car (cdr (cdr (cdr (cdr t-h-pair))))))       ;; (#PCDATA)
 		(hypothesis 
		 (car (cdr (cdr (cdr (cdr (cdr t-h-pair))))))) ;; (#PCDATA)
 		)

	    ;; Deal with the text
	    ;;;; TODO - what happens when there is no parse?
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

;; 		      (sneps::??? (format nil "~A" native-lkb-frame))

		    )
		
	      ) ;; Done with the hypothesis

 	    ) ;; Done with a particular t-h pair's LET 

 	  );; Done looping through the t-h pairs
    
    ))
