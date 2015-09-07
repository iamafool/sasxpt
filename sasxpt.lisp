(in-package :cl-user)

(push #p"c:/dropbox/lisp/practicals-1.0.3/Chapter08/" asdf:*central-registry*)
(push #p"c:/dropbox/lisp/practicals-1.0.3/Chapter24/" asdf:*central-registry*)
(push #p"c:/dropbox/lisp/practicals-1.0.3/Chapter15/" asdf:*central-registry*)

(asdf:oos 'asdf:load-op :binary-data)
(asdf:oos 'asdf:load-op :pathnames)

(ql:quickload "ieee-floats")

(defpackage :sasxpt
  (:use :common-lisp
        :com.gigamonkeys.binary-data
        :com.gigamonkeys.pathnames))

(in-package :sasxpt)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A few basic types

(define-binary-type unsigned-integer (bytes bits-per-byte)
  (:reader (in)
           (loop with value = 0
              for low-bit downfrom (* bits-per-byte (1- bytes)) to 0 by bits-per-byte do
                (setf (ldb (byte bits-per-byte low-bit) value) (read-byte in))
              finally (return value)))
  (:writer (out value)
           (loop for low-bit downfrom (* bits-per-byte (1- bytes)) to 0 by bits-per-byte
              do (write-byte (ldb (byte bits-per-byte low-bit) value) out))))

(define-binary-type u2 () (unsigned-integer :bytes 2 :bits-per-byte 8))
(define-binary-type u4 () (unsigned-integer :bytes 4 :bits-per-byte 8))
(define-binary-type u8 () (unsigned-integer :bytes 8 :bits-per-byte 8))

(defun u8-to-floating-point (bits)
  (declare (type (unsigned-byte 64) bits))
  (let* ((sign (ldb (byte 1 63) bits))
         (exponent (ldb (byte 7 56) bits))
         (significand (ldb (byte 56 0) bits)))
    ;; (format t "Bits: ~B~%" bits)
    ;; (format t "Sign: ~a Exponent: ~a Significand: ~a~%" sign exponent significand)
    ;; (format t "Sign: ~B~%Exponent: ~B~%Significand: ~B~%" sign exponent significand)
    (unless (zerop sign)
      (setf significand (- significand)))
    (* (scale-float (float significand) -56) (expt 16 (- exponent 64)))))

;; (u8-to-floating-point 4776067404826411008) ;; 72.0

(defun floating-point-to-u8 (float)
  (multiple-value-bind (significand exponent sign) (decode-float float)
    (let ((significand (scale-float significand (nth-value 1 (ceiling exponent 4))))
          (exponent (+ (ceiling exponent 4) 64))
          (sign (if (= sign 1.0) 0 1)))
      ;; (format t "Sign: ~a Exponent: ~a Significand: ~a~%" sign exponent significand)
      (setf significand (round (* (expt 2 56) significand)))
      ;; (format t "Sign: ~a Exponent: ~a Significand: ~a~%" sign exponent significand)
      ;; (format t "Sign: ~B~%Exponent: ~B~%Significand: ~B~%" sign exponent significand)
      (let ((bits 0))
        (declare (type (unsigned-byte 64) bits))
        (setf (ldb (byte 1 63) bits) sign
              (ldb (byte 7 56) bits) exponent
              (ldb (byte 56 0) bits) significand)
        ;; (format t "Bits: ~B~%" bits)
        bits))))

;; (floating-point-to-u8 -1110023.23)
;; (floating-point-to-u8 72.0) 


;;; Strings
(define-binary-type generic-string (length character-type)
  (:reader (in)
           (let ((string (make-string length)))
             (dotimes (i length)
               (setf (char string i) (read-value character-type in)))
             string))
  (:writer (out string)
           (dotimes (i length)
             (write-value character-type out (char string i)))))

(define-binary-type generic-terminated-string (terminator character-type)
  (:reader (in)
           (with-output-to-string (s)
             (loop for char = (read-value character-type in)
                until (char= char terminator) do (write-char char s))))
  (:writer (out string)
           (loop for char across string
              do (write-value character-type out char)
              finally (write-value character-type out terminator))))

;;; ISO-8859-1 strings

(define-binary-type iso-8859-1-char ()
  (:reader (in)
           (let ((code (read-byte in)))
             (or (code-char code)
                 (error "Character code ~d not supported" code))))
  (:writer (out char)
           (let ((code (char-code char)))
             (if (<= 0 code #xff)
                 (write-byte code out)
                 (error "Illegal character for iso-8859-1 encoding: character: ~c with code: ~d" char code)))))

(define-binary-type iso-8859-1-string (length)
  (generic-string :length length :character-type 'iso-8859-1-char))

(define-binary-type iso-8859-1-terminated-string (terminator)
  (generic-terminated-string :terminator terminator :character-type 'iso-8859-1-char))


;; XPT
(define-binary-class xpt-header ()
  (;; The first header record
   (xpt-header-record (iso-8859-1-string :length 80)) ;HEADER RECORD*******LIBRARY HEADER RECORD!!!!!!!!000000000000000000000000000000
   ;; The first real header record
   (sas-symbol-1 (iso-8859-1-string :length 8)) ;specifies 'SAS     '
   (sas-symbol-2 (iso-8859-1-string :length 8)) ;specifies 'SAS     '
   (sas-lib (iso-8859-1-string :length 8))      ;specifies 'SASLIB  '
   (sas-ver (iso-8859-1-string :length 8))      ;specifies the version of the SAS(r) System under which the file was created.
   (sas-os (iso-8859-1-string :length 8))       ;specifies the operating system that creates the record.
   (blanks-1 (iso-8859-1-string :length 24))    ;blanks
   (sas-create (iso-8859-1-string :length 16))  ;specifies the date and time created, formatted as ddMMMyy:hh:mm:ss.
   ;; The second real header record
   (sas-modify (iso-8859-1-string :length 16)) ;the datetime modified
   (blanks-2 (iso-8859-1-string :length 64))   ;blanks
   ))

(define-binary-class member-header ()
  ((member-header-record (iso-8859-1-string :length 80))  ;HEADER RECORD*******MEMBER  HEADER RECORD!!!!!!!000000000000000001600000000140
   (dscrptr-header-record (iso-8859-1-string :length 80)) ;HEADER RECORD*******DSCRPTR HEADER RECORD!!!!!!!000000000000000000000000000000
   (sas-symbol (iso-8859-1-string :length 8))             ;specifies 'SAS     '
   (sas-dsname (iso-8859-1-string :length 8))             ;specifies the data set name
   (sas-data (iso-8859-1-string :length 8))               ;is SASDATA
   (sas-ver (iso-8859-1-string :length 8))                ;specifies the version of the SAS System under which the file was created.
   (sas-os (iso-8859-1-string :length 8))                 ;specifies the operating system. 
   (blanks-1 (iso-8859-1-string :length 24))              ;blanks
   (sas-create (iso-8859-1-string :length 16))            ;the datetime created
   (sas-modify (iso-8859-1-string :length 16))            ;the datetime modified
   (blanks-2 (iso-8859-1-string :length 16))              ;blanks
   (dslabel (iso-8859-1-string :length 40))               ;a blank-padded data set label 
   (dstype (iso-8859-1-string :length 8))                 ;the blank-padded data set type
   ))

(define-binary-class namestr-header ()
  ;; HEADER RECORD*******NAMESTR HEADER RECORD!!!!!!!000000xxxx00000000000000000000
  ;; xxxx is the number of variables in the data set
  ((namestr-header-record (iso-8859-1-string :length 80))))

(defgeneric xpt-variables-number (type)
  (:documentation "The number of variables in XPT file."))

(defmethod xpt-variables-number ((type namestr-header))
  (parse-integer (subseq (namestr-header-record type) 54 58)))


(define-binary-class obs-header ()
  ;; HEADER RECORD*******OBS     HEADER RECORD!!!!!!!000000000000000000000000000000
  ((obs-header-record (iso-8859-1-string :length 80))))

(define-binary-class namestr-data ()
  ((ntype u2)                           ;VARIABLE TYPE: 1=NUMERIC, 2=CHAR
   (nhfun u2)                           ;HASH OF NNAME (always 0)
   (nlng u2)                            ;LENGTH OF VARIABLE IN OBSERVATION
   (nvar0 u2)                           ;VARNUM
   (nname (iso-8859-1-string :length 8)) ;NAME OF VARIABLE
   (nlabel (iso-8859-1-string :length 40)) ;LABEL OF VARIABLE
   (nform (iso-8859-1-string :length 8))   ;NAME OF FORMAT
   (nfl u2)                                ;FORMAT FIELD LENGTH OR 0
   (nfd u2)                                ;FORMAT NUMBER OF DECIMALS
   (nfj u2)                                ;0=LEFT JUSTIFICATION, 1=RIGHT JUST
   (nfill (iso-8859-1-string :length 2))   ;UNUSED, FOR ALIGNMENT AND FUTURE
   (niform (iso-8859-1-string :length 8))  ;NAME OF INPUT FORMAT
   (nifl u2)                               ;INFORMAT LENGTH ATTRIBUTE
   (nifd u2)                               ;INFORMAT NUMBER OF DECIMALS
   (npos u4)                               ;POSITION OF VALUE IN OBSERVATION
   (padding (iso-8859-1-string :length 52)) ;remaining fields are irrelevant
   ))


(define-binary-type namestr-records (no)
  ;; Each namestr field is 140 bytes long, but the fields are streamed together and 
  ;; broken in 80-byte pieces. If the last byte of the last namestr field does not
  ;; fall in the last byte of the 80-byte record, the record is padded with ASCII
  ;; blanks to 80 bytes.  
  (:reader (in)
           (loop with to-read = no
              while (plusp to-read)
              for namestr-rec = (read-value 'namestr-data in)
              while namestr-rec
              do (decf to-read)
              collect namestr-rec))
  (:writer (out namestr-recs)
           (loop with to-write = no
              for namestr-rec in namestr-recs
              do (write-value 'namestr-data out namestr-rec)
                (decf to-write))))


(defun obs-length (namestr-records)
  (apply #'+ (mapcar #'nlng namestr-records)))

;; (obs-length (namestr-records (read-xpt "dm.xpt")))


(define-binary-type obs-records (obs-length variables-number list-ntype list-nlng)
  ;; Data records are streamed in the same way that namestrs are. There is ASCII blank padding at 
  ;; the end of the last record if necessary. There is no special trailing record.
  (:reader (in)
           (loop while (>= (- (file-length in) (file-position in)) obs-length)
              collect (loop
                         for i from 1 to variables-number
                         for ntype in list-ntype 
                         for nlng in list-nlng
                         collect (if (eql ntype 1)
                                     (u8-to-floating-point (read-value 'u8 in))
                                     (read-value 'iso-8859-1-string in :length nlng)))))
  (:writer (out obs-records)
           (loop for obs-record in obs-records
              do
                (loop
                   for i from 1 to variables-number
                   for ntype in list-ntype
                   for nlng in list-nlng
                   do
                     (if (eql ntype 1)
                         (write-value 'u8 out (floating-point-to-u8 (nth (1- i) obs-record)))
                         (write-value 'iso-8859-1-string out (nth (1- i) obs-record)))))))

(define-binary-class sasxpt ()
  ((xpt-header xpt-header)
   (member-header member-header)
   (namestr-header namestr-header)
   (namestr-records (namestr-records :no (xpt-variables-number namestr-header)))
   (obs-header obs-header)
   (obs-records (obs-records :obs-length (obs-length namestr-records)
                             :variables-number (xpt-variables-number namestr-header)
                             :list-ntype (mapcar #'ntype namestr-records)
                             :list-nlng (mapcar #'nlng namestr-records)))))


(defmethod xpt-variables-number ((type sasxpt))
  (parse-integer (subseq (namestr-header-record (namestr-header type)) 54 58)))

;; (defconstant +shsp+ 0 "sas header start positon")
;; (defconstant +mhsp+ 240 "member header start positon")
;; (defconstant +nhsp+ 560 "namestr header start positon")
;; (defconstant +ndsp+ 640 "namestr data start postion")


(defun read-xpt (file)
  (with-open-file (in file :element-type '(unsigned-byte 8))
    (read-value 'sasxpt in)))

(defmethod print-object ((object sasxpt) stream)
  (let ((default-width 8)
        (var-widths (mapcar #'nlng (namestr-records (read-xpt "dm.xpt"))))
        (var-names (mapcar #'nname (namestr-records (read-xpt "dm.xpt")))))
    (format t "Variables:~%")
    (format t "~v@{~A~:*~}~%" 93 "-")
    (format t "~{|~{ ~{~Va|~}~}~%~}"
            (mapcar #'(lambda (r)
                        (mapcar #'(lambda (v)
                                    (list default-width v)) r))
                    (list (list "Variable" "Type" "Len" "Format" "InFormat" "Label                                   "))))

    (format t "~v@{~A~:*~}~%" 93 "-")
    (format t "~{|~{ ~{~Va|~}~}~%~}"
            (mapcar #'(lambda (r)
                        (mapcar #'(lambda (v)
                                    (list default-width v))
                                (list (nname r) (ntype r) (nlng r) (nform r) (niform r) (nlabel r))))
                    (namestr-records (read-xpt "dm.xpt"))))
    (format t "~v@{~A~:*~}~%" 93 "-")

    (format t "~%OBS:~%")
    (format t "~v@{~A~:*~}~%" (+ 1
                                 (* (length var-widths) 2)
                                 (reduce #'+ (mapcar #'(lambda (x) (max default-width x)) var-widths)))
            "-")
    (format t "|~{ ~{~Va|~}~}~%" (mapcar #'(lambda (x y)
                                                 (list (max x default-width) y)) var-widths var-names))
    (format t "~v@{~A~:*~}~%" (+ 1
                                 (* (length var-widths) 2)
                                 (reduce #'+ (mapcar #'(lambda (x) (max default-width x)) var-widths)))
            "-")
    (format t "~{|~{ ~{~Va|~}~}~%~}"
            (mapcar #'(lambda (r) (mapcar #'(lambda (v) (list default-width v)) r)) (obs-records (read-xpt "dm.xpt"))))
    (format t "~v@{~A~:*~}~%" (+ 1
                                 (* (length var-widths) 2)
                                 (reduce #'+ (mapcar #'(lambda (x) (max default-width x)) var-widths)))
            "-")
    ))


;; (read-xpt "dm.xpt")
;; (print (obs-records (read-xpt "dm.xpt")))
;; (xpt-variables-number (read-xpt "dm.xpt"))

