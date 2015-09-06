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

(defun read-floating-point (file)
  (with-open-file (in file :element-type '(unsigned-byte 8))
	(file-position in 3022)
    (read-value 'u8 in)))

(read-floating-point "c:/dropbox/lisp/dm.xpt")

(defun read-floating-point-sas7bdat (file)
  (with-open-file (in file :element-type '(unsigned-byte 8))
	(file-position in 164)
    (read-value 'u8  in)))

;; (ieee-floats:decode-float64 (read-floating-point-sas7bdat "d:/downloads/b.sas7bdat"))

(defun u8-to-floating-point (bits)
  (declare (type (unsigned-byte 64) bits))
  (let* ((sign (ldb (byte 1 63) bits))
	 (exponent (ldb (byte 7 56) bits))
	 (significand (ldb (byte 56 0) bits)))
    (format t "Bits: ~B~%" bits)
    (format t "Sign: ~a Exponent: ~a Significand: ~a~%" sign exponent significand)
    (format t "Sign: ~B~%Exponent: ~B~%Significand: ~B~%" sign exponent significand)

    (unless (zerop sign)
      (setf significand (- significand)))

    (* (scale-float (float significand) -56) (expt 16 (- exponent 64)))))


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

(floating-point-to-u8 (u8-to-floating-point (read-floating-point "c:/dropbox/lisp/dm.xpt")))

(floating-point-to-u8 -1110023.23)

;; (/ (u8-to-floating-point (read-floating-point-sas7bdat "d:/downloads/b.sas7bdat")) (* 60 60 24))
	  

(u8-to-floating-point (read-floating-point "dm.xpt"))


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




(defun xpt-p (file)
  (and
   (not (directory-pathname-p file))
   (string-equal "xpt" (pathname-type file))))

(xpt-p "dm.xpt")


(define-binary-class sas-header ()
  ((sas-header-record (iso-8859-1-string :length 80))
   (sas-symbol-1 (iso-8859-1-string :length 8))
   (sas-symbol-2 (iso-8859-1-string :length 8))
   (sas-lib (iso-8859-1-string :length 8))
   (sas-ver (iso-8859-1-string :length 8))
   (sas-os (iso-8859-1-string :length 8))
   (blanks-1 (iso-8859-1-string :length 24))
   (sas-create (iso-8859-1-string :length 16))
   (sas-modify (iso-8859-1-string :length 16))
   (blanks-2 (iso-8859-1-string :length 64))))

(define-binary-class member-header ()
  ((member-header-record (iso-8859-1-string :length 80))
   (dscrptr-header-record (iso-8859-1-string :length 80))
   (sas-symbol (iso-8859-1-string :length 8))
   (sas-dsname (iso-8859-1-string :length 8))
   (sas-data (iso-8859-1-string :length 8))
   (sas-ver (iso-8859-1-string :length 8))
   (sas-os (iso-8859-1-string :length 8))
   (blanks-1 (iso-8859-1-string :length 24))
   (sas-create (iso-8859-1-string :length 16))
   (sas-modify (iso-8859-1-string :length 16))
   (blanks-2 (iso-8859-1-string :length 16))
   (dslabel (iso-8859-1-string :length 40))
   (dstype (iso-8859-1-string :length 8))))

(define-binary-class namestr-header ()
  ((namestr-header-record (iso-8859-1-string :length 80))))

(define-binary-class obs-header ()
  ((obs-header-record (iso-8859-1-string :length 80))))

(define-binary-class namestr-data ()
  ((ntype u2)
   (nhfun u2)
   (nlng u2)
   (nvar0 u2)
   (nname (iso-8859-1-string :length 8))
   (nlabel (iso-8859-1-string :length 40))
   (nform (iso-8859-1-string :length 8))
   (nfl u2)
   (nfd u2)
   (nfj u2)
   (nfill (iso-8859-1-string :length 2))
   (niform (iso-8859-1-string :length 8))
   (nifl u2)
   (nifd u2)
   (npos u4)
   (padding (iso-8859-1-string :length 52))))

(defconstant +shsp+ 0 "sas header start positon")
(defconstant +mhsp+ 240 "member header start positon")
(defconstant +nhsp+ 560 "namestr header start positon")
(defconstant +ndsp+ 640 "namestr data start postion")


(defun read-sas-header (file)
  (with-open-file (in file :element-type '(unsigned-byte 8))
    (read-value 'sas-header in)))

(defun read-member-header (file)
  (with-open-file (in file :element-type '(unsigned-byte 8))
	(file-position in +mhsp+)
    (read-value 'member-header in)))

(defun read-namestr-header (file)
  (with-open-file (in file :element-type '(unsigned-byte 8))
	(file-position in +nhsp+)
    (read-value 'namestr-header in)))

(defun xpt-variables-number (file)
  (parse-integer (subseq (namestr-header-record (read-namestr-header file)) 54 58)))

(defun read-namestr-data (file)
  (with-open-file (in file :element-type '(unsigned-byte 8))
	(file-position in +nhsp+)

	(setf variables-number 
		  (parse-integer (subseq (namestr-header-record (read-value 'namestr-header in)) 54 58)))

	(loop for i from 1 to variables-number
	   collect (read-value 'namestr-data in))))

(defun xpt-variables-names (file)
  (mapcar #'nname (read-namestr-data file)))

(defun xpt-variables-labels (file)
  (mapcar #'nlabel (read-namestr-data file)))

(defun xpt-variables-types (file)
  (mapcar #'ntype (read-namestr-data file)))

(defun xpt-variables-lengths (file)
  (mapcar #'nlng (read-namestr-data file)))

(defun read-obs-record (file)
  (with-open-file (in file :element-type '(unsigned-byte 8))
	(file-position in +nhsp+)

	(setf variables-number 
		  (parse-integer (subseq (namestr-header-record (read-value 'namestr-header in)) 54 58)))

	(setf collect-namestr-data
		  (loop for i from 1 to variables-number
			 collect (read-value 'namestr-data in)))

	(read-value 'obs-header in)

	(setf file-length (file-length in))

	(setf obs-length (apply #'+ (mapcar #'nlng collect-namestr-data)))
	;; (format t "~a~%" obs-length)

	(loop while (>= (- file-length (file-position in)) obs-length)
	   collect (loop
				  for i from 1 to variables-number
				  for ntype in (mapcar #'ntype collect-namestr-data)
				  for nlng in (mapcar #'nlng collect-namestr-data)
				  collect (if (eql ntype 1)
							  (u8-to-floating-point (read-value 'u8 in))
							  (read-value 'iso-8859-1-string in :length nlng))))))

(defun show-sas-header (file slot)
  (format t "~a~%" (funcall slot (read-sas-header file))))

(defun show-member-header (file slot)
  (format t "~a~%" (funcall slot (read-member-header file))))

(show-member-header "dm.xpt" 'dslabel)

(read-sas-header "dm.xpt")

(show-sas-header "dm.xpt" 'sas-create)

(defun read-xpt (file)
  (with-open-file (in file :element-type '(unsigned-byte 8))
    (read-value 'sas-header in)

	(read-value 'member-header in)

	(setf variables-number 
		  (parse-integer (subseq (namestr-header-record (read-value 'namestr-header in)) 54 58)))

	(setf collect-namestr-data
		  (loop for i from 1 to variables-number
			 collect (read-value 'namestr-data in)))

	(read-value 'obs-header in)

	(setf file-length (file-length in))

	(setf obs-length (apply #'+ (mapcar #'nlng collect-namestr-data)))
	;; (format t "~a~%" obs-length)

	(loop while (>= (- file-length (file-position in)) obs-length)
	   collect (loop
				  for i from 1 to variables-number
				  for ntype in (mapcar #'ntype collect-namestr-data)
				  for nlng in (mapcar #'nlng collect-namestr-data)
				  collect (if (eql ntype 1)
							  (read-value 'iso-8859-1-string in :length nlng)
							  (read-value 'iso-8859-1-string in :length nlng))))))

(print (read-xpt "dm.xpt"))
