(require 'sb-bsd-sockets)

(defclass lispterm-output (fundamental-character-output-stream)
  ((socket 
    :initarg :socket)))

(defmethod stream-write-char ((stream lispterm-output)
			      char)
  (let ((code (char-code char))
	(buf (make-array 5
			 :element-type '(unsigned-byte 8)))
	(len 0))
    (cond ((< code 128)
	   (setf (aref buf 0) code)
	   (setf len 1))
	  ((< code #x800)
	   (setf (aref buf 0) (logior #xc0 (logand (ash code  -6) #x1f)))
	   (setf (aref buf 1) (logior #x80 (logand (ash code   0) #x3f)))
	   (setf len 2))
	  ((< code #x10000)
	   (setf (aref buf 0) (logior #xe0 (logand (ash code -12) #x1f)))
	   (setf (aref buf 1) (logior #x80 (logand (ash code  -6) #x1f)))
	   (setf (aref buf 2) (logior #x80 (logand (ash code   0) #x0f)))
	   (setf len 3))
	  ((< code #x10ffff)
	   (setf (aref buf 0) (logior #xf0 (logand (ash code -18) #x1f)))
	   (setf (aref buf 1) (logior #x80 (logand (ash code -12) #x1f)))
	   (setf (aref buf 2) (logior #x80 (logand (ash code  -6) #x1f)))
	   (setf (aref buf 3) (logior #x80 (logand (ash code   0) #x07)))
	   (setf len 4))
	  (t
	   (setf (aref buf 0) (char-code #\?))
	   (setf len 1)))
    (sb-bsd-sockets:socket-send (slot-value stream 'socket) buf len)))

(defclass lispterm ()
  ((socket
    :initarg :socket)
   (thread
    :initarg :tread)))

(defun lispterm-cleanup ()
  (format t "killing old lispterm threads~%")
  (let ((threads (loop
		    for thread in (sb-thread:list-all-threads)
		    when (string= (sb-thread:thread-name thread)
				  "lispterm")
		    collect thread)))
    (loop for thread in threads
	 do (sb-thread:terminate-thread thread))))
		      
(defvar *sock*)

(defun start-listen ()
  (lispterm-cleanup)

  (let ((listen (make-instance 'sb-bsd-sockets:inet-socket 
			       :type :stream
			       :protocol :tcp)))
    (sb-bsd-sockets:socket-listen listen 1)
    (multiple-value-bind (addr port)
	(sb-bsd-sockets:socket-name listen)
      (declare (ignore addr))
      (format t "listening on port ~d~%" port)
      (with-open-file (outf "PORT"
			    :direction :output
			    :if-exists :supersede)
	(format outf "~d~%" port)))
    (let ((sock (sb-bsd-sockets:socket-accept listen)))
      (setq *sock* sock)
      (sb-thread:make-thread #'(lambda () (lispterm-top-level sock))
			     :name "lispterm"))))

(defvar *out*)

;;; sb-bsd-sockets:socket-make-stream
(defun lispterm-top-level (sock)
  (format t "top level ~s~%" sock)
  (let ((outstream (make-instance 'lispterm-output
				  :socket sock)))
    (let ((*standard-output* outstream))
      (format t "welcome to é lispterm~%"))))

