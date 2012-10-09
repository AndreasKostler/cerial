(in-package #:cerial)
(annot:enable-annot-syntax)

@export-accessors
(defclass <serial-posix> (<serial-base>)
  ()
  (:documentation "Serial port class POSIX implementation. Serial port configuration is done with termios and fcntl. Runs on Linux and many other Un*x like systems."))

@export
(defun make-serial-port (&optional port 
                         &key (baudrate 9600)
                           (bytesize 8)
                           (parity :PARITY-NONE)
                           (stopbits 1)
                           (timeout nil)
                           (xonxoff nil)
                           (rtscts nil)
                           (write-timeout nil)
                           (dsrdtr nil)
                           (inter-char-timeout nil))
  (make-instance '<serial-posix>
		 :port port
		 :baudrate baudrate
		 :bytesize bytesize
		 :parity parity
		 :stopbits stopbits
		 :timeout timeout
		 :xonxoff xonxoff
		 :rtscts rtscts
		 :write-timeout write-timeout
		 :dsrdtr dsrdtr
		 :inter-char-timeout inter-char-timeout))

;; TODO: Will be different for different unixes
(defmethod device ((s <serial-posix>) port)
  (declare (ignore s))
  (format nil "/dev/ttyS~A" port))

@export
(defmethod open-serial ((s <serial-posix>))
  (let* ((fd (unistd:open
	      (port s)
	      (logior unistd:ORDWR 
		      unistd:ONOCTTY
		      unistd:ONONBLOCK))))
    (setf (slot-value s 'fd) fd)))

@export
(defmethod close-serial ((s <serial-posix>))
  (let ((fd (get-fd s)))
    (when fd
      (unistd:fcntl fd unistd:FSETFL)
      (unistd:close fd)
      (setf (slot-value s 'fd) nil))))

(defmethod configure-port :around ((s <serial-posix>))
  (unless (get-fd s) (error 'serial-error :text "Port has to be open!"))
  (call-next-method s))

(defmethod set-custom-baud-rate ((s <serial-posix>))
  (error 'value-error :text (format nil "Invalid baudrate: ~A" (baudrate s))))

(defmethod configure-port ((s <serial-posix>))
  (let ((termios (unistd:tcgetattr (get-fd s)))
	(vtime (or (and (inter-char-timeout s) (floor (* 10 (inter-char-timeout s)))) 0)))
    ;; setup raw mode/ no echo/ binary	
    (set-flag (unistd:cflag termios)
	      :on ("CLOCAL" "CREAD"))
    (set-flag (unistd:lflag termios)
	      :off ("ICANON" "ECHO" "ECHOE" "ECHOK" "ECHONL" "ISIG" "IEXTEN"))
    
    (set-flag (unistd:oflag termios)
	      :off ("OPOST"))
    (set-flag (unistd:iflag termios)
	      :off ("INLCR" "IGNCR" "ICRNL" "IGNBRK"))
    
    (maybe-set-flag (unistd:iflag termios)
		    :off ("PARMRK"))
    
    (maybe-set-flag (unistd:iflag termios)
		    :off ("IUCLC"))
    
    ;; setup char length
    (set-flag (unistd:cflag termios)
	      :off ("CSIZE"))
    
    (case (bytesize s)
      (8 (set-flag (unistd:cflag termios) :on ("CS8")))
      (7 (set-flag (unistd:cflag termios) :on ("CS7")))
      (6 (set-flag (unistd:cflag termios) :on ("CS6")))
      (5 (set-flag (unistd:cflag termios) :on ("CS5")))
      (t (error 'value-error :text (format nil "Invalid char len: ~A" (bytesize s)))))
    
    ;; setup stopbits
    
    (case (stopbits s)
      (1 (set-flag (unistd:cflag termios) :off ("CSTOPB")))
      ((or 2 1.5) (set-flag (unistd:cflag termios) :on ("CSTOPB")))
      (t (error 'value-error :text (format nil "Invalid stop bit spec: ~A" (stopbits s)))))
    
    ;; setup parity
    
    (set-flag (unistd:iflag termios)
	      :off ("INPCK" "ISTRIP"))
    (case (parity s)
      (:PARITY-NONE (set-flag (unistd:cflag termios) :off ("PARENB" "PARODD")))
      (:PARITY-EVEN (set-flag (unistd:cflag termios) :off ("PARODD") :on ("PARENB")))
      (:PARITY-ODD (set-flag (unistd:cflag termios) :on ("PARENB" "PARODD")))
      (t (error 'value-error :text (format nil "Invalid parity: ~A" (parity s)))))
    (if (get-exported-symbol "IXANY" :unistd)
	(if (xonxoff s)
	    (set-flag (unistd:iflag termios)
		      :on ("IXON" "IXOFF"))
	    (set-flag (unistd:iflag termios)
		      :off ("IXON" "IXOFF" "IXANY")))
	(if (xonxoff s)
	    (set-flag (unistd:iflag termios)
		      :on ("IXON" "IXOFF"))
	    (set-flag (unistd:iflag termios)
		      :off ("IXON" "IXOFF"))))
    (if (get-exported-symbol "CRTSCTS" :unistd)
	(if (rtscts s)
	    (set-flag (unistd:iflag termios)
		      :on ("CRTSCTS"))
	    (set-flag (unistd:iflag termios)
		      :off ("CRTSCTS")))
	(when (get-exported-symbol "CNEW-RTSCTS" :unistd)
	  (if (rtscts s)
	      (set-flag (unistd:iflag termios)
			:on ("CNEW_RTSCTS"))
	      (set-flag (unistd:iflag termios)
			:off ("CNEW_RTSCTS")))))
    (if (or (<= vtime 255) (>= vtime 0))
	(setf 
	 (aref (unistd:cc termios) unistd:VMIN) 1
	 (aref (unistd:cc termios) unistd:VTIME) vtime)
	(error 'value-error :text (format nil "Invalid timeout: ~A" vtime)))
    
    (let* ((ispeed (get-exported-symbol (format nil "B~A" (baudrate s)) :unistd))
	   (ospeed ispeed))
      (unless ispeed (set-custom-baud-rate s))
      (unistd:cfsetispeed termios (symbol-value ispeed))
      (unistd:cfsetospeed termios (symbol-value ospeed)))
    
    (unistd:tcsetattr (get-fd s) termios unistd:TCSANOW)))

;; TODO: This strikes me as a little inefficient...
@export
(defmethod write-serial-byte ((s <serial-posix>) byte)
  (unistd:write (get-fd s) (make-array 1 :initial-element byte :element-type '(mod 32)) 1))

@export
(defmethod write-serial-byte-seq ((s <serial-posix>) byte-seq)
  (unistd:write (get-fd s) byte-seq (array-dimension byte-seq 0)))

@export
(defmethod read-serial-byte ((s <serial-posix>))
  (aref (unistd:read (get-fd s) 1) 0))

@export
(defmethod read-serial-byte-seq ((s <serial-posix>) count)
  (unistd:read (get-fd s) count))

;; TODO: Blocking versions using select
