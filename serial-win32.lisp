(in-package #:cerial)
(annot:enable-annot-syntax)

@export-accessors
(defclass <serial-win32> (<serial-base>) ()
  (:documentation "Serial port class WIN32 implementation."))

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
  (make-instance '<serial-win32>
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

(defmethod device ((s <serial-win32>) port)
  (declare (ignore s))
  (format nil "\\\\.\\~A" port))


(defun stopbits->win32 (stopbits)
  (cond
    ((= stopbits 1) +ONESTOPBIT+)
    ((= stopbits 1.5) +ONE5STOPBITS+)
    ((= stopbits 2) +TWOSTOPBITS+)
    (t (error 'serial-error :text "unsupported stopbits"))))

(defun baudrate->win32 (baudrate)
  (cond 
    ((= baudrate 110)             +CBR_110+)
    ((= baudrate 300)             +CBR_300+)
    ((= baudrate 600)             +CBR_600+)
    ((= baudrate 1200)            +CBR_1200+)
    ((= baudrate 2400)            +CBR_2400+)
    ((= baudrate 4800)            +CBR_4800+)
    ((= baudrate 9600)            +CBR_9600+)
    ((= baudrate 14400)           +CBR_14400+)
    ((= baudrate 19200)           +CBR_19200+)
    ((= baudrate 38400)           +CBR_38400+)
    ((= baudrate 56000)           +CBR_56000+)
    ((= baudrate 57600)           +CBR_57600+)
    ((= baudrate 115200)          +CBR_115200+)
    ((= baudrate 128000)          +CBR_128000+)
    ((= baudrate 256000)          +CBR_256000+)
    (t (error 'serial-error :text "unsupported baudrate"))))

(defun parity->win32 (parity)
  (ecase parity
    (:PARITY-NONE +NOPARITY+)
    (:PARITY-EVEN +EVENPARITY+)
    (:PARITY-ODD +ODDPARITY+)
    (:PARITY-MARK +MARKPARITY+)
    (:PARITY-SPACE +SPACEPARITY+)))

@export
(defmethod open-serial ((s <serial-win32>))
  (let* ((null (cffi:null-pointer))
	 (handler (win32-create-file (device s (port s)) (logxor +GENERIC_READ+ +GENERIC_WRITE+) 0 null +OPEN_EXISTING+ 0 null)))
    (unless (valid-pointer-p handler)
      (error 'serial-error :text "CreateFile failed"))    
    (setf (slot-value s 'fd) handler)))

@export
(defmethod close-serial ((s <serial-win32>))
  (with-slots (fd) s
    (win32-close-handle fd)))


(defmethod configure-port ((s <serial-win32>))
  (with-slots (fd) s
    (cffi:with-foreign-object (ptr 'dcb)
      (win32-memset ptr 0 (cffi:foreign-type-size 'dcb))
      (cffi:with-foreign-slots ((DCBlength) ptr dcb)
	  (setf DCBlength (cffi:foreign-type-size 'dcb)))
      (win32-onerror (win32-get-comm-state fd ptr)
	(error 'serial-error :text "GetCommState failed"))
      (cffi:with-foreign-slots ((baudrate bytesize parity stopbits dcbflags) ptr dcb)
	  (setf baudrate (baudrate->win32 (baudrate s)))
	(setf bytesize (bytesize s))
	(setf stopbits (stopbits->win32 (stopbits s)))
	(setf parity (parity->win32 (parity s)))
	(setf dcbflags (cffi:foreign-bitfield-value 'dcb-flags '(fbinary))))
      (win32-onerror (win32-set-comm-state fd ptr)
	(error 'serial-error :text "SetCommState failed")))))


@export
(defmethod write-serial-byte ((s <serial-win32>) byte)
  (write-serial-byte-seq s (make-array 1 :initial-element byte)))

@export
(defmethod write-serial-byte-seq ((s <serial-win32>) byte-seq)
  (let ((seq-size (length byte-seq)))
    (cffi:with-foreign-object (buffer :char seq-size)
      (cffi:with-foreign-object (writtenbytes 'word)	
	(with-slots (fd) s
	  (dotimes (idx seq-size)
	    (setf (cffi:mem-aref buffer :char idx) (aref byte-seq idx)))
	  (win32-confirm (win32-write-file fd buffer seq-size writtenbytes (cffi:null-pointer))
			 (cffi:mem-ref writtenbytes 'word)
			 (error 'serial-error :text "could not write to device")))))))

@export
(defmethod read-serial-byte ((s <serial-win32>))
  (aref (read-serial-byte-seq s 1)))

@export
(defmethod read-serial-byte-seq ((s <serial-win32>) count)
  (cffi:with-foreign-object (buffer :char count)
    (cffi:with-foreign-object (readbytes 'word)
      (with-slots (fd) s
	  (win32-confirm (win32-read-file fd buffer count readbytes (cffi:null-pointer))
			 (loop with size = (cffi:mem-ref readbytes 'word)
			    with result = (make-array size :element-type '(integer 0 255))
			    for idx below size
			    do (setf (aref result idx) (cffi:mem-aref buffer :char  idx))
			    finally (return result))
			 (error 'serial-error :text "could not read from device"))))))
