# Moved to bitbucket
This repository is deprecated. Please go to https://bitbucket.org/blueswitch/cerial

# CeriaL
Mmmmh...Crunchy!

## Usage
WARNING! I'm still experimenting...Don't get used to the flavour just yet.

```common-lisp
;; serial port to write to
(setq *sout* (make-serial "pseudo0"))

;; the other end
(setq *sin* (make-serial "pseudo1"))

;; write some 
(write-byte-seq *sout* (flexi-streams:string-to-octets "Hello World!"))

;; read some
(flexi-streams:octets-to-string (cerial:read-byte-seq *sin* 12))

"Hello World!"
```

Or more 'idiomatically':

```common-lisp
(with-serial (sout "pseudo1"
		    :baudrate 115200
		    :parity :PARITY-ODD)
  (write-serial-byte sout 42))
```
For a full description of the API see the wiki pages of the future!

