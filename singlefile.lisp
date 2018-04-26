#!/usr/bin/sbcl --script
; #!/usr/bin/env sbcl --script

(load "wuseler.lisp")

(defun get-mnr ()
  "Reads Matrikelnummer from script command line arguments or STDIN"
  (if (> (length sb-ext:*posix-argv*) 1)
    (second sb-ext:*posix-argv*)
    (progn
      (format t "Matrikelnummer:~%")
      (read-line))))

(defun get-pw ()
  "Reads password from stdin while having echo disabled"
  (let* ((tio-o (sb-posix:tcgetattr 0))
         (lflags (logand (sb-posix:termios-lflag tio-o) (lognot sb-posix:echo)))
         (tio-n (sb-posix:tcgetattr 0)))
;    (format t "~a~%" (sb-posix:termios-lflag (sb-posix:tcgetattr 0)))
    (setf (sb-posix:termios-lflag tio-n) lflags)
    (sb-posix:tcsetattr 0 sb-posix:TCSANOW tio-n) ; disables echo
;    (format t "~a~%" (sb-posix:termios-lflag (sb-posix:tcgetattr 0)))
    (let ((res (read-line))) ; reads password here
      (sb-posix:tcsetattr 0 sb-posix:TCSANOW tio-o) ; enables echo again
;      (format t "~a~%" (sb-posix:termios-lflag (sb-posix:tcgetattr 0)))
      res)))


(let ((matrikelnummer (get-mnr)) (passwort (get-pw)))
  (with-wusel-session matrikelnummer passwort #'get-singlefile))
