#!/usr/bin/sbcl --script
; #!/usr/bin/env sbcl --script

(load "wuseler.lisp")

(defun get-mnr ()
  "reads Matrikelnummer from script command line arguments or query-io (stdin)"
  (if (> (length sb-ext:*posix-argv*) 1)
    (second sb-ext:*posix-argv*) ; read from command line arguments
    (progn
      (format *query-io* "matrikelnummer:~%")
      (read-line *query-io*))))

(defun get-pw ()
  "reads password from query-io (stdin) while having echo disabled"
  (format *query-io* "password:~%")
  (let* ((tio-o (sb-posix:tcgetattr 0)) ; terminal parametes ...
         (lflags (logand (sb-posix:termios-lflag tio-o) (lognot sb-posix:echo)))
         (tio-n (sb-posix:tcgetattr 0)))
    (setf (sb-posix:termios-lflag tio-n) lflags)
    (sb-posix:tcsetattr 0 sb-posix:TCSANOW tio-n) ; disable echo
    (let ((res (read-line *query-io*))) ; read password from user input
      (sb-posix:tcsetattr 0 sb-posix:TCSANOW tio-o) ; reenable echo
      res)))


(let ((matrikelnummer (get-mnr)) (passwort (get-pw)))
  (with-wusel-session matrikelnummer passwort #'get-singlefile))
