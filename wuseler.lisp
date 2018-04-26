(require 'asdf)
(asdf:oos 'asdf:load-op 'drakma)

(defun make-download-url (vid gid)
  "erstellt eine Download-URL aus Vid und Gid."
  (assert (stringp gid))
  (assert (stringp vid))
  (format nil "https://wusel.uni-wuppertal.de/qisserver/rds?state=verpublish&status=transform&vmfile=no&veranstaltung.veranstid=~A&veransttermin.parallelid=~A&moduleCall=iCalendarGruppe&publishConfFile=reports&publishSubDir=veranstaltung" vid (if gid gid "")))

(defun get-vid-by-url (url)
  "holt aus der URL die ID raus."
  (let* ((sv (search "Vid=" url))
         (ev (search "&" url :start2 (+ sv 4) ))
         (sg (search "Gid=" url :start2 (+ ev 4) ))
         (eg (search "Gid=" url :start2 (+ sg 4) )))
    (cons (subseq url (+ sv 4) ev) (subseq url (+ sg 4) eg))))

(defun rm-url-p (url)
  "Findet heraus, ob URL zum Löschen-Knopf passt.
   (Löschen-Knopf enthaelt Vid und Gid in URL.)"
  (when url
    (assert (stringp url))
    (search "Vid=" url)))

(defun get-ids (stream-orig)
  "Parst file nach bestätigten Vorlesungen"
  (with-open-stream (f stream-orig)
    (let ((r nil))
      (do ((l (read-line f) (read-line f nil nil)))
        ((eq l nil) r)
        (when (search "planGreen" l)
          (push
            (do* ((m (read-line f) (read-line f nil nil))
                  (p (search "a href" m) (search "a href" m))
                  (url nil
                       (when (numberp p)
                         (subseq m (+ p 8) (search "\"" m :start2 (+ p 8)))
                         ))
                  (url-match-p (rm-url-p url) (rm-url-p url)))
              ((or url-match-p (eq m nil))
               (when url (get-vid-by-url url) ))) r))) r)))

(defun sort-ids (x y)
  (< (parse-integer (car x)) (parse-integer (car y))))

; the following is a legacy function to be used if you downloaded the wplan
; into a file and want to get a (id url) list of iCal files of you lectures
; should be invoked like
; (format t "~{~{~a ~}~%~}" (wplan-file-to-urls "wplan.html"))
(defun wplan-file-to-urls (f)
  "accepts wplan filename and returns list of (lecture-id iCal-download-url)"
  (let ((ids (sort  (cdr (get-ids (open (merge-pathnames f)))) #'sort-ids)))
    (mapcar (lambda (x) (list  (car x) (make-download-url (car x) (cdr x))))
            (delete-duplicates ids :key #'car :test #'string=))))

; standard url template declarations
(defconstant wusel-login
     "https://wusel.uni-wuppertal.de/qisserver/rds?state=user&type=1&asdf=~a&fdsa=~a&submit=Anmelden")
(defconstant wusel-logout
             "https://wusel.uni-wuppertal.de/qisserver/rds?state=user&type=4")
(defconstant wusel-wplan
             "https://wusel.uni-wuppertal.de/qisserver/rds?state=wplan")

(defun get-lectures (cookie-jar)
  "fetches wplan and extract the ids of the lectures"
  ; first download wplan
  (let* ((ids-unsorted (get-ids
                         (drakma:http-request wusel-wplan
                                              :cookie-jar cookie-jar
                                              :want-stream t)))
         (ids (sort (cdr ids-unsorted) #'sort-ids)))
   ; extract urls to iCal files from wplan downloaded before
   (mapcar (lambda (x) (list  (car x) (make-download-url (car x) (cdr x))))
            (delete-duplicates ids :key #'car :test #'string=))))

(defun get-files (wusel-session)
  "downloads all lectures as seperate iCal files and store these in the
   current working directory"
  (mapcar (lambda (l)
            (with-open-file (f (concatenate 'string (car l) ".ics")
                               :direction :output :if-exists :supersede
                               :if-does-not-exist :create)
              (write-string
                (drakma:http-request (cadr l) :cookie-jar wusel-session) f)))
          (get-lectures wusel-session)))

(defun get-caldata (wusel-session url &optional include-header)
  "downloads calendar events and extracts VEVENT data. Also includes header of
   iCal file (from BEGIN:VCALENDAR until first VEVENT) if include-header is T"
  (let* ((cdata (drakma:http-request url :cookie-jar wusel-session))
         ; find VEVENT data. End position uses 1+ to include newline character.
         (start (search "BEGIN:VEVENT" cdata))
         (end (+ (length "END:VEVENT") 1 ; 1 -> include newline character
                 (search "END:VEVENT" cdata :from-end t))))
    (remove #\Return ; implicitly convert to UNIX newline format
            (subseq cdata
                    (if include-header 0 start) ; include header if wanted
                    end))))

(defun get-singlefile (wusel-session)
  "downloads calendar events into one iCal-file"
  (with-open-file (f "plan.ics" :direction :output :if-exists :supersede
                     :if-does-not-exist :create)
    (write-string (get-singleical wusel-session) f)))

(defun get-singleical (wusel-session)
  "downloads calendar events and composes a single iCal file"
  (let ((lectures (get-lectures wusel-session)))
    ; somewhat ugly list processing below but ... works somehow. FIXME
    (apply #'concatenate 'string
           `(,(get-caldata wusel-session (cadr (car lectures)) T)
              ,@(mapcar (lambda (l) (get-caldata wusel-session (cadr l)))
                        (cdr lectures))
              "END:VCALENDAR" (#\linefeed)))))


(defun with-wusel-session (mnr pm fun)
  "establishes wusel session and performs logout after executing fun"
  (let ((login-url (format nil wusel-login mnr pm))
        (content '())
        (cookie-jar (make-instance 'drakma:cookie-jar)))
    ; login
    (let ((login (drakma:http-request login-url :cookie-jar cookie-jar)))
      (when (not (search "Sie sind angemeldet als" login))
        (format t "EEH: login failed")
        (return-from with-wusel-session nil)))
    ; call stuff
    (setf content (funcall fun cookie-jar))
    ; logout
    (let ((logout (drakma:http-request wusel-logout :cookie-jar cookie-jar)))
     (when (not (search "Sicherheitshinweis" logout))
        (format t "EEH: logout failed")
        (return-from with-wusel-session nil)))
    content))
