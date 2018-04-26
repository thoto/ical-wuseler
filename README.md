# ical-wuseler

Crawls your WUSEL timetable and compiles a single iCalendar file.

iCal files are directly provided by WUSEL. This project just simplifies the
download procedure eliminating the need to click through every lecture and
download its ical file by hand.

Later it will be possible to supply a CalDAV target where your timetable
wile be uploaded.

## Invokation

Invoke using `./singlefile.lisp <matrikelnummer>`. You will be asked for your
WUSEL password on stdin. The characters of your password will not be
shown while you are typing. After download you will get a file `plan.ics` in
your working directory.

## Requirements

You need to have SBCL, ASDF and DRAKMA installed on your system. These
are provided by the `sbcl`, `cl-asdf` and `cl-drakma` Debian packages.

## TODO:
 - provide Dockerfile
 - implement CalDAV upload

Notice: this code is quite ugly and parts are somewhat old dating back to
the time I started attending Uni Wuppertal in 2012. There is also some dead
code which you don't need. I will clean this up later.

