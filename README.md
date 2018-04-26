# ical-wuseler

Crawls your WUSEL timetable and compiles a single iCalendar file.

Later it will be possible to supply a CalDAV target where your timetable
wile be uploaded.

## Invokation

Invoke using `./singlefile.lisp <matrikelnummer>`. You will be asked for your
WUSEL password on stdin. The characters of your password will not be
shown while you are typing.

## Requirements

You need to have SBCL, ASDF and DRAKMA
installed on your system. There are provided in the `sbcl`, `cl-asdf` and
`cl-drakma` Debian packages.

iCal files are directly provided by WUSEL. This project just simplifies the
download procedure eliminating the need to click through every lecture and
download its ical file by hand.

## TODO:
 - provide Dockerfile
 - implement CalDAV upload
