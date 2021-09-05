##
# CL Alphastrike
#
# Dependencies:
#  * SBCL
#  * Quicklisp
#  * TCL/TK
#
# Author: Jonathan A. Bennett <doulos05@gmail.com>
# @file
# @version 0.0.1


LISP ?= sbcl

build:
	$(LISP) --eval "(require 'asdf)" \
		--load cl-alphastrike.asd \
		--eval '(ql:quickload :cl-alphastrike)' \
		--eval '(asdf:make :cl-alphastrike)' \
		--eval '(quit)'

# end
