#!/usr/bin/env -S sbcl --script
(require 'asdf)
;; (pushnew (concatenate 'string (uiop:getenv "WORK_DIR") "/cltpt/")
;;          asdf:*central-registry*
;;          :test #'equal)
;; (asdf:load-system :cltpt :force t)
(asdf:load-system :cltpt)
(load (merge-pathnames "convert.lisp" *load-truename*))
(generate)