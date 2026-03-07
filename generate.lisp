#!/usr/bin/env -S sbcl --script
(require 'asdf)
;; (pushnew ""
;;          asdf:*central-registry*
;;          :test #'equal)
;; (pushnew (concatenate 'string (uiop:getenv "WORK_DIR") "/cltpt/")
;;          asdf:*central-registry*
;;          :test #'equal)
;; (asdf:load-system :cltpt :force t)
(asdf:load-system :template)
(generate)