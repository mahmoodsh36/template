#!/usr/bin/env -S sbcl --script
(require 'asdf)
(asdf:load-system :cltpt)
(load (merge-pathnames "convert.lisp" *load-truename*))
(generate)