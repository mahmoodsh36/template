#!/usr/bin/env -S sbcl --script
(require 'asdf)
(asdf:load-system :template)
(generate)