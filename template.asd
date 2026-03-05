(defsystem "template"
  :description "personal blog/webapp generator using cltpt"
  :depends-on ("cltpt" "cl-json")
  :components ((:file "convert")))