(asdf:load-system :cltpt)

(defun title-to-filename (title)
  (with-output-to-string (out)
    (loop for char across (string-downcase title)
          do (cond ((or (alphanumericp char)
                        (char= char #\-)
                        (char= char #\.))
                    (write-char char out))
                   ((or (char= char #\space)
                        (char= char #\/)
                        (char= char #\\))
                    (write-char #\_ out))))))

(defvar *my-metadata* nil)

(defun generate ()
  (let* ((template-dir (truename "~/work/template/"))
         (other-head-contents
           (uiop:read-file-string
            (uiop:merge-pathnames* template-dir "head.html")))
         (other-preamble-contents
           (uiop:read-file-string
            (uiop:merge-pathnames* template-dir "preamble.html")))
         (cltpt/org-mode:*org-convert-dest-dir*
           (truename "~/work/blog/"))
         (cltpt/html:*html-static-route* "/")
         (*my-metadata*
           (list :other-head-contents other-head-contents
                 :other-preamble-contents other-preamble-contents))
         (cltpt/html:*html-postamble* "</body></html>")
         (cltpt/html:*html-preamble*
           "<html>
<head>
  <title> %title </title>
  #(getf cl-user::*my-metadata* :other-head-contents)
</head>
<body>
  #(getf cl-user::*my-metadata* :other-preamble-contents)
")
         (rmr (cltpt/roam:from-files
               '((:path ("/home/mahmooz/brain/notes/")
                  :regex ".*\\.org"
                  :format "org-mode")))))
    (cltpt/roam:convert-all rmr (cltpt/base:text-format-by-name "html")
                            "%(identity cltpt/org-mode:*org-convert-dest-dir*)/%(cl-user::title-to-filename title).html")))

;; (generate)