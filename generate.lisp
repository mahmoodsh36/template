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
  (uiop:with-current-directory ((truename "~/work/blog/"))
    (let* ((template-dir (truename "~/work/template/"))
           (other-head-contents
             (uiop:read-file-string
              (uiop:merge-pathnames* template-dir "head.html")))
           (other-preamble-contents
             (uiop:read-file-string
              (uiop:merge-pathnames* template-dir "preamble.html")))
           (cltpt/html:*html-static-route* "/")
           (cltpt/latex:*latex-previews-cache-directory* #P"./")
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
      (cltpt/roam:convert-all
       rmr
       (cltpt/base:text-format-by-name "html")
       "/home/mahmooz/work/blog/%(cl-user::title-to-filename title).html")
      (mapc
       (lambda (item)
         (uiop:copy-file (uiop:merge-pathnames* template-dir item)
                         (uiop:merge-pathnames* (truename "~/work/blog/")
                                                item)))
       (list "head.html" "preamble.html" "search.html"))
      (cltpt/base:ensure-directory "/home/mahmooz/work/blog/static/")
      (mapc
       (lambda (item)
         (uiop:copy-file (uiop:merge-pathnames* (truename "~/work/template/static/")
                                                item)
                         (uiop:merge-pathnames* (truename "~/work/blog/static/")
                                                item)))
       (uiop:directory-files (truename "~/work/template/static/"))))))

;; (generate)