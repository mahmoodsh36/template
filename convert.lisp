;; this is a hack to gather all latex previews and compile them all at once
;; before exporting. this is to speed things up. one latex conversion commands
;; is faster than running it once for every file independently.
(defun compile-all-latex-previews (rmr)
  (let ((all-snippets)
        ;; snippets-at-once is how many snippets should be compiled at the same time.
        ;; one case in which this is helpful is when there are too many errors (>100)
        ;; and latex refuses to compile all previews at once and exits. so we
        ;; break them into batches, which make it more likely that more latex
        ;; previews will be rendered overall.
        (snippets-at-once 2500))
    (loop for node in (cltpt/roam:roamer-nodes rmr)
          for this-tree = (cltpt/roam:node-text-obj node)
          do (cltpt/base:map-text-object
              this-tree
              (lambda (obj)
                (when (or (typep obj 'cltpt/latex:inline-math)
                          (typep obj 'cltpt/latex:display-math)
                          (typep obj 'cltpt/latex:latex-env))
                  (push (cltpt/base:text-object-text obj) all-snippets)))))
    (loop for i from 0 to (length all-snippets) by snippets-at-once
          do (cltpt/latex:generate-svgs-for-latex
              (subseq all-snippets
                      i
                      (min (+ i snippets-at-once)
                           (length all-snippets)))))))

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
(defvar *blog-dir* "/home/mahmooz/work/blog/")
(defvar *blog-static-dir* "/home/mahmooz/work/blog/static/")
(defvar *template-dir* (truename "~/work/template/"))
(defvar *template-static-dir* (truename "~/work/template/static/"))
(defvar *rmr*)

(defun from-brain (filepath)
  (cltpt/file-utils:join-paths "/home/mahmooz/brain/" filepath))

;; get svg by #+name: or whatever?
(defun get-latex-preview-svg-by-blk-id (blk-id)
  (let* ((dest-node (cltpt/roam:get-node-by-id (cltpt/roam:current-roamer) blk-id)))
    (when dest-node
      (let* ((text-obj (cltpt/roam:node-text-obj dest-node))
             (match (cltpt/base:text-object-property text-obj :combinator-match))
             (latex-env-match
               (or (car (cltpt/combinator:find-submatch
                         match
                         'cltpt/org-mode::latex-env-1))
                   (car (cltpt/combinator:find-submatch
                         match
                         'cltpt/org-mode::latex-env))))
             (latex-env-text (getf latex-env-match :match)))
        (cdar (cltpt/latex:generate-svgs-for-latex (list latex-env-text)))))))

;; this is for postponing the execution to after roamer is done.
(defun get-latex-preview-svg-by-blk-id-1 (blk-id)
  (cons
   'after-roam
   (lambda ()
     (get-latex-preview-svg-by-blk-id blk-id))))


(defun generate ()
  (cltpt/file-utils:ensure-directory *blog-dir*)
  (setf cltpt/org-mode::*org-enable-macros* t)
  (setf cltpt:*debug* 1)
  (cltpt/zoo::init)
  (setf *rmr*
        (cltpt/roam:from-files
         '((:path ("/home/mahmooz/brain/notes/")
            :regex ".*\\.org"
            :format "org-mode"))))
  (uiop:with-current-directory (*blog-dir*)
    (let* ((other-head-contents
             (uiop:read-file-string
              (uiop:merge-pathnames* *template-dir* "head.html")))
           (other-preamble-contents
             (uiop:read-file-string
              (uiop:merge-pathnames* *template-dir* "preamble.html")))
           (cltpt/html:*html-static-route* "/")
           ;; (cltpt/latex:*latex-previews-cache-directory* #P"./")
           (cltpt/latex:*latex-previews-cache-directory* #P"")
           (*my-metadata*
             (list :other-head-contents other-head-contents
                   :other-preamble-contents other-preamble-contents))
           (cltpt/html:*html-postamble* "</body></html>")
           (cltpt/html:*html-preamble*
             "<!DOCTYPE html>
<html>
<head>
  <meta charset=\"UTF-8\">
  <title> %title </title>
  #(getf cl-user::*my-metadata* :other-head-contents)
</head>
<body>
  #(getf cl-user::*my-metadata* :other-preamble-contents)"))
      (compile-all-latex-previews *rmr*)
      (generate-index *rmr*)
      ;; (cltpt/roam:convert-all
      ;;  *rmr*
      ;;  (cltpt/base:text-format-by-name "html")
      ;;  "%(identity cl-user::*blog-dir*)%(cl-user::title-to-filename title).html")
      (mapc
       (lambda (item)
         (uiop:copy-file (uiop:merge-pathnames* *template-dir* item)
                         (uiop:merge-pathnames* (truename "~/work/blog/")
                                                item)))
       (list "head.html" "preamble.html" "search.html"))
      (cltpt/file-utils:ensure-directory *blog-static-dir*)
      (mapc
       (lambda (item)
         (uiop:copy-file (uiop:merge-pathnames*
                          (truename *template-static-dir*)
                          item)
                         (uiop:merge-pathnames*
                          (truename *blog-static-dir*)
                          item)))
       (uiop:directory-files *template-static-dir*)))))

;; should place the static file in the dir and return the href to it
(defun export-static-file (filepath)
  (when (and filepath (uiop:probe-file* filepath))
    (uiop:copy-file
     filepath
     (cltpt/file-utils:change-dir filepath *blog-dir*))
    (cltpt/file-utils:file-basename filepath)))

;; find "entries", files tagged with 'entry', as in 'blog entry'
(defun entry-nodes (rmr)
  (let ((final-nodes))
    (loop for node in (cltpt/roam:roamer-nodes rmr)
          for text-obj = (cltpt/roam:node-text-obj node)
          when (typep text-obj 'cltpt/org-mode::org-document)
            do (let ((tags (cltpt/base:text-object-property text-obj :tags)))
                 (when (member "entry" tags :test 'equal)
                   (push node final-nodes))))
    final-nodes))

;; /home/mahmooz/work/emacs.d/lisp/config-org.el
(defun generate-collage-html (entries)
  (with-output-to-string (out)
    (write-sequence "<div class=\"collage\">" out)
    (loop for entry in entries
          do (format out "<div class='card fancy-button' data-ref='blk:~A'>
  <img src='~A' class='card-image' />
  <span class='card-title'>~A</span>
  <span class='card-subtitle'>~A</span>
  <span class='card-subtitle'>~A</span>
</div>"
                     (getf entry :id)
                     (when (getf entry :id)
                       (if (uiop:probe-file* (getf entry :image))
                           (export-static-file (getf entry :image))
                           (format t "collage image ~A doesnt exist~%"
                                   (getf entry :image))))
                     (getf entry :title)
                     (or (getf entry :subtitle) "")
                     (or (getf entry :subsubtitle) "")))
    (write-sequence "</div>" out)
    out))

(defun generate-index (rmr)
  (let* ((entries (entry-nodes rmr))
         (index-file (uiop:merge-pathnames* *blog-dir* "index.html"))
         ;; (entries-html
         ;;   (loop for entry in entries
         ;;         collect (format
         ;;                  nil
         ;;                  "title: ~A~%"
         ;;                  (cltpt/roam:node-title entry))
         ;;           into strings
         ;;         finally (return (apply 'concatenate 'string strings))))
         (entries-html
           (generate-collage-html
            (loop for entry in entries
                  collect (list :title (cltpt/roam:node-title entry)
                                :id (cltpt/roam:node-id entry)
                                :image (cltpt/org-mode::text-object-org-keyword-value (cltpt/roam:node-text-obj entry) "image")
                                )))))
    (cltpt/file-utils:write-file
     index-file
     (cltpt/base::convert-text
      (cltpt/base:text-format-by-name "org-mode")
      (cltpt/base:text-format-by-name "html")
      (format nil
              "~A~%~A~%~A"
              "#+begin_export html"
              entries-html
              "#+end_export")))))