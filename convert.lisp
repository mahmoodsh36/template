(asdf:load-system :cl-json)

;; the code here is unorganized and full of hacks.

;; this is a hack to gather all latex previews and compile them all at once
;; before exporting. this is to speed things up. one latex conversion commands
;; is faster than running it once for every file independently.
(defun compile-all-latex-previews (rmr file-predicate)
  (let ((all-snippets)
        ;; snippets-at-once is how many snippets should be compiled at the same time.
        ;; one case in which this is helpful is when there are too many errors (>100)
        ;; and latex refuses to compile all previews at once and exits. so we
        ;; break them into batches, which make it more likely that more latex
        ;; previews will be rendered overall.
        (snippets-at-once 1000))
    (loop for node in (cltpt/roam:roamer-nodes rmr)
          for this-tree = (cltpt/roam:node-text-obj node)
          for node-file = (cltpt/roam:node-file node)
          when (funcall file-predicate node-file)
            do (cltpt/base:map-text-object
                this-tree
                (lambda (obj)
                  (when (or (typep obj 'cltpt/latex:inline-math)
                            (typep obj 'cltpt/latex:display-math)
                            (typep obj 'cltpt/latex:latex-env))
                    (pushnew (cltpt/base:text-object-contents obj)
                             all-snippets
                             :test 'string=)))))
    (setf all-snippets (reverse all-snippets))
    (loop for i from 0 to (length all-snippets) by snippets-at-once
          do (cltpt/latex-previews:generate-previews-for-latex
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

(defvar *work-dir*
  (or (sb-ext:posix-getenv "WORK_DIR")
      (error "couldnt get WORK_DIR env var")))
(defvar *brain-dir*
  (or (sb-ext:posix-getenv "BRAIN_DIR")
      (error "couldnt get BRAIN_DIR env var")))
(defvar *data-dir*
  (or (sb-ext:posix-getenv "DATA_DIR")
      (error "couldnt get DATA_DIR env var")))
(defvar *blog-dir*
  (cltpt/file-utils:join-paths *work-dir* "blog"))
(defvar *base-dir*
  (cltpt/file-utils:join-paths *work-dir* "template"))
(defvar *template-dir*
  (cltpt/file-utils:join-paths *base-dir* "template"))
(defvar *template-static-dir*
  (cltpt/file-utils:join-paths *base-dir* "static"))

(defun from-brain (filepath)
  (cltpt/file-utils:join-paths *brain-dir* filepath))
(defun from-data (filepath)
  (cltpt/file-utils:join-paths *data-dir* filepath))
(defun expand-brain-paths (relative-paths)
  (mapcar (lambda (rel-path)
            (from-brain rel-path))
          relative-paths))

(defvar *main-files*
  (expand-brain-paths
   '("notes/1678745440.org" ;; graph theory
     "notes/1729677442.6352403.org" ;; circuit complexity
     "notes/1709969723.org" ;; computability theory
     "notes/1723812143.2079227.org" ;; theory of computation
     "notes/1712601655.org" ;; programming
     "notes/1725811841.8613749.org" ;; media
     "notes/1725958715.1511376.org" ;; formal logic
     "notes/1709021041.org" ;; calculus
     "notes/1725810860.5115163.org" ;; databases
     "notes/1677361099.org" ;; machine learning
     "notes/1709021027.org" ;; linear algebra
     )))
(defvar *excluded-files*
  (expand-brain-paths
   (list
    "notes/1658738374.org"
    "notes/1709712959.org"

    "notes/1711448392.org"
    "notes/1712751117.org"
    "notes/1714912201.org"
    "notes/1716399914.org"

    "notes/1696961489.org"
    "notes/1707059561.org"

    "notes/1659132042.org"

    "notes/1664793548.org"
    )))
;; this is the same as setting it to nil.
(defvar *static-filepath-format*
  "%(getf *file-info* :filename)")
(defvar *filepath-format*
  "%(title-to-filename (getf *file-info* :root-title)).html")
(defvar *rmr*)

;; get svg by #+name: or whatever?
(defun get-latex-preview-svg-by-blk-id (blk-id)
  (let* ((dest-node (cltpt/roam:get-node-by-id (cltpt/roam:current-roamer) blk-id)))
    (when dest-node
      (let* ((text-obj (cltpt/roam:node-text-obj dest-node))
             (latex-env-text (cltpt/base:text-object-contents text-obj)))
        (cdar (cltpt/latex-previews:generate-previews-for-latex (list latex-env-text)))))))

(defun generate ()
  (setf cltpt/org-mode::*org-enable-macros* t)
  ;; (setf (getf cltpt:*debug* :convert) t)
  (cltpt/zoo::init)
  ;; generation with "restrictions" to the "main files/entries"
  (let ((rmr-files `((:path (,(concatenate 'string (from-brain "notes") "/"))
                      :glob "*.org"
                      :format "org-mode")
                     ;; "/home/mahmooz/work/cltpt/tests/test2.org"
                     )))
    (generate-from-files-to-dir rmr-files *blog-dir*))
  ;; convert everything for local browsing
  ;; (let ((rmr-files `((:path (,(concatenate 'string (from-brain "notes") "/"))
  ;;                     :glob "*.org"
  ;;                     :format "org-mode"))))
  ;;   (generate-from-files-to-dir rmr-files (cltpt/file-utils:join-paths *work-dir* "local") t))
  )

;; named it "to-dir", but some functionality in this file depends on CWD
(defun generate-from-files-to-dir (rmr-files dest-dir &optional (full-export))
  (cltpt/file-utils:ensure-dir-exists dest-dir)
  ;; we bind the current dir so we can use a relative path for latex-preview-cache-dir,
  ;; which otherwise would give absolute paths. perhaps that should be fixed in cltpt.
  (uiop:with-current-directory ((cltpt/file-utils:as-dir-path dest-dir))
    (let* ((cltpt/html:*html-static-route* "/")
           (cltpt/latex-previews:*latex-preview-preamble*
             "\\documentclass[11pt]{article}
\\usepackage{\\string~/.emacs.d/common}"
             )
           ;; (cltpt/latex-previews:*latex-previews-cache-directory* "./")
           (cltpt/latex-previews:*latex-previews-cache-directory* "")
           (cltpt/latex-previews:*latex-compiler-key* :lualatex)
           (cltpt/latex-previews::*latex-preview-pipeline-key* :dvisvgm)
           (rmr (cltpt/roam:from-files rmr-files))
           (*rmr* rmr)
           (files-to-convert
             (loop for main-file in (cltpt/base:concat
                                     (list *main-files*
                                           (mapcar
                                            'cltpt/roam:node-file
                                            (blog-nodes rmr))))
                   for node = (find main-file
                                    (cltpt/roam:roamer-nodes rmr)
                                    :key (lambda (node)
                                           (cltpt/roam:node-file node))
                                    :test 'string=)
                   append (cons main-file (find-linked-files rmr node))))
           (dest-dir-static (cltpt/file-utils:join-paths dest-dir "static"))
           (file-predicate
             (lambda (filepath)
               (if full-export
                   t
                   (member filepath files-to-convert :test 'string=))))
           (cltpt:*author* "mahmood")
           (cltpt/html:*html-template*
             (uiop:read-file-string
              (cltpt/file-utils:join-paths *template-dir* "page.html"))))
      ;; i dont like bbox=preview so modify the command to remove it
      (setf (getf (cdar cltpt/latex-previews::*latex-preview-pipelines*) :image-converter)
            "dvisvgm --page=1- --no-fonts --relative --clipjoin --optimize -o %B-%9p.svg %f")
      (convert-template dest-dir (cltpt/file-utils:join-paths *template-dir* "index.html"))
      (convert-template dest-dir (cltpt/file-utils:join-paths *template-dir* "about.html"))
      (convert-template dest-dir (cltpt/file-utils:join-paths *template-dir* "archive.html"))
      (convert-template dest-dir (cltpt/file-utils:join-paths *template-dir* "blog.html"))
      ;; apparently it doesnt work unless theres a '/' at the end.
      (cltpt/file-utils:ensure-dir-exists
       (concatenate 'string dest-dir-static "/"))
      ;; copy files from static dir of the template dir (js, css, etc)
      (mapc
       (lambda (item)
         (setf item (uiop:unix-namestring item))
         (uiop:copy-file
          item
          (cltpt/file-utils:join-paths
           dest-dir-static
           (cltpt/file-utils:file-basename item))))
       (uiop:directory-files (cltpt/file-utils:as-dir-path *template-static-dir*)))
      ;; copy robots.txt (it gets copied to static/ but we want it at root)
      (uiop:copy-file
       (cltpt/file-utils:join-paths *template-static-dir* "robots.txt")
       (cltpt/file-utils:join-paths dest-dir "robots.txt"))
      ;; (compile-all-latex-previews rmr (lambda (x) t))
      (compile-all-latex-previews rmr file-predicate)
      (export-metadata-to-json
       rmr
       "search.json"
       file-predicate)
      (cltpt/roam:convert-all
       rmr
       (cltpt/base:text-format-by-name "html")
       *filepath-format*
       :dest-dir dest-dir
       :convert-file-predicate file-predicate
       :static-filepath-format *static-filepath-format*))))

;; should place the static file in the dir and return the href to it
(defun export-static-file (filepath)
  (when (and filepath (uiop:probe-file* filepath))
    ;; if the image is already there, we dont want to copy it, this is true
    ;; for latex previews generated by `get-latex-preview-svg-by-blk-id', if
    ;; we try to copy those we'll be copying a file to itself which will
    ;; make it an empty file, causing issues.
    ;; TODO: we shouldnt be using uiop/os:getcwd. ideally, the destination
    ;; directory should be set dynamically as a variable, at an earlier
    ;; level in the conversion process.
    (let ((new-filepath (cltpt/file-utils:change-dir filepath (uiop/os:getcwd))))
      (unless (uiop:probe-file* new-filepath)
        (uiop:copy-file
         filepath
         new-filepath)))
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

(defun convert-template (dest-dir template-file)
  (cltpt/file-utils:write-file
   (cltpt/file-utils:change-dir template-file dest-dir)
   (cltpt/base::convert-tree
    (cltpt/base:parse
     cltpt/base:*simple-format*
     (cltpt/file-utils:read-file template-file))
    cltpt/base:*simple-format*
    cltpt/html:*html*
    :escape nil)))

(defun node-date (node)
  (let* ((text-obj (cltpt/roam:node-text-obj node))
         (actual-date-str (cltpt/base:alist-get
                           (cltpt/base:text-object-property
                            text-obj
                            :keywords-alist)
                           "actual_date"))
         (date-str
           (or actual-date-str
               (cltpt/base:text-object-property text-obj :date)))
         (timestamp-match
           (cltpt/combinator:apply-rule-normalized
            nil
            cltpt/org-mode::*org-timestamp-rule*
            (cltpt/reader:reader-from-input date-str)
            0))
         (dummy-obj (make-instance 'cltpt/base::text-object)))
    ;; we have to initialize the text-object :/
    ;; TODO: perhaps we shouldnt have to pass a text-object to do this? it would be nicer
    (setf (cltpt/buffer:buffer-own-text dummy-obj) date-str)
    (cltpt/base:text-object-init dummy-obj date-str timestamp-match)
    (cltpt/base:text-object-finalize dummy-obj)
    (cltpt/org-mode::org-timestamp-match-to-time dummy-obj timestamp-match)))

(defun blog-nodes (rmr)
  (sort
   (loop for node in (cltpt/roam:roamer-nodes rmr)
         for text-obj = (cltpt/roam:node-text-obj node)
         for val = (cltpt/base:alist-get
                    (cltpt/base:text-object-property
                     text-obj
                     :keywords-alist)
                    "export_section")
         when (and (equal val "blog") (node-date node))
           collect node)
   'local-time:timestamp>
   :key #'node-date))

(defun blog-entries (rmr)
  (let* ((nodes (blog-nodes rmr))
         (entries
           (loop for node in nodes
                 for filepath = (cltpt/roam:node-file node)
                 for title = (cltpt/roam:node-title node)
                 for text-obj = (cltpt/roam:node-text-obj node)
                 for date-str = (local-time:format-timestring
                                 nil
                                 (node-date node)
                                 :format '(:short-weekday
                                           ", "
                                           (:day 2)
                                           #\space
                                           :short-month
                                           #\space
                                           (:year 4)))
                 collect (list
                          :href (format nil
                                        "~A~A.html"
                                        cltpt/html:*html-static-route*
                                        (title-to-filename title))
                          :title title
                          :date date-str))))
    entries))

(defvar *card-node* nil)

(defun generate-posts-list (nodes)
  (let ((entries
          (loop for node in nodes
                collect (let ((*card-node* node)
                              (cltpt/base:*convert-info*
                                (cltpt/base:merge-plist
                                 cltpt/base:*convert-info*
                                 (list
                                  :text-obj (cltpt/roam:node-text-obj node)
                                  :dest-fmt cltpt/base:*simple-format*
                                  :src-fmt cltpt/base:*simple-format*))))
                          (cltpt/base:convert-tree
                           (cltpt/base:parse
                            cltpt/base:*simple-format*
                            (read-template-file "card.html"))
                           cltpt/base:*simple-format*
                           cltpt/base:*simple-format*)))))
    (format nil
            "<div class=\"posts-list\">
~A
</div>"
            (cltpt/base:concat entries))))

(defun generate-blog-entries-html (nodes)
  (let ((blog-nodes (sort
                     (loop for node in nodes
                           for text-obj = (cltpt/roam:node-text-obj node)
                           for val = (cltpt/base:alist-get
                                      (cltpt/base:text-object-property
                                       text-obj
                                       :keywords-alist)
                                      "export_section")
                           when (and (equal val "blog")
                                     (node-date node))
                             collect node)
                     'local-time:timestamp>
                     :key #'node-date)))
    (generate-posts-list blog-nodes)))

(defun generate-collage (nodes collage-title)
  (with-output-to-string (out)
    (format out
            "<div class=\"container\">
<div class=\"gallery-section\">
<h2 class=\"section-title\">~A</h2>
<div class=\"gallery-grid\">"
            collage-title)
    (loop for node in nodes
          for filepath = (cltpt/roam:node-file node)
          for title = (cltpt/roam:node-title node)
          for text-obj = (cltpt/roam:node-text-obj node)
          do (let* (;; we have to bind that the post-lexer code in my files can use the roamer
                    ;; TOOD: make this not needed
                    (cltpt/roam:*roam-parse-data*
                      (list :roamer *rmr*
                            :filepath-format nil
                            :node node))
                    (image (cltpt/org-mode::text-object-org-keyword-value
                            (cltpt/roam:node-text-obj
                             node)
                            "image"))
                    (image1 (if (uiop:probe-file* image)
                                (export-static-file image)
                                (format t
                                        "collage image ~A doesnt exist~%"
                                        image)))
                    (href (cltpt/roam:node-info-format-str node *filepath-format*))
                    (entry-html "<div class=\"gallery-item\" onclick=\"window.location='~A'\">
  <div class=\"item-image\" />
    <img src=\"~A\" />
  </div>
  <div class=\"item-overlay\"><h3 class=\"item-title\">~A</h3><p class=\"item-subtitle\"></p></div>
</div>"
                                ))
               (format out
                       entry-html
                       href
                       image1
                       title)))
    (write-sequence "</div>
</div>
</div>"
                    out)))

(defun find-linked-files (rmr root)
  "helper function to find all linked files from a node."
  (let ((linked-files)
        (nodes-left (list root)))
    (loop
      while nodes-left
      for node = (pop nodes-left)
      for text-obj = (cltpt/roam:node-text-obj node)
      do (loop
           for link-obj
             in (cltpt/base:find-children-recursively
                 text-obj
                 (lambda (child)
                   (typep child 'cltpt/base:text-link)))
           do (let* (;; we have to bind this so resolve-link can use the roamer
                     (cltpt/roam:*roam-convert-data*
                       (list :roamer rmr
                             :filepath-format nil
                             :node node))
                     (result (cltpt/base:text-link-resolve link-obj))
                     (linked-file
                       (when result
                         (cltpt/base:target-filepath result))))
                (when (and linked-file
                           (not (member linked-file
                                        *excluded-files*
                                        :test 'string=)))
                  (unless (member linked-file
                                  (cons (cltpt/roam:node-file node) linked-files)
                                  :test 'string=)
                    (push linked-file linked-files)
                    (when (typep result 'cltpt/roam:node)
                      (push result nodes-left)))))))
    linked-files))

(defun encode-list-of-plists-to-json (list-of-plists)
  "encodes a list of plists to a JSON string.
it works by first converting the plists to alists with string keys, which
cl-json's standard encoder handles perfectly."
  (let ((list-of-alists
          (loop for plist in list-of-plists
                collect (loop for (key value) on plist by #'cddr
                              ;; convert keyword keys (like :tags) to strings ("tags")
                              collect (cons (string-downcase (symbol-name key))
                                            value)))))
    (cl-json:encode-json-to-string list-of-alists)))

(defun export-metadata-to-json (rmr output-file file-predicate)
  (let ((metadata
          (loop for node in (cltpt/roam:roamer-nodes rmr)
                for text-obj = (cltpt/roam:node-text-obj node)
                for tags = (cltpt/base:text-object-property text-obj :tags)
                when (and (funcall file-predicate (cltpt/roam:node-file node))
                          (not (and tags (member "noexport" tags :test 'string=))))
                  collect (list
                           :date (cltpt/base:text-object-property text-obj :date)
                           :tags tags
                           :id (cltpt/roam:node-id node)
                           :title (cltpt/roam:node-title node)
                           :filepath (cltpt/roam:node-info-format-str node *filepath-format*)
                           :description (cltpt/roam:node-desc node)))))
    (with-open-file (stream output-file
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (write-string (encode-list-of-plists-to-json metadata)
                    stream))))

(defclass my-text-obj (cltpt/base:text-object)
  ())

(defmethod cltpt/base:text-object-convert ((obj my-text-obj)
                                           (backend cltpt/base:text-format))
  (list :text (cltpt/buffer:buffer-own-text obj)
        :escape nil))

(defun read-template-file (template-file)
  (uiop:read-file-string (cltpt/file-utils:join-paths *template-dir* template-file)))

;; we have to do this because by default a dummy text-object is created that doesnt have a special
;; text-object-convert assigned to it and so its results get :escape t by the default one.
(defun wrap-into-unescaping-obj (txt)
  (let ((obj (make-instance 'my-text-obj)))
    (setf (cltpt/buffer:buffer-own-text obj) txt)
    obj))

(defun read-template-file-into-text-obj (template-file)
  (let ((txt (uiop:read-file-string (cltpt/file-utils:join-paths *template-dir* template-file))))
    (wrap-into-unescaping-obj txt)))