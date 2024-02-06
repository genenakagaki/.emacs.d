;;; ox-edn.el --- testing

;;; Commentary:
;; 

(require 'org-element)
(require 'parseedn)
(require 'dash)

;;; Code:

(defvar gn/ox-edn-path
  (expand-file-name
   (concat user-emacs-directory "emacs-preview/src/emacs/preview/data.cljs")))

(defun gn/org-ast->edn (element)
  "Convert ELEMENT org-ast to edn data."
  (cond
   ((not element)
    nil)

   ((stringp element)
    (progn
      (message (concat "Element is a string.\n"))
      (substring-no-properties element)))

   (t
    (let* ((el (org-element-put-property element :parent nil))
           (type (-> (org-element-type el)
                     (prin1-to-string)))
           (contents (org-element-contents el)))
      (message (concat "Element is a " type))
      (list :type type
            :props
            (let ((props (-second-item el)))
              (cond ((string= type "babel-call")
                     props)

                    ((string= type "bold")
                     props)

                    ((string= type "center-block")
                     props)

                    ((string= type "citation")
                     props)

                    ((string= type "citation-reference")
                     props)

                    ((string= type "clock")
                     props)

                    ((string= type "code")
                     props)

                    ((string= type "comment")
                     props)

                    ((string= type "comment-block")
                     props)

                    ((string= type "diary-sexp")
                     props)

                    ((string= type "drawer")
                     props)

                    ((string= type "dynamic-block")
                     props)

                    ((string= type "entity")
                     props)

                    ((string= type "example-block")
                     (plist-put props :number-lines nil))

                    ((string= type "export-block")
                     props)

                    ((string= type "export-snippet")
                     props)

                    ((string= type "fixed-width")
                     props)

                    ((string= type "footnote-definition")
                     props)

                    ((string= type "footnote-reference")
                     props)

                    ((string= type "headline")
                     (-> props
                         (plist-put :title
                                    (->> (plist-get props :title)
                                         (-map #'gn/org-ast->edn)
                                         (apply 'vector)))
                         (plist-put :tags
                                    (->> (plist-get props :tags)
                                         (apply 'vector)))
                         (plist-put :todo-type
                                    (-> (plist-get props :todo-type)
                                        (prin1-to-string)))
                         (plist-put :todo-keyword
                                    (-> (plist-get props :todo-keyword)
                                        (gn/org-ast->edn)))
                         (plist-put :scheduled
                                    (-> (plist-get props :scheduled)
                                        (gn/org-ast->edn)))
                         (plist-put :deadline
                                    (-> (plist-get props :deadline)
                                        (gn/org-ast->edn)))))

                    ((string= type "horizontal-rule")
                     props)

                    ((string= type "inline-babel-call")
                     props)

                    ((string= type "inline-src-block")
                     props)

                    ((string= type "inlinetask")
                     props)

                    ((string= type "italic")
                     props)

                    ((string= type "item")
                     (-> props
                         (plist-put :structure
                                    (->> (plist-get props :structure)
                                         (--map (apply 'vector it))
                                         (apply 'vector)))
                         (plist-put :checkbox
                                    (-> (plist-get props :checkbox)
                                        (prin1-to-string)))
                         (plist-put :tag
                                    (apply 'vector (plist-get props :tag)))))

                    ((string= type "keyword")
                     props)

                    ((string= type "latex-environment")
                     props)

                    ((string= type "latex-fragment")
                     props)

                    ((string= type "line-break")
                     props)

                    ((string= type "link")
                     (plist-put props :format
                                (-> (plist-get props :format)
                                    (prin1-to-string))))

                    ((string= type "macro")
                     props)

                    ((string= type "node-property")
                     props)

                    ((string= type "paragraph")
                     (plist-put props :caption
                                (-> (plist-get props :caption)
                                    (-first-item)
                                    (-first-item)
                                    (-first-item)
                                    (gn/org-ast->edn))))

                    ((string= type "plain-list")
                     (-> props
                         (plist-put :type
                                    (-> (plist-get props :type)
                                        (prin1-to-string)))
                         (plist-put :structure
                                    (->> (plist-get props :structure)
                                         (--map (apply 'vector it))
                                         (apply 'vector)))))

                    ((string= type "planning")
                     (-> props
                         (plist-put :scheduled
                                    (-> (plist-get props :structure)
                                        (gn/org-ast->edn)))
                         (plist-put :deadline
                                    (-> (plist-get props :deadline)
                                        (gn/org-ast->edn)))))

                    ((string= type "property-drawer")
                     props)

                    ((string= type "quote-block")
                     props)

                    ((string= type "radio-target")
                     props)

                    ((string= type "section")
                     props)

                    ((string= type "special-block")
                     props)

                    ((string= type "src-block")
                     (plist-put props :number-lines nil))

                    ((string= type "statistics-cookie")
                     props)

                    ((string= type "strike-through")
                     props)

                    ((string= type "subscript")
                     props)

                    ((string= type "superscript")
                     props)

                    ((string= type "table")
                     (plist-put props :type
                                (-> (plist-get props :type)
                                    (prin1-to-string))))

                    ((string= type "table-cell")
                     props)

                    ((string= type "table-row")
                     (plist-put props :type
                                (-> (plist-get props :type)
                                    (prin1-to-string))))

                    ((string= type "target")
                     props)

                    ((string= type "timestamp")
                     (-> props
                         (plist-put :type
                                    (-> (plist-get props :type)
                                        (prin1-to-string)))
                         (plist-put :repeater-type
                                    (-> (plist-get props :repeater-type)
                                        (prin1-to-string)))
                         (plist-put :repeater-unit
                                    (-> (plist-get props :repeater-unit)
                                        (prin1-to-string)))))

                    ((string= type "underline")
                     props)

                    ((string= type "verbatim")
                     props)

                    ((string= type "verse-block")
                     props)

                    (t props)))
            :contents
            (when contents
              (->> contents
                   (-map #'gn/org-ast->edn)
                   (apply 'vector))))))))

(defun gn/ox-export-as-edn ()
  "Convert org to edn and export it to a buffer."
  (interactive)
  (message "

Export from org to edn starting...")
  (if (eq major-mode 'org-mode)
      (progn
        (let* ((edn (let* ((element-ast (org-element-parse-buffer)))
                      (gn/org-ast->edn element-ast))))
          (with-temp-buffer
            (insert "(ns emacs.preview.data)

(def image-data nil)

(def org-data
  (let [t true]
    
")
            (parseedn-print edn)
            (insert "))")
            (write-file gn/ox-edn-path))
          )
        (message "Export from org to edn finished successfully."))
    (message "Current mode is not org-mode.
Export from org to edn cancelled")))

(provide 'ox-edn)

;;; ox-edn.el ends here

