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
  (if (stringp element)
      (progn
        (message (concat "Element: \"" element "\" is a string.\n"))
        (substring-no-properties element))
    (let* ((el (org-element-put-property element :parent nil))
           (type (-> (org-element-type el)
                     (prin1-to-string)))
           (contents (org-element-contents el)))
      (message (concat "Element is a " type))
      (list :type type
            :props
            (let ((props (-second-item el)))
              (cond ((string= type "headline")
                     (plist-put props :title
                                  (->> (plist-get props :title)
                                       (-map #'gn/org-ast->edn)
                                       (apply 'vector))))

                    ((string= type "link")
                     (plist-put props :format
                                (-> (plist-get props :format)
                                    (prin1-to-string))))

                    ((string= type "plain-list")
                     (-> props
                         (plist-put :type
                                    (-> (plist-get props :type)
                                        (prin1-to-string)))
                         (plist-put :structure
                                    (->> (plist-get props :structure)
                                         (--map (apply 'vector it))
                                         (apply 'vector)))))

                    ((string= type "item") props
                     (plist-put props :structure
                                (->> (plist-get props :structure)
                                     (--map (apply 'vector it))
                                     (apply 'vector))))

                    (t props)))
            :contents
            (when contents
              (->> contents
                   (-map #'gn/org-ast->edn)
                   (apply 'vector)))))))

(defun gn/ox-export-as-edn ()
  "Convert org to edn and export it to a buffer."
  (interactive)
  (message "

Export from org to edn starting...")
  (let* ((edn (let* ((element-ast (org-element-parse-buffer)))
                (gn/org-ast->edn element-ast))))
    (with-temp-buffer
      (insert "(ns emacs.preview.data)

(def image-data nil)

(def org-data
")
      (parseedn-print edn)
      (insert ")")
      (write-file gn/ox-edn-path)))
  (message "Export from org to edn finished."))

(provide 'ox-edn)

;;; ox-edn.el ends here

