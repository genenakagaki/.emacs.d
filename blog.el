;;; ox-html.el --- HTML Back-End for Org Export Engine -*- lexical-binding: t; -*-

;; Copyright (C) 2011-2024 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten.dominik@gmail.com>
;;      Jambunathan K <kjambunathan at gmail dot com>
;; Maintainer: TEC <orgmode@tec.tecosaur.net>
;; Keywords: outlines, hypermedia, calendar, wp

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements a HTML back-end for Org generic exporter.
;; See Org manual for more information.

;;; Code:

;;; Dependencies

(require 'org-macs)
(org-assert-version)

(require 'cl-lib)
(require 'format-spec)
(require 'ox)
(require 'ox-publish)
(require 'table)


;;; Function Declarations

(declare-function org-id-find-id-file "org-id" (id))
(declare-function htmlize-region "ext:htmlize" (beg end))
(declare-function mm-url-decode-entities "mm-url" ())
(declare-function org-at-heading-p "org" (&optional _))
(declare-function org-back-to-heading "org" (&optional invisible-ok))
(declare-function org-next-visible-heading "org" (arg))

(defvar htmlize-css-name-prefix)
(defvar htmlize-output-type)
(defvar htmlize-output-type)
(defvar htmlize-css-name-prefix)


(setq print-level 10)
(setq print-length 100)

;;; Backend helper functions
(defun gn/blog-render-unsupported (element-type)
  (message (format "gn/blog-render %s not supported" element-type))
  nil)

(defun gn/blog-encode-text (text)
  (let ((result text))
    (dolist (pair '(("&" . "&amp;")
                    ("<" . "&lt;")
                    (">" . "&gt;"))
                  result)
      (setq result (replace-regexp-in-string (car pair) (cdr pair) result t t)))
    (format "%S" (substring-no-properties result))))


;;; Define Back-End

(defun gn/blog-render-bold (_bold contents _info)
  (format "[:b %s]" contents))

(defun gn/blog-render-center-block (center-block contents info)
  (gn/blog-render-unsupported "center-block"))

(defun gn/blog-render-clock (clock contents info) 
  (gn/blog-render-unsupported "clock"))

(defun gn/blog-render-code (code contents info) 
  (format "[:code %s]" (gn/blog-encode-text (org-element-property :value code))))

(defun gn/blog-render-drawer (drawer contents info) 
  (gn/blog-render-unsupported "drawer"))

(defun gn/blog-render-dynamic-block (dynamic-block contents info) 
  (gn/blog-render-unsupported "dynamic-block"))

(defun gn/blog-render-entity (entity contents info) 
  (gn/blog-render-unsupported "entity"))

(defun gn/blog-render-example-block (example-block contents info) 
  (format "[:example-block %s]" contents))

(defun gn/blog-render-export-block (export-block contents info) 
  (gn/blog-render-unsupported "export-block"))

(defun gn/blog-render-export-snippet (export-snippet contents info) 
  (gn/blog-render-unsupported "export-snippet"))

(defun gn/blog-render-fixed-width (fixed-width contents info) 
  (gn/blog-render-unsupported "fixed-width"))

(defun gn/blog-render-footnote-reference (footnote-reference contents info) 
  (gn/blog-render-unsupported "footnote-reference"))

(defun gn/blog-render-headline (headline contents info) 
  "A transcoder for headlines."
  (message "printing for headline")
  ;; (pp (org-element-property :title headline))
  (let* ((level (org-export-get-relative-level headline info))
         (text (org-export-data (org-element-property :title headline) info))
         (tags (and (plist-get info :with-tags)
                    (org-export-get-tags headline info)))
         (contents (or contents ""))
         (id (org-html--reference headline info)))
    (if (< 6 level)
        (throw 'gn/blog-render-headline "Cannot render headline level more than 6.")
      (format "
[:div 
  [:h%d {:id %S} %s] 
  %s]
"
              level
              id
              (format "[:a {:href \"#%s\"} %s]" id text)
              (concat contents)))))



(defun gn/blog-render-horizontal-rule (horizontal-rule contents info) 
  "[:hr]")

(defun gn/blog-render-inline-src-block (inline-src-block contents info) 
  "Transcoder for inline src block"
  (let* ((lang (org-element-property :language inline-src-block))
         (code (org-html-fontify-code
                (org-element-property :value inline-src-block)
                lang)))
    (format "[:code.language-%s %s]" lang code)))

(defun gn/blog-render-inlinetask (inlinetask contents info) 
  (gn/blog-render-unsupported "inlinetask"))

(defun gn/blog-render-inner-template (contents info) 
  contents)

(defun gn/blog-render-italic (italic contents info) 
  (format "[:i %s]" contents))

(defun gn/blog-render-item (item contents info) 
  (let* ((plain-list (org-export-get-parent item))
         (type (org-element-property :type plain-list))
         (counter (org-element-property :counter item))
         (checkbox (org-element-property :checkbox item))
         (term (let ((tag (org-element-property :tag item)))
                 (and tag (org-export-data tag info))))
         (text (if (org-string-nw-p contents)
                   (org-trim contents)
                 "")))
    (pcase type
      (`ordered
       (format "[:li %s %s]
"
               (if counter (format "{:value %S}" counter) "")
               text))
      (`unordered
       (format "[:li %s]
"
               text))
      (`descriptive
       (format "%s [:dd %s]
"
               (if term (format "[:dt %s]" term) "")
               text)))))

(defun gn/blog-render-keyword (keyword contents info) 
  (gn/blog-render-unsupported "keyword"))

(defun gn/blog-render-latex-environment (latex-environment contents info) 
  (gn/blog-render-unsupported "latex-environment"))

(defun gn/blog-render-latex-fragment (latex-fragment contents info) 
  (gn/blog-render-unsupported "latex-fragment"))

(defun gn/blog-render-line-break (line-break contents info) 
  (format "[:br ]" contents))

(defun gn/blog-render-link (link desc info) 
  "render link"
  (message "printing for link")
  (pp link)
  (let* ((type (org-element-property :type link))
         (path (org-element-property :raw-link link))
         ;; Ensure DESC really exists, or set it to nil.
         (desc (org-string-nw-p desc)))
    (cond
     ;; Image file.
     ((org-export-inline-image-p link org-html-inline-image-rules)
      (let ((caption (org-export-data
                      (org-export-get-caption
                       (org-export-get-parent-element link))
                      info)))
        (if (org-string-nw-p caption)
            (format "
[:figure 
  [:img {:src %S :alt %s}] 
  [:figcaption %s]]
"
                    path caption caption)
          (format "[:img {:src %S}]
" path))))
     ;; External link with a description part.
     (t (format "[:a {:href %S} %s]
"
                path (or desc path))))))

(defun gn/blog-render-node-property (node-property contents info) 
  (gn/blog-render-unsupported "node-property"))

(defun gn/blog-render-paragraph (paragraph contents info) 
  (format "[:p %s]" contents))

(defun gn/blog-render-plain-list (plain-list contents info) 
  "render plain list"
  (let* ((type (pcase (org-element-property :type plain-list)
                 (`ordered "ol")
                 (`unordered "ul")
                 (`descriptive "dl")
                 (other (error "Unknown HTML list type: %s" other)))))
    (format "[:%s %s]"
            type
            contents)))


(defun gn/blog-render-plain-text (text info) 
  "render plain text"
  ;; (let ((output text))
  ;;   ;; Protect following characters: <, >, &.
  ;;   (setq output (org-html-encode-plain-text output))
  ;;   ;; Handle smart quotes.  Be sure to provide original string since
  ;;   ;; OUTPUT may have been modified.
  ;;   (when (plist-get info :with-smart-quotes)
  ;;     (setq output (org-export-activate-smart-quotes output :html info text)))
  ;;   ;; Handle special strings.
  ;;   (when (plist-get info :with-special-strings)
  ;;     (setq output (org-html-convert-special-strings output)))
  ;;   ;; Handle break preservation if required.
  ;;   (when (plist-get info :preserve-breaks)
  ;;     (setq output
  ;;           (replace-regexp-in-string
  ;;            "\\(\\\\\\\\\\)?[ \t]*\n"
  ;;            (concat "[:br]\n") output)))
  ;;   ;; Return value.
  ;;   output)
  (gn/blog-encode-text text))

(defun gn/blog-render-planning (planning contents info) 
  (gn/blog-render-unsupported "planning"))

(defun gn/blog-render-property-drawer (property-drawer contents info) 
  (gn/blog-render-unsupported "property-drawer"))

(defun gn/blog-render-quote-block (quote-block contents info) 
  (format "[:block-quote %s]" contents))

(defun gn/blog-render-radio-target (radio-target contents info) 
  (gn/blog-render-unsupported "radio-target"))

(defun gn/blog-render-section (section contents info) 
  "render section"
  (let ((parent (org-export-get-parent-headline section)))
    ;; Before first headline: no container, just return CONTENTS.
    (if (not parent) contents
      ;; Get div's class and id references.
      (format "[:div %s]" (or contents "")))))

(defun gn/blog-render-special-block (special-block contents info) 
  (gn/blog-render-unsupported "special-block"))

(defun gn/blog-render-src-block (src-block contents info) 
  "render src block"
  (let* ((lang (org-element-property :language src-block))
         (code (car (org-export-unravel-code src-block))))
    (if (not lang) (format "[:pre %s]" code)
      (format "[:pre [:code {:class \"language-%s\"} %S]]"
              lang code))))

(defun gn/blog-render-statistics-cookie (statistics-cookie contents info) 
  (gn/blog-render-unsupported "statistic-cookie"))

(defun gn/blog-render-strike-through (strike-through contents info) 
  (format "[:del %s]" contents))

(defun gn/blog-render-subscript (subscript contents info) 
  (format "[:sub %s]" contents))

(defun gn/blog-render-superscript (superscript contents info) 
  (format "[:sup %s]" contents))

(defun gn/blog-render-table (table contents info) 
  (format "[:table %s]" contents))

(defun gn/blog-render-table-cell (table-cell contents info) 
  (format "[:td %s]" contents))

(defun gn/blog-render-table-row (table-row contents info) 
  (format "[:tr %s]" contents))

(defun gn/blog-render-target (target contents info) 
  (gn/blog-render-unsupported "target"))

(defun gn/blog-render-template (contents info) 
  (format "[:main %s]" contents))

(defun gn/blog-render-timestamp (timestamp contents info) 
  (gn/blog-render-unsupported "timestamp"))

(defun gn/blog-render-underline (underline contents info) 
  (format "[:span.underline %s]" contents))

(defun gn/blog-render-verbatim (verbatim contents info) 
  "Transcode VERBATIM from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format "[:code %s]" (org-element-property :value verbatim)))

(alist-get '4 '((1 . "test")
                (2 . "hello")
                (3 . "thirrd")))

(defun gn/blog-render-verse-block (verse-block contents info) 
  (gn/blog-render-unsupported "verse-block"))

(defun gn/blog-export-as-blog
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to an HTML buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"<body>\" and \"</body>\" tags.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org HTML Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (message "


Running gn/blog-export-as-blog...")
  (org-export-to-buffer 'blog "*Org BLOG Export*"
    async subtreep visible-only body-only ext-plist
    (lambda () (set-auto-mode t))))

(org-export-define-backend 'blog
  '((bold . gn/blog-render-bold)
    (center-block . gn/blog-render-center-block)
    (clock . gn/blog-render-clock)
    (code . gn/blog-render-code)
    (drawer . gn/blog-render-drawer)
    (dynamic-block . gn/blog-render-dynamic-block)
    (entity . gn/blog-render-entity)
    (example-block . gn/blog-render-example-block)
    (export-block . gn/blog-render-export-block)
    (export-snippet . gn/blog-render-export-snippet)
    (fixed-width . gn/blog-render-fixed-width)
    (footnote-reference . gn/blog-render-footnote-reference)
    (headline . gn/blog-render-headline)
    (horizontal-rule . gn/blog-render-horizontal-rule)
    (inline-src-block . gn/blog-render-inline-src-block)
    (inlinetask . gn/blog-render-inlinetask)
    (inner-template . gn/blog-render-inner-template)
    (italic . gn/blog-render-italic)
    (item . gn/blog-render-item)
    (keyword . gn/blog-render-keyword)
    (latex-environment . gn/blog-render-latex-environment)
    (latex-fragment . gn/blog-render-latex-fragment)
    (line-break . gn/blog-render-line-break)
    (link . gn/blog-render-link)
    (node-property . gn/blog-render-node-property)
    (paragraph . gn/blog-render-paragraph)
    (plain-list . gn/blog-render-plain-list)
    (plain-text . gn/blog-render-plain-text)
    (planning . gn/blog-render-planning)
    (property-drawer . gn/blog-render-property-drawer)
    (quote-block . gn/blog-render-quote-block)
    (radio-target . gn/blog-render-radio-target)
    (section . gn/blog-render-section)
    (special-block . gn/blog-render-special-block)
    (src-block . gn/blog-render-src-block)
    (statistics-cookie . gn/blog-render-statistics-cookie)
    (strike-through . gn/blog-render-strike-through)
    (subscript . gn/blog-render-subscript)
    (superscript . gn/blog-render-superscript)
    (table . gn/blog-render-table)
    (table-cell . gn/blog-render-table-cell)
    (table-row . gn/blog-render-table-row)
    (target . gn/blog-render-target)
    (template . gn/blog-render-template)
    (timestamp . gn/blog-render-timestamp)
    (underline . gn/blog-render-underline)
    (verbatim . gn/blog-render-verbatim)
    (verse-block . gn/blog-render-verse-block))
  :filters-alist '((:filter-options . org-html-infojs-install-script)
		   (:filter-parse-tree . org-html-image-link-filter)
		   (:filter-final-output . org-html-final-function))
  :menu-entry
  '(?h "Export to BLOG"
       ((?H "As BLOG buffer" org-html-export-as-html)
	(?h "As BLOG file" org-html-export-to-html)
	(?o "As BLOG file and open"
	    (lambda (a s v b)
	      (if a (org-html-export-to-html t s v b)
		(org-open-file (org-html-export-to-html nil s v b)))))))
  :options-alist
  nil
  ;; '((:html-doctype "HTML_DOCTYPE" nil org-html-doctype)
   ;;  (:html-container "HTML_CONTAINER" nil org-html-container-element)
   ;;  (:html-content-class "HTML_CONTENT_CLASS" nil org-html-content-class)
   ;;  (:description "DESCRIPTION" nil nil newline)
   ;;  (:keywords "KEYWORDS" nil nil space)
   ;;  (:html-html5-fancy nil "html5-fancy" org-html-html5-fancy)
   ;;  (:html-link-use-abs-url nil "html-link-use-abs-url" org-html-link-use-abs-url)
   ;;  (:html-link-home "HTML_LINK_HOME" nil org-html-link-home)
   ;;  (:html-link-up "HTML_LINK_UP" nil org-html-link-up)
   ;;  (:html-mathjax "HTML_MATHJAX" nil "" space)
   ;;  (:html-equation-reference-format "HTML_EQUATION_REFERENCE_FORMAT" nil org-html-equation-reference-format t)
   ;;  (:html-postamble nil "html-postamble" org-html-postamble)
   ;;  (:html-preamble nil "html-preamble" org-html-preamble)
   ;;  (:html-head "HTML_HEAD" nil org-html-head newline)
   ;;  (:html-head-extra "HTML_HEAD_EXTRA" nil org-html-head-extra newline)
   ;;  (:subtitle "SUBTITLE" nil nil parse)
   ;;  (:html-head-include-default-style
   ;;   nil "html-style" org-html-head-include-default-style)
   ;;  (:html-head-include-scripts nil "html-scripts" org-html-head-include-scripts)
   ;;  (:html-allow-name-attribute-in-anchors
   ;;   nil nil org-html-allow-name-attribute-in-anchors)
   ;;  (:html-divs nil nil org-html-divs)
   ;;  (:html-checkbox-type nil nil org-html-checkbox-type)
   ;;  (:html-extension nil nil org-html-extension)
   ;;  (:html-footnote-format nil nil org-html-footnote-format)
   ;;  (:html-footnote-separator nil nil org-html-footnote-separator)
   ;;  (:html-footnotes-section nil nil org-html-footnotes-section)
   ;;  (:html-format-drawer-function nil nil org-html-format-drawer-function)
   ;;  (:html-format-headline-function nil nil org-html-format-headline-function)
   ;;  (:html-format-inlinetask-function
   ;;   nil nil org-html-format-inlinetask-function)
   ;;  (:html-home/up-format nil nil org-html-home/up-format)
   ;;  (:html-indent nil nil org-html-indent)
   ;;  (:html-infojs-options nil nil org-html-infojs-options)
   ;;  (:html-infojs-template nil nil org-html-infojs-template)
   ;;  (:html-inline-image-rules nil nil org-html-inline-image-rules)
   ;;  (:html-link-org-files-as-html nil nil org-html-link-org-files-as-html)
   ;;  (:html-mathjax-options nil nil org-html-mathjax-options)
   ;;  (:html-mathjax-template nil nil org-html-mathjax-template)
   ;;  (:html-metadata-timestamp-format nil nil org-html-metadata-timestamp-format)
   ;;  (:html-postamble-format nil nil org-html-postamble-format)
   ;;  (:html-preamble-format nil nil org-html-preamble-format)
   ;;  (:html-prefer-user-labels nil nil org-html-prefer-user-labels)
   ;;  (:html-self-link-headlines nil nil org-html-self-link-headlines)
   ;;  (:html-table-align-individual-fields
   ;;   nil nil org-html-table-align-individual-fields)
   ;;  (:html-table-caption-above nil nil org-html-table-caption-above)
   ;;  (:html-table-data-tags nil nil org-html-table-data-tags)
   ;;  (:html-table-header-tags nil nil org-html-table-header-tags)
   ;;  (:html-table-use-header-tags-for-first-column
   ;;   nil nil org-html-table-use-header-tags-for-first-column)
   ;;  (:html-tag-class-prefix nil nil org-html-tag-class-prefix)
   ;;  (:html-text-markup-alist nil nil org-html-text-markup-alist)
   ;;  (:html-todo-kwd-class-prefix nil nil org-html-todo-kwd-class-prefix)
   ;;  (:html-toplevel-hlevel nil nil org-html-toplevel-hlevel)
   ;;  (:html-use-infojs nil nil org-html-use-infojs)
   ;;  (:html-validation-link nil nil org-html-validation-link)
   ;;  (:html-viewport nil nil org-html-viewport)
   ;;  (:html-inline-images nil nil org-html-inline-images)
   ;;  (:html-table-attributes nil nil org-html-table-default-attributes)
   ;;  (:html-table-row-open-tag nil nil org-html-table-row-open-tag)
   ;;  (:html-table-row-close-tag nil nil org-html-table-row-close-tag)
   ;;  (:html-xml-declaration nil nil org-html-xml-declaration)
   ;;  (:html-wrap-src-lines nil nil org-html-wrap-src-lines)
   ;;  (:html-klipsify-src nil nil org-html-klipsify-src)
   ;;  (:html-klipse-css nil nil org-html-klipse-css)
   ;;  (:html-klipse-js nil nil org-html-klipse-js)
   ;;  (:html-klipse-selection-script nil nil org-html-klipse-selection-script)
   ;;  (:infojs-opt "INFOJS_OPT" nil nil)
   ;;  ;; Redefine regular options.
   ;;  (:creator "CREATOR" nil org-html-creator-string)
   ;;  (:with-latex nil "tex" org-html-with-latex)
   ;;  ;; Retrieve LaTeX header for fragments.
   ;;  (:latex-header "LATEX_HEADER" nil nil newline))
  )


(provide 'ox-blog)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; ox-html.el ends here

