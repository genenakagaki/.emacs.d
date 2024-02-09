;;; emacs-preview.el --- Preview files open in Emacs with a web browser

;;; Commentary:
;; 


;;; Code:
(defvar gn/preview-file (expand-file-name "emacs-preview/src/preview-content.html" user-emacs-directory))

(defun gn/preview-image (image-url)
  "Preview IMAGE-URL image."
  (with-temp-file gn/preview-file
    (progn (insert "<img src=\"" image-url "\"/>"))))


                                        ;<img src=\"http://localhost:8080/png/SyfFKj2rKt3CoKnELR1IA4ajBW40\"/>
                                        ;<img src=\"http://localhost:8080/png/SyfFKj2rKt3CoKnELR1Io4ZDoSa70000\"/>


(defun gn/preview-plantuml ()
  "Preview for plantuml mode."
  (message "\n")
  (message  (substring-no-properties (buffer-string)))
  (message "\n"))

(gn/preview-plantuml)

(require 'org)
(require 'ox)

;; Reference
;; https://www.reddit.com/r/emacs/comments/swvbmm/you_want_to_write_a_custom_org_backend_lets_write/

;;; Transcoders 
;; To check the data for each transcoders,
;; run (org-element-parse-buffer) on the org mode buffer

(defun ox-clj/bold (object contents info)
  ""
  (format "[typography/bold %s]" contents))

(defun ox-clj/code (object contents info)
  "Transcode CODE from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format "[typography/code \"%s\"]" (org-element-property :value code)))

(defun ox-clj/example-block (example-block contents info)
  "Transcode a EXAMPLE-BLOCK element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (let ((attributes (org-export-read-attribute :attr_html example-block)))
    (if (plist-get attributes :textarea)
	(org-html--textarea-block example-block)
      (format "<pre class=\"example\"%s>\n%s</pre>"
	      (let* ((reference (org-html--reference example-block info))
		     (a (org-html--make-attribute-string
			 (if (or (not reference) (plist-member attributes :id))
			     attributes
			   (plist-put attributes :id reference)))))
		(if (org-string-nw-p a) (concat " " a) ""))
	      (org-html-format-code example-block info)))))

(defun ox-clj/headline (object contents info)
  ""
  (let ((level (org-element-property :level object))
        (title (org-export-data (org-element-property :title object) info)))
    (concat
     (format "[typography/h%s %s]" level title)
     contents)))

(defun ox-clj/paragraph (object contents info)
  ""
  (concat "[typography/body " contents "]"))

(defun ox-clj/plain-list (object contents info)
  ""
  (let ((type (pcase (org-element-property :type object)
		 (`ordered ":numbered true")
		 (`unordered "")
		 (`descriptive "")
		 (other (error "Unknown list type: %s" other)))))
    (format "[ui-list %s]" contents)))

(defun ox-clj/plain-text (text info)
  ""
  (format "\"%s\"" text))

(defun ox-clj/section (object contents info)
  ""
  contents)

(org-export-define-backend
    'ox-clj
  '((bold . ox-clj/bold)
    ;;(center-block . org-html-center-block)
    ;;(clock . org-html-clock)
    (code . ox-clj/code)
    ;;(drawer . org-html-drawer)
    ;;(dynamic-block . org-html-dynamic-block)
    ;;(entity . org-html-entity)
    (example-block . org-html-example-block)
    (export-block . org-html-export-block)
    (export-snippet . org-html-export-snippet)
    (fixed-width . org-html-fixed-width)
    (footnote-reference . org-html-footnote-reference)
    (headline . org-html-headline)
    (horizontal-rule . org-html-horizontal-rule)
    (inline-src-block . org-html-inline-src-block)
    (inlinetask . org-html-inlinetask)
    (inner-template . org-html-inner-template)
    (italic . org-html-italic)
    (item . org-html-item)
    (keyword . org-html-keyword)
    (latex-environment . org-html-latex-environment)
    (latex-fragment . org-html-latex-fragment)
    (line-break . org-html-line-break)
    (link . org-html-link)
    (node-property . org-html-node-property)
    (paragraph . ox-clj/paragraph)
    (plain-list . ox-clj/plain-list)
    (plain-text . ox-clj/plain-text)
    (planning . org-html-planning)
    (property-drawer . org-html-property-drawer)
    (quote-block . org-html-quote-block)
    (radio-target . org-html-radio-target)
    (section . ox-clj/section)
    (special-block . org-html-special-block)
    (src-block . org-html-src-block)
    (statistics-cookie . org-html-statistics-cookie)
    (strike-through . org-html-strike-through)
    (subscript . org-html-subscript)
    (superscript . org-html-superscript)
    (table . org-html-table)
    (table-cell . org-html-table-cell)
    (table-row . org-html-table-row)
    (target . org-html-target)
    (template . org-html-template)
    (timestamp . org-html-timestamp)
    (underline . org-html-underline)
    (verbatim . org-html-verbatim)
    (verse-block . org-html-verse-block)
    ))

(org-export-define-backend 'html
  '((bold . org-html-bold)
    (center-block . org-html-center-block)
    (clock . org-html-clock)
    (code . org-html-code)
    (drawer . org-html-drawer)
    (dynamic-block . org-html-dynamic-block)
    (entity . org-html-entity)
    (example-block . org-html-example-block)
    (export-block . org-html-export-block)
    (export-snippet . org-html-export-snippet)
    (fixed-width . org-html-fixed-width)
    (footnote-reference . org-html-footnote-reference)
    (headline . org-html-headline)
    (horizontal-rule . org-html-horizontal-rule)
    (inline-src-block . org-html-inline-src-block)
    (inlinetask . org-html-inlinetask)
    (inner-template . org-html-inner-template)
    (italic . org-html-italic)
    (item . org-html-item)
    (keyword . org-html-keyword)
    (latex-environment . org-html-latex-environment)
    (latex-fragment . org-html-latex-fragment)
    (line-break . org-html-line-break)
    (link . org-html-link)
    (node-property . org-html-node-property)
    (paragraph . org-html-paragraph)
    (plain-list . org-html-plain-list)
    (plain-text . org-html-plain-text)
    (planning . org-html-planning)
    (property-drawer . org-html-property-drawer)
    (quote-block . org-html-quote-block)
    (radio-target . org-html-radio-target)
    (section . org-html-section)
    (special-block . org-html-special-block)
    (src-block . org-html-src-block)
    (statistics-cookie . org-html-statistics-cookie)
    (strike-through . org-html-strike-through)
    (subscript . org-html-subscript)
    (superscript . org-html-superscript)
    (table . org-html-table)
    (table-cell . org-html-table-cell)
    (table-row . org-html-table-row)
    (target . org-html-target)
    (template . org-html-template)
    (timestamp . org-html-timestamp)
    (underline . org-html-underline)
    (verbatim . org-html-verbatim)
    (verse-block . org-html-verse-block))
  :options-alist
  '((:description "DESCRIPTION" nil nil newline)
    (:keywords "KEYWORDS" nil nil space)
    (:html-postamble nil "html-postamble" org-html-postamble)
    (:html-preamble nil "html-preamble" org-html-preamble)))

(defun ox-clj/export ()
  ""
  (interactive)
  (org-export-to-buffer 'ox-clj "*ox-clj*"))

(require 'cl-lib)
(require 'ox-html)
(require 'ox-publish)

(defgroup org-export-md nil
  "Options specific to Markdown export back-end."
  :tag "Org Markdown"
  :group 'org-export
  :version "24.4"
  :package-version '(Org . "8.0"))

(org-export-define-derived-backend 'md 'html
  :filters-alist '((:filter-parse-tree . org-md-separate-elements))
  :menu-entry
  '(?m "Export to Markdown"
       ((?M "To temporary buffer"
	    (lambda (a s v b) (org-md-export-as-markdown a s v)))
	(?m "To file" (lambda (a s v b) (org-md-export-to-markdown a s v)))
	(?o "To file and open"
	    (lambda (a s v b)
	      (if a (org-md-export-to-markdown t s v)
		(org-open-file (org-md-export-to-markdown nil s v)))))))
  :translate-alist '((bold . org-md-bold)
		     (center-block . org-md--convert-to-html)
		     (code . org-md-verbatim)
		     (drawer . org-md--identity)
		     (dynamic-block . org-md--identity)
		     (example-block . org-md-example-block)
		     (export-block . org-md-export-block)
		     (fixed-width . org-md-example-block)
		     (headline . org-md-headline)
		     (horizontal-rule . org-md-horizontal-rule)
		     (inline-src-block . org-md-verbatim)
		     (inlinetask . org-md--convert-to-html)
		     (inner-template . org-md-inner-template)
		     (italic . org-md-italic)
		     (item . org-md-item)
		     (keyword . org-md-keyword)
                     (latex-environment . org-md-latex-environment)
                     (latex-fragment . org-md-latex-fragment)
		     (line-break . org-md-line-break)
		     (link . org-md-link)
		     (node-property . org-md-node-property)
		     (paragraph . org-md-paragraph)
		     (plain-list . org-md-plain-list)
		     (plain-text . org-md-plain-text)
		     (property-drawer . org-md-property-drawer)
		     (quote-block . org-md-quote-block)
		     (section . org-md-section)
		     (special-block . org-md--convert-to-html)
		     (src-block . org-md-example-block)
		     (table . org-md--convert-to-html)
		     (template . org-md-template)
		     (verbatim . org-md-verbatim))
  :options-alist
  '((:md-footnote-format nil nil org-md-footnote-format)
    (:md-footnotes-section nil nil org-md-footnotes-section)
    ))


;;; Filters

(defun org-md-separate-elements (tree _backend info)
  "Fix blank lines between elements.

TREE is the parse tree being exported.  BACKEND is the export
back-end used.  INFO is a plist used as a communication channel.

Enforce a blank line between elements.  There are two exceptions
to this rule:

  1. Preserve blank lines between sibling items in a plain list,

  2. In an item, remove any blank line before the very first
     paragraph and the next sub-list when the latter ends the
     current item.

Assume BACKEND is `md'."
  (org-element-map tree (remq 'item org-element-all-elements)
    (lambda (e)
      (org-element-put-property
       e :post-blank
       (if (and (eq (org-element-type e) 'paragraph)
		(eq (org-element-type (org-element-property :parent e)) 'item)
		(org-export-first-sibling-p e info)
		(let ((next (org-export-get-next-element e info)))
		  (and (eq (org-element-type next) 'plain-list)
		       (not (org-export-get-next-element next info)))))
	   0
	 1))))
  ;; Return updated tree.
  tree)

;;; Internal functions

(defun org-md--headline-referred-p (headline info)
  "Non-nil when HEADLINE is being referred to.
INFO is a plist used as a communication channel.  Links and table
of contents can refer to headlines."
  (unless (org-element-property :footnote-section-p headline)
    (or
     ;; Global table of contents includes HEADLINE.
     (and (plist-get info :with-toc)
	  (memq headline
		(org-export-collect-headlines info (plist-get info :with-toc))))
     ;; A local table of contents includes HEADLINE.
     (cl-some
      (lambda (h)
	(let ((section (car (org-element-contents h))))
	  (and
	   (eq 'section (org-element-type section))
	   (org-element-map section 'keyword
	     (lambda (keyword)
	       (when (equal "TOC" (org-element-property :key keyword))
		 (let ((case-fold-search t)
		       (value (org-element-property :value keyword)))
		   (and (string-match-p "\\<headlines\\>" value)
			(let ((n (and
				  (string-match "\\<[0-9]+\\>" value)
				  (string-to-number (match-string 0 value))))
			      (local? (string-match-p "\\<local\\>" value)))
			  (memq headline
				(org-export-collect-headlines
				 info n (and local? keyword))))))))
	     info t))))
      (org-element-lineage headline))
     ;; A link refers internally to HEADLINE.
     (org-element-map (plist-get info :parse-tree) 'link
       (lambda (link)
	 (equal headline
                ;; Ignore broken links.
                (condition-case nil
                    (org-export-resolve-id-link link info)
                  (org-link-broken nil))))
       info t))))

(defun org-md--headline-title (style level title &optional anchor tags)
  "Generate a headline title in the preferred Markdown headline style.
STYLE is the preferred style (`atx' or `setext').  LEVEL is the
header level.  TITLE is the headline title.  ANCHOR is the HTML
anchor tag for the section as a string.  TAGS are the tags set on
the section."
  (let ((anchor-lines (and anchor (concat anchor "\n\n"))))
    ;; Use "Setext" style
    (if (and (eq style 'setext) (< level 3))
        (let* ((underline-char (if (= level 1) ?= ?-))
               (underline (concat (make-string (length title) underline-char)
				  "\n")))
          (concat "\n" anchor-lines title tags "\n" underline "\n"))
      ;; Use "Atx" style
      (let ((level-mark (make-string level ?#)))
        (concat "\n" anchor-lines level-mark " " title tags "\n\n")))))

(defun org-md--build-toc (info &optional n _keyword scope)
  "Return a table of contents.

INFO is a plist used as a communication channel.

Optional argument N, when non-nil, is an integer specifying the
depth of the table.

When optional argument SCOPE is non-nil, build a table of
contents according to the specified element."
  (concat
   (unless scope
     (let ((style (plist-get info :md-headline-style))
	   (title (org-html--translate "Table of Contents" info)))
       (org-md--headline-title style 1 title nil)))
   (mapconcat
    (lambda (headline)
      (let* ((indentation
	      (make-string
	       (* 4 (1- (org-export-get-relative-level headline info)))
	       ?\s))
	     (bullet
	      (if (not (org-export-numbered-headline-p headline info)) "-   "
		(let ((prefix
		       (format "%d." (org-last (org-export-get-headline-number
						headline info)))))
		  (concat prefix (make-string (max 1 (- 4 (length prefix)))
					      ?\s)))))
	     (title
	      (format "[%s](#%s)"
		      (org-export-data-with-backend
		       (org-export-get-alt-title headline info)
		       (org-export-toc-entry-backend 'md)
		       info)
		      (or (org-element-property :CUSTOM_ID headline)
			  (org-export-get-reference headline info))))
	     (tags (and (plist-get info :with-tags)
			(not (eq 'not-in-toc (plist-get info :with-tags)))
			(org-make-tag-string
			 (org-export-get-tags headline info)))))
	(concat indentation bullet title tags)))
    (org-export-collect-headlines info n scope) "\n")
   "\n"))

(defun org-md--footnote-formatted (footnote info)
  "Formats a single footnote entry FOOTNOTE.
FOOTNOTE is a cons cell of the form (number . definition).
INFO is a plist with contextual information."
  (let* ((fn-num (car footnote))
         (fn-text (cdr footnote))
         (fn-format (plist-get info :md-footnote-format))
         (fn-anchor (format "fn.%d" fn-num))
         (fn-href (format " href=\"#fnr.%d\"" fn-num))
         (fn-link-to-ref (org-html--anchor fn-anchor fn-num fn-href info)))
    (concat (format fn-format fn-link-to-ref) " " fn-text "\n")))

(defun org-md--footnote-section (info)
  "Format the footnote section.
INFO is a plist used as a communication channel."
  (let* ((fn-alist (org-export-collect-footnote-definitions info))
         (fn-alist (cl-loop for (n _type raw) in fn-alist collect
                            (cons n (org-trim (org-export-data raw info)))))
         (headline-style (plist-get info :md-headline-style))
         (section-title (org-html--translate "Footnotes" info)))
    (when fn-alist
      (format (plist-get info :md-footnotes-section)
              (org-md--headline-title headline-style 1 section-title)
              (mapconcat (lambda (fn) (org-md--footnote-formatted fn info))
                         fn-alist
                         "\n")))))

(defun org-md--convert-to-html (datum _contents info)
  "Convert DATUM into raw HTML, including contents."
  (org-export-data-with-backend datum 'html info))

(defun org-md--identity (_datum contents _info)
  "Return CONTENTS only."
  contents)


;;; Transcode Functions

;;;; Bold

(defun org-md-bold (_bold contents _info)
  "Transcode BOLD object into Markdown format.
CONTENTS is the text within bold markup.  INFO is a plist used as
a communication channel."
  (format "**%s**" contents))


;;;; Code and Verbatim

(defun org-md-verbatim (verbatim _contents _info)
  "Transcode VERBATIM object into Markdown format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let ((value (org-element-property :value verbatim)))
    (format (cond ((not (string-match "`" value)) "`%s`")
		  ((or (string-prefix-p "`" value)
		       (string-suffix-p "`" value))
		   "`` %s ``")
		  (t "``%s``"))
	    value)))


;;;; Example Block, Src Block and Export Block

(defun org-md-example-block (example-block _contents info)
  "Transcode EXAMPLE-BLOCK element into Markdown format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (replace-regexp-in-string
   "^" "    "
   (org-remove-indentation
    (org-export-format-code-default example-block info))))

(defun org-md-export-block (export-block contents info)
  "Transcode a EXPORT-BLOCK element from Org to Markdown.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (if (member (org-element-property :type export-block) '("MARKDOWN" "MD"))
      (org-remove-indentation (org-element-property :value export-block))
    ;; Also include HTML export blocks.
    (org-export-with-backend 'html export-block contents info)))


;;;; Headline

(defun org-md-headline (headline contents info)
  "Transcode HEADLINE element into Markdown format.
CONTENTS is the headline contents.  INFO is a plist used as
a communication channel."
  (unless (org-element-property :footnote-section-p headline)
    (let* ((level (org-export-get-relative-level headline info))
	   (title (org-export-data (org-element-property :title headline) info))
	   (todo (and (plist-get info :with-todo-keywords)
		      (let ((todo (org-element-property :todo-keyword
							headline)))
			(and todo (concat (org-export-data todo info) " ")))))
	   (tags (and (plist-get info :with-tags)
		      (let ((tag-list (org-export-get-tags headline info)))
			(and tag-list
			     (concat "     " (org-make-tag-string tag-list))))))
	   (priority
	    (and (plist-get info :with-priority)
		 (let ((char (org-element-property :priority headline)))
		   (and char (format "[#%c] " char)))))
	   ;; Headline text without tags.
	   (heading (concat todo priority title))
	   (style (plist-get info :md-headline-style)))
      (cond
       ;; Cannot create a headline.  Fall-back to a list.
       ((or (org-export-low-level-p headline info)
	    (not (memq style '(atx setext)))
	    (and (eq style 'atx) (> level 6))
	    (and (eq style 'setext) (> level 2)))
	(let ((bullet
	       (if (not (org-export-numbered-headline-p headline info)) "-"
		 (concat (number-to-string
			  (car (last (org-export-get-headline-number
				      headline info))))
			 "."))))
	  (concat bullet (make-string (- 4 (length bullet)) ?\s) heading tags "\n\n"
		  (and contents (replace-regexp-in-string "^" "    " contents)))))
       (t
	(let ((anchor
	       (and (org-md--headline-referred-p headline info)
		    (format "<a id=\"%s\"></a>"
			    (or (org-element-property :CUSTOM_ID headline)
				(org-export-get-reference headline info))))))
	  (concat (org-md--headline-title style level heading anchor tags)
		  contents)))))))

;;;; Horizontal Rule

(defun org-md-horizontal-rule (_horizontal-rule _contents _info)
  "Transcode HORIZONTAL-RULE element into Markdown format.
CONTENTS is the horizontal rule contents.  INFO is a plist used
as a communication channel."
  "---")


;;;; Italic

(defun org-md-italic (_italic contents _info)
  "Transcode ITALIC object into Markdown format.
CONTENTS is the text within italic markup.  INFO is a plist used
as a communication channel."
  (format "*%s*" contents))


;;;; Item

(defun org-md-item (item contents info)
  "Transcode ITEM element into Markdown format.
CONTENTS is the item contents.  INFO is a plist used as
a communication channel."
  (let* ((type (org-element-property :type (org-export-get-parent item)))
	 (struct (org-element-property :structure item))
	 (bullet (if (not (eq type 'ordered)) "-"
		   (concat (number-to-string
			    (car (last (org-list-get-item-number
					(org-element-property :begin item)
					struct
					(org-list-prevs-alist struct)
					(org-list-parents-alist struct)))))
			   "."))))
    (concat bullet
	    (make-string (- 4 (length bullet)) ? )
	    (pcase (org-element-property :checkbox item)
	      (`on "[X] ")
	      (`trans "[-] ")
	      (`off "[ ] "))
	    (let ((tag (org-element-property :tag item)))
	      (and tag (format "**%s:** "(org-export-data tag info))))
	    (and contents
		 (org-trim (replace-regexp-in-string "^" "    " contents))))))

;;;; Keyword

(defun org-md-keyword (keyword contents info)
  "Transcode a KEYWORD element into Markdown format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (pcase (org-element-property :key keyword)
    ((or "MARKDOWN" "MD") (org-element-property :value keyword))
    ("TOC"
     (let ((case-fold-search t)
	   (value (org-element-property :value keyword)))
       (cond
	((string-match-p "\\<headlines\\>" value)
	 (let ((depth (and (string-match "\\<[0-9]+\\>" value)
			   (string-to-number (match-string 0 value))))
	       (scope
		(cond
		 ((string-match ":target +\\(\".+?\"\\|\\S-+\\)" value) ;link
		  (org-export-resolve-link
		   (org-strip-quotes (match-string 1 value)) info))
		 ((string-match-p "\\<local\\>" value) keyword)))) ;local
	   (org-remove-indentation
	    (org-md--build-toc info depth keyword scope)))))))
    (_ (org-export-with-backend 'html keyword contents info))))


;;;; Latex Environment

(defun org-md-latex-environment (latex-environment _contents info)
  "Transcode a LATEX-ENVIRONMENT object from Org to Markdown.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (plist-get info :with-latex)
    (let ((latex-frag (org-remove-indentation
                       (org-element-property :value latex-environment)))
          (label (org-html--reference latex-environment info t)))
      (if (org-string-nw-p label)
          (replace-regexp-in-string "\\`.*"
                                    (format "\\&\n\\\\label{%s}" label)
                                    latex-frag)
        latex-frag))))

;;;; Latex Fragment

(defun org-md-latex-fragment (latex-fragment _contents info)
  "Transcode a LATEX-FRAGMENT object from Org to Markdown.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (plist-get info :with-latex)
    (let ((frag (org-element-property :value latex-fragment)))
      (cond
       ((string-match-p "^\\\\(" frag)
        (concat "$" (substring frag 2 -2) "$"))
       ((string-match-p "^\\\\\\[" frag)
        (concat "$$" (substring frag 2 -2) "$$"))
       (t frag))))) ; either already $-deliminated or a macro

;;;; Line Break

(defun org-md-line-break (_line-break _contents _info)
  "Transcode LINE-BREAK object into Markdown format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  "  \n")


;;;; Link

(defun org-md-link (link desc info)
  "Transcode LINK object into Markdown format.
DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information.  See
`org-export-data'."
  (let* ((link-org-files-as-md
	  (lambda (raw-path)
	    ;; Treat links to `file.org' as links to `file.md'.
	    (if (string= ".org" (downcase (file-name-extension raw-path ".")))
		(concat (file-name-sans-extension raw-path) ".md")
	      raw-path)))
	 (type (org-element-property :type link))
	 (raw-path (org-element-property :path link))
	 (path (cond
		((member type '("http" "https" "ftp" "mailto"))
		 (concat type ":" raw-path))
		((string-equal  type "file")
		 (org-export-file-uri (funcall link-org-files-as-md raw-path)))
		(t raw-path))))
    (cond
     ;; Link type is handled by a special function.
     ((org-export-custom-protocol-maybe link desc 'md info))
     ((member type '("custom-id" "id" "fuzzy"))
      (let ((destination (if (string= type "fuzzy")
			     (org-export-resolve-fuzzy-link link info)
			   (org-export-resolve-id-link link info))))
	(pcase (org-element-type destination)
	  (`plain-text			; External file.
	   (let ((path (funcall link-org-files-as-md destination)))
	     (if (not desc) (format "<%s>" path)
	       (format "[%s](%s)" desc path))))
	  (`headline
	   (format
	    "[%s](#%s)"
	    ;; Description.
	    (cond ((org-string-nw-p desc))
		  ((org-export-numbered-headline-p destination info)
		   (mapconcat #'number-to-string
			      (org-export-get-headline-number destination info)
			      "."))
		  (t (org-export-data (org-element-property :title destination)
				      info)))
	    ;; Reference.
	    (or (org-element-property :CUSTOM_ID destination)
		(org-export-get-reference destination info))))
	  (_
	   (let ((description
		  (or (org-string-nw-p desc)
		      (let ((number (org-export-get-ordinal destination info)))
			(cond
			 ((not number) nil)
			 ((atom number) (number-to-string number))
			 (t (mapconcat #'number-to-string number ".")))))))
	     (when description
	       (format "[%s](#%s)"
		       description
		       (org-export-get-reference destination info))))))))
     ((org-export-inline-image-p link org-html-inline-image-rules)
      (let ((path (cond ((not (string-equal type "file"))
			 (concat type ":" raw-path))
			((not (file-name-absolute-p raw-path)) raw-path)
			(t (expand-file-name raw-path))))
	    (caption (org-export-data
		      (org-export-get-caption
		       (org-export-get-parent-element link))
		      info)))
	(format "![img](%s)"
		(if (not (org-string-nw-p caption)) path
		  (format "%s \"%s\"" path caption)))))
     ((string= type "coderef")
      (format (org-export-get-coderef-format path desc)
	      (org-export-resolve-coderef path info)))
     ((string= type "radio")
      (let ((destination (org-export-resolve-radio-link link info)))
	(if (not destination) desc
	  (format "<a href=\"#%s\">%s</a>"
		  (org-export-get-reference destination info)
		  desc))))
     (t (if (not desc) (format "<%s>" path)
	  (format "[%s](%s)" desc path))))))


;;;; Node Property

(defun org-md-node-property (node-property _contents _info)
  "Transcode a NODE-PROPERTY element into Markdown syntax.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format "%s:%s"
          (org-element-property :key node-property)
          (let ((value (org-element-property :value node-property)))
            (if value (concat " " value) ""))))


;;;; Paragraph

(defun org-md-paragraph (paragraph contents _info)
  "Transcode PARAGRAPH element into Markdown format.
CONTENTS is the paragraph contents.  INFO is a plist used as
a communication channel."
  (let ((first-object (car (org-element-contents paragraph))))
    ;; If paragraph starts with a #, protect it.
    (if (and (stringp first-object) (string-prefix-p "#" first-object))
	(concat "\\" contents)
      contents)))


;;;; Plain List

(defun org-md-plain-list (_plain-list contents _info)
  "Transcode PLAIN-LIST element into Markdown format.
CONTENTS is the plain-list contents.  INFO is a plist used as
a communication channel."
  contents)


;;;; Plain Text

(defun org-md-plain-text (text info)
  "Transcode a TEXT string into Markdown format.
TEXT is the string to transcode.  INFO is a plist holding
contextual information."
  (when (plist-get info :with-smart-quotes)
    (setq text (org-export-activate-smart-quotes text :html info)))
  ;; The below series of replacements in `text' is order sensitive.
  ;; Protect `, *, _, and \
  (setq text (replace-regexp-in-string "[`*_\\]" "\\\\\\&" text))
  ;; Protect ambiguous #.  This will protect # at the beginning of
  ;; a line, but not at the beginning of a paragraph.  See
  ;; `org-md-paragraph'.
  (setq text (replace-regexp-in-string "\n#" "\n\\\\#" text))
  ;; Protect ambiguous !
  (setq text (replace-regexp-in-string "\\(!\\)\\[" "\\\\!" text nil nil 1))
  ;; Handle special strings, if required.
  (when (plist-get info :with-special-strings)
    (setq text (org-html-convert-special-strings text)))
  ;; Handle break preservation, if required.
  (when (plist-get info :preserve-breaks)
    (setq text (replace-regexp-in-string "[ \t]*\n" "  \n" text)))
  ;; Return value.
  text)


;;;; Property Drawer

(defun org-md-property-drawer (_property-drawer contents _info)
  "Transcode a PROPERTY-DRAWER element into Markdown format.
CONTENTS holds the contents of the drawer.  INFO is a plist
holding contextual information."
  (and (org-string-nw-p contents)
       (replace-regexp-in-string "^" "    " contents)))


;;;; Quote Block

(defun org-md-quote-block (_quote-block contents _info)
  "Transcode QUOTE-BLOCK element into Markdown format.
CONTENTS is the quote-block contents.  INFO is a plist used as
a communication channel."
  (replace-regexp-in-string
   "^" "> "
   (replace-regexp-in-string "\n\\'" "" contents)))


;;;; Section

(defun org-md-section (_section contents _info)
  "Transcode SECTION element into Markdown format.
CONTENTS is the section contents.  INFO is a plist used as
a communication channel."
  contents)


;;;; Template

(defun org-md-inner-template (contents info)
  "Return body of document after converting it to Markdown syntax.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  ;; Make sure CONTENTS is separated from table of contents and
  ;; footnotes with at least a blank line.
  (concat
   ;; Table of contents.
   (let ((depth (plist-get info :with-toc)))
     (when depth
       (concat (org-md--build-toc info (and (wholenump depth) depth)) "\n")))
   ;; Document contents.
   contents
   "\n"
   ;; Footnotes section.
   (org-md--footnote-section info)))

(defun org-md-template (contents _info)
  "Return complete document string after Markdown conversion.
CONTENTS is the transcoded contents string.  INFO is a plist used
as a communication channel."
  contents)



;;; Interactive function

;;;###autoload
(defun org-md-export-as-markdown (&optional async subtreep visible-only)
  "Export current buffer to a Markdown buffer.

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

Export is done in a buffer named \"*Org MD Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is
non-nil."
  (interactive)
  (org-export-to-buffer 'md "*Org MD Export*"
    async subtreep visible-only nil nil (lambda () (text-mode))))

;;;###autoload
(defun org-md-convert-region-to-md ()
  "Assume the current region has Org syntax, and convert it to Markdown.
This can be used in any buffer.  For example, you can write an
itemized list in Org syntax in a Markdown buffer and use
this command to convert it."
  (interactive)
  (org-export-replace-region-by 'md))


;;;###autoload
(defun org-md-export-to-markdown (&optional async subtreep visible-only)
  "Export current buffer to a Markdown file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".md" subtreep)))
    (org-export-to-file 'md outfile async subtreep visible-only)))

;;;###autoload
(defun org-md-publish-to-md (plist filename pub-dir)
  "Publish an org file to Markdown.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'md filename ".md" plist pub-dir))

(provide 'emacs-preview)

;;; emacs-preview.el ends here

