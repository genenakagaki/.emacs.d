;; ~/.config/emacs/templates

fundamental-mode ;; Available everywhere

(today (format-time-string "%Y-%m-%d"))

prog-mode

(fixme (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "FIXME ")
(todo (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "TODO ")
(bug (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "BUG ")
(hack (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "HACK ")

csharp-mode
(command "using ErrorOr;" n
         "using MediatR;" n
         "namespace CodeNet.Application." n
         "pwd"
         ";" n
         "public class " (s command) "CommandHandler(IOrganizationRepository _orgRepo)" n
         "  : IRequestHandler<" command "Command, ErrorOr<" (s returntype) ">>" n
         "{" n
         "    public async Task<ErrorOr<" returntype ">> Handle(" command "Command cmd, CancellationToken cancellationToken)" n
         "    {" n
         "        throw new NotImplementedException();" n
         "    }" n
         "}" n
         n
         "public record " command "Command(" n
         "    string arg1"
         ") : IRequest<ErrorOr<" returntype ">>;" n
         :post (eglot-format-buffer))

(query "using ErrorOr;" n
       "using MediatR;" n
       "namespace CodeNet.Application." n
       "pwd"
       ";" n
       "public class " (s query) "QueryHandler(IOrganizationRepository _orgRepo)" n
       "  : IRequestHandler<" query "Query, ErrorOr<" (s returntype) ">>" n
       "{" n
       "    public async Task<ErrorOr<" returntype ">> Handle(" query "Query cmd, CancellationToken cancellationToken)" n
       "    {" n
       "        throw new NotImplementedException();" n
       "    }" n
       "}" n
       n
       "public record " query "Query(" n
       "    string arg1"
       ") : IRequest<ErrorOr<" returntype ">>;" n
       :post (eglot-format-buffer))

(event "using MediatR;" n
       "namespace CodeNet.Application." n
       "pwd    " n
       ";" n
       "public class " (s event) "EventHandler()" n
       "  : INotificationHandler<" event "Event>" n
       "{" n
       "    public Task Handle(" event "Event notification, CancellationToken cancellationToken)" n
       "    {" n
       "        throw new NotImplementedException();" n
       "    }" n
       "}" n
       n
       "public record " event "Event(" n
       "    Guid eventArgs) : IDomainEvent;" n
       :post (eglot-format-buffer))

(configuration  "using Microsoft.EntityFrameworkCore;" n
                "using Microsoft.EntityFrameworkCore.Metadata.Builders;" n
                "namespace CodeNet.Infrastructure." n
                "pwd" n
                ";" n
                "public class " (s aggregate) "ConfigurationModel : IEntityTypeConfiguration<" aggregate ">" n
                "{" n
                "    public void Configure(EntityTypeBuilder<" aggregate "> builder)" n
                "    {" n
                "    }" n
                "}" n
                :post (eglot-format-buffer))

(repository "using CodeNet.Infrastructure.Common.Persistance;" n
            "" n
            "namespace CodeNet.Infrastructure" n
            "pwd" n
            ";" n
            "" n
            "public class " (s name) "Repository(CodeNetDbContext _dbContext)" n
            "  : I" name "Repository" n
            "{" n
            "}"
            :post (eglot-format-buffer))

latex-mode

(abstract "\\begin{abstract}\n" r> n> "\\end{abstract}")
(align "\\begin{align}\n" r> n> "\\end{align}")
(alignn "\\begin{align*}\n" r> n> "\\end{align*}")
(gather "\\begin{gather}\n" r> n> "\\end{gather}")
(gatherr "\\begin{gather*}\n" r> n> "\\end{gather*}")
(appendix "\\begin{appendix}\n" r> n> "\\end{appendix}")
(begin "\\begin{" (s env) "}" r> n> "\\end{" (s env) "}")
(center "\\begin{center}\n" r> n> "\\end{center}")
(displaymath "\\begin{displaymath}\n" r> n> "\\end{displaymath}")
(document "\\begin{document}\n" r> n> "\\end{document}")
(enumerate "\\begin{enumerate}\n\\item " r> n> "\\end{enumerate}")
(equation "\\begin{equation}" r> n> "\\end{equation}")
(flushleft "\\begin{flushleft}" r> n> "\\end{flushleft}")
(flushright "\\begin{flushright}" r> n> "\\end{flushright}")
(frac "\\frac{" p "}{" q "}")
(fussypar "\\begin{fussypar}" r> n> "\\end{fussypar}")
(itemize "\\begin{itemize}\n\\item " r> n> "\\end{itemize}")
(letter "\\begin{letter}\n" r> n> "\\end{letter}")
(math "\\begin{math}\n" r> n> "\\end{math}")
(minipage "\\begin{minipage}[t]{0.5\linewidth}\n" r> n> "\\end{minipage}")
(quotation "\\begin{quotation}\n" r> n> "\\end{quotation}")
(quote "\\begin{quote}\n" r> n> "\\end{quote}")
(sloppypar "\\begin{sloppypar}\n" r> n> "\\end{sloppypar}")
(theindex "\\begin{theindex}\n" r> n> "\\end{theindex}")
(trivlist "\\begin{trivlist}\n" r> n> "\\end{trivlist}")
(verbatim "\\begin{verbatim}\n" r> n> "\\end{verbatim}")
(verbatimm "\\begin{verbatim*}\n" r> n> "\\end{verbatim*}")

texinfo-mode

(defmac "@defmac " p n> r> "@end defmac")
(defun "@defun " p n> r> "@end defun")
(defvar "@defvar " p n> r> "@end defvar")
(example "@example " p n> r> "@end example")
(lisp "@lisp " p n> r> "@end lisp")
(bullet "@itemize @bullet{}" n> r> "@end itemize")
(code "@code{" p "}")
(var "@var{" p "}")

lisp-mode emacs-lisp-mode ;; Specify multiple modes

(lambda "(lambda (" p ")" n> r> ")")

emacs-lisp-mode

(autoload ";;;###autoload")
(pt "(point)")
(var "(defvar " p "\n  \"" p "\")")
(local "(defvar-local " p "\n  \"" p "\")")
(const "(defconst " p "\n  \"" p "\")")
(custom "(defcustom " p "\n  \"" p "\"" n> ":type '" p ")")
(face "(defface " p " '((t :inherit " p "))\n  \"" p "\")")
(group "(defgroup " p " nil\n  \"" p "\"" n> ":group '" p n> ":prefix \"" p "-\")")
(macro "(defmacro " p " (" p ")\n  \"" p "\"" n> r> ")")
(alias "(defalias '" p " '" p ")")
(fun "(defun " p " (" p ")\n  \"" p "\"" n> r> ")")
(iflet "(if-let (" p ")" n> r> ")")
(whenlet "(when-let (" p ")" n> r> ")")
(whilelet "(while-let (" p ")" n> r> ")")
(andlet "(and-let* (" p ")" n> r> ")")
(cond "(cond" n "(" q "))" >)
(pcase "(pcase " (p "scrutinee") n "(" q "))" >)
(let "(let (" p ")" n> r> ")")
(lett "(let* (" p ")" n> r> ")")
(pcaselet "(pcase-let (" p ")" n> r> ")")
(pcaselett "(pcase-let* (" p ")" n> r> ")")
(rec "(letrec (" p ")" n> r> ")")
(dotimes "(dotimes (" p ")" n> r> ")")
(dolist "(dolist (" p ")" n> r> ")")
(loop "(cl-loop for " p " in " p " do" n> r> ")")
(command "(defun " p " (" p ")\n  \"" p "\"" n> "(interactive" p ")" n> r> ")")
(advice "(defun " (p "adv" name) " (&rest app)" n> p n> "(apply app))" n>
        "(advice-add #'" (p "fun") " " (p ":around") " #'" (s name) ")")
(header ";;; " (file-name-nondirectory (or (buffer-file-name) (buffer-name)))
        " -- " p " -*- lexical-binding: t -*-" n
        ";;; Commentary:" n ";;; Code:" n n)
(provide "(provide '" (file-name-base (or (buffer-file-name) (buffer-name))) ")" n
         ";;; " (file-name-nondirectory (or (buffer-file-name) (buffer-name)))
         " ends here" n)

clojure-mode

eshell-mode

(for "for " (p "i") " in " p " { " q " }")
(while "while { " p " } { " q " }")
(until "until { " p " } { " q " }")
(if "if { " p " } { " q " }")
(ife "if { " p " } { " p " } { " q " }")
(unl "unless { " p " } { " q " }")
(unle "unless { " p " } { " p " } { " q " }")

text-mode

(box "┌─" (make-string (length str) ?─) "─┐" n
     "│ " (s str)                       " │" n
     "└─" (make-string (length str) ?─) "─┘" n)
(abox "+-" (make-string (length str) ?-) "-+" n
      "| " (s str)                       " |" n
      "+-" (make-string (length str) ?-) "-+" n)
(cut "--8<---------------cut here---------------start------------->8---" n r n
     "--8<---------------cut here---------------end--------------->8---" n)
(rot13 (p "plain text" text) n "----" n (rot13 text))
(calc (p "taylor(sin(x),x=0,3)" formula) n "----" n (format "%s" (calc-eval formula)))

rst-mode

(title (make-string (length title) ?=) n (p "Title: " title) n (make-string (length title) ?=) n)

java-mode

(class "public class " (p (file-name-base (or (buffer-file-name) (buffer-name)))) " {" n> r> n "}")

c-mode :when (re-search-backward "^\\S-*$" (line-beginning-position) 'noerror)

(inc "#include <" (p (concat (file-name-base (or (buffer-file-name) (buffer-name))) ".h")) ">")
(incc "#include \"" (p (concat (file-name-base (or (buffer-file-name) (buffer-name))) ".h")) "\"")

org-mode

(caption "#+caption: ")
(drawer ":" p ":" n r ":end:")
(begin "#+begin_" (s name) n> r> n "#+end_" name)
(quote "#+begin_quote" n> r> n "#+end_quote")
(sidenote "#+begin_sidenote" n> r> n "#+end_sidenote")
(marginnote "#+begin_marginnote" n> r> n "#+end_marginnote")
(example "#+begin_example" n> r> n "#+end_example")
(center "#+begin_center" n> r> n "#+end_center")
(ascii "#+begin_export ascii" n> r> n "#+end_export")
(html "#+begin_export html" n> r> n "#+end_export")
(latex "#+begin_export latex" n> r> n "#+end_export")
(comment "#+begin_comment" n> r> n "#+end_comment")
(verse "#+begin_verse" n> r> n "#+end_verse")
(src "#+begin_src " q n r n "#+end_src")
(src-example "#+begin_src " (s name) " :eval never" n> r> n "#+end_src")
(src-sh "#+begin_src shell :results output" n> r> n "#+end_src")
(src-config "#+begin_src emacs-lisp :tangle yes :results none" q n r n "#+end_src")
(gnuplot "#+begin_src gnuplot :var data=" (p "table") " :file " (p "plot.png") n r n "#+end_src" :post (org-edit-src-code))
(elisp "#+begin_src emacs-lisp" n r n "#+end_src" :post (org-edit-src-code))
(inlsrc "src_" p "{" q "}")
(title "#+title: " p n "#+author: Daniel Mendler" n "#+language: en")

plantuml-mode

(activity "@startuml 
skinparam ActivityBackgroundColor business
skinparam ActivityDiamondBackgroundColor business
skinparam ConditionEndStyle hline

legend top
    <back:application>システムの処理</back> 
    <back:business>会員アクション</back> 
    <back:palegreen>まだ考慮する必要あり</back> 
endlegend

@enduml")
(deployment "@startuml 
@enduml")
(sequence "@startuml 
actor User
participant System
dasabase Database

User --> System++: Api Request
System --> Database++: save
return void
User <-- System--: Response
@enduml")
(if "if (" (s condition) ") then (no" (s left) ")" n
    "else (yes" (s right) ")" n
    "endif"
    :post (indent-buffer))
(split "split
split again
end split"
       :post (indent-buffer))
(business "#business:" p ";")
(application "#application:" p ";")
(green "#palegreen:TODO: " n p ";")
(error "#pink:エラー;" n>
       "stop")
(font "<color:" p ">" q "</color>")
(note "note right" n
      > q n
      "end note"
      :post (indent-buffer))
;; task-specific
(reject "#business:与信を0にする;" n>
        "#business:会員へのメッセージ選択" n>
        "テンプレートID：" q ";" n>
        "#pink:却下;" n>
        "stop")
(remand "#business:メモ欄に記載" n>
        "「" q "」;" n>
        "#pink:差し戻し;" n>
        "stop")
(continue "#business:審査続行;" n>
          "stop")
(message "#business:会員へのメッセージ選択" n>
         "テンプレートID：" q ";")
(manual "\\n<back:salmon>自動化：不可</back>\\n<back:salmon>" q "</back>")
(auto-hard "\\n<back:orange>自動化：可能だけど難しい</back>\\n<back:orange>" q "</back>")
(back-orange "\\n<back:orange>" q "</back>")




;; Local Variables:
;; mode: lisp-data
;; outline-regexp: "[a-z]"
;; End:
