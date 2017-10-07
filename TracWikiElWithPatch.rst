{{{
;;  trac-wiki.el --- edit trac wiki pages in emacs via XML-RPC

;; Copyright (C) 2006  Shun-ichi GOTO

;; Author: Shun-ichi GOTO <shunichi.goto@gmail.com>
;; Keywords: trac, xml-rpc, wiki, wiki-rpc
;; Version: 1.7
;; URL: http://www.meadowy.org/~gotoh/projects/trac-wiki/

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:


;; Overview:

;; Features:
;;   * Multiple project access.
;;   * Retrieve page from remote site and edit it with highlighting.
;;   * Commit page with version check and detects remote update.
;;   * Diff / Ediff between editing text and original.
;;   * Revert local edit.
;;   * Merge with most recent version if it is modified by other user.
;;   * Show history of page (but not so informative)
;;   * Preview page on Emacs with w3m (textual).
;;   * Preview page with external browser with CSS.
;;   * Search words on remote trac site and view result with highlighting.
;;   * Jump to the page from search result.
;;   * Completion for macro name and wiki page name in buffer.
;;
;; Requirement
;;  (local side)
;;   * Works on Emacs 21.4 or later.
;;   * need xml-rpc.el with small patch for I18N (non-ascii)
;;   * mule-ucs is required to use CJK text on Emacs 21.4.
;;   * Optionally w3m and emacs-w3m is required for previewing.
;;  (server side)
;;   * Trac 0.10 or later.
;;   * XmlRpcPlugin is installed and enabled.
;;
;; Restriction:
;;   * It is not well on error handling (auth fail, spam-filtered, etc.)
;;   * Cannot delete page version.
;;   * Cannot operates tickets.
;;   * Cannot treat tags provided by TagsPlugin.


;; xml-rpc.el is required to use this program.
;; It can be get from following URL.  (now, newest is rev 1.7)
;; http://cvs.savannah.gnu.org/viewcvs/emacsweblogs/weblogger/lisp/xml-rpc.el
;;
;; If you want to edit wiki pages with non-ASCII characters,
;; you need xml-rpc.el rev 1.7 or later.

;; Use XmlRpcPlugin r1735 or later.  Earlier versions has a problem
;; (see http://trac-hacks.org/ticket/845) on previewing.

;; emacs-w3m can be get from:
;; http://emacs-w3m.namazu.org/index.html
;; Of course you must have w3m program because emacs-w3m use it.

;;; Configuration:

;;  Step 1. Get and enable XmlRpcPlugin on your trac site.
;;          http://trac-hacks.org/wiki/XmlRpcPlugin
;;
;;          Install and setup it with refering the page above.  Don't
;;          forget enabling plugin in trac.ini and adding permission
;;          to allow user accessing via XML-RPC.  For example, it is
;;          recommended to add 'XML_RPC' permission to 'authenticated'
;;          subject to allow XML-RPC access only for relyable users.
;;
;;  Step 2. Set project information variable `trac-projects'
;;          in your .emacs.
;;
;;          If you have a trac site frequently visit to edit, you can
;;          register the url of that site with alias name.  To do this
;;          use `trac-wiki-define-project' like this:
;;
;;            (trac-wiki-define-project "trac-hacks"
;;                                      "http://trac-hacks.org/" t)
;;
;;          1st argument is alias name which is used on selection.
;;          2nd arugment is actual url.  3rd optional argument indicates
;;          login is required for the site access.
;;          If you have multiple instance in one site, you can use
;;          `trac-wiki-define-multiple-projects'.
;;          Ex.
;;             (trac-wiki-define-multiple-projects
;;                   '("proj1" "proj2" "test")
;;                    "http://www.foo.bar.org/" t)
;;
;;          An example above is equivalent to three
;;          `trac-wiki-define-project' definition.
;;
;;          Note: Old version of `trac-wiki.el' required XML-RPC
;;                end-point url for these functions.  Recent veresion
;;                of `trac-wiki.el', however, `trac-wiki.el` expects
;;                normal site url instead of end-point url.
;;
;;  Step 3. Set proxy information.
;;
;;          Url library gets proxy information via variable
;;          `url-proxy-services'.  It is well to set in your .emacs.
;;
;;          See info of url pakage for more detail.
;;          Jump to info node by evaluating this:
;;            (Info-goto-node "(url)Proxies")

;;  Step 4. Set autoload and more.
;;
;;          Set autoload definition in your .emacs for convenience:
;;
;;           (autoload 'trac-wiki "trac-wiki"
;;                     "Trac wiki editing entry-point." t)
;;
;;          And load mule-ucs for Emacs 21.x user.

;;; NOTICE:

;; There is a notice for authentication.  If your target trac site
;; provides multiple authentication scheme (ex. both NTLM and BASIC)
;; and first one is not supported by url package, authentication step
;; is ignored. It's bug of url-http.el. On this case, you may encount
;; endless user/pass query.  For example, this case will be occured
;; when trac site uses mod_auth_sspi for domain/ActiveDirectory
;; authentication and allowing fallback to basic authentication. This
;; setting generates two WWW-Authenticate: line and first one is NTLM
;; auth and url package cannot recognize it. Thus fail.
;;
;; To avoid this:
;;  - Apply following patch
;;     http://www.meadowy.org/~gotoh/trac-wiki/url-http.el-multi-auth.patch
;;
;; or
;;
;;  - Preset auth information by your hand into url-basic-auth-storage
;;    (or url-digets-auth-storage) variable like this:
;;
;;    (let ((auth (base64-encode-string (format "%s:%s" user pass))))
;;      (set (symbolvalue 'url-basic-real-auth-storage)
;;           '(("www.some.org:80" (realm . auth))
;;             ("www.other.net:80" ....
;;

;;; Usage:

;; You can start editing by `M-x trac-wiki`.
;; Flow of editing is:

;;  1. M-x trac-wiki
;;  2. Specify project name.
;;  3. Specify page name.
;;  4. Edit page content.
;;  5. Check difference.
;;  6. Preview page output.
;;  6. Commit it.

;; `trac-wiki' command ask you project name defined by
;; `trac-wiki-define-project' or `trac-wiki-define-multiple-projects'
;; with completion.  If you want to specify URL directly, hit ENTER
;; wihout any characters when you asked for project name, then enter site URL
;; next prompt.
;;
;; Then ask page name to edit with completion.  Available page names
;; are retreived from remote site by XML-RPC request dynamicaly.
;; If you specified non existing name, it means creating new page and
;; start editing from empty.
;;
;; After you edit the page content, you should commit by
;; `trac-wiki-commit' (C-c C-c) to finish editing.
;; If you want to cancel editing, you can kill the buffer simply.
;; Or use `trac-wiki-revert' (C-c C-u) for cancel changes in buffer.

;; While editing the page, the buffer is in `trac-wiki-mode' which is
;; based on `text-mode'. you can specify some mode specific commands:
;;
;;   C-c C-c ... `trac-wiki-commit'
;;       Commit current editing content.
;;       Same project (end-point) is used, or ask project with C-u.
;;   C-c C-o ... `trac-wiki-edit'
;;       Edit another page in new buffer.
;;   C-c C-p ... `trac-wiki-preview'
;;       Preview current content by w3m (text base).
;;       With C-u, preview by external browser (graphical).
;;   C-c = ... `trac-wiki-diff'
;;   C-c C-d ... `trac-wiki-diff'
;;       Make diff between current content and original content
;;       With C-u,  execute ediff instead of diff.
;;   C-c C-m ... `trac-wiki-merge'
;;       Merge with most recent page content using `ediff-merge'.
;;       If not modified, turn current buffer to newest version.
;;   C-c C-u ... `trac-wiki-revert'
;;       Revert to original content discarding current modification.
;;       It shows diff and confirm you before do it.
;;   M-C-i ... `trac-wiki-complete-at-point'
;;       Complete macro name or page name on current point.
;;       The macro names are collected from "WikiMacros" page on the
;;       site (and cached).
;;   C-c C-h ... `trac-wiki-history'
;;       Show page history in other buffer.
;;       History is information returned from xmlrpc plugin.
;;       On each revision entry, you can show diff on its revision
;;       by '=' key.
;;   C-c C-s ... `trac-wiki-search'
;;       Search on project site for specified keywords.
;;       You can specify keywords and filters. The result is shown
;;       in another buffer with highlighting.
;;
;; Since outline minor mode is also enabled by trac-wiki mode,
;; You can fold the sections.

;;; References:

;; - JSPWiki: Wiki RPC Interface 2
;;   http://www.jspwiki.org/Wiki.jsp?page=WikiRPCInterface2
;;
;; - XmlRpcPlugin - Trac Hacks
;;   http://trac-hacks.org/wiki/XmlRpcPlugin
;;

;;; Code:

(require 'xml-rpc)
(require 'url)
(require 'url-http)

(eval-when-compile
  (require 'cl)
  (require 'w3m nil t)			; optional
  (require 'ediff)
  (require 'hi-lock))


(defvar trac-projects
  '(("emacs-trac-wiki"			; annonymous (read-only)
     :endpoint "http://www.meadowy.org/~gotoh/projects/trac-wiki/xmlrpc")
    ("trac-hacks"			; TracHacs site (need login)
     :endpoint "http://trac-hacks.org/login/xmlrpc"))
  "*List of project definitions.
The value is alist of project name and information plist.
For example:

  '((\"meadow\"
     :endpoint \"http://www.meadowy.org/meadow/login/xmlrpc\"
     :login-name \"gotoh\")
    (\"local-test\"
     :endpoint \"http://localhost/project/test/xmlrpc\"
     :login-name \"gotoh\"
     :name \"Shun-ichi Goto\")))")

(defvar trac-wiki-hide-system-pages t
  "If non-nil, do not list system pages on completion.
System pages are defined by `trac-wiki-system-pages' as regexp.
Although these pages are not listed, you can visit them by
specifying page name explicitly.")

(defvar trac-wiki-hidden-pages nil
  "*List of regexp to be hidden on completion of page name.
System pages are always hidden if `trac-wiki-hide-system-pages'
is non-nil.  So specify extra page names like provided by plugins.
For example, graphviz plugin provides some example pages but you
may want to edit these pages.  Following example directs hiding
pages start with \"Graphviz\":

 (setq trac-wiki-hidden-pages '(\"Graphviz.*\"))

Choise to hide pages by this variable is work after hiding system pages.
So if `trac-wiki-hide-system-pages' is nil, you can hide specific
system pages by this variable.")


(defvar trac-wiki-search-default-filters '("wiki")
  "*List of search filter name to use as default.
Available filter names are:
  wiki      : search in all the wiki pages.
  ticket    : search description and comment of all the tickets.
  changeset : search commit log of all changesets.")


(defvar trac-wiki-max-history 100
  "*Maximum number of wiki page history to fetch.
See `trac-wiki-history'.")

(defvar trac-wiki-history-count 10
  "*Usual number of wiki page history to fetch.
See `trac-wiki-history'.")

(defvar trac-wiki-use-keepalive
  (and (boundp 'url-http-real-basic-auth-storage)
       url-http-attempt-keepalives)
  "*If non-nil, use keep-alive option for http connection.
This value should be nil for old url library such as one on
debian sarge stable .")

(defvar trac-wiki-update-page-name-cache-on-visit t
  "*If non-nil, update page name cache on every `trac-wiki-edit' call.
Collecting page names might be expensive for some environment and
some usage.  By setting this variable nil, page name cache is
updated only when invoking `trac-wiki' command or do completion
with prefix explicitly on page editing buffer.")

;;; internal variables

(defconst trac-wiki-diff-buffer-name
  (if (<= 22 emacs-major-version)
      "*Diff*"
    "*diff*")
  "Buffer name of `diff' output.")

(defvar trac-wiki-macro-name-cache nil
  "Alist of endpoint and macro name list.")

(defvar trac-wiki-page-name-cache nil
  "Alist of endpoint and page name list.")

(defvar trac-rpc-endpoint nil)
(make-variable-buffer-local 'trac-rpc-endpoint)

(defvar trac-wiki-project-info nil)
(make-variable-buffer-local 'trac-wiki-project-info)

(defvar trac-wiki-page-info nil)
(make-variable-buffer-local 'trac-wiki-page-info)

(defvar trac-wiki-search-keyword-hist nil)
(defvar trac-wiki-search-filter-hist nil)

(defvar trac-wiki-search-filter-cache nil
  "Alist of end-point and list of filter names supported in site.
This value is made automaticaly on first search access.")

(defconst trac-wiki-system-pages
  '("CamelCase" "InterMapTxt" "InterTrac" "InterWiki"
    "RecentChanges" "TitleIndex"
    "TracAccessibility" "TracAdmin" "TracBackup" "TracBrowser"
    "TracCgi" "TracChangeset" "TracEnvironment" "TracFastCgi"
    "TracGuide" "TracHacks" "TracImport" "TracIni"
    "TracInstall" "TracInstallPlatforms" "TracInterfaceCustomization"
    "TracLinks" "TracLogging" "TracModPython" "TracMultipleProjects"
    "TracNotification" "TracPermissions" "TracPlugins" "TracQuery"
    "TracReports" "TracRevisionLog" "TracRoadmap" "TracRss"
    "TracSearch" "TracStandalone" "TracSupport" "TracSyntaxColoring"
    "TracTickets" "TracTicketsCustomFields" "TracTimeline"
    "TracUnicode" "TracUpgrade" "TracWiki" "WikiDeletePage"
    "WikiFormatting" "WikiHtml" "WikiMacros" "WikiNewPage"
    "WikiPageNames" "WikiProcessors" "WikiRestructuredText"
    "WikiRestructuredTextLinks"
    ;; appeared in 0.11
    "TracWorkflow" "PageTemplates")
  "List of page names provided by trac as default.
These files are hidden on completion since not edited usualy.
These can be listed setting by variable
`trac-wiki-hide-system-pages' as nil.
Two pages WikiStart and SandBox is not in this list because
user may need or want to edit them.")

;; font-lock

(defconst trac-wiki-link-type-keywords
  '("ticket" "comment" "report" "changeset" "log" "diff" "wiki"
    "milestone" "attachment" "source")
  "Trac link type keywords to be used in font-lock.")

(defun trac-wiki-link-face (face)
  "Return FACE if not escaped by '!', or return 'normal."
  (if (eq (char-before (match-beginning 0)) ?!)
      'shadow
    face))

(defconst trac-wiki-camel-case-regexp
  "\\<\\([A-Z][a-z]+\\(?:[A-Z][a-z]*[a-z/]\\)+\\)"
  "Regular expression for camel case.")

;; for trac-wiki mode, simple
(defvar trac-wiki-font-lock-keywords
  `(("^\\(\\(=+\\) \\(.*\\) \\(=+\\)\\)\\(.*\\)" ; section heading
     (1 (if (string= (match-string 2) (match-string 4))
	    'bold
	  ;; Warn if starting/ending '='  count is not ballanced.
	  'font-lock-warning-face))
     (5 'shadow))
    ("^=.*" . font-lock-warning-face)	   ; invalid section heading
    ("`[^`\n]*`" . 'shadow)		   ; inline quote
    ("\\(''+\\)[^'\n]*\\(''+\\)"	   ; bold and italic
     (0 (let ((b (match-string 1))
	      (e (match-string 2)))
	  (if (not (string= b e))
	      font-lock-warning-face
	    (cond
	     ((string= b "''") 'italic)
	     ((string= b "'''") 'bold)
	     ((string= b "''''") 'bold-italic))))))
    ("\\[\\[\\([^]()]+\\)[^]\n]*\\]\\]"	; macro
     ;; font-lock-preprocessor-face is not defined in emacs 21
     (0 (trac-wiki-link-face
	 (if (trac-wiki-macro-exist-p (match-string 1))
	     font-lock-type-face
	   font-lock-warning-face))))
    ("{[0-9]+}"	;  {1}
     (0 (trac-wiki-link-face font-lock-type-face)))
    ("\\[\\(\\w+:\\)?\\([^] #\n]*\\)[^]\n]*\\]" ; bracket trac link
     (0 (let ((whole (match-string 0))
	      (scheme (match-string 1))
	      (name (match-string 2)))
	  (trac-wiki-link-face
	   (cond
	    ((save-match-data	 ; [1], [1/trunk], [../file], [/trunk]
	       (string-match "^\\[[1-9./]" whole))
	     font-lock-function-name-face)
	    ((or (string= scheme "wiki:")
		 (null scheme))
	     (if (trac-wiki-page-exist-p name)
		 font-lock-function-name-face
	       font-lock-warning-face))
	    (t
	     font-lock-function-name-face))))))
    (,(format "\\(?:%s\\):\\(?:\"[^\"\n]*\"\\|[[^|\t\n]]\\)+" ; types
	      (regexp-opt trac-wiki-link-type-keywords))
     (0 (trac-wiki-link-face font-lock-function-name-face)))
    ("\\w+://[[^|\t\n]]+" . font-lock-function-name-face)	  ; raw url
    ("\\(?:#\\|\\br\\)[0-9]+\\(?::[0-9a-z]+\\)?\\b" ; r123 or #123
     (0 (trac-wiki-link-face font-lock-function-name-face)))
    (,(concat trac-wiki-camel-case-regexp ; camel case
	      "\\(?:#\\(?:\\w\\|[-:.]\\)+\\)?\\>")	       ; fragments
     (0 (trac-wiki-link-face
	 (if (trac-wiki-page-exist-p (match-string 1))
	     font-lock-function-name-face
	   font-lock-warning-face))))
    ("||" . 'shadow)			; table delimiter
    )
  "For `trac-wiki-mode'.")

;; for history buffer
(defvar trac-wiki-history-font-lock-keywords
  '(("^\\([^:]+:\\) +\\(.*\\)"
     (1 'bold)
     (2 'shadow)))
  "For history buffer.")

;; for search result
(defvar trac-wiki-search-result-font-lock-keywords
  '(("^\\([[^|\n]]+\\):.*"
     (1 'bold))
    ("^  \\(http://.*\\)"
     (1 font-lock-function-name-face))
    ("^\\(By \\w+ -- [[-:|0-9]]+\\)\n\n"
     (1 font-lock-type-face)))
  "For search result buffer.")


;; history holder
(defvar trac-wiki-project-history nil)
(defvar trac-wiki-url-history nil)
(defvar trac-wiki-page-history nil)
(defvar trac-wiki-comment-history nil)

;; key map

(defvar trac-wiki-mode-map (make-sparse-keymap))

(define-key trac-wiki-mode-map "\C-c\C-c" 'trac-wiki-commit)
(define-key trac-wiki-mode-map "\C-c=" 'trac-wiki-diff)
(define-key trac-wiki-mode-map "\C-c\C-d" 'trac-wiki-diff)
(define-key trac-wiki-mode-map "\C-c\C-u" 'trac-wiki-revert)
(define-key trac-wiki-mode-map "\C-c\C-m" 'trac-wiki-merge)
(define-key trac-wiki-mode-map "\C-c\C-p" 'trac-wiki-preview)
(define-key trac-wiki-mode-map "\C-c\C-l" 'trac-wiki-history)
(define-key trac-wiki-mode-map "\C-c\C-o" 'trac-wiki-edit)
(define-key trac-wiki-mode-map "\C-c\C-s" 'trac-wiki-search)
(define-key trac-wiki-mode-map "\C-c\C-v" 'trac-wiki-view-page)
(define-key trac-wiki-mode-map "\C-c\C-i" 'trac-wiki-show-info)
(define-key trac-wiki-mode-map "\C-\M-i" 'trac-wiki-complete-at-point)

(define-key trac-wiki-mode-map "\C-x\C-s" 'trac-wiki-save)


(defvar trac-wiki-search-result-mode-map nil)
(let ((map (make-sparse-keymap)))
  (define-key map "n" 'trac-wiki-search-result-next)
  (define-key map "p" 'trac-wiki-search-result-prev)
  (define-key map "e" 'trac-wiki-search-result-edit)
  (define-key map "o" 'trac-wiki-search-result-edit)
  (define-key map "\C-c\C-s" 'trac-wiki-search)
  (define-key map "s" 'trac-wiki-search)
  (define-key map "q" 'trac-wiki-delete-window-or-bury-buffer)
  (setq trac-wiki-search-result-mode-map map))

;; accessor

(defsubst trac-wiki-page-version ()
  "Get page version of current page in buffer.
If editing page is newly created, returns 0."
  (if (null trac-wiki-page-info)
      (error "Page information is not exist!"))
  (cdr (assoc "version" trac-wiki-page-info)))

(defsubst trac-wiki-page-name ()
  "Get page name of current page."
  (if (null trac-wiki-page-info)
      (error "Page information is not exist!"))
  (cdr (assoc "name" trac-wiki-page-info)))

(defsubst trac-wiki-page-author ()
  "Get last modified author of current page.
If editing page is newly created, return nil."
  (if (null trac-wiki-page-info)
      (error "Page information is not exist!"))
  (cdr (assoc "author" trac-wiki-page-info)))

(defsubst trac-wiki-page-hash ()
  "Get hash value of original page content.
If editing page is newly created, return hash of empty page."
  (if (null trac-wiki-page-info)
      (error "Page information is not exist!"))
  (cdr (assoc "hash" trac-wiki-page-info)))

(defsubst trac-wiki-page-modified-time ()
  "Get last modified time as readable format.
If editing page is newly created, return buffer creation time."
  (let ((modified (cdr (assoc "lastModified" trac-wiki-page-info))))
    (trac-wiki-convert-to-readable-time-string modified)))

;; workaround for old emacs/url
(when (eq emacs-major-version 21)
  ;; mail-header-extract<f> is required by url-extract-mime-headers<f>
  ;; but it does not do require<f>. So I do it.
  (require 'mailheader)

  (defmacro with-local-quit (&rest body)
    (declare (debug t) (indent 0))
    `(condition-case nil
	 (let ((inhibit-quit nil))
	   ,@body)
       (quit (setq quit-flag t)
	     (eval '(ignore nil)))))

  ;; adapt to new multi-byte handling.
  (defadvice encode-coding-string (around trac-wiki (str enc))
    "Adapt to allow emacs 22 style multi-byte operation."
    (setq ad-return-value
	  (with-temp-buffer
	    (insert str)
	    (encode-coding-region
	     (point-min) (point-max) enc)
	    (buffer-string))))

  ;; Making callback argument.
  (defadvice url-retrieve (before trac-wiki
				  (url &optional callback args) activate)
    "Bug workaround advice for Emacs 21 and w3-url-e21(2005.10.23-5).
Url package (debian testing, stable) for Emacs 21.4 has a bug.
It seems the function `url-retrieve-synchronously' does not pass
callback argument but `url-http-handle-authentication' expects url is
in 1st element of callback argument. This advice fakes for this."
    (if (null args)
	(setq args (list url))))

  ;;; these 2 advices are for asking on every 401 auth reply.
  (defadvice url-http-handle-authentication (before trac-wiki (proxy) activate)
    "Clear auth info if last request with authentication is failed.
This behaviour is required  for old url libraries as workaround because
it re-use bad auth data on retrying although authentication is failed."
    (when (< 0 trac-wiki-auth-retry-count) ; internal global variable
      (setq url-http-real-basic-auth-storage nil
	    url-http-real-digest-auth-storage nil))
    (setq trac-wiki-auth-retry-count (1+ trac-wiki-auth-retry-count)))

  )


(when (and (< emacs-major-version 22)
	   (boundp 'url-basic-auth-storage)
	   (not (boundp 'url-http-real-basic-auth-storage)))
  ;; This is for Emacs 21 and old url library (on debian sarge stable)
  ;; Old url library does not remember authentication data
  ;; due to local binding in url-http-handle-authentication<f>.
  ;; So it asked you user/pass every time.
  ;; This advice grab the authentication info very after
  ;; prompted and hold to use on next time.
  (defvar trac-rpc-basic-auth-storage nil
    "Grabbed basic authentication data")
  (defvar trac-rpc-digest-auth-storage nil
    "Grabbed digest authentication data")
  (defadvice url-get-authentication (around trac-wiki activate)
    "Trap to grab authentication data."
    ;; remember auth info into our own storage
    (let ((url-basic-auth-storage trac-rpc-basic-auth-storage)
	  (url-digest-auth-storage trac-rpc-digest-auth-storage))
      ad-do-it
      (setq trac-rpc-basic-auth-storage url-basic-auth-storage
	    trac-rpc-digest-auth-storage url-digest-auth-storage))))

(if (not (fboundp 'buffer-local-value))
    (defun buffer-local-value (sym buf)
      (if (not (buffer-live-p buf))
	  nil
	(with-current-buffer buf
	  (if (boundp sym)
	      (symbol-value sym)
	    nil)))))

(defun trac-wiki-url-retrieve-synchronously (url)
  "Fetch the content from URL.
This is alternative function of `url-retrieve-synchronously' to
avoid buf of url library.  Url library comes with Emacs 22 and
older veresion has a problem on synchronizing when authentication
is occured.  The code is copied from `url.el' of Emacs 22 and
modified not to quit by closing (or exiting) of first process.
Without this, `url-retrieve-synchronously' returns wrong buffer
on getting response \"403: auth required\"."
  (url-do-setup)
  (lexical-let ((retrieval-done nil)
		(asynch-buffer nil))
    (setq asynch-buffer
	  (url-retrieve url (lambda (&rest ignored)
			      (url-debug 'retrieval "Synchronous fetching done (%S)" (current-buffer))
			      (setq retrieval-done t
				    asynch-buffer (current-buffer)))))
    (if (null asynch-buffer)
        ;; We do not need to do anything, it was a mailto or something
        ;; similar that takes processing completely outside of the URL
        ;; package.
        nil
      (let ((proc (get-buffer-process asynch-buffer)))
	;; If the access method was synchronous, `retrieval-done' should
	;; hopefully already be set to t.  If it is nil, and `proc' is also
	;; nil, it implies that the async process is not running in
	;; asynch-buffer.  This happens e.g. for FTP files.  In such a case
	;; url-file.el should probably set something like a `url-process'
	;; buffer-local variable so we can find the exact process that we
	;; should be waiting for.  In the mean time, we'll just wait for any
	;; process output.
	(while (not retrieval-done)
	  (url-debug 'retrieval
		     "Spinning in url-retrieve-synchronously: %S (%S)"
		     retrieval-done asynch-buffer)
          (if (buffer-local-value 'url-redirect-buffer asynch-buffer)
              (setq proc (get-buffer-process
                          (setq asynch-buffer
                                (buffer-local-value 'url-redirect-buffer
                                                    asynch-buffer))))
            (if (and proc (memq (process-status proc)
                                ;;'(closed exit signal failed))
				;; MODIFIED! do not accept 'closed nor 'exit
				'(signal failed))
                     ;; Make sure another process hasn't been started.
                     (eq proc (or (get-buffer-process asynch-buffer) proc)))
                ;; FIXME: It's not clear whether url-retrieve's callback is
                ;; guaranteed to be called or not.  It seems that url-http
                ;; decides sometimes consciously not to call it, so it's not
                ;; clear that it's a bug, but even then we need to decide how
                ;; url-http can then warn us that the download has completed.
                ;; In the mean time, we use this here workaround.
		;; XXX: The callback must always be called.  Any
		;; exception is a bug that should be fixed, not worked
		;; around.
                (setq retrieval-done t))
            ;; We used to use `sit-for' here, but in some cases it wouldn't
            ;; work because apparently pending keyboard input would always
            ;; interrupt it before it got a chance to handle process input.
            ;; `sleep-for' was tried but it lead to other forms of
            ;; hanging.  --Stef
            (unless (or (with-local-quit
			  (accept-process-output proc))
			(null proc))
              ;; accept-process-output returned nil, maybe because the process
              ;; exited (and may have been replaced with another).  If we got
	      ;; a quit, just stop.
	      (sit-for 0.01)		; give a chance to callback
	      (when quit-flag
		(delete-process proc))
              (setq proc (and (not quit-flag)
			      (get-buffer-process asynch-buffer)))))))
      ;; wait a little to avoid bug in e21 not to re-use closing  session.
      (if (< emacs-major-version 22)
	  (sleep-for 0.05))		; not to re-use closing session
      ;; fix-up lacked tail. On old emacs (emacs 21), sometime some bytes
      ;; of response are lacked. like "</methodRespons"
      ;; So fix it.
      (with-current-buffer asynch-buffer
	(save-excursion
	  (goto-char (point-max))
	  (beginning-of-line)
	  (when (looking-at "</method")
	    (delete-region (point) (save-excursion
				     (end-of-line) (point)))
	    (insert "</methodResponse>\n")
	    (message "*** Fixed http response ***")
	    (sit-for 0.5))))
      asynch-buffer)))


(defvar trac-wiki-auth-retry-count 0
  "Internal global variable to count auth retry.")

(defadvice url-retrieve-synchronously (around trac-wiki (url) activate)
  "Advice to use our alternative function.
And aslo providing workaround of buf for storing auth info in old bad
url library behaviour."
  ;; to avoid endless auth retry wihtout asking in old url library.
  (let ((orig-basic-auth-storage (copy-sequence
				  url-http-real-basic-auth-storage)))
    (unwind-protect
	(setq trac-wiki-auth-retry-count 0
	      ad-return-value (trac-wiki-url-retrieve-synchronously url))
      ;; restore original if fail
      ;; merge with original if success
      (if (and ad-return-value
	       (with-current-buffer ad-return-value
		 (save-excursion
		   (goto-char 1)
		   (looking-at "HTTP/[0-9.]+ 2[0-9]+"))))
	  ;; success, merge backuped storage into real storage
	  (trac-wiki-merge-storage url-http-real-basic-auth-storage
				   orig-basic-auth-storage)
	;; failed, restore original
	(setq url-http-real-basic-auth-storage
	      orig-basic-auth-storage))
      ;; clean up on ether successed or fail.
      (trac-wiki-cleanup-auth-storage url-http-real-basic-auth-storage))))


;; predicate
(defun trac-wiki-page-exist-p (page)
  "Return non-nil if PAGE exists in page name cache or no cache.
Note that if buffer does not has end-point information, return
also non-nil because we cannot get cache data.  In other word,
\"I don't know\" is non-nil."
  (or (null trac-rpc-endpoint)
      (trac-wiki-cache-item-exist-p page trac-wiki-page-name-cache)))

(defun trac-wiki-macro-exist-p (macro)
  "Return non-nil if MACRO exists in macro name cache or no cache.
Note that if buffer does not has end-point information, return
also non-nil because we cannot get cache data.  In other word,
\"I don't know\" is non-nil."
  (or (null trac-rpc-endpoint)
      (trac-wiki-cache-item-exist-p macro trac-wiki-macro-name-cache)))


(defun trac-wiki-cache-item-exist-p (item cache)
  "Return non-nil if ITEM exists in CACHE or CACHE is nil.
Note that if buffer does not has end-point information, return
also non-nil because we cannot get cache data.  In other word,
\"I don't know\" is non-nil."
  (let ((items (and trac-rpc-endpoint
		    (cdr (assoc trac-rpc-endpoint cache)))))
    (or (null items)
	(member item items))))

;; cache macro
(defmacro trac-wiki-with-cache (cache-name ep no-cache &rest body)
  "Update CACHE-NAME for EP regarding NO-CACHE with result of BODY.
CACHE-NAME is symbol of variable which is cache data storage formated
as alist of end-point and cache data.
EP is end-point string and works as key of cache data to select.
If NO-CACHE is nil, return data in cache if exist without executing
BODY.  If NO-CACHE is non-nil, always run BODY and update cache with its
result data."
  `(let* ((entry (assoc ,ep (symbol-value ,cache-name)))
	  (data (if (and (not ,no-cache) entry)
		    (cdr entry)
		  ,@body)))
     (if (null entry)
	 (set ,cache-name (cons (cons ,ep data)
				(symbol-value ,cache-name)))
       (setcdr entry data))
     data))
(put 'trac-wiki-with-cache 'lisp-indent-function 3)


(defun trac-wiki-update-page-name-cache ()
  "Update cache of wiki page names."
  (interactive)
  (prog1
      (trac-wiki-with-cache
	  'trac-wiki-page-name-cache
	  trac-rpc-endpoint 'update
	(trac-rpc-get-all-pages))
    (if (interactive-p)
	(message "Page name cache is updated."))))

(defun trac-wiki-update-macro-name-cache ()
  "Update cache of wiki macro names."
  (interactive)
  (prog1
      (trac-wiki-with-cache
	  'trac-wiki-macro-name-cache
	  trac-rpc-endpoint 'update
	(trac-wiki-collect-macro-names))
    (if (interactive-p)
	(message "Macro name cache is updated."))))


;; mode

(define-derived-mode trac-wiki-mode text-mode "TracWiki"
  "Trac Wiki authorizing mode with XML-RPC access."
  (set (make-local-variable 'font-lock-defaults)
       '(trac-wiki-font-lock-keywords t))
  (require 'font-lock)
  (if font-lock-mode
      (font-lock-fontify-buffer))
  (set (make-local-variable 'outline-regexp) "^=+ ")
  (outline-minor-mode 1))


;; XML-RPC functions

(defun trac-rpc-call (method &rest args)
  "Call METHOD with ARGS via XML-RPC and return response data.
WARNING: This functionis not use because synchronous
`xml-rpc-method-call' has strange behavour on authentication
retrying.  Use `trac-rpc-call-async' instead."
  (when (< emacs-major-version 22)
    (ad-activate 'encode-coding-string))
  (unwind-protect
      (let* ((url-http-attempt-keepalives trac-wiki-use-keepalive)
	     (ep trac-rpc-endpoint)
	     (xml-rpc-base64-encode-unicode nil)
	     (xml-rpc-base64-decode-unicode nil)
	     (result (with-temp-buffer
		       (apply 'xml-rpc-method-call
			      ep method args))))
	(if (and (numberp result) (= result 0))
	    nil
	  (if (stringp result)
	      (apply `concat (split-string result "\r"))
	    result)))
    (when (< emacs-major-version 22)
      (ad-deactivate 'encode-coding-string))))

(defun trac-rpc-get-page (page &optional version)
  "Get content of PAGE in VERSION invoking XML-RPC call.
If VERSION is omitted, most recent version is selected."
  (if version
      (trac-rpc-call 'wiki.getPageVersion page version)
    (trac-rpc-call 'wiki.getPage page)))

(defun trac-rpc-get-page-info (page &optional version)
  "Get information of PAGE in VERSION invoking XML-RPC call.
If VERSION is omitted, most recent version is selected."
  (if version
      (trac-rpc-call 'wiki.getPageInfoVersion page version)
    (trac-rpc-call 'wiki.getPageInfo page)))

(defun trac-rpc-get-page-html (page &optional version)
  "Get rendered content of PAGE in VERSION invoking XML-RPC call.
If VERSION is omitted, most recent version is selected."
  (if version
      (trac-rpc-call 'wiki.getPageHTMLVersion page version)
    (trac-rpc-call 'wiki.getPageHTML page)))

(defun trac-rpc-get-all-pages (&optional endpoint)
  "Get list of page names available in remote site of ENDPOINT.
If optional argument EP is nil, use `trac-rpc-endpoint' is used."
  (let ((trac-rpc-endpoint (or endpoint trac-rpc-endpoint)))
    (trac-rpc-call 'wiki.getAllPages)))

(defun trac-rpc-put-page (page content comment)
  "Update PAGE as CONTENT with COMMENT.
COMMENT can be nil."
  (let ((attributes `(("comment" . ,(or comment "")))))
    (trac-rpc-call 'wiki.putPage page content attributes)))

(defun trac-rpc-wiki-to-html (content)
  "Covnert wiki CONTENT into html via XML-RPC method call."
  (trac-rpc-call 'wiki.wikiToHtml content))


(defun trac-rpc-get-page-version (&optional page)
  "Get latest version of PAGE in remote."
  (if (or (null trac-wiki-page-info)
	  (null trac-rpc-endpoint))
      (error "Page information is not exist!"))
  (let ((info (trac-rpc-get-page-info
	       (or page (trac-wiki-page-name)))))
    (if (null info)
	0				; no page, return version 0
      (cdr-safe (assoc "version" info)))))


;; mode functions and utilities

(defun trac-wiki-read-page-name (&optional default)
  "Enter page name with competion.
If DEFAULT is specified, use it as initial input on completion."
  (let ((cached (and (not trac-wiki-update-page-name-cache-on-visit)
		     (cdr (assoc trac-rpc-endpoint
				 trac-wiki-page-name-cache))))
	(all (trac-wiki-with-cache
		 'trac-wiki-page-name-cache
		 trac-rpc-endpoint trac-wiki-update-page-name-cache-on-visit
	       (trac-rpc-get-all-pages)))
	(re (concat "^\\(?:"
		    (if trac-wiki-hide-system-pages
			(concat (regexp-opt trac-wiki-system-pages) "\\|"))
		    (mapconcat 'identity trac-wiki-hidden-pages "\\|")
		    "\\)\\(?:\\.[a-z]\\{2\\}\\)?$")) ; lang suffix
	pages page)
    (dolist (page all)
      (unless (string-match re page)
	(add-to-list 'pages page)))
    (while (null page)
      (setq page (completing-read (if cached "Page name (cached): "
				    "Page name: ")
				  (mapcar 'list pages)
				  nil nil default 'trac-wiki-page-history)))
    page))

(defun trac-wiki-save ()
  "Alternative function to avoid usual file save function."
  (interactive)
  (cond
   ((buffer-file-name)
    (save-buffer))			; usual save when editing local file
   ((y-or-n-p "You are editing remote page.  Save to file? ")
    ;; add header information and write to file
    (let ((content (buffer-string))
	  (filename))
      (with-temp-buffer
	(insert content)
	(setq filename (read-file-name "Enter filename to save: "))
	;; save to file with confirming overwriting.
	(write-region (point-min) (point-max) filename nil nil nil t))
      ;; ask to visit onto saved file.
      (if (and filename (y-or-n-p "Visit to saved file? "))
	  (find-file-other-window filename)
	(message "saved."))))
   (t
    (message "Canceled. (You may want to commit current page by %s)"
	     (substitute-command-keys "\\[trac-wiki-commit]")))))

(defun trac-wiki-ask-project ()
  "Prompts to enter project name and return its information data.
Returns project info which is property list of some data.  If hit
enter without project name, ask enter project informations
interectively and remember temporary project information data
named as \"dir@host\".  It will be kept until re-start Emacs."
  (let* ((project (and (or trac-projects
			   trac-wiki-project-history)
		       (completing-read "Select project (or empty to define): "
					trac-projects
					nil t nil
					'trac-wiki-project-history)))
	 (pinfo (and project
		     (cdr (assoc project trac-projects)))))
    (or pinfo
	;; make project data interactively.
	(let* ((rawurl (read-string "Site URL: "
				 trac-rpc-endpoint
				 'trac-wiki-url-history))
	       (url (url-generic-parse-url
		     (trac-wiki-strip-url-trailer
		      rawurl '("xmlrpc" "login" "wiki"))))
	       (login (or (url-user url)
                          (string-match "/login\\(?:/\\|$\\)" rawurl)
                          (y-or-n-p "Login? ")))
	       (host (url-host url))
	       (name (file-name-nondirectory
		      (directory-file-name (url-filename url))))
	       (project-name (format "%s@%s" name host))
	       info)
	  ;; build project info property list
	  (prog1
	      ;; make return value
	      (setq info (list :name project-name
			       :endpoint (format "%s://%s:%s%s%s/xmlrpc"
						 (url-type url)
						 (url-host url)
						 (url-portspec url)
						 (directory-file-name (url-filename url))
						 (if (string= login "")
						     ""
						   "/login"))
			       :login (and (stringp login)
					   (not (string= login ""))
					   login)))
	    ;; remember it
	    (if (not (assoc project-name trac-projects))
		(add-to-list 'trac-projects (cons project-name info))
	      ;; already exist, overwrite
	      (setcdr (assoc project-name trac-projects) info)))))))

(defmacro trac-wiki-protected (&rest body)
  "Run BODY or report readable message from response code on error."
  `(condition-case e
       (progn
	 ,@body)
     (error
      (if (and (listp e) (listp (cdr e)) (stringp (cadr e)))
	  (let ((emsg (cadr e)))
	    (cond
	     ((string-match ": 404$" emsg)
	      (message "ERROR: The site seems not support XML-RPC."))
	     ((string-match ": 401$" emsg)
	      (message "ERROR: Authentication failed."))
	     ((string-match "privilege is required" emsg)
	      (message "ERROR: You are not privileged for this operation."))
	     ((string-match ": 403$" emsg)
	      ;; if XML_RPC privilege is not set, you'll get 403
	      (message "ERROR: forbidden, you may not have XML_RPC privilege for this operation."))
	     (t
	      (message "ERROR: %s" emsg))))
	(message "Error: %s" e)))))


(defun trac-wiki ()
  "Initial interface to edit trac wiki page.
You can select trac project by name which is pre-defined,
or enter raw URL of XML-RPC endpoint."
  (interactive)
  (let ((trac-wiki-update-page-name-cache-on-visit t))
    (trac-wiki-edit 'ask)))		; always ask endpoint

(defun trac-wiki-edit (&optional ask-project)
  "Retreive wiki page content with new buffer.
If with prefix argument ASK-PROJECT, force asking project instead
of current buffer's one."
  (interactive "P")
  (let ((pinfo (or (and (not ask-project) trac-wiki-project-info)
		   (trac-wiki-ask-project)))
	(page (and (eq major-mode 'trac-wiki-mode) ; only in trac-wiki buffer
		   (not ask-project)
		   (trac-wiki-pick-wiki-name-at-point))))
    (let ((ep (plist-get pinfo :endpoint)))
      (if (not (and (stringp ep)
		    (string-match "\\`https?://[[^|]]+/\\(?:.*/\\)?xmlrpc" ep)))
	  (error "Invalid endpoint: %s" ep)))
    (trac-wiki-protected
     (trac-wiki-visit pinfo page))))


(defun trac-wiki-visit (project &optional page force)
  "Access to PROJECT and visit to PAGE to edit.
PROJECT is project information data which is property list
defined in `trac-projects'.  You will be asked page name to edit
with completion.  Page names are retrieved by fetching all the
page names in remote site via XML-RPC call at this time if
`trac-wiki-update-page-name-cache-on-visit' is non-nil.  If PAGE
is specified, use it as initial input.  If FORCE is non-nil,
visit to PAGE without interaction."
  ;; case of project argument is project name
  (if (and (stringp project)
	   (assoc project trac-projects))
      (setq project (cdr (assoc project trac-projects))))
  ;; case of project argument is endpoint
  (if (and (stringp project)
	   (string-match "^https?://" project)) ; assume endpoint url
      (dolist (proj trac-projects)
	(when (and (stringp project)
		   (string= project (plist-get (cdr proj) :endpoint)))
	  (setq project (cdr proj)))))
  (if (or (null project)
	  (not (listp project)))
      (error "Invalid project info"))
  (let ((ep (plist-get project :endpoint)))
    ;; this is workaround for bug of url-generic-parse-url<f>
    ;; (bug is found in url-parse.el rev 1.13)
    (url-generic-parse-url ep)
    ;; clean up wasted buffer named as " *http://xxx.xxx .....*"
    (dolist (buf (buffer-list))
      (if (and (string-match "^ \\*http" (buffer-name buf))
	       (let ((proc (get-buffer-process buf)))
		 (or (null proc)
		     (not (member (process-status proc) '(open run connect))))))
	  (kill-buffer buf)))
    ;; main
    (if (not force)
	(let ((trac-rpc-endpoint ep))
	  (setq page (trac-wiki-read-page-name page))
	  (if (or (null page) (string= page ""))
	      (error "Page name should be specified"))))
    ;; If not page is not already visited, retrieve and edit.
    ;; Else ask re-use already visited buffer.
    ;; If re-used, check version is up-to-date and merge if need.

    (if (catch 'found
	  (dolist (buf (buffer-list))
	    (set-buffer buf)
	    (if (and (eq major-mode 'trac-wiki-mode)
		     (string= trac-rpc-endpoint ep)
		     (string= (trac-wiki-page-name) page))
		(throw 'found
		       (and (y-or-n-p "The page is already visited.  Use it? ")
			    (switch-to-buffer (current-buffer)))))))
	;; re-use already exising buffer
	(let ((rver (trac-rpc-get-page-version page)) ; remote version
	      (ver (trac-wiki-page-version))	      ; local version
	      (modified (trac-wiki-modified-p)))
	  (if (eq ver rver)
	      (message "This page is version %d (%s)."
		       rver (if modified
				"up-to-date and modified"
			      "up-to-date"))
	    ;; not up-to-date
	    (cond
	     ((and (not modified)
		   (y-or-n-p
		    (format "This page has new version %d.  Update to it? "
			    rver)))
	      (trac-wiki-fetch-page page)) ; get latest
	     ((and modified
		   (y-or-n-p
		    (format "This page has new version %d.  Merge with it? "
			    rver)))
	      (trac-wiki-merge))
	     (t
	      (message "Continue editing current vresion %s (latest version is %s)."
		       ver rver)))))
      ;; newly visit page
      (switch-to-buffer (generate-new-buffer (format "%s" page)))
      (erase-buffer)
      (trac-wiki-mode)
      (setq trac-rpc-endpoint ep)
      (setq trac-wiki-project-info project)
      (trac-wiki-fetch-page page))))

(defun trac-wiki-fetch-page (page &optional version)
  "Fetch specified PAGE of VERSION into current buffer.
If VERSION is nil, most recent version will be fetched."
  (erase-buffer)
  (let ((info (trac-rpc-get-page-info page version)))
    (if (null info)
	(progn
	  ;; make dummy information
	  (setq trac-wiki-page-info `(("version" . 0)
				      ("name" . ,page)
				      ("lastModified" .
				       (:datetime ,(butlast (current-time))))
				      ("hash" . ,(md5 ""))))
	  (message "new page"))
      (insert (trac-rpc-get-page page))
      (goto-char (point-min))
      (trac-wiki-update-page-info page info))
    (set-buffer-modified-p nil)
    (let ((ver (trac-wiki-page-version)))
      (if (= ver 0)
	  (message "New page.")
	(message "Page is retrieved (version = %s)" ver)))))

(defun trac-wiki-update-page-info (page &optional info)
  "Update information of PAGE as INFO.
If INFO is not specified, information is retrieved via XML-RPC call.
This information is page specific data holded as buffer local variable."
  (setq info (or info (trac-rpc-get-page-info page)))
  (when (eq info 0)
    ;; case of new page (no page information exists).
    (setq info `(("name" . ,page)
		 ("version" . 0))))
  (add-to-list 'info `("hash" . ,(md5 (buffer-string) nil nil 'utf-8)))
  (setq trac-wiki-page-info info))

(defun trac-wiki-commit ()
  "Commit current content to remote site.
Before commit, check the version of this page in remote site is match
with local version.  If not matched, show warning and do merging.
If local content is not changed, confirm doing."
  (interactive)
  (if (null trac-rpc-endpoint)
      (error "This buffer is not managed as trac wiki mode")
  (cond
   ((not (buffer-modified-p))
    (message "Nothing changed."))
   ((and (string= (trac-wiki-page-hash)
		  (md5 (buffer-string) nil nil 'utf-8))
	 (not (y-or-n-p "Buffer seems to be same.  Commit it? ")))
    (message "canceled."))
   ((/= (trac-rpc-get-page-version)
	(trac-wiki-page-version))
    (if (y-or-n-p (format "Remote page is updated (version: local=%s, remote=%s).  Merge with it? "
			  (trac-wiki-page-version)
			  (trac-rpc-get-page-version)))
	(trac-wiki-merge)
      (message "commit canceled.")))
   (t
    ;; do it
    (let ((comment (read-string "Comment: " nil 'trac-wiki-comment-history))
	  (page (trac-wiki-page-name)))
      (trac-rpc-put-page page (buffer-string) comment)
      ;; update new info
      (trac-wiki-update-page-info page)
      (set-buffer-modified-p nil)
      (let* ((entry (assoc trac-rpc-endpoint trac-wiki-page-name-cache))
	     (names (cdr entry)))
	(when (not (member page names))
	  ;; add this page into cache
	  (if (null entry)
	      (setq trac-wiki-page-name-cache
		    (list trac-rpc-endpoint page))
	    (setcdr entry (cons page names)))
	  ;; re-fontify all the pages which has same endpoint
	  (save-excursion
	    (let ((ep trac-rpc-endpoint))
	      (dolist (buf (buffer-list))
		(set-buffer buf)
		(if (and (eq major-mode 'trac-wiki-mode)
			 (eq trac-rpc-endpoint ep)
			 font-lock-mode)
		    (font-lock-fontify-buffer)))))))
      (message "Committed as version %s" (trac-wiki-page-version)))))))

(defun trac-wiki-modified-p ()
  "Return non-nil if buffer content is modified.
Jadgement of 'modified' is done by `buffer-modified-p' flag
and comparation of MD5 hash with current and original content.
Note that return nil if MD5 is equal althogh `buffer-modified-p' is non-nil."
  (let ((modified (and (buffer-modified-p)
		       (not (string= (trac-wiki-page-hash)
				     (md5 (buffer-string) nil nil 'utf-8))))))
    modified))

(defun trac-wiki-revert ()
  "Revert to original content with discarding local change."
  (interactive)
  (if (not (trac-wiki-modified-p))
      (message "Nothing changed.")
    (let ((config (current-window-configuration)))
      (trac-wiki-diff nil)
      (if (not (y-or-n-p "Really revert these changes? "))
	  (message "canceled.")
	(erase-buffer)
	(let ((page (trac-wiki-page-name))
	      (ver (trac-wiki-page-version)))
	  (insert (trac-rpc-get-page page ver))
	  (set-buffer-modified-p nil)
	  (goto-char 1)
	  (message "Reverted to original (version=%s)" ver)))
      (set-window-configuration config)
      ;; erase diff buffer
      (let ((win (get-buffer-window trac-wiki-diff-buffer-name)))
	(if win
	    (delete-window win))))))


(defun trac-wiki-diff (arg)
  "Diff with original version in remote.
If with prefix ARG, invoke `ediff' instead of `diff'."
  ;; WISH: If content is not so big, it is better that the original
  ;;  text is holded in local variable, and we don't need access to
  ;;  server for diff.
  (interactive "P")
  (if arg
      (trac-wiki-ediff)
    ;; clean up diff output window
    (let ((win (get-buffer-window trac-wiki-diff-buffer-name)))
      (if win
	  (delete-window win)))
    ;; check and confirm unmodified guess.
    (if (not (trac-wiki-modified-p))
	(message "Nothing changed.")
      (let* ((page (trac-wiki-page-name))
	     (version (trac-wiki-page-version)))
	(if (= 0 version)
	    (message "No need to diff. This is initial version.")
	  (let ((orig (trac-rpc-get-page page version)))
	    (if (null orig)
		(error "Error on fetching page: %s, version %s" page version))
	    (trac-wiki-diff-internal (buffer-substring-no-properties
				      (point-min) (point-max)) orig)))))))

(defun trac-wiki-diff-internal (str1 str2)
  "Show diff of two content STR1 and STR2 in popup buffer."
  (let* ((trac-rpc-endpoint trac-rpc-endpoint)
	 (page (trac-wiki-page-name))
	 (tmpa (make-temp-file "wiki"))
	 (tmpb (make-temp-file "wiki")))
    (unwind-protect
	(progn
	  (require 'diff)
	  (let ((coding-system-for-write 'utf-8)
		(coding-system-for-read 'utf-8))
	    (with-temp-file tmpa
	      (insert str2))
	    (with-temp-file tmpb
	      (insert str1))
	    (condition-case nil
		(diff tmpa tmpb nil 'no-async) ; for emacs 22.50 or later
	      (error
	       ;; for emacs 21 or before
	       (diff tmpa tmpb))))
	  (with-current-buffer (get-buffer trac-wiki-diff-buffer-name)
	    ;; wait for process completion, required for Emacs 21
	    (let ((proc (get-buffer-process (current-buffer))))
	      (while (and proc
			  (member (process-status proc)
				  '(run open connect)))
		(accept-process-output proc)))
	    (local-set-key "q" 'trac-wiki-delete-window-or-bury-buffer)
	    (let ((win (get-buffer-window (current-buffer))))
	      (if (not (re-search-forward "^Diff finished (no differences)."
					  nil t))
		  (progn
		    (setq buffer-read-only t)
		    (shrink-window-if-larger-than-buffer win)
		    (message "done."))	; clear last message
		(delete-window win)
		(message "No difference.")))))
      (delete-file tmpa)
      (delete-file tmpb))))

(defun trac-wiki-ediff ()
  "Invoke `ediff' with original content."
  (interactive)
  (let* ((mode major-mode)
	 (page (trac-wiki-page-name))
	 (version (trac-wiki-page-version)))
    (if (= version 0)
	(message "No need to ediff. This is initial version.")
      (let ((content (trac-rpc-get-page page version))
	    (buf (generate-new-buffer (format "*%s BASE*" page))))
	(when (null buf)
	  (kill-buffer buf)
	  (error "Error on fetching page: %s, version %s" page version))
	(with-current-buffer buf
	  (erase-buffer)
	  (insert content)
	  (funcall mode)
	  (set-buffer-modified-p nil))
	(ediff-buffers (current-buffer) buf)))))


(defvar trac-wiki-merge-windows nil)
(defvar trac-wiki-merge-buffer nil)
(defvar trac-wiki-merge-page-info nil)

(defun trac-wiki-merge ()
  "Merge with most recent version if exist.
Compare md5 of page info in current buffer and md5 of latest remote page
content.  If md5 and version number is same, two page is up-to-date.
If version number is differ, remote page is revised or deleted.  This case
need merging.
If version number is same but md5 is differ, remote page is deleted then
revised.  This case also need merging."
  (interactive)
  (let* ((page (trac-wiki-page-name))
	 (page-info (trac-rpc-get-page-info page)) ; of recent one
	 (rver (cdr (assoc "version" page-info)))  ; remote version
	 (rcontent (trac-rpc-get-page page))	   ; remote content
	 (rmd5 (md5 rcontent nil nil 'utf-8))	   ; remote md5
	 (lver (trac-wiki-page-version))	   ; local version
	 (lmd5 (trac-wiki-page-hash))		   ; local md5
	 (bcontent (if (< rver lver)		   ; base content
		       rcontent
		     (trac-rpc-get-page page lver)))
	 (bmd5 (md5 bcontent nil nil 'utf-8))
	 msg)
    (if (and (= lver rver)
	     (string= lmd5 rmd5))
	;; up-to-date
	(message "This page is up-to-date. No need to merge.")
      ;; something different
      (cond
       ((< rver lver)			; base version is not exist
	(setq msg "Remote page is deleted."))
       ((and (< lver rver)
	     (string= lmd5 bmd5))
	(setq msg "Remote page is updated.")) ; usual update
       (t
	(setq msg "Remote page might be deleted and updated.")))

      (if (not (save-window-excursion
		 (delete-other-windows)
		 (let ((act (if (trac-wiki-modified-p)
				"Merge with"
			      "Update to")))
		   (with-temp-buffer
		     (insert "        Ver.  MD5\n"
			     (format "local:  %4d  %s\n" lver lmd5)
			     (format "remote: %4d  %s\n" rver rmd5))
		     (pop-to-buffer (current-buffer))
		     (shrink-window-if-larger-than-buffer)
		     (y-or-n-p (format "%s %s latest version? " msg act))))))
	  (message "Canceld.")
	(if (not (trac-wiki-modified-p))
	    ;; simply replace current content and update page info
	    (let ((pt (point)))
	      (erase-buffer)
	      (insert rcontent)
	      (trac-wiki-update-page-info page page-info)
	      (set-buffer-modified-p nil)
	      (message "Page is updated to version %d" rver))
	  (let* ((mode major-mode)
		 (config (current-window-configuration))
		 (cur (current-buffer))
		 (mine-buf (generate-new-buffer (format "*%s MINE*" page)))
		 (their-buf (generate-new-buffer (format "*%s OTHER*" page)))
		 (base-buf (generate-new-buffer (format "*%s BASE*" page))))
	    (require 'ediff)
	    (with-current-buffer mine-buf
	      (insert-buffer-substring cur)
	      (funcall mode))
	    (with-current-buffer their-buf
	      (insert rcontent)
	      (funcall mode))
	    (with-current-buffer base-buf
	      (insert bcontent)
	      (funcall mode))
	    ;; start merging
	    (set-buffer
	     (ediff-merge-buffers-with-ancestor mine-buf their-buf base-buf))
	    ;; prepare for sentinel action
	    (set (make-local-variable 'trac-wiki-merge-windows) config)
	    (set (make-local-variable 'trac-wiki-merge-buffer) cur)
	    (set (make-local-variable 'trac-wiki-merge-page-info)
		 (append page-info `("hash" ,(md5 (buffer-string)
						  nil nil 'utf-8))))
	    (set (make-local-variable 'ediff-quit-hook) 'trac-wiki-merge-sentinel)
	    (message "Please merge with recent version.")))))))


(defun trac-wiki-merge-sentinel ()
  "Called by `ediff-quit-hook' for cleanup and aplying merge result."
  (let ((buffers (list ediff-buffer-A ediff-buffer-B ediff-buffer-C
		       ediff-ancestor-buffer))
	(merged ediff-buffer-C)
	(windows trac-wiki-merge-windows)
	(page-info trac-wiki-merge-page-info)
	(buf trac-wiki-merge-buffer))
    (ediff-cleanup-mess)
    (with-current-buffer buf
      (if (not (y-or-n-p "Accept merge result? "))
	  (message "Discarded merge result and stay on version %s."
		   (trac-wiki-page-version))
	(let ((buffer-read-only nil))
	  (erase-buffer)
	  (insert-buffer-substring merged)
	  (setq trac-wiki-page-info page-info)
	  (goto-char 1))
	(message "Merged with version %s and now on it."
		 (trac-wiki-page-version))))
    ;; kill all the buffuers
    (dolist (buf buffers)
      (if (bufferp buf)
	  (kill-buffer buf)))
    (set-window-configuration windows)))


(defun trac-wiki-strip-url-trailer (url trailers)
  "Return modified URL removing trailing words specfied in TRAILERS.
TRAILERS is list of string to be removed."
  (let ((re (concat "\\(?:/\\(?:" (regexp-opt trailers) "\\)\\)+$")))
    (if (string-match re url)
	(substring url 0 (match-beginning 0))
      url)))

(defun trac-wiki-preview (arg)
  "Preview current wiki content as html page.
Usualy this function requests conversion to html via XML-RPC
then render in Emacs buffer with `w3m' feature (if available)
With prefix ARG, execute browser using `browse-url' to preview
html.  It supports styles sheet."
  (interactive "P")
  (let ((html (trac-rpc-wiki-to-html (buffer-string)))
	(buf (get-buffer-create (if arg " *html-preview-tmp*" "*preview*")))
	(name (trac-wiki-page-name))
	;; this depen on trac url structure
	(base-url (trac-wiki-strip-url-trailer trac-rpc-endpoint
					       '("xmlrpc" "login" "wiki"))))
    (save-excursion
      (set-buffer buf)
      (setq buffer-read-only t)
      (let ((buffer-read-only nil)
	    (css (mapconcat
		  (lambda (x)
		    (format "<link rel='stylesheet' href='%s%s%s' type='text/css' />"
			    base-url "/chrome/common/css/" x))
		  '("trac.css" "wiki.css" "site_common.css")
		  "\n")))
	(erase-buffer)
	;; add some supplements as valid html content
	(insert (format "<html><head><title>%s (preview)</title>" name)
		"\n"
		css
		"\n</head><body>\n"
		"<div id='content' class='wiki'><div class='wikipage'>"
		html
		"</div></div></body>")
	;; replace links
	(goto-char (point-min))
	(while (re-search-forward "\\(?:href\\|src\\)=\"/" nil t)
	  (backward-char 1)
	  (insert base-url))
	(if arg
	    (progn
	      (require 'browse-url)
	      (let ((coding-system-for-write 'utf-8))
		(browse-url-of-buffer)
		(message "Previewing with external browser.")))
	  (require 'w3m)
	  (w3m-region (point-min) (point-max))
	  (goto-char (point-min))
	  (pop-to-buffer buf)
	  ;; define 'q' key to close preview buffer
	  (local-set-key "q" 'trac-wiki-delete-window-or-bury-buffer)
	  (message "Hit 'q' to quit preview window"))))))

(defun trac-wiki-view-page ()
  "View current page on remote trac site with external browser.
The page is viewed by `browse-url' function, not emacs-w3m."
  (interactive)
  (let* ((base (trac-wiki-strip-url-trailer trac-rpc-endpoint
					   '("xmlrpc" "login" "wiki")))
	 (url (format "%s/wiki/%s"
		      (directory-file-name base)
		      (trac-wiki-page-name))))
    (let ((default-directory temporary-file-directory))
      (browse-url url))
    (message "View page in browser: %s" url)))

(defun trac-wiki-show-info ()
  "Show url and version information of current page."
  (interactive)
  (let* ((version (trac-wiki-page-version))
	 (author (or (trac-wiki-page-author) "you"))
	 (date (or (trac-wiki-page-modified-time) "now"))
	 (state (if (and (< 0 version)
			 (not (string= (trac-wiki-page-hash)
				       (md5 (buffer-string) nil nil 'utf-8))))
		    " (localy modified)"
		  ""))
	 (page (cdr (assoc "name" trac-wiki-page-info)))
	 (lines `(("End Point" . trac-rpc-endpoint)
		  ("Page Name" . ,(concat page state))
		  ("Base Version" . (if (< 0 version)
					,(format "%s (made by %s at %s)"
						 version author date)
				      "(newly creating now)"))))
	 (wid 0))
    ;; align key strings with right justification.
    (dolist (elem lines)
      (if elem
	  (setq wid (max wid (string-width (car elem))))))
    ;; print message in echo erea (for emacs 22 )
    (message
     (mapconcat (lambda (elem)
		  (if elem
		      (concat (make-string (- wid (string-width (car elem))) ? )
			      (car elem)
			      ": "
			      (eval (cdr elem)))))
		lines "\n"))
    ))

(defun trac-wiki-html2text-string (str)
  "Return plain text string convert from html markup'ed STR."
  (require 'w3m)
  (with-temp-buffer
    (insert str)
    (require 'w3m)
    (w3m-region (point-min) (point-max))
    (goto-char (point-max))
    (skip-chars-backward " \t\n")
    (buffer-substring (point-min) (point))))

(defun trac-wiki-input-search-filters (filters)
  "Utility to make multiple choise from FILTERS.
In Emacs 22, this function simply use `completing-read-multiple'.
In Emacs 21, this function provides own selection loop because
`completing-read-multiple' has bit strange behaviour."
  (let ((more t)
	(alist (mapcar 'list filters))
	(prompt "Select filters: ")
	items result)
    (if (not (< emacs-major-version 22))
	(completing-read-multiple prompt
				  alist nil t
				  (mapconcat 'identity
					     trac-wiki-search-default-filters
					     ",")
				  'trac-wiki-search-filter-hist
				  "wiki")
      ;; completing-read-multiple of emacs 21 has strange behaviour.
      ;; so make alternative.
      (while more
	(setq more nil
	      result nil)
	(dolist (item (completing-read-multiple
		       prompt
		       alist nil nil
		       (mapconcat 'identity
				  trac-wiki-search-default-filters
				  ",")
		       'trac-wiki-search-filter-hist
		       "wiki"))
	  (let ((all (all-completions item alist)))
	    (if (null all)
		(setq more t)
	      (dolist (cand all)
		(if (not (member cand result))
		    (setq result (cons cand result)))))))
	(setq prompt "[again] Select filters: "))
      (or result '("wiki")))))

(defun trac-wiki-search (query &optional filters)
  "Search QUERY keywords on remote trac.
Keywords and filters can be specified.
FILTERS is interactively selected if not specified."
  (interactive
   (list
    ;; enter query string
    (let ((str (read-string "Query string: " nil
			    'trac-wiki-search-keyword-hist)))
      (if (or (null str) (string= str ""))
	  (error "Query string must be specified"))
      str)
    ;; select filters
    (let ((filters (trac-wiki-with-cache
		       'trac-wiki-search-filter-cache
		       trac-rpc-endpoint nil
		     (mapcar 'car
			     (trac-rpc-call 'search.getSearchFilters)))))
      (trac-wiki-input-search-filters filters))))
  ;; args are prepared,
  ;; try search request
  (let ((result (trac-rpc-call 'search.performSearch query filters))
	(ep trac-rpc-endpoint)
	(buf (get-buffer-create "*search result*")))
    ;; close last result first
    (if (get-buffer-window buf)
	(delete-window (get-buffer-window buf)))
    (if (null result)
	(message "No match.")
      (with-current-buffer buf
	(let ((buffer-read-only nil))
	  (erase-buffer)
	  (require 'hi-lock)
	  (if hi-lock-mode
	      (hi-lock-mode 0))
	  (dolist (elem result)
	    (setq elem (mapcar
			(lambda (x) (replace-regexp-in-string
				     "[[|\t\r\n]]+" " " x))
			elem))
	    ;; elem := (href title date author excerpt)
	    (require 'url-util)
	    (let ((url (nth 0 elem))
		  (title (nth 1 elem))
		  (date (nth 2 elem))
		  (author (nth 3 elem))
		  (excerpt (nth 4 elem)))
	      (if (string-match "<[a-z]+.*>" title)
		  (setq title (trac-wiki-html2text-string title)))
	      (insert title "\n"	; title
		      "  "
		      (decode-coding-string
		       (url-unhex-string (string-as-unibyte url))
		       'utf-8)
		      "\n"		; href
		      excerpt "\n"
		      (format "By %s -- %s\n"
			      author
			      (format-time-string
			       "%Y-%m-%d %H:%M:%S"
			       (seconds-to-time (string-to-number date))))
		      "\n"))))
	(goto-char (point-min))
	;; setup font-lock
	(set (make-local-variable 'font-lock-defaults)
	     '(trac-wiki-search-result-font-lock-keywords t t))
	(if font-lock-mode
	    (font-lock-default-fontify-buffer)) ; immediately
	;; highlight search keywords
	(let ((colors '(hi-yellow hi-blue hi-green hi-pink)))
	  (dolist (q (split-string (downcase query)))
	    (highlight-regexp q (or (car-safe colors)
				    'highlight))
	    (setq colors (cdr-safe colors))))
	(setq trac-rpc-endpoint ep)
	;; local keys
	(use-local-map trac-wiki-search-result-mode-map)
	(setq buffer-read-only t))
      (pop-to-buffer buf))))

(defconst trac-wiki-search-result-top-regexp
  "\\(?:\n\\|\\`\\)\\([[^|:\n]]+\\):")

(defun trac-wiki-search-result-next ()
  "Move to next entry."
  (interactive)
  (end-of-line)
  (if (re-search-forward trac-wiki-search-result-top-regexp nil t)
      (goto-char (match-beginning 1))
    (message "No more entry")))

(defun trac-wiki-search-result-prev ()
  "Move to prev entry."
  (interactive)
  (beginning-of-line)
  (if (re-search-backward trac-wiki-search-result-top-regexp nil t)
      (goto-char (match-beginning 1))
    (message "No more entry")))

(defun trac-wiki-search-result-get-page-name ()
  "Get page name of entry at point."
  (interactive)
  (save-excursion
    (end-of-line)
    (if (re-search-backward trac-wiki-search-result-top-regexp nil t)
	(match-string-no-properties 1))))

(defun trac-wiki-search-result-edit ()
  "Edit page of entry at point."
  (interactive)
  (let ((page (trac-wiki-search-result-get-page-name)))
    (cond
     ((string-match "^\\`#[1-9]+\\'" page)
      (message "Sorry, you cannot visit to ticket."))
     ((y-or-n-p (format "Visit to '%s' ? " page))
      (trac-wiki-visit trac-rpc-endpoint page t))
     (t (message "Sorry, you cannot visit to this target.")))))

(defun trac-wiki-complete-at-point (no-cache)
  "Do completion for the wiki page or wiki macro at point.
Completion candidates are collected from remote site and cached
localy.  So second completion works fast with cache if exist.
With prefix arg NO-CACHE, it means canceling current cache and update
with data retrieved from remote site again.

NOTE: Wiki macro names are retrieved from HTML content of
WikiMacros page on remote site."
  (interactive "P")
  (let ((ep (or trac-rpc-endpoint
		(error "XML-RPC endpoint is not known")))
	kind candidates part)
    (cond
     ;; macro completion
     ((trac-wiki-looking-back "\\[[\\[\\(\\w*\\)")
|(setq kind "macro"
	    part (match-string 1)
	    candidates (append (list "BR")
			       (trac-wiki-with-cache
				   'trac-wiki-macro-name-cache
				   ep no-cache
				 (trac-wiki-collect-macro-names)))))
     ((trac-wiki-looking-back "\\[wiki:\\(\\(?:\\w\\|[/.]]\\)*\\)")
      ;; wiki link
      (setq kind "page name"
	    part (match-string 1)
	    candidates (trac-wiki-with-cache
			   'trac-wiki-page-name-cache
			   ep no-cache
			 (trac-rpc-get-all-pages))))
     ((trac-wiki-looking-back "\\(^\\|\\W\\)\\([A-Z]\\(\\w\\|[/.]\\)*\\)")
      ;; camel case wiki name
      (setq kind "wiki name"
	    part (match-string 2)
	    candidates (trac-wiki-with-cache
			   'trac-wiki-page-name-cache
			   ep no-cache
			 (trac-rpc-get-all-pages)))))

    (when (and kind candidates part)
      (let* ((pos (point))
	     (beg (- pos (length part))))
	;; try completion
	(let ((cmpl (try-completion part (mapcar 'list candidates))))
	  (cond
	   ((null cmpl)
	    (message "no matching %s" kind))
	   ((eq cmpl t)
	    (message "Sole completion"))
	   (t
	    (let ((repl (if (string= cmpl part)
			    (completing-read (format "[%s] " kind)
					     (mapcar 'list candidates)
					     nil t part)
			  cmpl)))
	      (kill-region beg pos)
	      (insert repl)
	      (if (eq t (try-completion repl (mapcar 'list candidates)))
		  (message "Sole completion")
		(message "Complete, but not uniq"))))))))))

(defun trac-wiki-history (arg)
  "Show history of visiting page in popup'ed buffer.
In history buffer, you can get diff of each versions.
Maximum count of history is limited by `trac-wiki-max-history'.
If with prefix ARG, all the history is displayed but it might slow
if too many version exists."
  (interactive "P")
  (let* ((page-info trac-wiki-page-info)
	 (page (trac-wiki-page-name))
	 (current (trac-wiki-page-version))
	 (ver (trac-rpc-get-page-version))
	 (buf (get-buffer-create (format " *page history*")))
	 (ep trac-rpc-endpoint)
	 (keyhelp (substitute-command-keys "\\[trac-wiki-history]"))
	 (rest (if arg trac-wiki-max-history
		 trac-wiki-history-count))
	 info)
    (save-excursion
      (set-buffer buf)
      (setq buffer-read-only nil)
      (erase-buffer)
      (message "fetching version info...")
      (let ((trac-rpc-endpoint ep)
	    info)
	(insert "--- History of " page " ---")
	(while (and (< 0 rest) (< 0 ver))
	  (setq info (condition-case nil
			 (trac-rpc-get-page-info page ver)
		       (error nil)))
	  (when (and info (listp info))
	    (setq rest (1- rest))
	    (insert (format "\nversion:  %s" (cdr (assoc "version" info))))
	    (insert "\nmodified: "
		    (trac-wiki-convert-to-readable-time-string
		     (cdr (assoc "lastModified" info))))
	    (insert "\nauthor:   " (cdr (assoc "author" info)))
	    (if (assoc "comment" info)
		(insert "\ncomment:  " (or (cdr-safe (assoc "comment" info))
					   "")))
	    (insert "\n"))
	  (setq ver (1- ver))))
      (message "fetching version info...done")
      (if (and (null arg) (< 1 ver))
	  (insert "\n... to show more versions, use C-u " keyhelp "\n"))
      (setq buffer-read-only t)
      (re-search-backward (format "version: +%s" current) nil t)
      (trac-wiki-history-mode)
      (setq trac-rpc-endpoint ep
	    trac-wiki-page-info page-info)
      (pop-to-buffer buf)
      (shrink-window-if-larger-than-buffer))))

(define-derived-mode trac-wiki-history-mode text-mode "trac-wiki-history"
  "History operation mode"
  (setq buffer-read-only t)
  (set (make-local-variable 'font-lock-defaults)
       '(trac-wiki-history-font-lock-keywords t))
  (if font-lock-mode
      (font-lock-fontify-buffer))
  (local-set-key "?" 'trac-wiki-history-help)
  (local-set-key "q" 'trac-wiki-delete-window-or-bury-buffer)
  (local-set-key "=" 'trac-wiki-history-diff)
  (local-set-key "n" 'trac-wiki-history-next)
  (local-set-key "p" 'trac-wiki-history-prev)
  (trac-wiki-history-help))

(defun trac-wiki-history-help ()
  "Show small help in echo area."
  (interactive)
  (message "[help] =:diff, n:next, p:prev, q:quit, ?:help"))

(defun trac-wiki-history-next ()
  "Move to next entry."
  (interactive)
  (if (not (re-search-forward "version: +" nil t))
      (message "No more older version.")))

(defun trac-wiki-history-prev ()
  "Move to previous entry."
  (interactive)
  (let ((pos (point)))
    (beginning-of-line)
    (while (looking-at "\\w+:")
      (forward-line -1))
    (if (not (re-search-backward "version: +" nil t))
	(message "No more newer version.")
      (goto-char (match-end 0)))))

(defun trac-wiki-history-diff ()
  "Show change of this version as diff output in popup buffer."
  (interactive)
  (let (ver1 ver2)
    (save-excursion
      (end-of-line)
      (if (not (re-search-backward "version: +\\([0-9]+\\)" nil t))
	  (message "No version")
	(setq ver1 (string-to-number (match-string 1)))
	(end-of-line)
	(if (not (re-search-forward "version: +\\([0-9]+\\)" nil t))
	    (message "This is initial version.")
	  (setq ver2 (string-to-number (match-string 1))))))
    (when (and ver1 ver2)
      (let* ((page (trac-wiki-page-name))
	     (str1 (trac-rpc-get-page page ver1))
	     (str2 (trac-rpc-get-page page ver2)))
	(if (null str1)
	    (error "Cannot fetch version %s" ver1))
	(if (null str2)
	    (error "Cannot fetch version %s" ver2))
	(trac-wiki-diff-internal str1 str2)))))

;; utilities
;; FIXME: alter
(if (fboundp 'looking-back)
    (defalias 'trac-wiki-looking-back 'looking-back)
  ;; for Emacs 21 or before
  (defun trac-wiki-looking-back (regex)
    "Easy implementation of `looking-back' of Emacs 22."
    (let ((pos (point)))
      (and (save-excursion
	     (re-search-backward regex nil t))
	   (eq (match-end 0) pos)))))

(defun trac-wiki-delete-window-or-bury-buffer (&optional buf)
  "Close window of BUF if displayed or bury if it is solo window."
  (interactive)
  (setq buf (or buf (current-buffer)))
  (condition-case nil
      (let ((win (get-buffer-window buf)))
	(and win (window-live-p win)
	     (delete-window win)))
    (error (bury-buffer buf))))

(defun trac-wiki-convert-to-readable-time-string (datetime)
  "Return time string for time encoded as <(:datetime (1234 124))>."
  (format-time-string "%Y-%m-%d %H:%M:%S%z" (cddr datetime)))

(defun trac-wiki-collect-macro-names ()
  "Collect available macro names from WikiMacro page."
  (let ((html (trac-rpc-get-page-html "WikiMacros"))
	names)
    (with-temp-buffer
      (insert html)
      (goto-char (point-min))
      (while (re-search-forward "<code>\\[\\[\\(\\w+\\)\\]\\]</code>" nil t)
	(let ((name (match-string 1)))
	  (if (not (member name names))
	      (setq names (cons name names))))))
    (sort names 'string<)))		; return sorted names

(defun trac-wiki-pick-wiki-name-at-point ()
  "Pick wiki page name at current point.
This feature is available on camel case word, on short-hand wiki: link
or inside bracket wiki link.
Return page name string or nil if not found."
  (save-excursion
    (save-restriction
      (let ((pt (point))
	    (beg (line-beginning-position))
	    (end (line-end-position)))
	;; find
	(cond
	 ((and (save-excursion
		 (re-search-backward "wiki:\\([^] \t\n]+\\)" beg t))
	       (<= (match-beginning 0) pt)
	       (<= pt (match-end 0)))
	  (goto-char (match-beginning 0))
	  (looking-at "wiki:\\([^] \t\n]+\\)")
	  (match-string-no-properties 1))
	 ((save-excursion
	    (and (re-search-backward "\\([^]]\\|^\\)\\[["|beg t)
		 (goto-char (match-end 0))
		 (looking-at "\\(?:wiki:\\)?\\([^./]][^] \t:\n]*\\)[^]\n]*\\]")
		 (<= (match-beginning 0) pt)
		 (<= pt (match-end 0))))
	  (match-string-no-properties 1))
	 ((save-excursion
	    ;; FIXME: not enough in fragment part
	    (and (skip-chars-backward "A-Za-z/")
		 (looking-at trac-wiki-camel-case-regexp)
		 (<= (match-beginning 0) pt)
		 (<= pt (match-end 0))))
	  (match-string-no-properties 1))
	 (t nil))))))


;; Utility functions to define project.

(defun trac-wiki-define-project (name url &optional login)
  "Add project as NAME which is on URL to `trac-projects'.
URL is not XML-RPC end-point but url for usual browser access.
End-point url will be made automatically.  NAME is readable name
string for selection.  If optional argument LOGIN is specified,
XML-PRC endpoint is made with login.  LOGIN is one of nil, t or
login name string.  If LOGIN is non-nil, end-point is made to
access via login module.  If LOGIN is string, use it as deafult
user name on prompting instead of function `user-real-login-name' is used.
If nil, without login.  If project NAME is already defined,
simply replace with new definition."
  (interactive
   (list (let ((name (read-string "Project name: ")))
	   (if (and (assoc name trac-projects)
		    (not (y-or-n-p (format "'%s' is already defined.  Replace it? " name))))
	       (error "Canceled"))
	   name)
	 (read-string "Project url: " "http://")
	 (y-or-n-p "Login? ")))
  (setq trac-projects (let ((pair (assoc name trac-projects)))
			(if pair
			    (delq pair trac-projects)
			  trac-projects)))
  (if (string-match "/$" url)
      (setq url (substring url 0 (match-beginning 0)))) ; remove
  (let ((plist (list :endpoint (concat url (if login "/login") "/xmlrpc"))))
    (if login
	(plist-put plist :login login)) ; might t or username
    (add-to-list 'trac-projects (cons name plist)))
  (if (interactive-p)
      (message "Project '%s' is defined." name)))

(defun trac-wiki-define-multiple-projects (projects parent &optional login)
  "Define multiple PROJECTS which has same PARENT url.
PROJECTS is a list of project name string or cons of project name
and sub directory name under PARENT url.  If name, it is also used
as sub-directory name.  If LOGIN is specified, XML-RPC end-point
is made to access via login module.  This function uses command
`trac-wiki-define-project'."
  (let (plist)
    (if (not (string-match "/$" parent))
	(setq parent (concat parent "/")))
    (dolist (proj projects)
      (let ((name (or (and (consp proj) (stringp (car proj)) (car proj))
		      (and (stringp proj) proj)
		      (error "Invalid entry of PROJECTS: %s" proj)))
	    (subdir (or (and (consp proj) (stringp (cdr proj)) (cdr proj))
			(and (stringp proj) proj)
			(error "Invalid entry of PROJECTS: %s" proj))))
	(trac-wiki-define-project name (concat parent subdir) login)))))

;;; utility functions

(defun trac-wiki-cleanup-auth-storage (storage)
  "Clean up duplicated auth info in STORAGE destructively.
STORAGE is a list of site and auth-alist.
auth-alist is alist of realm and auth data.
Entries which has same realm are removed except 1st one
and repeat all the realms and all the sites.
For exmple:
 (trac-wiki-cleanup-auth-storage
   '((\"site1:80\" (\"r1\" . \"A\")
		   (\"r1\" . \"B\")
		   (\"r2\" . \"C\")
		   (\"r1\" . \"D\")
		   (\"r2\" . \"E\"))
     (\"site2:80\" (\"r3\" . \"A\")
		   (\"r4\" . \"B\")
		   (\"r5\" . \"C\")
		   (\"r5\" . \"D\")
		   (\"r3\" . \"E\"))))
=> '((\"site1:80\" (\"r1\" . \"A\")
		   (\"r2\" . \"C\"))
     (\"site2:80\" (\"r3\" . \"A\")
		   (\"r4\" . \"B\")
		   (\"r5\" . \"C\"))))

"
  (dolist (entry storage)
    (setq entry (cdr entry))
    (while entry
      (let ((e entry))
	(while (cdr e)
	  (if (string= (caadr e) (caar entry))
	      (setcdr e (cddr e))
	    (setq e (cdr e)))))
      (setq entry (cdr entry))))
  storage)

(defun trac-wiki-merge-storage (dst src)
  "Merge auth data of SRC into DST destructively.
 (trac-wiki-append-merge-storage
   '((\"site1\" (\"a\" . \"A\")  ; dst
              (\"a\" . \"B\")
              (\"b\" . \"C\")))
   '((\"site1\" (\"a\" . \"AA\") ; src
              (\"c\" . \"CC\")
              (\"b\" . \"BB\"))
     (\"site2\" (\"a\" . \"AAA\")
              (\"b\" . \"BBB\")
              (\"c\" . \"CCC\"))))
=> '((\"site1\" (\"a\" . \"A\")     ; remain
              (\"a\" . \"B\")     ; remain
              (\"b\" . \"C\")     ; remain
              (\"c\" . \"CC\"))   ; added
     (\"site2\" (\"a\" . \"AAA\")   ; added
              (\"b\" . \"BBB\")   ; added
              (\"c\" . \"CCC\"))) ; added
"
  (if (null dst)
      src
    (dolist (s src)
      (let ((d (assoc (car s) dst)))	; find site entry
	(if (null d)
	    ;; not exist, simply append it
	    (nconc dst (list s))
	  ;; exist append auth item
	  (dolist (a (cdr s))
	    (if (null (assoc (car a) (cdr d)))
		;; not found, append
		(nconc d (list a)))))))
    dst))

(provide 'trac-wiki)

;;; trac-wiki.el ends here
}}}