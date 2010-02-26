{{{
# patch "trac-wiki.el"
============================================================
--- trac-wiki.el
+++ trac-wiki.el
@@ -967,12 +967,12 @@ named as \"dir@host\".  It will be kept 
 	       (url (url-generic-parse-url
 		     (trac-wiki-strip-url-trailer
 		      rawurl '("xmlrpc" "login" "wiki"))))
-	       (login  (or (elt url 1)
-			   (string-match "/login\\(?:/\\|$\\)" rawurl)
-			   (y-or-n-p "Login? ")))
-	       (host (elt url 3))
+	       (login (or (url-user url)
+                          (string-match "/login\\(?:/\\|$\\)" rawurl)
+                          (y-or-n-p "Login? ")))
+	       (host (url-host url))
 	       (name (file-name-nondirectory
-		      (directory-file-name (elt url 5))))
+		      (directory-file-name (url-filename url))))
 	       (project-name (format "%s@%s" name host))
 	       info)
 	  ;; build project info property list
@@ -980,10 +980,10 @@ named as \"dir@host\".  It will be kept 
 	      ;; make return value
 	      (setq info (list :name project-name
 			       :endpoint (format "%s://%s:%s%s%s/xmlrpc"
-						 (elt url 0)
-						 (elt url 3)
-						 (elt url 4)
-						 (directory-file-name (elt url 5))
+						 (url-type url)
+						 (url-host url)
+						 (url-portspec url)
+						 (directory-file-name (url-filename url))
 						 (if (string= login "")
 						     ""
 						   "/login"))
@@ -1120,7 +1120,7 @@ visit to PAGE without interaction."
 			    rver)))
 	      (trac-wiki-merge))
 	     (t
-	      (message "Continue editing current veresion %s (latest version is %s)."
+	      (message "Continue editing current vresion %s (latest version is %s)."
 		       ver rver)))))
       ;; newly visit page
       (switch-to-buffer (generate-new-buffer (format "%s" page)))
@@ -1141,7 +1141,7 @@ If VERSION is nil, most recent version w
 	  (setq trac-wiki-page-info `(("version" . 0)
 				      ("name" . ,page)
 				      ("lastModified" .
-				       ,(format-time-string "%Y%m%dT%T"))
+				       (:datetime ,(butlast (current-time))))
 				      ("hash" . ,(md5 ""))))
 	  (message "new page"))
       (insert (trac-rpc-get-page page))
@@ -1919,28 +1919,10 @@ if too many version exists."
 	     (delete-window win)))
     (error (bury-buffer buf))))
 
-(defun trac-wiki-convert-to-readable-time-string (str)
-  "Parse STR as ISO format time and return encoded time value."
-  (if (not (string-match (concat "\\`"
-				 "\\([0-9][0-9][0-9][0-9]\\)"
-				 "\\([0-9][0-9]\\)"
-				 "\\([0-9][0-9]\\)"
-				 "T"
-				 "\\([0-9][0-9]?\\)"
-				 ":"
-				 "\\([0-9][0-9]?\\)"
-				 ":"
-				 "\\([0-9][0-9]?\\)"
-				 "\\([-+][0-9][0-9][0-9][0-9]\\)?"
-				 "\\'")
-			 str))
-      (error "Invalid time format: %s" str)
-    (apply 'format "%s-%s-%s %s:%s:%s%s"
-	   (append (mapcar (lambda (n)
-			     (or (match-string n str) ""))
-			   '(1 2 3 4 5 6 7))))))
+(defun trac-wiki-convert-to-readable-time-string (datetime)
+  "Return time string for time encoded as <(:datetime (1234 124))>."
+  (format-time-string "%Y-%m-%d %H:%M:%S%z" (cddr datetime)))
 
-
 (defun trac-wiki-collect-macro-names ()
   "Collect available macro names from WikiMacro page."
   (let ((html (trac-rpc-get-page-html "WikiMacros"))
}}}