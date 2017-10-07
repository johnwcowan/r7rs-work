= Editing Trac Pages in Emacs =

The trac-wiki.el package allows one to view and edit wiki pages in
Trac directly from Emacs.  I've gotten it working with GNU Emacs
23.1.1, but I had to patch it first.  Here's what I did:

 1. I installed [[http://www.emacswiki.org/emacs/xml-rpc.el|xml-rpc.el version 1.6.8]].

 2. I downloaded [[http://trac-hacks.org/wiki/EmacsWikiEditScript|trac-wiki.el 1.7]].

 3. I made some changes to trac-wiki.el to make it compatibile with
 this version of xml-rpc.el: TracWikiElPatch.  The resulting file,
 which you can install directly, is TracWikiElWithPatch.

Once you've installed xml-rpc.el and the patched trac-wiki.el in your load-path,
you are ready.  To use trac-wiki.el, load it, then do:

{{{
  M-x trac-wiki RET
  RET
  http://trac.sacrideo.us/wg RET
  y
  <username> RET
  <password> RET
  WikiStart RET
}}}

This will take you to the Trac wiki's home page.  Use C-h m for
details about more commands you can use to get info about pages,
including diffs, and to edit them directly in Emacs.

I'm using Emacs 23.2 on OS X.  I haven't tested this with other
versions of Emacs.