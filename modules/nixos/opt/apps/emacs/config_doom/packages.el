;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)

(package! string-inflection)
(package! pulsar)
(package! direnv)
;; - `p' Visit previous historic version
;; - `n' Visit next historic version
;; - `w' Copy the abbreviated hash of the current historic version
;; - `W' Copy the full hash of the current historic version
;; - `g' Goto nth revision
;; - `t' Goto revision by selected commit message
;; - `q' Exit the time machine.
;; - `b' Run `magit-blame` on the currently visited revision (if magit available).
;; - `c' Show current commit using magit (if magit available).
(package! git-timemachine)

;; Show git info in Emacs dired
(package! dired-git-info)

;; In Magit status buffer:
;; `j T' Jump to the to-do list. If the section is empty (e.g. when using manual updates), it will scan for items.
;; With point in to-do list:
;; `b' Show branch (git diff) to-do list.
;; `B' Set commit reference used in branch to-do list.
;; `j T' When configured for manual updates, manually update the to-do list.
;; `j l' Open dedicated to-do list buffer.
;; `RET' Show item at point, or open dedicated buffer if point is on top heading.
;; `SPC' Peek at the item at point.
(package! magit-todos)



(package! gptel)

;; ;; This package does not work for now
;; (package! evedel
;;   :recipe (:host github :repo "daedsidog/evedel"))

(package! chatgpt-shell
  :recipe (:host github :repo "xenodium/chatgpt-shell"))

;; AIt
(package! elysium)

(package! minuet)

(package! consult-ghq)

(package! paren-face)

(package! svg-tag-mode)

(package! doom-nano-modeline
  :recipe (:host github :repo "ronisbr/doom-nano-modeline"))

(package! paren-face)

(package! org-modern
  :recipe (:host github :repo "minad/org-modern"))

(package! show-mode
  :recipe (:host github :repo "protesilaos/show-font"))

(package! fontaine
  :recipe (:host github :repo "protesilaos/show-font"))

;; Display ^L glyphs as horizontal lines
(package! form-feed)

;; TODO: https://abode.karthinks.com/org-latex-preview/

;; TODO: ichernyshovvv/org-timeblock

;; TODO: https://github.com/chenyanming/calibredb.el and check https://flibusta.site/opds

;; TODO: https://github.com/yuchen-lea/org-media-note

(package! copilot
  :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el")))

(package! sticky-scroll-mode
  :recipe (:host github :repo "/jclasley/sticky-scroll-mode"))
