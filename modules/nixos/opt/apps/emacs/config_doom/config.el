;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Olexandr Sydorchuk"
      user-mail-address "olexandr.syd@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;

(setq doom-font (font-spec :family "Fira Code" :size 14 :weight 'semi-light)
      doom-symbol-font (font-spec :family "JuliaMono")
      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 15))

;; (setq doom-font (font-spec :family " Iosevka Comfy" :height 100 :weight 'semi-light)
;;       doom-symbol-font (font-spec :family "JuliaMono")
;;       doom-variable-pitch-font (font-spec :family "Iosevka Comfy Duo" :height 100))

;; (setq doom-font (font-spec :family "Iosevka Comfy" :height 100)
;;       doom-symbol-font (font-spec :family "JuliaMono")
;;       doom-variable-pitch-font (font-spec :family "Iosevka Comfy Duo" :height 100))

;; (setq doom-font (font-spec :family "Roboto Mono" :size 14 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "DejaVu Sans" :size 15)
;;       doom-symbol-font (font-spec :family "JuliaMono")
;;       doom-big-font (font-spec :family "Roboto Mono" :size 24))

;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(use-package! nxml-mode
  :config
  ;; Incase if no LSP, otherwise good to use lsp-headerline-breadcrumb-mode
  (defun nxml-where ()
    "Display the hierarchy of XML elements the point is on as a path."
    (interactive)
    (let ((path nil))
      (save-excursion
        (save-restriction
          (widen)
          (while (and (< (point-min) (point)) ;; Doesn't error if point is at beginning of buffer
                      (condition-case nil
                          (progn
                            (nxml-backward-up-element) ; always returns nil
                            t)
                        (error nil)))
            (setq path (cons (xmltok-start-tag-local-name) path)))
          (if (called-interactively-p t)
              (message "/%s" (mapconcat 'identity path "/"))
            (format "/%s" (mapconcat 'identity path "/")))))))
  )

(use-package! string-inflection
  :init
  (global-set-key (kbd "M-C-<right>") 'string-inflection-all-cycle)
  (global-set-key (kbd "M-C-<up>") 'string-inflection-camelcase)
  (global-set-key (kbd "M-C-<down>") 'string-inflection-lower-camelcase)
  (global-set-key (kbd "M-C-<left>") 'string-inflection-lisp))

(use-package! anzu
  :init
  (global-set-key (kbd "M-%") 'anzu-query-replace)
  (global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp))

;; I dont like highlight current line! Also not funny with pulsar
(setq global-hl-line-modes nil)

(use-package! pulsar
  :init
  (setq
   pulsar-delay 0.03
   pulsar-face 'pulsar-cyan
   pulsar-iterations 8
   pulsar-pulse t)
  :config
  (pulsar-global-mode))

(setq scroll-margin 3)

;; Navigate windows with M-<arrows>
(windmove-default-keybindings 'meta)
(setq windmove-wrap-around t)

(global-set-key [C-f4] 'next-error) ;; C-x `
(global-set-key [S-f4] 'previous-error)
(global-set-key [f12] 'toggle-truncate-lines)
(global-set-key [C-f5] 'dired-jump)

;; try to make projectile always open when in project
;;(setq projectile-switch-project-action 'treemacs)
(after! treemacs (treemacs-follow-mode))

(after! lsp-mode
  (setq lsp-enable-suggest-server-download t
        lsp-clients-typescript-prefer-use-project-ts-server t)
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)
       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))
  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)
  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (message "==> lsp-booster--advice-final-command")
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)                             ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection))  ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command))

;; TODO: Find how to fix LSP indentation
(setq-hook! 'web-mode-hook +format-with-lsp nil)
(setq-hook! 'web-mode-hook lsp-enable-indentation nil)

(use-package! direnv
 :config
 (direnv-mode))

;; Haskell
(setq lsp-haskell-formatting-provider "ormolu")


;; (use-package chatgpt-shell
;;   :bind
;;   (:prefix-map
;;    chatgpt-shell-cmd-map
;;    :prefix "C-x C"
;;    ("C" . chatgpt-shell)
;;    ("u"  . chatgpt-shell-generate-unit-test)
;;    ("e"  . chatgpt-shell-explain-code)
;;    ("i"  . chatgpt-shell-interrupt)
;;    ("p"  . chatgpt-shell-proofread-region)
;;    ("S"  . chatgpt-shell-send-region)
;;    ("d"  . chatgpt-shell-describe-code)
;;    ("r"  . chatgpt-shell-refactor-code)
;;    ("g"  . chatgpt-shell-write-git-commit)
;;    ("s"  . chatgpt-shell-send-and-review-region)
;;    ("R"  . chatgpt-shell-restore-session-from-transcript))
;;   :custom
;;   (chatgpt-shell-model-version 6)       ; gpt-4-1106-preview
;;   (chatgpt-shell-openai-key
;;    (lambda ()
;;      (nth 0 (process-lines "pass" "show" "openai-key"))))
;;   (chatgpt-shell-system-prompt 3)
;;   (chatgpt-shell-system-prompts
;;    '(("English" .
;;       "I want you to act as an English translator, spelling corrector and improver. I will speak to you in any language and you will detect the language, translate it and answer in the corrected and improved version of my text, in English. I want you to replace my simplified A0-level words and sentences with more beautiful and elegant, upper level English words and sentences. Keep the meaning same, but make them more literary. I want you to only reply the correction, the improvements and nothing else, do not write explanations.")
;;      ("Programming" .
;;       "The user is a programmer with very limited time. You treat their time as precious. You do not repeat obvious things, including their query. You are as concise as possible in responses. You never apologize for confusions because it would waste their time. You use markdown liberally to structure responses. Always show code snippets in markdown blocks with language labels. Don't explain code snippets. Whenever you output updated code for the user, only show diffs, instead of entire snippets.")
;;      ("Positive Programming" .
;;       "Your goal is to help the user become an amazing computer programmer. You are positive and encouraging. You love see them learn. You do not repeat obvious things, including their query. You are as concise in responses. You always guide the user go one level deeper and help them see patterns. You never apologize for confusions because it would waste their time. You use markdown liberally to structure responses. Always show code snippets in markdown blocks with language labels. Don't explain code snippets. Whenever you output updated code for the user, only show diffs, instead of entire snippets.")
;;      ("Travel Guide" .
;;       "I want you to act as a travel guide. I will write you my location and you will suggest a place to visit near my location. In some cases, I will also give you the type of places I will visit. You will also suggest me places of similar type that are close to my first location."))))


(use-package! gptel
  :hook
  (gptel-post-stream-hook . gptel-auto-scroll)
  (gptel-post-response-functions . gptel-end-of-response)
  :bind ("C-c ?" . gptel-menu)
  :custom
  (gptel-default-mode 'org-mode)       ; default is 'markdown-mode
  (gptel-model 'gpt-4o-mini)
  (gptel-api-key (lambda ()
                   (string-trim
                    (shell-command-to-string "pass show openai-key"))))
  :config
  (defun gptel-switch-to-claude ()
    "Testing claude sonnet."
    (interactive)
    (setq
     gptel-model "claude-3-5-sonnet-20240620"
     gptel-backend (gptel-make-anthropic "Claude"
                     :stream t :key (string-trim
                                     (shell-command-to-string "pass show anthropic-key")))))
  (gptel-make-gh-copilot "Copilot")
  )


;;    '(("English" .
;;       "I want you to act as an English translator, spelling corrector and improver. I will speak to you in any language and you will detect the language, translate it and answer in the corrected and improved version of my text, in English. I want you to replace my simplified A0-level words and sentences with more beautiful and elegant, upper level English words and sentences. Keep the meaning same, but make them more literary. I want you to only reply the correction, the improvements and nothing else, do not write explanations.")
;;      ("Programming" .
;;       "The user is a programmer with very limited time. You treat their time as precious. You do not repeat obvious things, including their query. You are as concise as possible in responses. You never apologize for confusions because it would waste their time. You use markdown liberally to structure responses. Always show code snippets in markdown blocks with language labels. Don't explain code snippets. Whenever you output updated code for the user, only show diffs, instead of entire snippets.")
;;      ("Positive Programming" .
;;       "Your goal is to help the user become an amazing computer programmer. You are positive and encouraging. You love see them learn. You do not repeat obvious things, including their query. You are as concise in responses. You always guide the user go one level deeper and help them see patterns. You never apologize for confusions because it would waste their time. You use markdown liberally to structure responses. Always show code snippets in markdown blocks with language labels. Don't explain code snippets. Whenever you output updated code for the user, only show diffs, instead of entire snippets.")
;;      ("Travel Guide" .
;;       "I want you to act as a travel guide. I will write you my location and you will suggest a place to visit near my location. In some cases, I will also give you the type of places I will visit. You will also suggest me places of similar type that are close to my first location."))))


;; Updated version available at https://github.com/karthink/gptel/wiki
(use-package gptel-rewrite
  :after gptel
  :bind (:map gptel-rewrite-actions-map
         ("C-c C-i" . gptel--rewrite-inline-diff))
  :config
  (defun gptel--rewrite-inline-diff (&optional ovs)
    "Start an inline-diff session on OVS."
    (interactive (list (gptel--rewrite-overlay-at)))
    (unless (require 'inline-diff nil t)
      (user-error "Inline diffs require the inline-diff package."))
    (when-let* ((ov-buf (overlay-buffer (or (car-safe ovs) ovs)))
                ((buffer-live-p ov-buf)))
      (with-current-buffer ov-buf
        (cl-loop for ov in (ensure-list ovs)
                 for ov-beg = (overlay-start ov)
                 for ov-end = (overlay-end ov)
                 for response = (overlay-get ov 'gptel-rewrite)
                 do (delete-overlay ov)
                 (inline-diff-words
                  ov-beg ov-end response)))))
  (when (boundp 'gptel--rewrite-dispatch-actions)
    (add-to-list
     'gptel--rewrite-dispatch-actions '(?i "inline-diff")
     'append)))


;; accept completion from copilot and fallback to company
(use-package! copilot
  ;; :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))


(use-package elysium
  :custom
  (elysium-window-size 0.33) ; The elysium buffer will be 1/3 your screen
  (elysium-window-style 'vertical)) ; Can be customized to horizontal

;; This package does not work for now
;; (use-package! evedel
;;   :after gpttel
;;   )


(use-package chatgpt-shell
  :commands (chatgpt-shell-prompt-improve-english chatgpt-shell-prompt-translate-into-russian)
  :hook (chatgpt-shell-mode . chatgpt-shell--load-chatgpt-api-key)
  :config
  (add-to-list 'chatgpt-shell-model-versions "gpt-4o-mini")
  (add-to-list 'chatgpt-shell-system-prompts '("English, m*ckr, do you speak it" .
                                              "I want you to act as an English translator, spelling corrector and improver. I will speak to you in any language and you will detect the language, translate it and answer in the corrected and improved version of my text, in English. I want you to replace my simplified A0-level words and sentences with more beautiful and elegant, upper level English words and sentences. Keep the meaning same, but make them more literary. I want you to only reply the correction, the improvements and nothing else, do not write explanations."))
  (setq chatgpt-shell-model-version
        (seq-position chatgpt-shell-model-versions "gpt-4o-mini"))

  (defun chatgpt-shell--load-chatgpt-api-key ()
    (setq chatgpt-shell-openai-key (string-trim (shell-command-to-string "pass show openai-key"))))

  (defun my-chatgpt-shell ()
    "Setup and enter ChatGPT shell with API key loaded."
    (interactive)
    (chatgpt-shell--load-chatgpt-api-key)
    (chatgpt-shell))

  (defun chatgpt-shell-prompt-improve-english ()
    "Improve English grammar."
    (interactive)
    (chatgpt-shell-send-region-with-header "Please translate into English and correct the grammar, do not write explanations:"))

  (defun chatgpt-shell-prompt-translate-into-russian ()
    "Translate into Russian"
    (interactive)
    (chatgpt-shell-send-region-with-header "Переведи на русский:"))

  (map! :leader
        :desc "Setup and Open ChatGPT Shell"
        "o c" #'my-chatgpt-shell))

;; TODO check https://github.com/11111000000/pro/blob/main/%D0%BF%D1%80%D0%BE-%D0%B8%D0%B8.el
;; TODO check https://github.com/milanglacier/minuet-ai.el/blob/main/recipes.md

;; Python
(add-hook! '(python-mode-hook)
  (lambda ()
    (highlight-lines-matching-regexp "import ipdb")
    (highlight-lines-matching-regexp "ipdb.set_trace()")
    (highlight-lines-matching-regexp "import wdb")
    (highlight-lines-matching-regexp "wdb.set_trace()")))


;; (use-package! org
;;   :hook
;;   (org-mode . (lambda ()
;;                 (variable-pitch-mode +1)
;;                 (org-modern-mode +1))))

;; Org
(use-package! org
  :hook
  (org-mode . (lambda ()
                (when (display-graphic-p)
                  (variable-pitch-mode +1)
                  (org-modern-mode +1))))
  :config

  (setq org-hide-emphasis-markers t)

  (defface org-level-1-stars '((t :inherit (fixed-pitch org-level-1)))
    "Face used for level 1 headline stars."
    :group 'org-faces)
  (defface org-level-2-stars '((t :inherit (fixed-pitch org-level-2)))
    "Face used for level 2 headline stars."
    :group 'org-faces)
  (defface org-level-3-stars '((t :inherit (fixed-pitch org-level-3)))
    "Face used for level 3 headline stars."
    :group 'org-faces)
  (defface org-level-4-stars '((t :inherit (fixed-pitch org-level-4)))
    "Face used for level 4 headline stars."
    :group 'org-faces)
  (defface org-level-5-stars '((t :inherit (fixed-pitch org-level-5)))
    "Face used for level 5 headline stars."
    :group 'org-faces)
  (defface org-level-6-stars '((t :inherit (fixed-pitch org-level-6)))
    "Face used for level 6 headline stars."
    :group 'org-faces)
  (defface org-level-7-stars '((t :inherit (fixed-pitch org-level-7)))
    "Face used for level 7 headline stars."
    :group 'org-faces)
  (defface org-level-8-stars '((t :inherit (fixed-pitch org-level-8)))
    "Face used for level 8 headline stars."
    :group 'org-faces)
  (defconst org-level-stars-faces
    '(org-level-1-stars org-level-2-stars org-level-3-stars org-level-4-stars
      org-level-5-stars org-level-6-stars org-level-7-stars
      org-level-8-stars))
  (defun my-org-get-level-face (n)
    "Get the right face for match N in font-lock matching of headlines.
Modifies the orginal to assign separate faces org-level-*-stars to the
stars and the space after."
    (let* ((org-l0 (- (match-end 2) (match-beginning 1) 1))
           (org-l (if org-odd-levels-only (1+ (/ org-l0 2)) org-l0))
           (org-i (if org-cycle-level-faces
                      (% (1- org-l) org-n-level-faces)
                    (1- (min org-l org-n-level-faces))))
           (org-f (nth org-i org-level-faces))
           (org-s (nth org-i org-level-stars-faces)))
      (cond
       ((eq n 1) (if org-hide-leading-stars 'org-hide org-s))
       ((eq n 2) org-s)
       (t (unless org-level-color-stars-only org-f)))))

  (advice-add 'org-get-level-face :override #'my-org-get-level-face)

  (add-hook 'org-mode-hook
            (lambda ()
              (set-face-attribute 'org-hide nil :inherit 'fixed-pitch)))

  (add-hook! 'org-mode-hook #'doom-disable-line-numbers-h)

  (custom-set-faces
   '(org-block ((t (:inherit fixed-pitch))))
   '(org-code ((t (:inherit fixed-pitch))))
   '(org-table ((t (:inherit fixed-pitch))))
   '(org-verbatim ((t (:inherit fixed-pitch))))
   '(org-special-keyword ((t (:inherit fixed-pitch))))
   )
  )

;; Markdown
(use-package! markdown-mode
  :custom
  (markdown-header-scaling t)
  (markdown-hide-urls t)
  (markdown-hide-markup t))

;; Sometimes I want to start fresh emacs without changing config and run test nix rebuild
(when (file-readable-p "~/.config/doom/config-local.el")
  (load "~/.config/doom/config-local.el"))


;; Lets make "<>" not so visible and ugly
(use-package! paren-face
  :config
  (setq-hook! 'web-mode-hook paren-face-regexp "[<>]")
  (setq-hook! 'web-mode-hook paren-face-mode t)
  (setq-hook! 'nxml-mode-hook paren-face-regexp "[<>]")
  (setq-hook! 'nxml-mode-hook paren-face-mode t))

(use-package! smartparens
  :config
  ;; Keybindings for wrapping with different brackets
  (map! :map smartparens-mode-map
        "C-c (" #'sp-wrap-round
        "C-c {" #'sp-wrap-curly
        "C-c [" #'sp-wrap-square))

;; SQL

(defun sql-format-region-mysql (beg end)
  "Beautify SQL in region between beg and END.
Dependency:
npm i -g sql-formatter-cli"
  (interactive "r")
  (save-excursion
    (shell-command-on-region beg end "sql-formatter -l mysql" nil t)))
(defun sql-format-buffer-mysql ()
  "Beautify SQL in buffer."
  (interactive)
  (sql-format-region-mysql (point-min) (point-max)))

(defun sql-format-region-sqlite (beg end)
  (interactive "r")
  (save-excursion
    (shell-command-on-region beg end "sql-formatter -l sqlite" nil t)))
(defun sql-format-buffer-sqlite ()
  (interactive)
  (sql-format-region-sqlite (point-min) (point-max)))


;; Inspired https://github.com/ronisbr/doom.d/blob/main/settings/setup-svg-tag-mode.el
(use-package! svg-tag-mode
  :init
  ;; Definition of regexps to apply the SVG tags.
  (defconst date-regexp             "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
  (defconst day-regexp              "[A-Za-z]\\{3\\}")
  (defconst time-regexp             "[0-9]\\{2\\}:[0-9]\\{2\\}")
  (defconst day-time-regexp         (format "\\(%s\\)? ?\\(%s\\)?" day-regexp time-regexp))
  (defconst name-regexp             "\\(?:[[:word:]]\\|_\\)+")
  (defconst name-with-spaces-regexp "\\(?:[[:word:]]\\|[[:blank:]]\\)+")
  (defconst field-regexp            "\\(?:[[:word:]]\\|[[:blank:]]\\)+")

  (defun svg-progress-percent (value)
    (svg-image (svg-lib-concat
                (svg-lib-progress-bar (/ (string-to-number value) 100.0)
                                      nil :height: 12 :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                (svg-lib-tag (concat value "%")
                             nil :stroke 0 :margin 0)) :ascent 'center))

  (defun svg-progress-count (value)
    (let* ((seq (mapcar #'string-to-number (split-string value "/")))
           (count (float (car seq)))
           (total (float (cadr seq))))
      (svg-image (svg-lib-concat
                  (svg-lib-progress-bar (/ count total) nil
                                        :height: 12 :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                  (svg-lib-tag value nil
                               :stroke 0 :margin 0)) :ascent 'center)))

  (add-hook! 'org-mode-hook
            (defun my/set-svg-tag-tags-for-org-mode ()
              "Set the SVG tags for `org-mode'."
              (when (display-graphic-p)
                (setq-local
                 svg-tag-tags
                 ;; === TODO keywords =======================================================

                 `(("^[\\*]* \\(TODO\\)" . ((lambda (tag) (svg-tag-make "TODO" :face 'org-todo :inverse t :margin 0))))
                   ("^[\\*]* \\(WAIT\\)" . ((lambda (tag) (svg-tag-make "WAIT" :face '+org-todo-onhold :inverse t :margin 0))))
                   ("^[\\*]* \\(STRT\\)" . ((lambda (tag) (svg-tag-make "STRT" :face '+org-todo-active :margin 0))))
                   ("^[\\*]* \\(DLGT\\)" . ((lambda (tag) (svg-tag-make "DLGT" :face '+org-todo-active :inverse t :margin 0))))
                   ("^[\\*]* \\(CANC\\)" . ((lambda (tag) (svg-tag-make "CANC" :face 'org-done :margin 0))))
                   ("^[\\*]* \\(DONE\\)" . ((lambda (tag) (svg-tag-make "DONE" :face 'org-done :margin 0))))

                   ;; ;; tags
                   ;; (":\\([A-Za-z0-9]+\\)" . ((lambda (tag) (svg-tag-make tag))))
                   ;; (":\\([A-Za-z0-9]+[ \-]\\)" . ((lambda (tag) tag)))

                   ;; ;; Progress [40%] or [1/2]
                   ;; ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
                   ;;                                     (svg-progress-percent (substring tag 1 -2)))))
                   ;; ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
                   ;;                                   (svg-progress-count (substring tag 1 -1)))))

                   ;; === Fields ============================================================

                   (,(format "\\(## %s ##\\)" field-regexp) .
                    ((lambda (tag) (svg-tag-make tag :beg 3 :end -3 :inverse t :face 'doom-themes-org-hash-tag :margin 0))))

                   ;; === Name reference ====================================================

                   (,(format "\\(@%s\\)" name-regexp) .
                    ((lambda (tag) (svg-lib-tag (substring (replace-regexp-in-string "_" " " tag) 1 nil)
                                                nil
                                                :background ,(doom-color 'bg)
                                                :foreground ,(doom-color 'nano-salient)
                                                :font-weight 'bold
                                                :margin 0
                                                :stroke 2))))

                   (,(format "\\(@(%s)\\)" name-with-spaces-regexp) .
                    ((lambda (tag) (svg-lib-tag (substring (replace-regexp-in-string "_" " " tag) 2 -1)
                                                nil
                                                :background ,(doom-color 'bg)
                                                :foreground ,(doom-color 'nano-salient)
                                                :font-weight 'bold
                                                :margin 0
                                                :stroke 2))))

                   ;; === Timestamps ========================================================

                   (,(format "^[\\*]* \\(%s\\)" time-regexp) .
                    ((lambda (tag) (svg-tag-make tag
                                                 :margin 0))))

                   (,(format "^[\\*]* \\(%s \\)-- %s" time-regexp time-regexp) .
                    ((lambda (tag) (svg-tag-make tag
                                                 :crop-right t
                                                 :margin 0))))

                   (,(format "^[\\*]* %s \\(-- %s\\)" time-regexp time-regexp) .
                    ((lambda (tag) (svg-tag-make tag :beg 3 :crop-left t :inverse t :margin 0))))

                   (,(format "\\(<%s>\\)" date-regexp) .
                    ((lambda (tag) (svg-tag-make tag :beg 1 :end -1 :margin 0))))

                   (,(format "\\(<%s \\)%s>" date-regexp day-time-regexp) .
                    ((lambda (tag) (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0))))

                   (,(format "<%s \\(%s>\\)" date-regexp day-time-regexp) .
                    ((lambda (tag) (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0))))

                   (,(format "\\(\\[%s\\]\\)" date-regexp) .
                    ((lambda (tag) (svg-tag-make tag :beg 1 :end -1 :margin 0 :face 'org-date))))

                   (,(format "\\(\\[%s \\)%s\\]" date-regexp day-time-regexp) .
                    ((lambda (tag) (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0 :face 'org-date))))

                   (,(format "\\[%s \\(%s\\]\\)" date-regexp day-time-regexp) .
                    ((lambda (tag) (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0 :face 'org-date))))))

                ;; (svg-tag-mode 1)
                )))

  (add-hook! 'magit-log-mode-hook
    (defun my-magit-init-svg ()
      "Set the SVG tags for `org-mode'."
      (when (display-graphic-p)
        (setq-local svg-tag-tags
            '(("\\([A-Za-z]\\{2,\\}-[0-9]+\\)" .
               ((lambda (tag) (svg-tag-make tag :face 'org-tag :inverse t))))

              ("\\(\\[[A-Za-z0-9[:space:]-]+\\]\\)" . ((lambda (tag) (svg-tag-make tag :face 'org-tag :inverse t :beg 1 :end -1))))

              ;; foo(bar)
              ;; ^^^
              ("\\([A-Za-z]+\\)\(.+\)" . ((lambda (tag) (svg-tag-make tag :face 'org-priority :inverse t :crop-right t))))
              ;; foo(bar)
              ;;     ^^^
              ("[A-Za-z]+\\(\(.+?\)\\)" . ((lambda (tag) (svg-tag-make tag :beg 1 :end -1 :face 'org-priority :crop-left t))))))
      (svg-tag-mode t))))

  :config
  ;; (plist-put svg-lib-style-default :font-family "Iosevka Comfy")
  ;; (plist-put svg-lib-style-default :font-size 14)
  )


(use-package! form-feed
  :hook (doom-first-file . global-form-feed-mode))


;;; ~/.doom.d/config.el

(after! org-roam
  ;;––– detect “project” files (buffers containing any TODO entry) ––––––––––––––––
  (defun vulpea-project-p ()
    "Return non-nil if current buffer has any TODO entry (ignores done)."
    (seq-find
     (lambda (type) (eq type 'todo))
     (org-element-map
         (org-element-parse-buffer 'headline)
         'headline
       (lambda (h) (org-element-property :todo-type h)))))

  ;;––– update the “project” tag on save/open –––––––––––––––––––––––––––––––––
  (defun vulpea-project-update-tag ()
    "Add or remove the PROJECT tag on this buffer, based on `vulpea-project-p`."
    (when (and buffer-file-name
               (string-prefix-p (file-truename org-roam-directory)
                                (file-truename (file-name-directory buffer-file-name)))
               (not (active-minibuffer-window)))
      (save-excursion
        (goto-char (point-min))
        (let* ((tags     (vulpea-buffer-tags-get))
               (orig     tags))
          (if (vulpea-project-p)
              (cl-pushnew "project" tags :test #'string=)
            (setq tags (remove "project" tags)))
          (unless (equal tags orig)
            (apply #'vulpea-buffer-tags-set tags))))))

  (add-hook 'find-file-hook   #'vulpea-project-update-tag)
  (add-hook 'before-save-hook #'vulpea-project-update-tag)

  ;;––– build the agenda list from all files tagged “project” ––––––––––––––––
  (defun vulpea-project-files ()
    "Return all org-roam files that have a “project” tag."
    (seq-uniq
     (seq-map #'car
              (org-roam-db-query
               [:select [nodes:file]
                :from tags
                :left-join nodes :on (= tags:node-id nodes:id)
                :where (like tags:tag (quote "%\"project\"%"))]))))

  (defun vulpea-agenda-files-update (&rest _)
    "Refresh `org-agenda-files' to the list of project files."
    (setq org-agenda-files (vulpea-project-files)))

  (advice-add 'org-agenda    :before #'vulpea-agenda-files-update)
  (advice-add 'org-todo-list :before #'vulpea-agenda-files-update))

;;;; Fontaine (font configurations)
;; Read the manual: <https://protesilaos.com/emacs/fontaine>

(use-package! fontaine
  :hook
  (doom-after-init-hook . fontaine-mode)
  :config
  ;; This is defined in Emacs C code: it belongs to font settings.
  (setq x-underline-at-descent-line nil)
  ;; And this is for Emacs28.
  (setq-default text-scale-remap-header-line t)
  (setq fontaine-latest-state-file (locate-user-emacs-file "fontaine-latest-state.eld"))

  (setq fontaine-presets
        '((small
           :default-family "Iosevka Comfy Wide Motion"
           :default-height 80
           :fixed-pitch-family "Iosevka Comfy Wide Motion"
           :variable-pitch-family "Iosevka Comfy Wide Duo")
          (regular) ; like this it uses all the fallback values and is named `regular'
          (medium
           :default-weight semilight
           :default-height 115
           :bold-weight extrabold)
          (large
           :inherit medium
           :default-height 150)
          (live-stream
           :default-family "Iosevka Comfy Wide Motion"
           :default-height 150
           :default-weight medium
           :fixed-pitch-family "Iosevka Comfy Wide Motion"
           :variable-pitch-family "Iosevka Comfy Wide Duo"
           :bold-weight extrabold)
          (presentation
           :default-height 180)
          (jumbo
           :default-height 260)
          (t
           ;; I keep all properties for didactic purposes, but most can be
           ;; omitted.  See the fontaine manual for the technicalities:
           ;; <https://protesilaos.com/emacs/fontaine>.
           :default-family "Iosevka Comfy"
           :default-weight regular
           :default-slant normal
           :default-width normal
           :default-height 100

           :fixed-pitch-family "Iosevka Comfy"
           :fixed-pitch-weight nil
           :fixed-pitch-slant nil
           :fixed-pitch-width nil
           :fixed-pitch-height 1.0

           :fixed-pitch-serif-family nil
           :fixed-pitch-serif-weight nil
           :fixed-pitch-serif-slant nil
           :fixed-pitch-serif-width nil
           :fixed-pitch-serif-height 1.0

           :variable-pitch-family "Iosevka Comfy Motion Duo"
           :variable-pitch-weight nil
           :variable-pitch-slant nil
           :variable-pitch-width nil
           :variable-pitch-height 1.0

           :mode-line-active-family nil
           :mode-line-active-weight nil
           :mode-line-active-slant nil
           :mode-line-active-width nil
           :mode-line-active-height 1.0

           :mode-line-inactive-family nil
           :mode-line-inactive-weight nil
           :mode-line-inactive-slant nil
           :mode-line-inactive-width nil
           :mode-line-inactive-height 1.0

           :header-line-family nil
           :header-line-weight nil
           :header-line-slant nil
           :header-line-width nil
           :header-line-height 1.0

           :line-number-family nil
           :line-number-weight nil
           :line-number-slant nil
           :line-number-width nil
           :line-number-height 1.0

           :tab-bar-family nil
           :tab-bar-weight nil
           :tab-bar-slant nil
           :tab-bar-width nil
           :tab-bar-height 1.0

           :tab-line-family nil
           :tab-line-weight nil
           :tab-line-slant nil
           :tab-line-width nil
           :tab-line-height 1.0

           :bold-family nil
           :bold-slant nil
           :bold-weight bold
           :bold-width nil
           :bold-height 1.0

           :italic-family nil
           :italic-weight nil
           :italic-slant italic
           :italic-width nil
           :italic-height 1.0

           :line-spacing nil)))


  (with-eval-after-load 'pulsar
    (add-hook 'fontaine-set-preset-hook #'pulsar-pulse-line))

  ;; Safely apply font after frame is created (graphical only)
  (defun my/apply-fontaine-if-graphic (frame)
    (when (display-graphic-p frame)
      (with-selected-frame frame
        (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular)))))

  ;; Apply font settings when a new graphical frame is created
  (add-hook 'after-make-frame-functions #'my/apply-fontaine-if-graphic)

  ;; Also apply if current frame is already graphical (e.g. not in daemon mode)
  (when (display-graphic-p)
    (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))))


;; (use-package! fontaine
;;   :if (display-graphic-p)
;;   :hook

;;   ;; ;; Persist the latest font preset when closing/starting Emacs and
;;   ;; ;; while switching between themes.
;;   (doom-after-init-hook . (lambda ()
;;                             (fontaine-mode)
;;                             ;; Set last preset or fall back to desired style from `fontaine-presets'.
;;                             (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))))
;;   :config
;;   ;; This is defined in Emacs C code: it belongs to font settings.
;;   (setq x-underline-at-descent-line nil)

;;   ;; And this is for Emacs28.
;;   (setq-default text-scale-remap-header-line t)

;;   ;; This is the default value.  Just including it here for
;;   ;; completeness.
;;   (setq fontaine-latest-state-file (locate-user-emacs-file "fontaine-latest-state.eld"))

;;   (setq fontaine-presets
;;         '((small
;;            :default-family "Iosevka Comfy Wide Motion"
;;            :default-height 80
;;            :fixed-pitch-family "Iosevka Comfy Wide Motion"
;;            :variable-pitch-family "Iosevka Comfy Wide Duo")
;;           (regular) ; like this it uses all the fallback values and is named `regular'
;;           (medium
;;            :default-weight semilight
;;            :default-height 115
;;            :bold-weight extrabold)
;;           (large
;;            :inherit medium
;;            :default-height 150)
;;           (live-stream
;;            :default-family "Iosevka Comfy Wide Motion"
;;            :default-height 150
;;            :default-weight medium
;;            :fixed-pitch-family "Iosevka Comfy Wide Motion"
;;            :variable-pitch-family "Iosevka Comfy Wide Duo"
;;            :bold-weight extrabold)
;;           (presentation
;;            :default-height 180)
;;           (jumbo
;;            :default-height 260)
;;           (t
;;            ;; I keep all properties for didactic purposes, but most can be
;;            ;; omitted.  See the fontaine manual for the technicalities:
;;            ;; <https://protesilaos.com/emacs/fontaine>.
;;            :default-family "Iosevka Comfy"
;;            :default-weight regular
;;            :default-slant normal
;;            :default-width normal
;;            :default-height 100

;;            :fixed-pitch-family "Iosevka Comfy"
;;            :fixed-pitch-weight nil
;;            :fixed-pitch-slant nil
;;            :fixed-pitch-width nil
;;            :fixed-pitch-height 1.0

;;            :fixed-pitch-serif-family nil
;;            :fixed-pitch-serif-weight nil
;;            :fixed-pitch-serif-slant nil
;;            :fixed-pitch-serif-width nil
;;            :fixed-pitch-serif-height 1.0

;;            :variable-pitch-family "Iosevka Comfy Motion Duo"
;;            :variable-pitch-weight nil
;;            :variable-pitch-slant nil
;;            :variable-pitch-width nil
;;            :variable-pitch-height 1.0

;;            :mode-line-active-family nil
;;            :mode-line-active-weight nil
;;            :mode-line-active-slant nil
;;            :mode-line-active-width nil
;;            :mode-line-active-height 1.0

;;            :mode-line-inactive-family nil
;;            :mode-line-inactive-weight nil
;;            :mode-line-inactive-slant nil
;;            :mode-line-inactive-width nil
;;            :mode-line-inactive-height 1.0

;;            :header-line-family nil
;;            :header-line-weight nil
;;            :header-line-slant nil
;;            :header-line-width nil
;;            :header-line-height 1.0

;;            :line-number-family nil
;;            :line-number-weight nil
;;            :line-number-slant nil
;;            :line-number-width nil
;;            :line-number-height 1.0

;;            :tab-bar-family nil
;;            :tab-bar-weight nil
;;            :tab-bar-slant nil
;;            :tab-bar-width nil
;;            :tab-bar-height 1.0

;;            :tab-line-family nil
;;            :tab-line-weight nil
;;            :tab-line-slant nil
;;            :tab-line-width nil
;;            :tab-line-height 1.0

;;            :bold-family nil
;;            :bold-slant nil
;;            :bold-weight bold
;;            :bold-width nil
;;            :bold-height 1.0

;;            :italic-family nil
;;            :italic-weight nil
;;            :italic-slant italic
;;            :italic-width nil
;;            :italic-height 1.0

;;            :line-spacing nil)))

;;     (with-eval-after-load 'pulsar
;;         (add-hook 'fontaine-set-preset-hook #'pulsar-pulse-line))
;;   )

(use-package show-font
  :ensure t
  :commands (show-font-select-preview show-font-list)

(use-package! sticky-scroll-mode)

;; Demap configuration for minimap
(use-package! demap
  :config
  (setq demap-minimap-window-width 15)
  (let ((gray1 "#1A1C22")
        (gray2 "#21242b")
        (gray3 "#282c34")
        (gray4 "#2b3038"))
    (face-spec-set 'demap-minimap-font-face
                   `((t :background ,gray2
                      :inherit    unspecified
                      :family     "minimap"
                      :height     10)))
    (face-spec-set 'demap-visible-region-face
                   `((t :background ,gray4
                      :inherit    unspecified)))
    (face-spec-set 'demap-visible-region-inactive-face
                   `((t :background ,gray3
                      :inherit    unspecified)))
    (face-spec-set 'demap-current-line-face
                   `((t :background ,gray1
                      :inherit    unspecified)))
    (face-spec-set 'demap-current-line-inactive-face
                   `((t :background ,gray1
                      :inherit    unspecified)))))
