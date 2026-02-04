;;; keybindings-export.el --- Export Emacs keybindings to JavaScript -*- lexical-binding: t; -*-

;; Author: Chii~
;; Description: Export Emacs keybindings to JS file for HTML visualization

;;; Commentary:

;; This module extracts keybindings from Emacs keymaps and exports them
;; to a JavaScript file that can be used with an HTML visualization.
;; Categories are based on Emacs modes (keymaps).

;;; Code:

(require 'cl-lib)

(defgroup keybindings-export nil
  "Export Emacs keybindings to JavaScript."
  :group 'tools)

(defcustom keybindings-export-output-dir
  (expand-file-name "~/.config/emacs/.local/cache/my-keybindings/")
  "Directory for exported keybindings files."
  :type 'directory
  :group 'keybindings-export)

(defcustom keybindings-export-template-file
  (expand-file-name "~/.config/doom/keybindings-template.html")
  "Path to HTML template file."
  :type 'file
  :group 'keybindings-export)

(defcustom keybindings-export-colors
  '("#ff6c6b" "#da8548" "#ecbe7b" "#98be65" "#46d9ff"
    "#51afef" "#a9a1e1" "#c678dd" "#ff6ac1" "#b5bd68"
    "#be5046" "#9f7efe" "#6c71c4" "#3071f7" "#f0c674")
  "Colors for categories (Doom One theme palette)."
  :type '(repeat string)
  :group 'keybindings-export)

(defcustom keybindings-export-keymaps
  '((doom-leader-map . "Leader (SPC)")
    (doom-leader-project-map . "Project (SPC p)")
    (doom-leader-buffer-map . "Buffer (SPC b)")
    (doom-leader-file-map . "File (SPC f)")
    (doom-leader-code-map . "Code (SPC c)")
    (doom-leader-git-map . "Git (SPC g)")
    (doom-leader-open-map . "Open (SPC o)")
    (doom-leader-toggle-map . "Toggle (SPC t)")
    (doom-leader-search-map . "Search (SPC s)")
    (doom-leader-notes-map . "Notes (SPC n)")
    (doom-leader-insert-map . "Insert (SPC i)")
    (global-map . "Global")
    (evil-normal-state-map . "Evil Normal")
    (evil-insert-state-map . "Evil Insert")
    (evil-visual-state-map . "Evil Visual")
    (evil-motion-state-map . "Evil Motion")
    (org-mode-map . "Org Mode")
    (magit-mode-map . "Magit")
    (dired-mode-map . "Dired")
    (prog-mode-map . "Programming")
    (copilot-completion-map . "Copilot")
    (smartparens-mode-map . "Smartparens")
    (gptel-mode-map . "GPTel"))
  "Alist of (KEYMAP-SYMBOL . DISPLAY-NAME) to export."
  :type '(alist :key-type symbol :value-type string)
  :group 'keybindings-export)

(defcustom keybindings-export-ignore-commands
  '(self-insert-command
    undefined
    ignore
    digit-argument
    negative-argument
    universal-argument
    universal-argument-more
    universal-argument-other-key
    keyboard-escape-quit
    keyboard-quit
    abort-recursive-edit
    minibuffer-keyboard-quit)
  "Commands to ignore when exporting."
  :type '(repeat symbol)
  :group 'keybindings-export)

(defcustom keybindings-export-ignore-prefixes
  '("menu-bar" "tool-bar" "mode-line" "header-line"
    "vertical-line" "horizontal-scroll-bar" "vertical-scroll-bar"
    "mouse-" "down-mouse-" "drag-mouse-" "double-mouse-" "triple-mouse-"
    "wheel-" "pinch" "touch")
  "Key prefixes to ignore."
  :type '(repeat string)
  :group 'keybindings-export)

;;; Helper Functions

(defun keybindings-export--key-to-string (key)
  "Convert KEY vector to human-readable string."
  (key-description key))

(defun keybindings-export--should-ignore-key (key-str)
  "Return non-nil if KEY-STR should be ignored."
  (cl-some (lambda (prefix)
             (string-prefix-p prefix key-str))
           keybindings-export-ignore-prefixes))

(defun keybindings-export--should-ignore-command (cmd)
  "Return non-nil if CMD should be ignored."
  (or (not (symbolp cmd))
      (memq cmd keybindings-export-ignore-commands)
      (null cmd)))

(defun keybindings-export--parse-emacs-key (key-str)
  "Parse Emacs key notation KEY-STR into list of individual keys.
For example: 'C-x C-f' -> '(\"Ctrl\" \"x\" \"Ctrl\" \"f\")'
             'M-x' -> '(\"Alt\" \"x\")'
             'SPC f f' -> '(\"Space\" \"f\" \"f\")'"
  (let ((parts (split-string key-str " " t))
        result)
    (dolist (part parts)
      (cond
       ;; Handle special keys
       ((string= part "SPC") (push "Space" result))
       ((string= part "RET") (push "Enter" result))
       ((string= part "TAB") (push "Tab" result))
       ((string= part "ESC") (push "Escape" result))
       ((string= part "DEL") (push "Backspace" result))
       ((string-match "^<\\(.+\\)>$" part)
        ;; Handle <key> notation like <f1>, <left>, etc.
        (let ((inner (match-string 1 part)))
          (cond
           ((string-match "^f\\([0-9]+\\)$" inner)
            (push (concat "F" (match-string 1 inner)) result))
           ((string= inner "left") (push "Left" result))
           ((string= inner "right") (push "Right" result))
           ((string= inner "up") (push "Up" result))
           ((string= inner "down") (push "Down" result))
           ((string= inner "return") (push "Enter" result))
           ((string= inner "tab") (push "Tab" result))
           ((string= inner "escape") (push "Escape" result))
           ((string= inner "backspace") (push "Backspace" result))
           ((string= inner "delete") (push "Delete" result))
           ((string= inner "home") (push "Home" result))
           ((string= inner "end") (push "End" result))
           ((string= inner "prior") (push "PageUp" result))
           ((string= inner "next") (push "PageDown" result))
           (t (push (capitalize inner) result)))))
       ;; Handle modifier combinations like C-x, M-x, C-M-x, s-x
       ((string-match "^\\([CMAsSH]-\\)+\\(.\\)$" part)
        (let ((mods (match-string 1 part))
              (key (match-string 2 part)))
          (when (string-match "C-" mods) (push "Ctrl" result))
          (when (string-match "M-" mods) (push "Alt" result))
          (when (string-match "s-" mods) (push "Super" result))
          (when (string-match "S-" mods) (push "Shift" result))
          (when (string-match "H-" mods) (push "Hyper" result))
          (when (string-match "A-" mods) (push "Alt" result))
          (push key result)))
       ;; Single character
       ((= (length part) 1)
        (push part result))
       ;; Other
       (t (push part result))))
    (nreverse result)))

(defun keybindings-export--command-description (cmd)
  "Get description for command CMD."
  (or (and (symbolp cmd)
           (documentation cmd t)
           (let ((doc (documentation cmd t)))
             (car (split-string doc "\n"))))
      (symbol-name cmd)))

(defun keybindings-export--extract-keymap (keymap keymap-id)
  "Extract bindings from KEYMAP with KEYMAP-ID."
  (let (bindings)
    (when (and (boundp keymap) (keymapp (symbol-value keymap)))
      (map-keymap
       (lambda (event def)
         (when (and def (not (keymapp def)))
           (let* ((key-vec (vector event))
                  (key-str (keybindings-export--key-to-string key-vec)))
             (unless (or (keybindings-export--should-ignore-key key-str)
                         (keybindings-export--should-ignore-command def))
               (push (list :combo key-str
                           :keys (keybindings-export--parse-emacs-key key-str)
                           :action (if (symbolp def)
                                       (symbol-name def)
                                     (format "%s" def))
                           :category keymap-id)
                     bindings)))))
       (symbol-value keymap)))
    bindings))

(defun keybindings-export--extract-all-keymaps ()
  "Extract bindings from all configured keymaps."
  (let (all-bindings)
    (dolist (keymap-spec keybindings-export-keymaps)
      (let* ((keymap-sym (car keymap-spec))
             (keymap-id (symbol-name keymap-sym))
             (bindings (keybindings-export--extract-keymap keymap-sym keymap-id)))
        (setq all-bindings (append all-bindings bindings))))
    ;; Remove duplicates (same combo + action), prefer more specific keymaps
    (cl-remove-duplicates all-bindings
                          :test (lambda (a b)
                                  (and (equal (plist-get a :combo)
                                              (plist-get b :combo))
                                       (equal (plist-get a :action)
                                              (plist-get b :action))))
                          :from-end t)))

(defun keybindings-export--escape-js-string (str)
  "Escape STR for JavaScript string literal.
Order matters: escape backslash FIRST, then other characters."
  (let* ((s1 (replace-regexp-in-string "\\\\" "\\\\\\\\" str))      ; \ -> \\
         (s2 (replace-regexp-in-string "'" "\\\\'" s1))             ; ' -> \'
         (s3 (replace-regexp-in-string "\"" "\\\\\"" s2))           ; " -> \"
         (s4 (replace-regexp-in-string "\n" "\\\\n" s3)))           ; newline -> \n
    s4))

(defun keybindings-export--generate-categories-js ()
  "Generate JavaScript object for categories."
  (let ((lines '())
        (idx 0)
        (colors keybindings-export-colors))
    (dolist (keymap-spec keybindings-export-keymaps)
      (let* ((keymap-sym (car keymap-spec))
             (keymap-id (symbol-name keymap-sym))
             (display-name (cdr keymap-spec))
             (color (nth (mod idx (length colors)) colors)))
        (push (format "    '%s': { name: '%s', color: '%s' }"
                      keymap-id
                      (keybindings-export--escape-js-string display-name)
                      color)
              lines)
        (setq idx (1+ idx))))
    (concat "const categories = {\n"
            (mapconcat #'identity (nreverse lines) ",\n")
            "\n};")))

(defun keybindings-export--generate-keybindings-js (bindings)
  "Generate JavaScript array for BINDINGS."
  (let ((lines '()))
    (dolist (binding bindings)
      (let* ((combo (plist-get binding :combo))
             (keys (plist-get binding :keys))
             (action (plist-get binding :action))
             (category (plist-get binding :category))
             (keys-js (concat "["
                              (mapconcat (lambda (k)
                                           (format "'%s'"
                                                   (keybindings-export--escape-js-string k)))
                                         keys ", ")
                              "]")))
        (push (format "    { combo: '%s', keys: %s, action: '%s', category: '%s' }"
                      (keybindings-export--escape-js-string combo)
                      keys-js
                      (keybindings-export--escape-js-string action)
                      category)
              lines)))
    (concat "const keybindings = [\n"
            (mapconcat #'identity (nreverse lines) ",\n")
            "\n];")))

(defun keybindings-export--generate-js ()
  "Generate complete JavaScript data."
  (let ((bindings (keybindings-export--extract-all-keymaps)))
    (concat (keybindings-export--generate-categories-js)
            "\n\n"
            (keybindings-export--generate-keybindings-js bindings))))

;;;###autoload
(defun keybindings-export-to-js ()
  "Export keybindings to JavaScript and generate HTML.
Writes to `keybindings-export-output-dir'."
  (interactive)
  (let* ((output-dir keybindings-export-output-dir)
         (js-file (expand-file-name "keybindings-data.js" output-dir))
         (html-file (expand-file-name "index.html" output-dir))
         (template-file keybindings-export-template-file)
         (js-content (keybindings-export--generate-js)))

    ;; Create output directory if needed
    (unless (file-directory-p output-dir)
      (make-directory output-dir t))

    ;; Write JS data file
    (with-temp-file js-file
      (insert js-content))

    ;; Read template and insert JS data
    (if (file-exists-p template-file)
        (let ((template (with-temp-buffer
                          (insert-file-contents template-file)
                          (buffer-string))))
          (with-temp-file html-file
            (insert (replace-regexp-in-string
                     "// keybindings-data-placeholder.*"
                     js-content
                     template nil t))))  ; FIXEDCASE=t to preserve replacement case
      (message "Warning: Template file not found: %s" template-file)
      (message "Only JS data file was created: %s" js-file))

    (message "Keybindings exported to: %s" output-dir)
    (when (y-or-n-p "Open in browser? ")
      (browse-url (concat "file://" html-file)))))

;;;###autoload
(defun keybindings-export-preview ()
  "Preview extracted keybindings in a buffer."
  (interactive)
  (let ((bindings (keybindings-export--extract-all-keymaps))
        (buf (get-buffer-create "*Keybindings Preview*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "# Extracted Keybindings\n\n")
      (let ((by-category (make-hash-table :test 'equal)))
        ;; Group by category
        (dolist (b bindings)
          (let ((cat (plist-get b :category)))
            (puthash cat (cons b (gethash cat by-category)) by-category)))
        ;; Print each category
        (maphash
         (lambda (cat items)
           (insert (format "## %s (%d bindings)\n\n" cat (length items)))
           (dolist (item (nreverse items))
             (insert (format "  %s -> %s\n"
                             (plist-get item :combo)
                             (plist-get item :action))))
           (insert "\n"))
         by-category))
      (goto-char (point-min)))
    (pop-to-buffer buf)))

(provide 'keybindings-export)

;;; keybindings-export.el ends here
