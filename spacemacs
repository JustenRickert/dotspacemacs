;; -*- MODE: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     pdf-tools
     ;; keyboard-layout
     ipython-notebook
     python
     typescript
     yaml
     markdown
     auto-completion
     ;; better-defaults
     chrome
     clojure
     emacs-lisp
     ;; erc
     haskell
     go
     git
     ;; markdown
     org
     java
     javascript
     latex
     html
     ruby
     shell
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     spell-checking
     syntax-checking
     version-control
     themes-megapack
     typography
     vimscript
     ;; w3m
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(org-journal
                                      plant-uml-mode
                                      org-alert
                                      org-agenda-property
                                      android-mode)
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '(adaptive-wrap)
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'. (default t)

   dotspacemacs-delete-orphan-packages t)) ; disgusting

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)

   dotspacemacs-elpa-https t
   ;; ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; ;; If non nil then spacemacs will check for updates at startup
   ;; ;; when the current branch is not `develop'. (default t)
   dotspacemacs-check-for-update nil

   ;; One of `vim', `emacs' or `hybrid'. Evil is always enabled but if the
   ;; variable is `emacs' then the `holy-mode' is enabled at startup. `hybrid'
   ;; uses emacs key bindings for vim's insert mode, but otherwise leaves evil
   ;; unchanged. (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'random
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'.
   ;; (default '(recents projects))
   dotspacemacs-startup-lists '(recents projects)
   ;; Number of recent files to show in the startup buffer. Ignored if
   ;; `dotspacemacs-startup-lists' doesn't include `recents'. (default 5)
   dotspacemacs-startup-recent-list-size 5
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(
                         sanityinc-tomorrow-eighties
                         farmhouse-dark
                         farmhouse-light

                         ;; spacegray
                         ;; twilight-anti-bright ; very purple, dark blue, I like
                                        ; this one makes use of highlight well.
                                        ; Also highlights begin_src lines
                         ;; gandalf
                         ;; soft-stone
                         ;; zenburn      ; gray theme, growing old of it, honestly.
                         ;; twilight     ; this and zen-and-art look very similar
                         ;; ujelly       ; very black and while
                         ;; zen-and-art
                         ;; tronesque      ; Very blue
                         ;; toxi           ; blue modeline, gray theme
                         ;; soft-morning
                         ;; spacemacs-dark
                         ;; spacemacs-light
                         ;; solarized-light
                         ;; solarized-dark
                         ;; leuven
                         ;; monokaia
                         )
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.05)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m)
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; (Not implemented) dotspacemacs-distinguish-gui-ret nil
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; If non nil `Y' is remapped to `y$'. (default t)
   dotspacemacs-remap-Y-to-y$ t
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f), `find-spacemacs-file' (SPC f e s), and
   ;; `find-contrib-file' (SPC f e c) are replaced. (default nil)
   dotspacemacs-use-ido nil
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize t
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header t
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-micro-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs
initialization after layers configuration. This is the place
where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a
package is loaded, you should place your code here."

  ;; (font-lock-add-keywords 'org-mode-hook '(("\\(\\\\(.*\\\\)\\)"
  ;;                                           1 font-lock-warning-face prepend)))

  ;; evil-surround (automatic parentheses)
  (add-hook 'org-mode-hook
            (lambda nil
              (push '(?\( . ("\(" . "\)")) evil-surround-pairs-alist)
              (push '(?\\ . ("\\(" . "\\)")) evil-surround-pairs-alist)))

  ;; maybe I'll like this?
  (setq scroll-conservatively 10000
        scroll-preserve-screen-position t)

  (add-hook 'go-mode-hook
            (lambda ()
              ;; go-guru
              (go-guru-hl-identifier-mode 1)
              (setq go-guru-hl-identifier-idle-time 0.1)))

  ;; golang
  (setenv "GOPATH" "/home/justen/go/")
  (add-hook 'go-mode-hook 'go-eldoc-setup)
  ;; (exec-path-from-shell-copy-env "GOPATH")

  ;; ;; org-babel haskell -- for running inline code
  ;; (add-to-list 'load-path "~/.emacs.d/private/local")
  ;; (require 'ob-haskell)

  ;; org-babel plantuml -- for making inline graphs
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(;; other Babel languages
     ;; (plantuml . t)
     (latex . t)
     (python . t)
     (haskell . t)))                    ; I might need the local ob-haskell
                                        ; directory. I have it, look above.
  ;; plantuml needs this!
  (setq org-plantuml-jar-path
        (expand-file-name "~/.emacs.d/plantumljarthing/plantuml.jar"))

  ;; need that python
  ;; (setq python-shell-interpreter "python3.5")

  ;; Who I am
  (setq user-full-name "Justen Rickert")
  (setq user-mail-address "justenrickert@gmail.com")

  ;; It is 600 by default
  (setq max-lisp-eval-depth 1000)

  ;; moves the mouse out of the way.
  (mouse-avoidance-mode 'cat-and-mouse)

  (require 'org-alert)
  ;; libnotify uses notify-send to make dunst do cool shit
  (setq org-alert-notification-title "org")
  (setq alert-default-style 'libnotify)

  ;; this doesn't matter with `org-alert-enable'
  (setq org-alert-interval 600)        ; 600 is 10m

  ;; shorten up the powerline! Also displays the clock.
  (setq spaceline-buffer-size-p nil
        spaceline-org-clock-p t
        spaceline-minor-modes-p nil
        spaceline-buffer-encoding-abbrev-p nil)

  ;; These funs replace the ones in the org-journal-mode. They do something
  ;; stupid that breaks the functionality of evil-mode. This is pretty way to
  ;; deal with the problem, and there were just two lines that I uncommented and
  ;; voila. Those two lines just were not what I wanted.
  ;; (require 'org-journal)
  ;; (defun org-journal-open-previous-entry ()
  ;;   "Open the previous journal entry starting from a currently displayed one"
  ;;   (interactive)
  ;;   (let ((calendar-date (org-journal-file-name->calendar-date
  ;;                         (file-name-nondirectory (buffer-file-name))))
  ;;         ;; (view-mode-p view-mode)
  ;;         (dates (reverse (org-journal-list-dates))))
  ;;     ;; insert current buffer in list if not present
  ;;     (unless (file-exists-p (buffer-file-name))
  ;;       (setq dates (cons calendar-date dates))
  ;;       ;; reverse-sort!
  ;;       (sort dates (lambda (a b) (calendar-date-compare (list b) (list a)))))
  ;;     (calendar-basic-setup nil t)
  ;;     (while (and dates (calendar-date-compare (list calendar-date) dates))
  ;;       (setq dates (cdr dates)))
  ;;     (calendar-exit)
  ;;     (if (and dates (cadr dates))
  ;;         (let* ((time (org-journal-calendar-date->time (cadr dates)))
  ;;                (filename (org-journal-get-entry-path time)))
  ;;           (find-file filename)
  ;;           (org-journal-decrypt)
  ;;           ;; (view-mode (if view-mode-p 1 -1))
  ;;           (org-show-subtree))
  ;;       (message "No previous journal entry before this one"))))
  ;; (defun org-journal-open-next-entry ()
  ;;   "Open the next journal entry starting from a currently displayed one"
  ;;   (interactive)
  ;;   (let ((calendar-date (org-journal-file-name->calendar-date
  ;;                         (file-name-nondirectory (buffer-file-name))))
  ;;         ;; (view-mode-p view-mode)
  ;;         (dates (org-journal-list-dates)))
  ;;     ;; insert current buffer in list if not present
  ;;     (unless (file-exists-p (buffer-file-name))
  ;;       (setq dates (cons calendar-date dates))
  ;;       (sort dates (lambda (a b) (calendar-date-compare (list a) (list b)))))
  ;;     (calendar-basic-setup nil t)
  ;;     (while (and dates (not (calendar-date-compare (list calendar-date) dates)))
  ;;       (setq dates (cdr dates)))
  ;;     (calendar-exit)
  ;;     (if dates
  ;;         (let* ((time (org-journal-calendar-date->time (car dates)))
  ;;                (filename (org-journal-get-entry-path time)))
  ;;           (find-file filename)
  ;;           (org-journal-decrypt)
  ;;           ;; (view-mode (if view-mode-p 1 -1))
  ;;           (org-show-subtree))
  ;;       (message "No next journal entry after this one"))))

  (require 'golden-ratio)

  ;; Org-mode
  (add-hook 'org-mode-hook
            (lambda nil
              (setq org-hide-emphasis-markers t)
              (setq dabbrev--case-fold-search t)
              (setq dabbrev-case-replace t)
              (setq truncate-lines nil)
              ;; (pabbrev-mode)
              ;; (org-indent-mode)
              (abbrev-mode)
              (golden-ratio-mode)
              (auto-fill-mode)))
  (add-to-list 'golden-ratio-exclude-buffer-names '("*Choices*"
                                                    "*Org todo*"
                                                    "*Org Agenda*"))
  (add-to-list 'golden-ratio-exclude-modes '("dired-mode"))

  ;; makes super and sub scripts interact more predictably
  (setq org-use-sub-superscripts (quote {}))

  ;; I think this puts nice font coloring in code blocks
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)

  ;; I think this gets rid of the things on the right margin on git. I find them
  ;; distracting when working on only 80 columns. I don't even think this is the
  ;; right command, though. >_>
  (setq version-control-global-margin nil)

  ;; need this to rescale images with ATTR_HTML tags. I don't even think this
  ;; works, though. >_>
  (setq org-inline-actual-width 100)

  (setq org-bullets-bullet-list '("■" "◆" "▲" "▶"))

  ;; abbrev
  (quietly-read-abbrev-file)
  (setq abbrev-file-name "~/org/.extra/abbrev_defs")
  (setq save-abbrevs 'silently)

  ;; org-agenda files
  (setq org-agenda-files
        (delq nil
              (mapcar (lambda (x) (and (file-exists-p x) x))
                      '("~/org/note.org"
                        "~/org/classes/schedule.org"
                        "~/org/finance.org"))))

  ;; todos
  ;; (setq org-todo-keywords
  ;;       '((sequence "en passant(p)" "en attente(a)" "|" "fini(f)" "terminé(t)")
  ;;         (sequence "will(w)" "won't(n)" "|" "did(d)" "didn't(!)")))

  ;; (setq org-todo-keyword-faces
  ;;       '(("en passant" . "orange")
  ;;         ("en attente" . "sky blue")
  ;;         ("fini" . "brown")
  ;;         ("terminé" . (:background "dark gray" :foreground "brown"))
  ;;         ("will" . "green")
  ;;         ("won't" . "orange")
  ;;         ("did" . "sky blue")
  ;;         ("didn't" . "red")))

  ;; enter insert state automatically, because I can never otherwise.
  (add-hook 'org-capture-mode-hook 'evil-insert-state)
  (add-hook 'with-editor-mode-hook 'evil-insert-state)
  (add-hook 'view-mode-hook 'evil-emacs-state)

  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
  ;; typescript automatic recompilation
  (add-hook 'typescript-mode-hook
            (lambda nil
              'save-buffer-and-call-interactive-hooks)
            nil t)

  ;; company delete word
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "C-w") 'evil-delete-backward-word))
  (with-eval-after-load 'helm
    (define-key helm-map (kbd "C-w") 'evil-delete-backward-word))

  ;; Emacs-like C-a, C-e. Honestly, I don't even know C-e does otherwise...
  (define-key evil-normal-state-map (kbd "C-e") nil)
  (define-key evil-insert-state-map (kbd "C-a") 'back-to-indentation)
  (define-key evil-normal-state-map (kbd "C-a") 'back-to-indentation)
  (define-key evil-visual-state-map (kbd "C-a") 'back-to-indentation)
  (define-key evil-normal-state-map (kbd "C-e") 'move-end-of-line)
  (define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
  (define-key evil-visual-state-map (kbd "C-e") 'move-end-of-line)

  ;; I've yet to implement this, yet.
  (defun just-org-screenshot ()
    "Take a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
    (interactive)
    (setq filename
          (concat
           (make-temp-name
            (concat (buffer-file-name)
                    "_"
                    (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
    (call-process "import" nil nil nil filename)
    (insert (concat "[[" filename "]]"))
    (org-display-inline-images))

  ;; this has to deal with inline image displaying.
  (setq org-image-actual-width nil)

  ;; I use these. They're pretty good funs. just is the first 4 letters of my
  ;; name...
  (defun just-org-show-next-heading-tidily ()
    "Show next entry, keeping other entries closed."
    (interactive)
    (if (save-excursion (end-of-line) (outline-invisible-p))
        (progn (org-show-entry) (show-children))
      (outline-next-heading)
      (unless (and (bolp) (org-on-heading-p))
        (org-up-heading-safe)
        (hide-subtree)
        (error "Boundary reached"))
      (org-overview)
      (org-reveal t)
      (org-show-entry)
      (show-children)))
  (defun just-org-show-previous-heading-tidily ()
    "Show previous entry, keeping other entries closed."
    (interactive)
    (let ((pos (point)))
      (outline-previous-heading)
      (unless (and (< (point) pos) (bolp) (org-on-heading-p))
        (goto-char pos)
        (hide-subtree)
        (error "Boundary reached"))
      (org-overview)
      (org-reveal t)
      (org-show-entry)
      (show-children)))
  (defun just-hide-other ()
    (interactive)
    (save-excursion
      (org-back-to-heading 'invisible-ok)
      (hide-other)
      (org-cycle)
      (org-cycle)
      (org-cycle)))

  ;; I use this to replace the narrow-org-to-sub-buffer or whatever. The other
  ;; thing breaks the org major mode if the encoding changes while the buffer is
  ;; narrowed to region.
  (defun narrow-or-widen-dwim (p)
    "If the buffer is narrowed, it widens.  Otherwise, it narrows intelligently.
Intelligently means: region, subtree, or defun, whichever applies
first.

With prefix P, don't widen, just narrow even if buffer is already
narrowed."
    (interactive "P")
    (declare (interactive-only))
    (cond ((and (buffer-narrowed-p) (not p)) (widen))
          ((region-active-p)
           (narrow-to-region (region-beginning) (region-end)))
          ((derived-mode-p 'org-mode) (org-narrow-to-subtree))
          (t (narrow-to-defun))))

  ;; Additional leader keys that I've added. Pretty straight forward.
  (spacemacs/set-leader-keys
    "w;" 'narrow-or-widen-dwim
    "aa" 'abbrev-mode
    ;; "ad" 'ranger
    ;; `tT' is default, but `tt' is not taken
    "tt" 'spacemacs/toggle-typographic-substitutions
    "Ss" 'flyspell-mode
    "+" 'text-scale-increase
    "-" 'text-scale-decrease
    "=" 'text-scale-mode)

  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "b" 'narrow-or-widen-dwim
    "ht" 'org-ctrl-c-star
    "n" 'just-org-show-next-heading-tidily
    "N" 'just-org-show-previous-heading-tidily
    "u" 'just-hide-other)

  (setq org-emphasis-alist
        '(("*" bold)
          ("/" italic)
          ("_" underline)
          ("=" org-verbatim verbatim)
          ("~" org-code verbatim)
          ("+" (:strike-through t))
          ("^" (:overline t))))         ; overline does not export to latex.

  ;; annoying validate thing in org export that I don't understand
  (setq org-export-html-validation-link nil)
  (setq org-html-validation-link nil)

  (desktop-read)
  (desktop-auto-save-enable)
  (load "server")
  (unless (server-running-p) (server-start)))  ; end defun user-config

  ;; AUTOMATIC CONFIG

  ;; Do not write anything past this comment. This is where Emacs will
  ;; auto-generate custom variable definitions.


  (defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Linum-format "%7i ")
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(evil-want-Y-yank-to-eol t)
 '(fci-rule-color "#343d46" t)
 '(fringe-mode 6 nil (fringe))
 '(linum-format "%3i")
 '(main-line-color1 "#1E1E1E")
 '(main-line-color2 "#111111")
 '(main-line-separator-style (quote chamfer))
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(org-capture-templates (quote (("t" "task" entry (file "~/org/notes.org") ""))))
 '(package-selected-packages
   (quote
    (powerline org projectile go-guru git-gutter flyspell-correct iedit autothemer tern go-mode eclim hydra inflections multiple-cursors cider seq spinner clojure-mode bind-key auto-complete company highlight anzu smartparens bind-map undo-tree flycheck haskell-mode markdown-mode yasnippet request helm helm-core skewer-mode js2-mode magit magit-popup git-commit with-editor async f s dash rvm ruby-tools ruby-test-mode rubocop rspec-mode robe rbenv rake minitest chruby bundler inf-ruby flatland-black-theme paren-face icicles pdf-tools tablist tide typescript-mode yaml-mode fountain-mode ein python-environment sourcerer-theme company-auctex auctex olivetti vimrc-mode dactyl-mode typit typing typing-game erc-yt erc-view-log erc-social-graph erc-image erc-hl-nicks gitter hide-comnt yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode helm-pydoc cython-mode company-anaconda anaconda-mode pythonic helm-purpose window-purpose imenu-list zonokai-theme zenburn-theme zen-and-art-theme xterm-color ws-butler window-numbering which-key web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package underwater-theme ujelly-theme typo twilight-theme twilight-bright-theme twilight-anti-bright-theme tronesque-theme toxi-theme toc-org tao-theme tangotango-theme tango-plus-theme tango-2-theme tagedit sunny-day-theme sublime-themes subatomic256-theme subatomic-theme stekene-theme spacemacs-theme spaceline spacegray-theme soothe-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smeargle slim-mode shell-pop seti-theme scss-mode sass-mode reverse-theme restart-emacs ranger rainbow-delimiters railscasts-theme quelpa purple-haze-theme pug-mode professional-theme popwin planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme persp-mode pcre2el pastels-on-dark-theme paradox orgit organic-green-theme org-projectile org-present org-pomodoro org-plus-contrib org-download org-bullets org-alert org-agenda-property open-junk-file omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme niflheim-theme neotree naquadah-theme mwim mustang-theme multi-term move-text monokai-theme monochrome-theme molokai-theme moe-theme mmm-mode minimal-theme material-theme markdown-toc majapahit-theme magit-gitflow macrostep lush-theme lorem-ipsum livid-mode linum-relative link-hint light-soap-theme less-css-mode json-mode js2-refactor js-doc jbeans-theme jazz-theme ir-black-theme intero inkpot-theme info+ indent-guide ido-vertical-mode hungry-delete htmlize hlint-refactor hl-todo hindent highlight-parentheses highlight-numbers highlight-indentation heroku-theme hemisu-theme help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-hoogle helm-gitignore helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag hc-zenburn-theme haskell-snippets gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme google-translate golden-ratio go-eldoc gnuplot gmail-message-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gh-md gandalf-theme flyspell-correct-helm flycheck-pos-tip flycheck-haskell flx-ido flatui-theme flatland-theme firebelly-theme fill-column-indicator farmhouse-theme fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu espresso-theme eshell-z eshell-prompt-extras esh-help emmet-mode elisp-slime-nav edit-server dumb-jump dracula-theme django-theme diff-hl define-word darktooth-theme darkokai-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme company-web company-tern company-statistics company-go company-ghci company-ghc company-emacs-eclim company-cabal column-enforce-mode colorsarenice-theme color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized coffee-mode cmm-mode clues-theme clojure-snippets clj-refactor clean-aindent-mode cider-eval-sexp-fu cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile apropospriate-theme anti-zenburn-theme android-mode ample-zen-theme ample-theme alect-themes aggressive-indent afternoon-theme ace-window ace-link ace-jump-helm-line ac-ispell)))
 '(paradox-github-token t)
 '(pos-tip-background-color "#303030")
 '(powerline-color1 "#3d3d68")
 '(powerline-color2 "#292945")
 '(rainbow-identifiers-cie-l*a*b*-lightness 80)
 '(rainbow-identifiers-cie-l*a*b*-saturation 18)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#bf616a")
     (40 . "#DCA432")
     (60 . "#ebcb8b")
     (80 . "#B4EB89")
     (100 . "#89EBCA")
     (120 . "#89AAEB")
     (140 . "#C189EB")
     (160 . "#bf616a")
     (180 . "#DCA432")
     (200 . "#ebcb8b")
     (220 . "#B4EB89")
     (240 . "#89EBCA")
     (260 . "#89AAEB")
     (280 . "#C189EB")
     (300 . "#bf616a")
     (320 . "#DCA432")
     (340 . "#ebcb8b")
     (360 . "#B4EB89"))))
 '(vc-annotate-very-old-color nil)
 '(when
      (or
       (not
        (boundp
         (quote ansi-term-color-vector)))
       (not
        (facep
         (aref ansi-term-color-vector 0)))))
 '(xterm-color-names
   ["#303030" "#D66F84" "#D79887" "#D49A8A" "#94B1A3" "#A8938C" "#989584" "#E8DACC"])
 '(xterm-color-names-bright
   ["#3A3A3A" "#E47386" "#CC816B" "#769188" "#7D6F6A" "#9C8772" "#E8DACC"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
