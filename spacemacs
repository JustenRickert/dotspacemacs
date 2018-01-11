;; -*- MODE: emacs-lisp -*-

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(ocaml
     elfeed
     haskell
     reason
     (mu4e :variables
           mu4e-installation-path "/usr/share/emacs/site-lisp")
     rust
     clojure
     ;; parinfer
     (typescript :variables
                 typescript-indent-level 2)
     yaml
     markdown
     (better-defaults :variables
                      better-defaults-move-to-beginning-of-code-first t
                      better-defaults-move-to-end-of-code-first nil)
     emacs-lisp
     git
     (org :variables
          org-enable-reveal-js-support t
          org-todo-keywords '((sequence "TODO" "WAITING" "|" "DONE")
                              (sequence "|" "CANCELED"))
          org-todo-keyword-faces '(
                                   ;; ("TODO"     . (:foreground "red" :weight bold))
                                   ;; ("DONE"     . (:foreground "green" :weight bold))
                                   ("WAITING"  . (:foreground "LightSkyBlue" :weight bold))
                                   ("CANCELED" . (:foreground "RosyBrown" :weight bold))))
     react
     javascript
     latex
     html
     ;; ruby
     shell
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     spell-checking
     syntax-checking
     auto-completion
     version-control
     typography
     ;; themes-megapack
     )

   dotspacemacs-additional-packages '(w3m
                                      reason-mode
                                      parinfer
                                      all-the-icons
                                      ox-reveal
                                      rjsx-mode
                                      writeroom-mode
                                      prettier-js)
   dotspacemacs-excluded-packages '()
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update nil

   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner nil
   dotspacemacs-startup-lists '(recents projects)
   dotspacemacs-startup-recent-list-size 5
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-themes '(spacemacs-dark
                         gruvbox-light-hard
                         whiteboard
                         white-sand
                         material-light)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Source Code Pro"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.05)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-command-key ":"
   dotspacemacs-remap-Y-to-y$ t
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-use-ido nil
   dotspacemacs-helm-resize t
   dotspacemacs-helm-no-header t
   dotspacemacs-helm-position 'bottom
   dotspacemacs-enable-paste-micro-state nil
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers nil
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first.")

(defun dotspacemacs/user-config ()
  "Configuration function for user code. This function is called at the very end
of Spacemacs initialization after layers configuration. This is the place where
most of your configurations should be done. Unless it is explicitly specified
that a variable should be set before a package is loaded, you should place your
code here."

  (setq user-full-name "Justen Rickert")
  (setq user-mail-address "justenrickert@gmail.com")

  (spacemacs/set-leader-keys-for-major-mode 'mu4e-view-mode
    "," 'mu4e~view-browse-url-from-binding)

  (add-hook 'mu4e-compose-mode-hook
            (lambda ()
              (visual-line-mode)
              (spacemacs/toggle-auto-fill-mode-off)))

  ;; (setq elfeed-feeds
  ;;       '(("http://nullprogram.com/feed/" blog emacs)
  ;;         "http://www.50ply.com/atom.xml" ; no autotagging
  ;;         ("http://nedroid.com/feed/" webcomic)))

  (setq mu4e-maildir "~/.mail"
        mu4e-trash-folder "/gmail/[Gmail].Trash"
        mu4e-sent-folder "/gmail/[Gmail].Sent Mail"
        mu4e-drafts-folder "/gmail/[Gmail].Drafts"
        mu4e-get-mail-command "mbsync gmail"
        mu4e-update-interval 1800
        mu4e-compose-signature-auto-include t
        mu4e-view-show-images t
        mu4e-view-show-addresses t
        mu4e-user-mail-address-list '("justenrickert@gmail.com")
        mu4e-hide-index-messages nil
        ;; mu4e-use-fancy-chars t
        mu4e-change-filenames-when-moving t
        mu4e-cache-maildir-list nil)

  (with-eval-after-load 'message
    (setq message-cite-style message-cite-style-gmail))

  (setq message-send-mail-function 'smtpmail-send-it
        ;; message-cite-style message-cite-style-gmail ;this needs to be with-eval-after-load ???
        starttls-use-gnutls t
        smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
        smtpmail-auth-credentials (expand-file-name "~/.authinfo")
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587)

  (setq message-kill-buffer-on-exit t)

  (add-hook 'dired-mode-hook
            (lambda ()
              (dired-hide-details-mode)))

  (setq org-enforce-todo-checkbox-dependencies t
        org-enforce-todo-dependencies t
        org-directory "~/life/org/"
        org-agenda-files (list org-directory "~/Dropbox/org")
        org-default-notes-file "~/life/org/inbox.org"
        org-refile-targets '((nil :maxlevel . 2)
                             (org-agenda-files :maxlevel . 2))
        org-capture-templates
        '(("n" "Task" entry (file+headline "~/life/org/inbox.org"
                                           "Inbox")
           "* TODO %? \n%i  - Task created on %U \\\\" :empty-lines 1)

          ("s" "Sync" entry (file+headline "~/Dropbox/org/sync.org"
                                           "Sync")
           "* TODO %? \n%i  - Task created on %U \\\\" :empty-lines 1)

          ("j" "Journal" entry (file+datetree "~/life/org/journal.org")
           "* %<%H:%M> %?\n" :empty-lines 1)))

  ;; For some reason, neo doesn't load the `icons' package, so I have to do
  ;; this.
  (add-hook 'neo-after-create-hook
            #'(lambda (_)             ;idk why I need to write this like this...
                (setq neo-theme (if (display-graphic-p) 'icons 'ascii))))

  ;; Apparently this stopped evil-forward-search from freezing.
  (setq dotspacemacs-mode-line-unicode-symbols nil)

  (defun load-if-exists (f)
    (if (file-exists-p (expand-file-name f))
        (load-file (expand-file-name f))))

  (load-if-exists "~/.emacs.d/private/local/beancount/beancount.el")

  (setq company-tooltip-align-annotations t)

  (setq racer-rust-src-path "/home/justen/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src")

  (setq clojure-enable-fancify-symbols t)

  (defun unfill-paragraph (&optional region)
    "Takes a multi-line paragraph and makes it into a single line of text."
    (interactive (progn (barf-if-buffer-read-only) '(t)))
    (let ((fill-column (point-max))
          ;; This would override `fill-column' if it's an integer.
          (emacs-lisp-docstring-fill-column t))
      (fill-paragraph nil region)))

  (defun movement-visual-lines ()
    "Make evil movement commands move across visual lines instead of logical
lines"
    (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
    (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
    (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
    (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
    ;; Make horizontal movement cross lines
    (setq-default evil-cross-lines t))

  (add-hook 'org-mode-hook
            (lambda nil
              (local-set-key (kbd "M-q") 'unfill-toggle)
              (local-set-key (kbd "C-a") 'mwim-beginning-of-code-or-line)
              (movement-visual-lines)
              (visual-line-mode)
              (setq org-hide-emphasis-markers t)
              (push '(?\( . ("\(" . "\)")) evil-surround-pairs-alist)
              (push '(?\{ . ("\{" . "\}")) evil-surround-pairs-alist)
              (push '(?\\ . ("\\(" . "\\)")) evil-surround-pairs-alist)))

  (setq scroll-conservatively 10000
        scroll-preserve-screen-position t)

  (add-hook 'go-mode-hook
            (lambda nil
              ;; go-guru
              (go-guru-hl-identifier-mode 1)
              (setq go-guru-hl-identifier-idle-time 0.1)))

  (setenv "GOPATH" "/home/justen/go/")
  (add-hook 'go-mode-hook 'go-eldoc-setup)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((dot . t)
     (clojure . t)
     (python . t)
     (haskell . t)
     (ledger . t)
     (js . t)
     (latex . t)))

  (setq org-latex-listings 'minted
        org-latex-packages-alist '(("" "minted"))
        org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

  ;; epub reader nov
  (push '("\\.pub\\'" . nov-mode) auto-mode-alist)

  ;; javascript
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))

  ;; typescript
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

  (with-eval-after-load 'web-mode
    (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
    (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
    (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil)))

  (add-hook 'scss-mode-hook
            (lambda ()
              (setq css-indent-offset 2)))

  (add-hook 'typescript-mode-hook
            (lambda ()
              (prettier-js-mode)
              (setq prettier-js-args '("--print-width" "80"
                                       "--single-quote" "true"))
              (add-hook 'before-save-hook
                        #'(shell-command
                           (concat (reduce
                                    #'(lambda (cmd arg)
                                        (if (boundp 'cmd)
                                            (concat cmd " " arg)
                                          (concat prettier-js-command " " arg)))
                                    prettier-js-args)
                                   " "
                                   buffer-file-name)))))

  (add-hook 'rjsx-mode-hook
            (lambda ()
              (prettier-js-mode)
              (add-hook 'before-save-hook 'prettier-before-save)
              (setq js2-basic-offset 2
                    ;; js2-strict-missing-semi-warning nil
                    prettier-js-args '("--trailing-comma" "none"
                                       "--no-bracket-spacing") ;; "--no-semi"
                    web-mode-enable-auto-quoting nil)))

  (add-hook 'react-mode-hook
            (lambda ()
              (prettier-js-mode)
              (add-hook 'before-save-hook 'prettier-before-save)
              (setq
               js2-basic-offset  2
               css-indent-offset 2
               web-mode-markup-indent-offset 2
               web-mode-css-indent-offset 2
               web-mode-code-indent-offset 2
               web-mode-attr-indent-offset 2)
              (setq prettier-js-args '("--trailing-comma" "none"
                                       "--no-bracket-spacing"
                                       "--no-semi"))
              (setq web-mode-enable-auto-quoting nil)))

  (setq org-export-with-smart-quotes t)

  ;; It is 600 by default
  (setq max-lisp-eval-depth 1000)

  ;; shorten up the powerline! Also displays the clock.
  (setq spaceline-buffer-size-p nil
        spaceline-org-clock-p t
        spaceline-minor-modes-p nil
        spaceline-buffer-encoding-abbrev-p nil)

  (font-lock-add-keywords            ; A bit silly but my headers are now
   'org-mode `(("^\\*+ \\(TODO\\) "  ; shorter, and that is nice canceled
                (1 (progn (compose-region (match-beginning 1) (match-end 1) "⚑")
                          nil)))
               ("^\\*+ \\(WAITING\\) "
                (1 (progn (compose-region (match-beginning 1) (match-end 1) "⧗")
                          nil)))
               ("^\\*+ \\(CANCELED\\) "
                (1 (progn (compose-region (match-beginning 1) (match-end 1) "✘")
                          nil)))
               ("^\\*+ \\(DONE\\) "
                (1 (progn (compose-region (match-beginning 1) (match-end 1) "✔")
                          nil)))))

  (spacemacs/set-leader-keys-for-major-mode 'org-agenda-mode
    "T" 'org-agenda-todo-nextset
    "t" 'org-agenda-todo)

  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "tts"  'org-hide-block-all
    "tt-"  'org-show-block-all
    "ttp"  'org-toggle-pretty-entities
    "tte"  'just-toggle-emphasis-markers
    "j"    'set-justification-full
    "f"    'org-forward-heading-same-level
    "b"    'org-backward-heading-same-level
    "ht"   'org-ctrl-c-star
    "n"    'org-next-visible-heading
    "N"    'org-previous-visible-heading
    ;; "u" 'just-hide-other
    "q"    'unfill-paragraph)

  (defun just-toggle-emphasis-markers nil
    (interactive)
    (if org-hide-emphasis-markers
        (progn (setq org-hide-emphasis-markers nil)
               (message "Showing emphasis-markers"))
      (progn (message "Hiding emphasis-markers")
             (setq org-hide-emphasis-markers t))))

  ;; I think this puts nice font coloring in code blocks
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)

  (setq version-control-global-margin nil)

  (setq org-inline-actual-width 100)

  (setq org-ellipsis ".▼")
  (setq org-bullets-bullet-list '("■" "◆" "▲" "▶"))

  ;; enter insert state automatically, because I can never otherwise.
  (add-hook 'org-capture-mode-hook 'evil-insert-state)
  (add-hook 'with-editor-mode-hook 'evil-insert-state)
  (add-hook 'view-mode-hook 'evil-emacs-state)

  (with-eval-after-load 'company
    (define-key company-active-map (kbd "C-w") 'evil-delete-backward-word))
  (with-eval-after-load 'helm
    (define-key helm-map (kbd "C-w") 'evil-delete-backward-word))

  (define-key evil-normal-state-map (kbd "C-e") 'mwim-end-of-code-or-line)
  (define-key evil-insert-state-map (kbd "C-e") 'mwim-end-of-code-or-line)
  (define-key evil-visual-state-map (kbd "C-e") 'mwim-end-of-code-or-line)
  (define-key evil-insert-state-map (kbd "C-w") nil)
  (define-key evil-insert-state-map (kbd "C-w") 'evil-delete-backward-word)

  (spacemacs/set-leader-keys
    "w;" 'narrow-or-widen-dwim
    "aa" 'abbrev-mode
    "tt" 'spacemacs/toggle-typographic-substitutions
    "Ss" 'flyspell-mode
    "+" 'text-scale-increase
    "-" 'text-scale-decrease
    "=" 'text-scale-mode)

  (setq org-emphasis-alist
        '(("*" bold)
          ("/" italic)
          ("_" underline)
          ("=" org-verbatim verbatim)
          ("~" org-code verbatim)
          ("+" (:strike-through t))
          ("^" (:overline t) "\\overline{%s}"))) ;no LaTeX export...

  (setq org-export-html-validation-link nil)
  (setq org-html-validation-link nil)) ; end defun user-config



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
 '(elfeed-feeds
   (quote
    ("https://toddmotto.com/feed.xml" "https://www.javascript.com/feed/rss" "https://www.reddit.com/r/javascript/.rss" "http://nullprogram.com/feed/"
     ("http://nullprogram.com/feed/" blog emacs)
     "http://www.50ply.com/atom.xml"
     ("http://nedroid.com/feed/" webcomic))))
 '(evil-want-Y-yank-to-eol t)
 '(package-selected-packages
   (quote
    (yasnippet-snippets elfeed-web elfeed-org elfeed-goodies ace-jump-mode noflet elfeed powerline intero hlint-refactor hindent helm-hoogle haskell-snippets flycheck-haskell dante company-ghci company-ghc ghc haskell-mode company-cabal cmm-mode w3m yaml-mode xterm-color ws-butler writeroom-mode winum white-sand-theme which-key web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package unfill typo toml-mode toc-org tide tagedit symon string-inflection spaceline smeargle slim-mode shell-pop scss-mode sayid sass-mode rjsx-mode restart-emacs rainbow-delimiters racer pug-mode prettier-js popwin persp-mode pcre2el password-generator parinfer paradox ox-reveal orgit org-projectile org-present org-pomodoro org-download org-bullets org-brain open-junk-file neotree mwim multi-term mu4e-maildirs-extension mu4e-alert move-text mmm-mode material-theme markdown-toc magit-gitflow macrostep lorem-ipsum livid-mode linum-relative link-hint json-mode js2-refactor js-doc info+ indent-guide impatient-mode hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-purpose helm-projectile helm-mode-manager helm-make helm-gitignore helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag gruvbox-theme google-translate golden-ratio gnuplot gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gh-md fuzzy flyspell-correct-helm flycheck-rust flycheck-pos-tip flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-org evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eshell-z eshell-prompt-extras esh-help emmet-mode elisp-slime-nav editorconfig dumb-jump diff-hl define-word company-web company-tern company-statistics company-auctex column-enforce-mode coffee-mode clojure-snippets clj-refactor clean-aindent-mode cider-eval-sexp-fu cargo browse-at-remote auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile auctex-latexmk all-the-icons aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell)))
 '(tramp-syntax (quote default) nil (tramp)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-date ((t (:foreground "#7590db" :height 1.0))))
 '(org-level-1 ((t (:inherit bold :foreground "#4f97d7" :height 1.0))))
 '(org-level-2 ((t (:inherit bold :foreground "#2d9574" :height 1.0))))
 '(org-level-3 ((t (:foreground "#67b11d" :weight normal :height 1.0)))))
)

