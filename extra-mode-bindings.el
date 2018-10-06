(require 'which-key)
(which-key-mode)

(defvar my-mc-keymap (make-sparse-keymap))
(define-key my-mc-keymap (kbd "k") 'mc/mark-next-like-this)
(define-key my-mc-keymap (kbd "i") 'mc/unmark-next-like-this)
(defun my-mc-start ()
  (interactive)
  (set-transient-map my-mc-keymap t)
  )


(defun my-bindkey-xfk-command-mode ()
  "Define keys for `xah-fly-command-mode-activate-hook'"
  (interactive)

  (cond

   ((eq major-mode 'magit-mode)
    (define-key xah-fly-key-map "q" 'magit-bury-buffer))
   ;; more major-mode checking here
    ;; if nothing match, do nothing
    (t nil)))
  
(add-hook 'xah-fly-command-mode-activate-hook 'my-bindkey-xfk-command-mode)

(define-key ivy-minibuffer-map (kbd "M-i") 'previous-line)
(define-key ivy-minibuffer-map (kbd "M-k") 'next-line)
(define-key ivy-minibuffer-map (kbd "M-I") 'ivy-insert-current)

;; general settings
(defun my-xah-fly-command-mode-activate-2 (&optional a b)
  "Version that takes argument - to be used in advice"
  (xah-fly-command-mode-activate)
  )
(defun my-xah-fly-command-mode-activate-1 (&optional a)
  "Version that takes argument - to be used in advice"
  (xah-fly-command-mode-activate)
  )
(advice-add #'quit-window :after #'my-xah-fly-command-mode-activate-2)
(define-key key-translation-map (kbd "ESC") (kbd "<insert>"))


;; add vi-like delete and switch to insert mode

(defun my-xah-kill-word ()
  (interactive)
  (xah-kill-word)
  (xah-fly-insert-mode-activate)
  )
(defun my-xah-backward-kill-word ()
  (interactive)
  (xah-backward-kill-word)
  (xah-fly-insert-mode-activate)
  )
(defun my-delete-char ()
  (interactive)
  (delete-char 1)
  (xah-fly-insert-mode-activate)
  )
(defun my-delete-backward-char ()
  (interactive)
  (delete-char -1)
  (xah-fly-insert-mode-activate)
  )

(define-prefix-command 'vi-type-delete-and-insert-keymap)
(define-key vi-type-delete-and-insert-keymap "r" #'my-xah-kill-word)
(define-key vi-type-delete-and-insert-keymap "e" #'my-xah-backward-kill-word)
(define-key vi-type-delete-and-insert-keymap "d" #'my-delete-char)
(define-key vi-type-delete-and-insert-keymap "s" #'my-delete-backward-char)

(defun add-vi-delete-and-switch-to-insert-mode-bindings ()
  (interactive)
  (define-key xah-fly-key-map "w" 'vi-type-delete-and-insert-keymap)
  (define-key xah-fly-key-map "a" 'vi-type-delete-and-insert-keymap)
  )

(add-hook 'xah-fly-command-mode-activate-hook 'add-vi-delete-and-switch-to-insert-mode-bindings)

;; pdf-vew settings
(defun setup-pdf-view ()
  (interactive)
  (define-key pdf-view-mode-map "\C-s" 'isearch-forward)
  (define-key pdf-view-mode-map "w" 'pdf-view-fit-width-to-window)
  (define-key pdf-view-mode-map "h" 'pdf-view-fit-height-to-window)
  (define-key pdf-view-mode-map "H" 'describe-mode)
  (define-key pdf-view-mode-map "g" 'pdf-view-goto-page)
  (define-key pdf-view-mode-map "i" 'previous-line)
  (define-key pdf-view-mode-map "k" 'next-line)
  (define-key pdf-view-mode-map "a" 'pdf-annot-add-highlight-markup-annotation)
  (define-key pdf-view-mode-map "\C-f" 'pdf-links-isearch-link)
  (require 'pdf-links)
  (define-key pdf-links-minor-mode-map "f" nil)
  )

(add-hook 'pdf-view-mode-hook 'setup-pdf-view)

;; dired settings
(defun setup-dired ()
  (interactive)
  (define-key dired-mode-map "i" 'previous-line)
  (define-key dired-mode-map "k" 'next-line))
(add-hook 'dired-mode-hook 'setup-dired)

;; multiple cursor setting
(add-hook 'multiple-cursors-mode-disabled-hook #'xah-fly-command-mode-activate)

;; insert blank line above and below
(defun my-insert-blank-line-below ()
  "insert blank line below"
  (interactive)
  (next-line)
  (beginning-of-line)
  (open-line 1)
  (xah-fly-insert-mode-activate)
  )

(defun my-insert-blank-line-above ()
  "insert blank line above"
  (interactive)
  (previous-line)
  (beginning-of-line)
  (open-line 1)
  (xah-fly-insert-mode-activate)
  )
(define-key xah-fly-leader-key-map (kbd "h") 'my-insert-blank-line-above)
(define-key xah-fly-leader-key-map (kbd "n") 'my-insert-blank-line-below)

;; change binding for moving to beginning and end of buffer
(add-hook 'xah-fly-command-mode-activate-hook
          (lambda () (define-key xah-fly-key-map "<" 'beginning-of-buffer))
          )
(define-key xah-fly-leader-key-map (kbd "<") 'end-of-buffer)
(add-hook 'xah-fly-insert-mode-activate-hook
          (lambda () (define-key xah-fly-key-map "<" nil))
          )

(provide 'extra-mode-bindings)

 
