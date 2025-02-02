(setq xah-fly--convert-key t)

(xah-fly--convert-key (kbd "a"))

(setq xah-fly-key-current-layout "qwerty-no")


(defun xah-fly-define-keys-2 ()
  "Define the keys for xah-fly-keys.
Created: 2022-10-31
Version: 2024-04-22"
  (interactive)
  (let ()
    (xah-fly--define-keys
     (define-prefix-command 'xah-fly-leader-key-map)    
    ))


(define-key xah-fly-leader-key-map (kbd "M-i") 'delete-window-up)
(define-key xah-fly-leader-key-map (kbd "M-k") 'delete-window-down)
(define-key xah-fly-leader-key-map (kbd "M-j") 'delete-window-left)
(define-key xah-fly-leader-key-map (kbd "M-l") 'delete-window-right)
(define-key xah-fly-leader-key-map (kbd "+") 'org-agenda)

(define-key xah-fly-t-keymap "y" 'xah-query-replace-current-word)

(define-key xah-fly-leader-key-map (kbd "RET") 'newline)
(define-key xah-fly-leader-key-map (kbd "<return>") 'newline)

;; (define-key xah-fly-key-map (kbd "<insert>") 'xah-fly-command-mode-activate)
(define-key xah-fly-key-map [escape] 'xah-fly-command-mode-activate)
(define-key xah-fly-key-map (kbd "<insertchar>") 'xah-fly-command-mode-activate)
(define-key xah-fly-key-map (kbd "<f1>") 'xah-fly-command-mode-activate)

(define-key xah-fly-key-map (kbd "2") 'my-mc-start)
(define-key xah-fly-key-map (kbd "<return>") 'xah-fly-insert-mode-activate)
(define-key xah-fly-key-map (kbd "F") 'swiper-isearch-backward)
(define-key xah-fly-key-map (kbd "C") 'xav-append-copy-line-or-region)
(define-key xah-fly-key-map (kbd "X") 'xav-append-cut-line-or-region)
(define-key xah-fly-key-map (kbd "d") 'delete-char)
(define-key xah-fly-key-map (kbd "ø") 'xah-end-of-line-or-block)
(define-key xah-fly-key-map (kbd "M-i") 'my-windmove-up)
(define-key xah-fly-key-map (kbd "M-k") 'my-windmove-down)
(define-key xah-fly-key-map (kbd "M-j") 'my-windmove-left)
(define-key xah-fly-key-map (kbd "M-l") 'my-windmove-right)

(define-key xah-fly-key-map (kbd "F") nil)
(define-key xah-fly-key-map (kbd "<return>") nil)
(define-key xah-fly-key-map (kbd "C") nil)
(define-key xah-fly-key-map (kbd "X") nil)
(define-key xah-fly-key-map (kbd "M-i") nil)
(define-key xah-fly-key-map (kbd "M-k") nil)
(define-key xah-fly-key-map (kbd "M-j") nil)
(define-key xah-fly-key-map (kbd "M-l") nil)
(define-key xah-fly-key-map (kbd "ø") nil)
(define-key xah-fly-key-map (kbd "M-I") nil)
(define-key xah-fly-key-map (kbd "M-K") nil)
(define-key xah-fly-key-map (kbd "M-J") nil)
(define-key xah-fly-key-map (kbd "M-L") nil)
(define-key xah-fly-key-map (kbd "æ") nil)
(define-key xah-fly-key-map (kbd "I") nil)
(define-key xah-fly-key-map (kbd "K") nil)
(define-key xah-fly-key-map (kbd "J") nil)
(define-key xah-fly-key-map (kbd "L") nil)

(defvar xah-fly-insert-state-before-minibuffer-q nil
  "value xah-fly-insert-state-q before entering minibuffer")

(defun xah-minibuffer-setup-hook ()
  (setq xah-fly-insert-state-before-minibuffer-q xah-fly-insert-state-q)
  (xah-fly-insert-mode-activate)
  )

(defun xah-minibuffer-exit-hook ()
  (if xah-fly-insert-state-before-minibuffer-q
      (xah-fly-insert-mode-activate)
    (xah-fly-command-mode-activate)
    )
  )

(defun copy-to-other-window ()
  (interactive)
  (if (use-region-p)
      (copy-region-as-kill (region-beginning) (region-end))
    (copy-region-as-kill (line-beginning-position) (line-end-position))
    (end-of-line)
    (forward-char))
  (with-selected-window (next-window)
    (yank)
    (insert "\n"))
  )
(define-key xah-fly-r-keymap "v" 'copy-to-other-window)
