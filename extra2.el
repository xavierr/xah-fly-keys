;; Additional functions

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


(defun xav-append-copy-line-or-region ()
  (interactive)
  (append-next-kill)
  (if (use-region-p)
      ()
    (kill-append "\n" nil))
  (xah-copy-line-or-region)
  )

(defun xav-append-cut-line-or-region ()
  (interactive)
  (append-next-kill)
  (if (use-region-p)
      ()
    (kill-append "\n" nil))
  (xah-cut-line-or-region)
  )

(defun xah-copy-dir-path ()
  (interactive)
  (xah-copy-file-path t)
  )


(setq xah-fly-use-control-key nil)

(define-key xah-fly-insert-map [escape] 'xah-fly-command-mode-activate)
(define-key xah-fly-insert-map (kbd "<insertchar>") 'xah-fly-command-mode-activate)
(define-key xah-fly-insert-map (kbd "<f1>") 'xah-fly-command-mode-activate)

(defun xah-fly-define-keys-extra ()
  "Define the keys for xah-fly-keys.
Created: 2022-10-31
Version: 2024-04-22"
  (interactive)
  (let ()
    (xah-fly--define-keys
     xah-fly-leader-key-map
     '(("<" . xah-fly--tab-key-map)
       ("0" . quicklink-keymap)
       ("a" . exchange-point-and-mark)
       ("g" . swiper-isearch)
       ("o" . save-buffer)
       ("M-i" . 'delete-window-up)
       ("M-k" . 'delete-window-down)
       ("M-j" . 'delete-window-left)
       ("M-l" . 'delete-window-right)
       ("RET" . 'newline)
       ("<return>" . 'newline)

       
       ("c a" . write-file)
       ("c ." . xah-open-file-fast)
       ("c u" . switch-to-buffer-other-window)
       ("c g" . find-file-other-window)
       ("c d" . xah-copy-dir-path)
       ("c m" . xavier-copy-filename)
       ("c r" . find-file)
       ("M-i" . split-window-vertically)
       ("M-k" . split-window-vertically)
       ("M-j" . split-window-horizontally)
       ("M-l" . split-window-horizontally)
       ("r"   . isearch-forward-regexp)

       ("r e" . xavier/call-last-kbd-macro)
       ("r v" . copy-to-other-window)
       
       ("t e" . my-counsel-fzf)
       ("t i" . org-recoll-search)
       ("t r" . occur)
       ("t u" . my-counsel-rg)
       ("t k") . xah-query-replace-current-word
    ))))

(xah-fly-define-keys-extra)

(define-key xah-fly-leader-key-map (kbd "M-i") 'delete-window-up)
(define-key xah-fly-leader-key-map (kbd "M-k") 'delete-window-down)
(define-key xah-fly-leader-key-map (kbd "M-j") 'delete-window-left)
(define-key xah-fly-leader-key-map (kbd "M-l") 'delete-window-right)
(define-key xah-fly-leader-key-map (kbd "+") 'org-agenda)

(define-key xah-fly-leader-key-map (kbd "k y") 'xah-query-replace-current-word)
(define-key xah-fly-leader-key-map (kbd "RET") 'newline)
(define-key xah-fly-leader-key-map (kbd "<return>") 'newline)

(define-key xah-fly-command-map (kbd "2") 'my-mc-start)
(define-key xah-fly-command-map (kbd "<return>") 'xah-fly-insert-mode-activate)
(define-key xah-fly-command-map (kbd "F") 'swiper-isearch-backward)
(define-key xah-fly-command-map (kbd "C") 'xav-append-copy-line-or-region)
(define-key xah-fly-command-map (kbd "X") 'xav-append-cut-line-or-region)
(define-key xah-fly-command-map (kbd "d") 'delete-char)
(define-key xah-fly-command-map (kbd "ø") 'xah-end-of-line-or-block)
(define-key xah-fly-command-map (kbd "M-i") 'my-windmove-up)
(define-key xah-fly-command-map (kbd "M-k") 'my-windmove-down)
(define-key xah-fly-command-map (kbd "M-j") 'my-windmove-left)
(define-key xah-fly-command-map (kbd "M-l") 'my-windmove-right)

;; (define-key xah-fly-key-map (kbd "F") nil)
;; (define-key xah-fly-key-map (kbd "<return>") nil)
;; (define-key xah-fly-key-map (kbd "C") nil)
;; (define-key xah-fly-key-map (kbd "X") nil)
;; (define-key xah-fly-key-map (kbd "M-i") nil)
;; (define-key xah-fly-key-map (kbd "M-k") nil)
;; (define-key xah-fly-key-map (kbd "M-j") nil)
;; (define-key xah-fly-key-map (kbd "M-l") nil)
;; (define-key xah-fly-key-map (kbd "ø") nil)
;; (define-key xah-fly-key-map (kbd "M-I") nil)
;; (define-key xah-fly-key-map (kbd "M-K") nil)
;; (define-key xah-fly-key-map (kbd "M-J") nil)
;; (define-key xah-fly-key-map (kbd "M-L") nil)
;; (define-key xah-fly-key-map (kbd "æ") nil)
;; (define-key xah-fly-key-map (kbd "I") nil)
;; (define-key xah-fly-key-map (kbd "K") nil)
;; (define-key xah-fly-key-map (kbd "J") nil)
;; (define-key xah-fly-key-map (kbd "L") nil)



(provide 'extra2)
