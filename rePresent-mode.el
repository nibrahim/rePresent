;; Maps, faces, hooks and font-lock settings
(defvar rePresent-mode-map
  (let 
      ((rePresent-mode-map (make-keymap)))
    (define-key rePresent-mode-map (kbd "<next>") 'rePresent/next-bullet)
    (define-key rePresent-mode-map (kbd "<prior>") 'rePresent/previous-bullet)
    rePresent-mode-map)
  "Keymap for rePresent major mode")

(defface rePresent-header-face 
  '(
    (((type tty) (class color)) (:foreground "Blue" :background "White" :height 350))
    (((type graphic) (class color)) (:foreground "Blue" :background "White" :height 350))
    (t (:foreground "Blue" :background "White" :height 350 ))
    )
  "Face for a slide header")

(defface rePresent-bullet-visible-face 
  '(
    (((type tty) (class color)) (:foreground "Green" :background "White" :height 250))
    (((type graphic) (class color)) (:foreground "Green" :background "White" :height 250))
    (t (:foreground "Green" :background "White" :height 250 ))
    )
  "Face for visible slide bullet")

(defface rePresent-bullet-hidden-face 
  '(
    (((type tty) (class color)) (:foreground "Black" :background "Black" :height 250))
    (((type graphic) (class color)) (:foreground "Black" :background "Black" :height 250))
    (t (:foreground "Black" :background "Black" :height 250 ))
    )
  "Face for visible slide bullet")

(defvar rePresent-header-face 'rePresent-header-face "Face for slide header")
(defvar rePresent-bullet-visible-face 'rePresent-bullet-visible-face "Face for visible slide bullet")
(defvar rePresent-bullet-hidden-face 'rePresent-bullet-hidden-face "Face for hidden slide bullet")

(defconst rePresent-font-lock-keywords
  (list
   '("^\\*.*" . rePresent-header-face)
   '("^\\-.*" . rePresent-bullet-visible-face)
   '("^\\+.*" . rePresent-bullet-hidden-face)
   ))

(defvar rePresent-mode-hook nil
  "Hook called by \"rePresent-mode\"")

;; Utility functions
(defun rePresent/centre-line ()
  (interactive)
    (let
	((fill-column (frame-width)))
      (progn
	(center-line))))
	;; (replace-string "	" " " nil (point-at-bol) (point-at-eol)))))

(defun rePresent/activate-current-bullet ()
  "Activates the current bullet if it's a bullet line"
  (save-excursion
    (beginning-of-line)
    (if (not (looking-at "^\\+\\|-"))
	(error "Not at a bullet line")
      (delete-char 1)
      (insert-char ?- 1))))

(defun represent/deactivate-current-bullet ()
  "Activates the current bullet if it's a bullet line"
  (save-excursion
    (beginning-of-line)
    (if (not (looking-at "^\\+\\|-"))
	(error "Not at a bullet line")
      (delete-char 1)
      (insert-char ?+ 1))))


(defun rePresent/jump-to-first-bullet ()
  "Goes to the first bullet on the current slide"
  (goto-char (point-min))
  (search-forward-regexp "^\\+\\|-"))


(defun rePresent/next-bullet ()
  "Jumps to the next bullet."
  (interactive)
  ; Jump to the first bullet if we're not at a bullet line
  (if (not (save-excursion
	     (beginning-of-line)
	     (looking-at "\\+\\|-")))
      (progn
	(rePresent/jump-to-first-bullet)
	(forward-line -1)))
  ; Go to the next line and activate it
  (forward-line 1)
  (rePresent/activate-current-bullet)
  ; If we've gone beyond the last one, return to the last one
  (if (not (save-excursion
	     (beginning-of-line)
	     (looking-at "\\+\\|-")))
      (forward-line -1)))

(defun rePresent/previous-bullet ()
  "Jumps to the previous bullet."
  (interactive)
  (represent/deactivate-current-bullet)
  (forward-line -1))



;; Entry points and public interfaces.
(defun rePresent-mode ()
    "Major mode for Emacs Presentations"
    (interactive)
    (kill-all-local-variables)
    (use-local-map rePresent-mode-map)
    (set (make-local-variable 'font-lock-defaults) '(rePresent-font-lock-keywords))
    (setq major-mode 'represent-mode
	  mode-name "rePresent")
    (run-hooks rePresent-mode-hook)
    )

(provide 'rePresent-mode)  
  
  
  
  
