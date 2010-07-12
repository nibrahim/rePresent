;; Maps, faces, hooks and font-lock settings
(defvar rePresent-mode-map
  (let 
      ((rePresent-mode-map (make-keymap)))
    (define-key rePresent-mode-map (kbd "<next>") 'rePresent/next-bullet)
    (define-key rePresent-mode-map (kbd "<prior>") 'rePresent/previous-bullet)
    (define-key rePresent-mode-map (kbd "SPC") 'rePresent/next-slide)
    (define-key rePresent-mode-map (kbd "<backspace>") 'rePresent/previous-slide)
    (define-key rePresent-mode-map (kbd "q") 'rePresent/quite-presentation)
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

;; Navigation functions

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
	(rePresent/jump-to-first-bullet))
  ; Go to the next line and activate it if it's a valid bullet
  (rePresent/activate-current-bullet)  
  (if (save-excursion
	(forward-line 1)
	(looking-at "\\+\\|-"))
      (forward-line 1)))

(defun rePresent/previous-bullet ()
  "Jumps to the previous bullet."
  (interactive)
  (represent/deactivate-current-bullet)
  (if (save-excursion
	(forward-line -1)
	(looking-at "\\+\\|-"))
      (forward-line -1)))


(defun rePresent/next-slide ()
  "Goes to the next slide"
  (interactive)
  (if (not (and (boundp 'current-slide)
		(boundp 'rePresent/slide-deck)))
      (error "Can't change slides before initialising deck"))
  (if (not (eq current-slide (- (length rePresent/slide-deck) 1)))
      (progn
	(setq current-slide (+ current-slide 1))
	(switch-to-buffer (nth current-slide rePresent/slide-deck)))))

(defun rePresent/previous-slide ()
  "Goes to previous slide"
  (interactive)
  (if (not (and (boundp 'current-slide)
		(boundp 'rePresent/slide-deck)))
      (error "Can't change slides before initialising deck"))
  (if (not (eq current-slide 0))
      (progn
	(setq current-slide (- current-slide 1))
	(switch-to-buffer (nth current-slide rePresent/slide-deck)))))

;; Functions to load a slide deck and create the slides
(defun rePresent/centre-line ()
  (interactive)
    (let
	((fill-column (frame-width)))
      (progn
	(center-line))))
	;; (replace-string "	" " " nil (point-at-bol) (point-at-eol)))))


(defun rePresent/create-bullet-slide (contents)
  (let
      ((spec contents)
       (slide nil))
    (while spec
      (cond ((eq (car spec) :title)
	     (progn
	       (setq slide (set-buffer (get-buffer-create (cadr spec))))
	       (rePresent-mode)
	       (goto-char (point-min))
	       (message (concat "rePresent : Creating slide " (cadr spec)))
	       (insert (concat "* " (cadr spec) "\n\n"))))
	    ((eq (car spec) :bullets)
	     (progn 
	       (dolist (bullet (cadr spec))
		 (insert (concat "+ " bullet "\n"))))
	     (goto-char (point-min))))
      (setq spec (cddr spec)))
    slide
    ))

(defun rePresent/load-file (fname)
  "Load a presentation file and convert it into a series of slide buffers"
  (makunbound 'presentation)
  (load-file fname)
  (if (not (boundp 'presentation))
      (error "Invalid presentation file"))
  (let
      ((slides '()))
    (dolist (sl presentation (reverse slides))
      (if (eq (car sl) 'slide)
	  (setq slides (append 
			(list (rePresent/create-bullet-slide (cdr sl)))
			slides))))))

(defun rePresent/start-presentation (fname)
  "Load up the presentation file, create the slides, make
   necessary face/colour changes and start the presentation"
  (interactive "frePresent presentation to load : ")
  (setq rePresent/slide-deck (rePresent/load-file fname))
  (setq current-slide 0)
  (switch-to-buffer (nth current-slide rePresent/slide-deck))
  )

(defun rePresent/quite-presentation ()
  (interactive)
  (while rePresent/slide-deck
    (kill-buffer (pop rePresent/slide-deck)))
  (makunbound 'rePresent/slide-deck)
  (makunbound 'current-slide))



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

  
  
  
