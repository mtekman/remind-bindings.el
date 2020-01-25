;;; omni-quotes-rememberbindings.el --- Get reminders of your bindings -*- lexical-binding: t; -*-

;; Copright (C) 2020 Mehmet Tekman <mtekman89@gmail.com>

;; Author: Mehmet Tekman
;; URL: https://github.com/mtekman/omni-quotes-rememberbindings.el
;; Keywords: outlines
;; Package-Requires: ((emacs "24.4") (omni-quotes))
;; Version: 0.2

;;; Commentary:

;; This package parses your emacs init file for use-package or
;; global-set-key calls and summarizes the bindings it detects on a
;; package by package basis.
;;
;; The package makes use of the omni-quotes package to give you
;; a small reminder during idle periods.

(defun next-usepackage-name-and-bounds ()
  "Get the name and parenthesis bounds of the next use-package"
  (search-forward "(use-package")
  (beginning-of-line)
  (let* ((bound (show-paren--default))
         (inner (nth 0 bound))
         (outer (nth 3 bound)))
    (if (not bound)
        (progn (move-end-of-line 1) nil)
      (search-forward "use-package " outer t)
      (let* ((beg (point))
             (end (progn
                    (search-forward-regexp "\\( \\|)\\|$\\)" outer)
                    (point)))
             (name (buffer-substring-no-properties beg end)))
        (goto-char outer)
        `(,name ,inner ,outer)))))

(defun next-globalkeybind ()
  "Get the binding and name of the next global-set-key"
  (search-forward "(global-set-key ") ;; throw error if no more
  (beginning-of-line) ;; get the total bounds
  (let* ((bound (show-paren--default))
         (first (nth 0 bound))
         (last (nth 3 bound)))
    (search-forward "global-set-key " last)
    (let* ((bound (show-paren--default))
           (keybf (nth 0 bound))
           (keybl (nth 3 bound))
           (keyb (buffer-substring-no-properties
                  keybf keybl)))
      (when (search-forward "kbd \"" keybl t)
        (let ((beg (point))
              (end (search-forward "\"" keybl)))
          (setq keyb (buffer-substring-no-properties
                      beg (- end 1)))))
    ;; Try to grab the command, quote or interactive
      (condition-case nofuncstart
          (progn (unless (search-forward "(interactive) " last t)
                   (unless (search-forward "'" last t)
                     (unless (search-forward "(" last t))))
                 (let* ((func
                         (buffer-substring-no-properties
                          (point) (- last 1)))
                        (package-name (get-package-from-function-name func)))
                   (end-of-line)
                   (let ((bname (format "%s → %s" keyb func)))
                     `(,package-name ,bname))))
        (error
         ;; Move to end of line and give nil
         (end-of-line))))))

(defun get-package-from-function-name (fname)
  "Get the name of the package the FUNCTION belongs to. Returns nil if none found."
  (let ((packname (symbol-file (intern fname))))
    (when packname
      (let* ((bnamext (car (last (split-string packname "/")))))
        ;; name without extension
        (car (split-string bnamext "\\."))))))


(defun get-binds-in-packagename (packinfo)
  "Return the name and bindings for the current package named and bounded by PACKINFO"
  (let ((name (nth 0 packinfo))
        (inner (nth 1 packinfo))
        (outer (nth 2 packinfo))
        (bindlist nil))
    (when inner
      (goto-char inner)
      (save-excursion
        (search-forward ":bind " outer t)
        (while (search-forward-regexp "\(\"[^)]*\" \. [^\")]*\)" outer t)
          (save-excursion
            (let* ((end (- (point) 1))
                   (sta (+ (search-backward "(") 1))
                   (juststr (buffer-substring-no-properties sta end))
                   (bin-comm (split-string juststr " . ")))
              (let* ((bin  (nth 1 (split-string (car bin-comm) "\"")))
                     (comm (car (cdr bin-comm)))
                     (psnickle (format "%s → %s" bin comm)))
                (add-to-list 'bindlist psnickle t)))))
        `(,name . (,bindlist))))))

(defun buffer-package-binds ()
  "Process entire emacs init.el for package bindings"
  (with-current-buffer "init.el"
    (save-excursion
      (goto-char 0)
      (let ((packbinds nil)
            (stop nil))
        (while (not stop)
          (condition-case err
              (let ((packinfo (next-usepackage-name-and-bounds)))
                (when (nth 1 packinfo) ;; has bounds
                  (let ((binds (get-binds-in-packagename packinfo)))
                    (message (car binds))
                    (when (nth 1 binds)
                      (add-to-list 'packbinds binds t)))))
            (error
             ;; End of file
             (setq stop t)))
          (end-of-line))
        packbinds))))


(defun buffer-global-binds ()
  "Process entire emacs init.el for global bindings"
  (with-current-buffer "init.el"
    (save-excursion
      (goto-char 0)
      (let ((globbers '())
            (stop nil))
        (while (not stop)
          (condition-case err
              (let ((glob (next-globalkeybind)))
                (when glob
                  (let ((pname (nth 0 glob))
                        (bindr (nth 1 glob)))
                    (unless (assoc pname globbers)
                      ;; Package name not in the list, add
                      (add-to-list 'globbers `'(,pname)))
                    ;; add bindings to list
                    `(nconc (cdr (assoc ,pname globbers)) '(,bindr))))))
            (error
             (setq stop t)))
          (end-of-line))
        globbers)))


(defun make-quotes (alist)
  "Convert an alist of bindings into a single string list"
  (let ((total))
    (dolist (pbind alist total)
      (let ((packname (car pbind))
            (bindings (car (cdr pbind))))
        (let ((fmt (format "%s::: %s"
                           packname
                           (mapconcat 'identity bindings "\t"))))
          (setq total (cons fmt total)))))))
