;;; lang/treesit-f/autoload/treesit-f.el

;;;###autoload
(defun copy-outer-class-name-at-point ()
  "Copy the outer class name at point to the kill ring."
  (interactive)
  (if-let ((name (find-outter-class-name-at-point)))
      (progn (kill-new name)
             (message "Copied class name: %s" name))
    (message "Cannot find and class name from point" name)))

;;;###autoload
(defun treesit-move-declaration-up (ARG)
  (interactive "P")
  (treesit-move-declaration (- (prefix-numeric-value ARG))))

;;;###autoload
(defun treesit-move-declaration-down (ARG)
  (interactive "P")
  (treesit-move-declaration (prefix-numeric-value ARG)))
