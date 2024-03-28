;;; ui/side-window/autoload/side-window.el

;;;###autoload
(defun delete-other-not-side-windows ()
  (interactive)
  (let* ((main-window (window-main-window))
         (main-window-buffer (window-buffer main-window))
         (window (selected-window))
         (buffer (current-buffer)))
    (when (and main-window-buffer
               (not (eq main-window window)))
      (set-window-buffer main-window buffer)
      (select-window (window-main-window))))
  (mapc #'delete-window
        (delq (selected-window)
              (seq-filter (lambda (window)
                            (not (window-parameter window 'window-side)))
                          (window-list)))))

;;;###autoload
(defun my/open-side-info-windows ()
  "Open info buffers at sides."
  (interactive)
  (when-let ((buffer (messages-buffer))
             (window (side-window-open-by-buffer buffer 'left)))
    (with-current-buffer buffer
      (toggle-truncate-lines 0)
      (display-line-numbers-mode 1)
      (set-window-point window (point-max))))
  (when-let* ((buffer (get-buffer "*lsp-log*"))
              (window (side-window-open-by-buffer buffer 'top)))
    (with-current-buffer buffer
      (set-window-point window (point-max))))
  (when-let ((buffer (get-buffer "*lsp session*")))
    (side-window-open-by-buffer buffer 'left))
  (when-let ((buffer (get-buffer "*Ibuffer*")))
    (side-window-open-by-buffer buffer 'left)))
