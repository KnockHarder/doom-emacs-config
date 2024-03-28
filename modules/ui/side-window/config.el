;; ui/side-window/config.el

(defun side-window-new-slot-auto (buffer side)
  "Get max window slot on the side"
  (let* ((major (window-with-parameter 'window-side side nil t))
         windows next)
    (cond
     ((window-live-p major)
      (setq windows (list major)))
     ((window-valid-p major)
      (setq next (window-child major) windows (list next))
      (while (setq next (window-next-sibling next))
        (add-to-list 'windows next))))
    (if windows
        (if-let ((founds (seq-filter (lambda (w)
                                       (equal (window-buffer w) buffer))
                                     windows)))
            (window-parameter (car founds) 'window-slot)
          (1+ (apply #'max (mapcar (lambda (w) (window-parameter w 'window-slot)) windows))))
      0)))

(defun side-window-open-by-buffer (buf side)
  "Open side window with BUF at SIDE"
  (let* ((widown-width (if (memq side  (list 'left 'right)) 35 nil))
         (window-height (if (memq side (list 'top 'bottom)) 10 nil))
         (slot (side-window-new-slot-auto buf side)))
    (display-buffer buf `(display-buffer-in-side-window
                          .
                          (
                           (side . ,side)
                           (window-width . ,widown-width)
                           (window-height . ,window-height)
                           (slot . ,slot)
                           (dedicated . t)
                           (window-parameters . ((no-other-window . t))))))))

(global-set-key (kbd "C-x 1") #'delete-other-not-side-windows)
(add-hook! 'doom-first-buffer-hook #'my/open-side-info-windows)
