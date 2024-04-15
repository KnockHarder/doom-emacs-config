;;; lang/treesit-f/config.el

(defun treesit-parent-util-type (NODE TYPE)
  "Return the closest parent of NODE by TYPE."
  (treesit-parent-until NODE (lambda (node)
                               (string-equal (treesit-node-type node) TYPE))))

(defun find-outter-class-name-at-point ()
  "Return the class name of the class that the point is in."
  (when-let* ((node (treesit-node-at (point)))
              (node (treesit-parent-util-type node "class_declaration"))
              (class-name-node (treesit-node-child-by-field-name node "name")))
    (treesit-node-text class-name-node t)))

(defun treesit-declaration-node-start (NODE)
  (let ((start (treesit-node-start NODE))
        (prev-node NODE))
    (while (and prev-node
                (setq prev-node (treesit-node-prev-sibling prev-node))
                (setq prev-type (treesit-node-type prev-node))
                (not (equal prev-type "{"))
                (not (member prev-type declaration-types)))
      (setq start (treesit-node-start prev-node)))
    start))

(defun treesit-move-declaration (NUM)
  "Move the declaration where the point is in, up or down by NUM."
  (when-let* ((declaration-types '("method_declaration"
                                   "field_declaration"
                                   "class_declaration"
                                   "constructor_declaration"))
              (current-point (point))
              (point-node (treesit-node-at current-point))
              (node (treesit-parent-util-type point-node "method_declaration"))
              (start1 (treesit-declaration-node-start node))
              (end1 (treesit-node-end node))
              (done (gensym "done")))
    (let ((i 0)
          start2 end2 prev-node prev-type next-node next-type)
      (cond
       ((< NUM 0) (while (and (< i (- NUM))
                              (setq prev-node (treesit-node-prev-sibling node))
                              (setq prev-type (treesit-node-type prev-node))
                              (not (equal prev-type "{")))
                    (unless end2
                      (setq end2 (treesit-node-end prev-node)))
                    (when (member prev-type declaration-types)
                      (setq i (1+ i))
                      (setq start2 (treesit-declaration-node-start prev-node)))
                    (setq node prev-node)))
       ((> NUM 0) (while (and (< i NUM)
                              (setq next-node (treesit-node-next-sibling node))
                              (setq next-type (treesit-node-type next-node))
                              (not (equal next-type "}")))
                    (unless start2
                      (setq start2 (treesit-node-start next-node)))
                    (when (member next-type declaration-types)
                      (setq end2 (treesit-node-end next-node))
                      (setq i (1+ i)))
                    (setq node next-node))))
      (when (and start2 end2)
        (transpose-regions start1 end1 start2 end2)))))

(use-package java-ts-mode
  :bind
  (:map java-ts-mode-map
        ("C-c C-p" . #'treesit-beginning-of-defun)
        ("C-c C-n" . #'treesit-end-of-defun)))
