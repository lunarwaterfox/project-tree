(defgroup project-tree nil
  "Support a project tree side bar."
  :group 'environment)

(defcustom project-tree-buffer-name "*project-tree*"
  "Project tree buffer name."
  :type 'string
  :group 'project-tree)

(defcustom project-tree-home-key "project-tree"
  "Project tree buffer name."
  :type 'string
  :group 'project-tree)


(defvar project-tree-dir-tree nil)
(defvar project-tree-side-window nil)

(defun project-tree-get-file-struc (dir file)
  "Create structure for file."
  (let ((is-dir (file-directory-p (concat (file-name-as-directory dir) file))))
    (list file 'is-dir is-dir 'sub-list nil)))

(defun project-tree-get-dir-struc (dir)
  "Create structure for dir."
  (let ((files (directory-files dir nil directory-files-no-dot-files-regexp))
        (struc-list ()))
    (dolist (file files)
      (push (project-tree-get-file-struc dir file) struc-list))
    struc-list))


(defun project-tree-create-tree (path-list)
  "Create project tree at path."
  (let ((dir (project-home-dir-by-key project-tree-home-key))
        (node))
    (dolist (path path-list)
      (setq dir (concat (file-name-as-directory dir) path))
      (setq node (assoc path project-tree-dir-tree)))
    (let ((sub-list (project-tree-get-dir-struc dir)))
      (if (not path-list)
          (setq project-tree-dir-tree sub-list)
        (setcdr node (plist-put (cdr node) 'sub-list sub-list))
        project-tree-dir-tree))))

(defun project-tree-export-line (node level)
  "Export sting line for the node"
  (let ((indent (make-string (* level 2) ? ))
        (name (car node))
        (plist (cdr node))
        (sign))
    (if (plist-get plist 'is-dir)
        (if (plist-get plist 'sub-list)
            (setq sign "- ")
          (setq sign "+ "))
      (setq sign "  "))
    (concat indent sign name "\n")))

(defun project-tree-export-content (nodes level)
  "Export content for the node"
  (let ((content ""))
    (dolist (node nodes)
      (setq content (concat content (project-tree-export-line node level)))
      (let ((sub (plist-get (cdr node) 'sub-list)))
        (when sub
          (setq content (concat content (project-tree-export-content sub (+ level 1)))))))
    content))
  

(defun project-tree-show-side-bar ()
  "Show project side bar."
  (interactive)
  (unless project-tree-dir-tree
    (project-tree-create-tree nil))
  (let ((buffer (get-buffer-create project-tree-buffer-name)))
    (with-current-buffer buffer
      (dolist (node project-tree-dir-tree)
        (insert (project-tree-export-line node 0))))
    (setq project-tree-side-window (display-buffer-in-side-window buffer '((side . left))))))

(defun project-tree-close-side-bar ()
  "Close project side bar."
  (interactive)
  (quit-window nil project-tree-side-window)
  (setq project-tree-side-window nil))


(defun project-tree-trigger-side-bar ()
  "Trigger project side bar."
  (interactive)
  (if project-tree-side-window
      (project-tree-close-side-bar)
    (project-tree-show-side-bar)))

(provide 'project-tree)
