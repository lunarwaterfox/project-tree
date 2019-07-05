(defgroup project-tree nil
  "Support a project tree side bar."
  :group 'environment)

(defcustom project-tree-buffer-name "*project-tree*"
  "Project tree buffer name."
  :type 'string
  :group 'project-tree)

(defcustom project-tree-home-key 'project-tree
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



(defun project-tree-node-click (path-list)
  "Create project tree at path."
  (let ((dir (project-home-dir-by-key project-tree-home-key))
        (nodes project-tree-dir-tree)
        (node))
    (dolist (path path-list)
      (setq dir (concat (file-name-as-directory dir) path))
      (setq node (assoc path nodes))
      (setq nodes (plist-get (cdr node) 'sub-list)))
    (message (car node))
    (if (not (plist-get (cdr node) 'is-dir))
        (find-file dir)
      (let ((sub-list (plist-get (cdr node) 'sub-list)))
        (if sub-list
            (setcdr node (plist-put (cdr node) 'sub-list nil))
          (let ((sub (project-tree-get-dir-struc dir)))
            (setcdr node (plist-put (cdr node) 'sub-list sub))))))))


(defun project-tree-line-click (btn)
  "A line button clicked."
  (let ((args (button-get btn 'help-args)))
    (project-tree-node-click (reverse args)))
  (let ((buffer (get-buffer-create project-tree-buffer-name)))
    (project-tree-refresh-all)))


(defun project-tree-export-line (node path-list)
  "Export sting line for the node"
  (let ((indent (make-string (* (safe-length path-list) 2) ? ))
        (name (car node))
        (plist (cdr node))
        (sign))
    (if (plist-get plist 'is-dir)
        (if (plist-get plist 'sub-list)
            (setq sign "- ")
          (setq sign "+ "))
      (setq sign "  "))
    (insert (concat indent sign))
    (insert-text-button
     name
     'action 'project-tree-line-click
     'follow-link t
     'help-args (cons name path-list))
    (insert "\n")))


(defun project-tree-export-content (nodes path-list)
  "Export content for the node"
  (dolist (node nodes)
    (project-tree-export-line node path-list)
    (let ((sub (plist-get (cdr node) 'sub-list)))
      (when sub
          (project-tree-export-content sub (cons (car node) path-list))))))

(defun project-tree-refresh-all ()
  "Refresh current buffer."
  (let ((buffer (get-buffer-create project-tree-buffer-name)))
    (with-current-buffer buffer
      (read-only-mode 0)
      (erase-buffer)
      (project-tree-export-content project-tree-dir-tree nil)
      (read-only-mode 1))))

(defun project-tree-create-tree ()
  "Create project tree at path."
  (let* ((dir (project-home-dir-by-key project-tree-home-key))
         (dir-list (project-tree-get-dir-struc dir)))
    (setq project-tree-dir-tree dir-list)))




;; project tree side bar window

(defun project-tree-show-side-bar ()
  "Show project side bar."
  (interactive)
  (unless project-tree-dir-tree
    (project-tree-create-tree))
  (project-tree-refresh-all)
  (let ((buffer (get-buffer-create project-tree-buffer-name)))
    (setq project-tree-side-window (display-buffer-in-side-window buffer '((side . left))))))

(defun project-tree-close-side-bar ()
  "Close project side bar."
  (interactive)
  (when project-tree-side-window      
    (delete-window project-tree-side-window)
    (setq project-tree-side-window nil)))

(defun project-tree-trigger-side-bar ()
  "Trigger project side bar."
  (interactive)
  (if project-tree-side-window
      (project-tree-close-side-bar)
    (project-tree-show-side-bar)))

(defun project-tree-change-home-dir ()
  "Trigger project side bar."
  (interactive)
  (let ((home (project-home-query-home project-tree-home-key)))
    (project-tree-change-home home))
  (project-tree-show-side-bar))

;; project tree mode

(defun project-tree-change-home (dir)
  "Change home dir of project tree."
  (project-home-set-dir-for-key project-tree-home-key dir)
  (setq project-tree-dir-tree nil))


(defun project-tree-dnd-handle-local-file (uri _action)
  "Open a local file. The file is opened in the current window."
  (let ((path (dnd-get-local-file-name uri t)))
    (if (not (file-directory-p path))
        (dnd-open-local-file uri _action)
      (project-tree-change-home path)
      (project-tree-show-side-bar))))

(defcustom project-tree-dnd-protocol-alist
  '(("^file:"    . project-tree-dnd-handle-local-file))
    ;("^file:///" . dired-dnd-handle-local-file)
    ;("^file://"  . dired-dnd-handle-file)
  "Project tree buffer name."
  :type '(alist :key-type stringl :value-type function)
  :group 'project-tree)

(defvar project-tree-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-ct" 'project-tree-trigger-side-bar)
    (define-key map "\C-cc" 'project-tree-change-home-dir)
    map))

(define-minor-mode project-tree-minor-mode
  "Project tree minor mode."
  nil " PTREE"
  project-tree-mode-map
  (if project-tree-minor-mode
      (setq-local dnd-protocol-alist
                  (append project-tree-dnd-protocol-alist dnd-protocol-alist))
    (kill-local-variable 'dnd-protocol-alist)))

(defun project-tree-minor-mode-on ()
  "Project tree minor mode switch."
  (project-tree-minor-mode))

(define-globalized-minor-mode project-tree-global-mode project-tree-minor-mode project-tree-minor-mode-on)

(provide 'project-tree)
