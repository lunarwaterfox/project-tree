;; customization
(defgroup project-tree nil
  "Support a project tree side bar."
  :group 'environment)

(defcustom project-tree-buffer-name "*project-tree*"
  "Project tree buffer name."
  :type 'string
  :group 'project-tree)

(defcustom project-tree-default-width 30
  "Project tree side window width"
  :type 'integer
  :group 'project-tree)

;; variables
(defvar project-tree-buffer nil
  "Project tree buffer.")

(defvar project-tree-window nil
  "Project tree window.")

(defvar project-tree-target-window nil
  "Project tree target window.")

(defvar project-tree-home-dir nil
  "Project tree home directory.")

(defvar project-tree-root nil
  "Project tree root node.")

;; utilise functions
(defun project-tree-exist-p ()
  "Return `not-nil' if `project-tree' exist."
  (and (buffer-live-p project-tree-buffer)
       (window-live-p project-tree-window)))

(defun project-tree-query-home-dir ()
  "Querry project tree home director."
  (read-directory-name "Setup home directory: " default-directory nil t))




(defun project-tree-get-file-property (dir file)
  "Create structure for file."
  (let ((is-dir (file-directory-p (concat (file-name-as-directory dir) file))))
    (list file 'is-dir is-dir 'sub-list nil)))

(defun project-tree-list-dir (dir)
  "Create structure for dir."
  (let ((files (directory-files dir nil directory-files-no-dot-files-regexp))
        (struc-list ()))
    (dolist (file files)
      (push (project-tree-get-file-property dir file) struc-list))
    struc-list))

(defun project-tree-update ()
  "Update project tree."
  (setq project-tree-root (project-tree-list-dir project-tree-home-dir)))





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


(defun project-tree-refresh-buffer ()
  "Refresh project tree buffer."
  (with-current-buffer project-tree-buffer
    (read-only-mode 0)
    (erase-buffer)
    (project-tree-export-content project-tree-root nil)
    (read-only-mode 1)))




(defun project-tree-node-click (path-list)
  "Create project tree at path."
  (let ((dir project-tree-home-dir)
        (nodes project-tree-root)
        (node))
    (dolist (path path-list)
      (setq dir (concat (file-name-as-directory dir) path))
      (setq node (assoc path nodes))
      (setq nodes (plist-get (cdr node) 'sub-list)))
    (message (car node))
    (if (not (plist-get (cdr node) 'is-dir))
        (progn
          (select-window project-tree-target-window)
          (find-file dir))
      (let ((sub-list (plist-get (cdr node) 'sub-list)))
        (if sub-list
            (setcdr node (plist-put (cdr node) 'sub-list nil))
          (let ((sub (project-tree-list-dir dir)))
            (setcdr node (plist-put (cdr node) 'sub-list sub))))))))


(defun project-tree-line-click (btn)
  "A line button clicked."
  (let ((args (button-get btn 'help-args)))
    (project-tree-node-click (reverse args)))
  (project-tree-refresh-buffer))

;; interactive

(defun project-tree-open ()
  "Open project side bar."
  (interactive)
  (setq project-tree-target-window (selected-window))
  (unless project-tree-home-dir
    (setq project-tree-home-dir (project-tree-query-home-dir)))
  (unless (buffer-live-p project-tree-buffer)
    (setq project-tree-buffer (get-buffer-create project-tree-buffer-name)))
  (project-tree-update)
  (project-tree-refresh-buffer)
  (unless (window-live-p project-tree-window)
    (let ((width-param (- project-tree-default-width)))
      ;;      (setq project-tree-window (split-window project-tree-target-window width-param 'left))
      ;;      (set-window-buffer project-tree-window project-tree-buffer))))
      (setq project-tree-window (display-buffer-in-side-window project-tree-buffer '((side . left)))))))

(defun project-tree-close ()
  "Close project side bar."
  (interactive)
  (when (window-live-p project-tree-window)
    (delete-window project-tree-window)))
  
(defun project-tree-toggle ()
  "Toggle project side bar."
  (interactive)
  (if (project-tree-exist-p)
      (project-tree-close)
    (project-tree-open)))

;; minor mode

(defcustom project-tree-dnd-protocol-alist
  '(("^file:"    . project-tree-dnd-handle-local-file))
    ;("^file:///" . dired-dnd-handle-local-file)
    ;("^file://"  . dired-dnd-handle-file)
  "Project tree buffer name."
  :type '(alist :key-type stringl :value-type function)
  :group 'project-tree)


(defun project-tree-dnd-handle-local-file (uri _action)
  "Open a local file. The file is opened in the current window."
  (let ((path (dnd-get-local-file-name uri t)))
    (if (not (file-directory-p path))
        (dnd-open-local-file uri _action)
      (setq project-tree-home-dir path)
      (project-tree-open))))



(defvar project-tree-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-xt" 'project-tree-toggle)
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
