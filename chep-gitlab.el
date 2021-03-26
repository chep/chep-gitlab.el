
(require 'gitlab)
(require 'helm)

(defgroup chep-gitlab nil
  "Customization of chep-gitlab variables."
  :tag "chep-gitlab"
  :group 'chep)

(defcustom chep-gitlab-token-file "~/.config/gitlab/token"
  "Gitlab token file"
  :tag "Gitlab token file"
  :type 'number
  :group 'chep-gitlab)

(defcustom chep-gitlab-default-project-id 382
  "Default project ID for gitlab"
  :tag "Default project ID for gitlab"
  :type 'number
  :group 'chep-gitlab)

(defface chep-helm-gitlab--title
  '((((class color) (background light)) :foreground "red" :weight semi-bold)
    (((class color) (background dark)) :foreground "green" :weight semi-bold))
  "Face of Gitlab information"
  :group 'chep-gitlab)

(defvar chep-gitlab-project-filter ""
  "Filter for projects")

(defvar chep-helm-gitlab-current-project-id nil
  "Default project ID for gitlab")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gitlab functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun chep-gitlab-list-user-issues (&optional params)
  "Get a list of user issues.
PARAMS: an alist for query parameters. Exple: '((state . \"opened\"))"
  (let* ((page 0)
         (per-page 100)
         (parameters (list
                      (cons 'scope "assigned_to_me")
                      (cons 'per_page (number-to-string per-page))))
         (all-issues (list))
         (all-issues-count 0))
    (when params
      (setq parameters (append parameters params)))
    (while (>= all-issues-count (* page per-page))
      (setq page (1+ page))
      (let* ((p (append parameters
                        (list (cons 'page
                              (number-to-string page)))))
             (issues (perform-gitlab-request "GET"
                                             "issues"
                                             p
                                             200)))
        (setq all-issues (vconcat all-issues issues)))
      (setq all-issues-count (length all-issues)))
    all-issues))

(defun chep-gitlab-list-project-issues (project-id &optional params)
  "Get a list of all PROJECT-ID issues.
PARAMS: an alist for query parameters. Exple: '((state . \"opened\"))"
  (let* ((page 0)
         (per-page 100)
         (parameters (append (list
                              (cons 'scope "all"))
                             params))
         (all-issues (list))
         (all-issues-count 0))
    (while (>= all-issues-count (* page per-page))
      (setq page (1+ page))
      (let ((issues (gitlab-list-project-issues project-id
                                                page
                                                per-page
                                                parameters)))
        (setq all-issues (vconcat all-issues issues)))
      (setq all-issues-count (length all-issues)))
    all-issues))


(defun chep-gitlab-list-projects (&optional page per-page)
  "Get a list of projects accessible by the authenticated user.
PAGE: current page number
PER-PAGE: number of items on page max 100"
  (let* ((params (list (cons 'search chep-gitlab-project-filter)
                       (cons 'membership "yes"))))
    (when page
      (add-to-list 'params (cons 'per_page (number-to-string per-page))))
    (when per-page
      (add-to-list 'params (cons 'page (number-to-string page))))
    (print params t)
    (perform-gitlab-request "GET"
                            "projects"
                            params
                            200)))

(defun chep-gitlab-list-all-projects ()
  "Get a list of all projects accessible by the authenticated user."
  (interactive)
    (let* ((page 1)
           (per-page 100)
           (projects)
           (all-projects (chep-gitlab-list-projects page per-page))
           (all-projects-count (length all-projects)))
      (while (>= all-projects-count (* page per-page))
        (setq projects (chep-gitlab-list-projects page per-page))
        (setq all-projects (vconcat all-projects projects))
        (setq all-projects-count (length all-projects))
        (setq page (1+ page)))
      all-projects))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun chep-helm-gitlab--issue-browse-link (cand)
  (browse-url-default-browser
   (gitlab-projects--get-issue-link (plist-get cand :project-id)
                                    (plist-get cand :issue-id))))

(defun chep-helm-gitlab--read-token ()
  (setq gitlab-token-id (with-temp-buffer
                          (insert-file-contents chep-gitlab-token-file)
                          (buffer-substring-no-properties
                           (point-min)
                           (point-max)))))

(defun chep-helm-gitlab--issues-init (closed-too)
  (with-gitlab-auth
   (let* ((params (unless closed-too
                    '((state . "opened"))))
          (issues (chep-gitlab-list-user-issues params)))
     (mapcar (lambda (i)
               (cons (format "[%s] %s [%s]"
                             (assoc-default 'iid i)
                             (propertize (assoc-default 'title i)
                                         'face
                                         'chep-helm-gitlab--title)
                             (assoc-default 'state i))
                     (list :project-id (assoc-default 'project_id i)
                           :issue-id (assoc-default 'iid i)
                           :name (assoc-default 'title i))))
             issues))))

(defun chep-helm-gitlab--issues-source (closed-too)
  (list (cons 'name "Gitlab user issues")
        (cons 'candidates (chep-helm-gitlab--issues-init closed-too))
        (cons 'action '(("Browse Link" . chep-helm-gitlab--issue-browse-link)))))

(defun chep-helm-gitlab-issues (closed-too)
  "Browse assigned issues
Use prefix argument to see closed issues too"
  (interactive "P")
  (chep-helm-gitlab--read-token)
  (helm :sources (chep-helm-gitlab--issues-source closed-too)))



(defun chep-helm-gitlab--project-issues-init (project-id closed-too)
  (with-gitlab-auth
   (let* ((params (unless closed-too
                    (list (cons 'state  "opened"))))
          (issues (chep-gitlab-list-project-issues project-id
                                                   params)))
     (mapcar (lambda (i)
               (cons (format "[%s] %s [%s]"
                             (assoc-default 'iid i)
                             (propertize (assoc-default 'title i)
                                         'face
                                         'chep-helm-gitlab--title)
                             (assoc-default 'state i))
                     (list :project-id (assoc-default 'project_id i)
                           :issue-id (assoc-default 'iid i)
                           :name (assoc-default 'title i))))
             issues))))

(defun chep-helm-gitlab--project-issues-source (project-id closed-too)
  (list (cons 'name "Gitlab project issues")
        (cons 'candidates (chep-helm-gitlab--project-issues-init project-id closed-too))
        (cons 'action '(("Browse Link" . chep-helm-gitlab--issue-browse-link)))))



(defun chep-helm-gitlab--project-select-init ()
  (with-gitlab-auth
   (let ((projects (chep-gitlab-list-all-projects)))
     (mapcar (lambda (p)
               (cons (format "[%s] %s (%s)"
                             (assoc-default 'id p)
                             (propertize (assoc-default 'name p)
                                         'face
                                         'helm-gitlab--title)
                             (assoc-default 'path_with_namespace p))
                     (list :page (assoc-default 'web_url p)
                           :name (assoc-default 'name p)
                           :project-id (assoc-default 'id p))))
             projects))))


(defun chep-helm-gitlab--project-select-source ()
  (list (cons 'name "Gitlab project")
        (cons 'candidates (chep-helm-gitlab--project-select-init))
        (cons 'action '(("Select" . (lambda (project)
                                      (setq chep-helm-gitlab-current-project-id
                                            (plist-get project :project-id))))))))

(defun chep-helm-gitlab-issues-project (closed-too)
  "Browse default project issues.
chep-gitlab-default-project-id is used to select the project
Use prefix argument to see closed issues too."
  (interactive "P")
  (chep-helm-gitlab--read-token)
  (helm :sources (chep-helm-gitlab--project-issues-source
                  chep-gitlab-default-project-id
                  closed-too)))

(defun chep-helm-gitlab-issues-project-specific (closed-too)
  "Browse specific project issues.
chep-gitlab-default-project-id is used to select the project
Use prefix argument to see closed issues too."
  (interactive "P")
  (chep-helm-gitlab--read-token)
  (setq chep-gitlab-project-filter (read-from-minibuffer "project filter: "))
  (setq chep-helm-gitlab-current-project-id chep-gitlab-default-project-id)
  (helm :sources (chep-helm-gitlab--project-select-source))
  (helm :sources (chep-helm-gitlab--project-issues-source
                  chep-helm-gitlab-current-project-id
                  closed-too)))

(provides 'chep-gitlab)
