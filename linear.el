;;; linear.el --- Linear.app integration for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author:
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (request "0.3.0") (dash "2.17.0") (s "1.12.0"))
;; Keywords: tools
;; URL: https://github.com/yourusername/linear.el

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the MIT License.

;;; Commentary:

;; linear.el provides an interface to Linear.app issue tracking from Emacs.
;; It allows you to view, create, and update issues without leaving your editor.

;;; Code:

(require 'request)
(require 'json)
(require 'dash)
(require 's)

(defgroup linear nil
  "Integration with Linear issue tracking."
  :group 'tools
  :prefix "linear-")

(defcustom linear-api-key nil
  "API key for Linear.app."
  :type 'string
  :group 'linear)

(defcustom linear-graphql-url "https://api.linear.app/graphql"
  "GraphQL endpoint URL for Linear API."
  :type 'string
  :group 'linear)

(defcustom linear-default-team-id nil
  "Default team ID to use for creating issues."
  :type 'string
  :group 'linear)

(defcustom linear-debug nil
  "Enable debug logging for Linear requests."
  :type 'boolean
  :group 'linear)

(defvar linear--cache-issues nil
  "Cache for issues.")

(defvar linear--cache-teams nil
  "Cache for teams.")

(defun linear--headers ()
  "Return headers for Linear API requests."
  (unless linear-api-key
    (error "Linear API key not set. Use M-x customize-variable RET linear-api-key"))

  ;; For personal API keys, the format is: "Authorization: <API_KEY>"
  ;; No "Bearer" prefix for personal API keys
  `(("Content-Type" . "application/json")
    ("Authorization" . ,linear-api-key)))

(defun linear--log (format-string &rest args)
  "Log message with FORMAT-STRING and ARGS if debug is enabled."
  (when linear-debug
    (apply #'message (concat "[Linear] " format-string) args)))

(defun linear--graphql-request (query &optional variables)
  "Make a GraphQL request to Linear API with QUERY and optional VARIABLES."
  (linear--log "Making GraphQL request with query: %s" query)
  (when variables
    (linear--log "Variables: %s" (prin1-to-string variables)))

  (let ((response nil)
        (error-response nil)
        (request-data (json-encode `(("query" . ,query)
                                     ,@(when variables `(("variables" . ,variables)))))))
    (linear--log "Request payload: %s" request-data)

    (request
      linear-graphql-url
      :type "POST"
      :headers (linear--headers)
      :data request-data
      :parser 'json-read
      :sync t
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (linear--log "Response received: %s" (prin1-to-string data))
                  (setq response data)))
      :error (cl-function
              (lambda (&key error-thrown response data &allow-other-keys)
                (setq error-response error-thrown)
                (linear--log "Error: %s" error-thrown)
                (linear--log "Response status: %s" (request-response-status-code response))
                (when data
                  (linear--log "Error response: %s" (prin1-to-string data))))))

    (if error-response
        (progn
          (message "Linear API error: %s" error-response)
          nil)
      response)))

(defun linear-get-teams ()
  "Get a list of teams from Linear."
  (linear--log "Fetching teams")
  (let* ((query "query { teams { nodes { id name } } }")
         (response (linear--graphql-request query)))
    (if response
        (let ((teams (cdr (assoc 'nodes (assoc 'teams (assoc 'data response))))))
          (linear--log "Retrieved %d teams" (length teams))
          (setq linear--cache-teams teams)
          teams)
      (message "Failed to retrieve teams")
      nil)))

(defun linear-select-team ()
  "Prompt user to select a team."
  (let* ((teams (or linear--cache-teams (linear-get-teams)))
         (team-names (when teams
                       (mapcar (lambda (team)
                                 (cons (cdr (assoc 'name team)) team))
                               teams)))
         (selected (completing-read "Select team: " team-names nil t)))
    (cdr (assoc selected team-names))))

(defun linear-get-issues ()
  "Get a list of issues assigned to the user."
  (linear--log "Fetching assigned issues")
  (let* ((query "query { viewer { assignedIssues { nodes { id identifier title state { name color } } } } }")
         (response (linear--graphql-request query)))
    (if response
        (if (assoc 'data response)
            (let ((issues (cdr (assoc 'nodes (assoc 'assignedIssues (assoc 'viewer (assoc 'data response)))))))
              (linear--log "Retrieved %d issues" (length issues))
              (setq linear--cache-issues issues)
              issues)
          (message "Invalid response format from Linear API")
          nil)
      (message "Failed to retrieve issues")
      nil)))

(defun linear-get-states (team-id)
  "Get workflow states for the given TEAM-ID."
  (linear--log "Fetching workflow states for team %s" team-id)
  (let* ((query "query GetWorkflowStates($teamId: String!) {
                  team(id: $teamId) {
                    states {
                      nodes {
                        id
                        name
                        color
                      }
                    }
                  }
                }")
         (variables `(("teamId" . ,team-id)))
         (response (linear--graphql-request query variables)))
    (when response
      (cdr (assoc 'nodes (assoc 'states (assoc 'team (assoc 'data response))))))))

(defun linear-get-priorities ()
  "Get priority options for Linear issues."
  ;; Linear uses integers for priorities: 0 (No priority), 1 (Urgent), 2 (High), 3 (Medium), 4 (Low)
  '(("No priority" . 0)
    ("Urgent" . 1)
    ("High" . 2)
    ("Medium" . 3)
    ("Low" . 4)))

(defun linear-get-team-members (team-id)
  "Get members for the given TEAM-ID."
  (linear--log "Fetching team members for team %s" team-id)
  (let* ((query "query GetTeamMembers($teamId: String!) {
                  team(id: $teamId) {
                    members {
                      nodes {
                        id
                        name
                        displayName
                      }
                    }
                  }
                }")
         (variables `(("teamId" . ,team-id)))
         (response (linear--graphql-request query variables)))
    (when response
      (let ((members (cdr (assoc 'nodes (assoc 'members (assoc 'team (assoc 'data response)))))))
        (linear--log "Retrieved %d team members" (length members))
        (let ((formatted-members
               (mapcar (lambda (member)
                         (cons (or (cdr (assoc 'displayName member))
                                   (cdr (assoc 'name member)))
                               (cdr (assoc 'id member))))
                       members)))
          (linear--log "Formatted team members: %s" (prin1-to-string formatted-members))
          formatted-members)))))

(defun linear-get-issue-types (team-id)
  "Get issue types for the given TEAM-ID."
  (linear--log "Fetching issue types for team %s" team-id)
  (let* ((query "query GetIssueTypes($teamId: String!) {
                  team(id: $teamId) {
                    labels {
                      nodes {
                        id
                        name
                        color
                      }
                    }
                  }
                }")
         (variables `(("teamId" . ,team-id)))
         (response (linear--graphql-request query variables)))
    (when response
      (let ((labels (cdr (assoc 'nodes (assoc 'labels (assoc 'team (assoc 'data response)))))))
        (mapcar (lambda (label)
                  (cons (cdr (assoc 'name label))
                        (cdr (assoc 'id label))))
                labels)))))

(defun linear-create-issue (title description team-id)
  "Create a new issue with TITLE, DESCRIPTION, and TEAM-ID."
  (linear--log "Creating issue: %s" title)
  (let* ((query "mutation CreateIssue($title: String!, $description: String, $teamId: String!) {
                  issueCreate(input: {title: $title, description: $description, teamId: $teamId}) {
                    success
                    issue {
                      id
                      identifier
                      title
                    }
                  }
                }")
         (variables `(("title" . ,title)
                      ("description" . ,description)
                      ("teamId" . ,team-id)))
         (response (linear--graphql-request query variables)))
    (if response
        (let ((issue-data (assoc 'issue (assoc 'issueCreate (assoc 'data response)))))
          (message "Created issue %s: %s"
                   (cdr (assoc 'identifier issue-data))
                   (cdr (assoc 'title issue-data)))
          issue-data)
      (message "Failed to create issue")
      nil)))

(defun linear-test-connection ()
  "Test the connection to Linear API."
  (interactive)
  (linear--log "Testing connection to Linear API")
  (let* ((query "query { viewer { id name } }")
         (response (linear--graphql-request query)))
    (if response
        (let ((viewer (assoc 'viewer (assoc 'data response))))
          (message "Connected to Linear as: %s" (cdr (assoc 'name viewer)))
          t)
      (message "Failed to connect to Linear API")
      nil)))

;;;###autoload
(defun linear-list-issues ()
  "Display a list of the user's assigned Linear issues."
  (interactive)
  (let* ((issues (linear-get-issues))
         (buffer (get-buffer-create "*Linear Issues*")))
    (if issues
        (with-current-buffer buffer
          (erase-buffer)
          (insert (format "%-10s %-15s %s\n" "ID" "Status" "Title"))
          (insert (make-string 70 ?-))
          (insert "\n")
          ;; Convert vector to list if needed
          (when (vectorp issues)
            (setq issues (append issues nil)))
          (dolist (issue issues)
            (let ((id (cdr (assoc 'id issue)))
                  (identifier (cdr (assoc 'identifier issue)))
                  (title (cdr (assoc 'title issue)))
                  (state (cdr (assoc 'name (assoc 'state issue))))
                  (color (cdr (assoc 'color (assoc 'state issue)))))
              (insert (format "%-10s %-15s %s\n" identifier state title))))
          (goto-char (point-min))
          (switch-to-buffer buffer))
      (message "No issues found or failed to retrieve issues"))))

;;;###autoload
(defun linear-new-issue ()
  "Create a new Linear issue with additional attributes."
  (interactive)
  ;; Select team first (needed for states, members, etc.)
  (let* ((team (if linear-default-team-id
                   (list (cons 'id linear-default-team-id))
                 (linear-select-team)))
         (team-id (cdr (assoc 'id team))))

    (if team-id
        (let* ((title (read-string "Issue title: "))
               (description (read-string "Description: "))

               ;; Get workflow states
               (states (linear-get-states team-id))
               (state-options (when states
                                (mapcar (lambda (state)
                                          (cons (cdr (assoc 'name state))
                                                (cdr (assoc 'id state))))
                                        states)))
               (selected-state (when state-options
                                 (cdr (assoc (completing-read "State: " state-options nil t)
                                             state-options))))

               ;; Get priorities
               (priority-options (linear-get-priorities))
               (selected-priority (cdr (assoc (completing-read "Priority: " priority-options nil t)
                                              priority-options)))

               ;; Get team members for assignee
               (members (linear-get-team-members team-id))
               (assignee-prompt (completing-read
                                 "Assignee: "
                                 (mapcar #'car members)
                                 nil nil nil nil ""))
               (selected-assignee (unless (string-empty-p assignee-prompt)
                                    (cdr (assoc assignee-prompt members))))

               ;; Estimate (points)
               (estimate (read-string "Estimate (points, leave empty for none): "))
               (estimate-num (when (and estimate (not (string-empty-p estimate)))
                               (string-to-number estimate)))

               ;; Issue type (label)
               (issue-types (linear-get-issue-types team-id))
               (label-names (mapcar #'car issue-types))
               ;; Group labels by category (e.g., "Docs", "Feature", etc.)
               (label-categories (let ((categories (make-hash-table :test 'equal)))
                                   (dolist (label label-names)
                                     (when-let* ((parts (split-string label " - " t))
                                                 (category (car parts)))
                                       (puthash category
                                                (cons label (gethash category categories nil))
                                                categories)))
                                   categories))
               (category-names (hash-table-keys label-categories))
               ;; First select a category, then a specific label
               (selected-category (completing-read
                                   "Label category: "
                                   (append '("All") category-names)
                                   nil nil nil nil "All"))
               (filtered-labels (if (string= selected-category "All")
                                    label-names
                                  (gethash selected-category label-categories nil)))
               (label-prompt (completing-read
                              (if (string= selected-category "All")
                                  "Label (type for fuzzy search): "
                                (format "Label in %s category: " selected-category))
                              filtered-labels
                              nil nil nil nil ""))
               (matching-labels (when (not (string-empty-p label-prompt))
                                  (cl-remove-if-not
                                   (lambda (label-name)
                                     (string-match-p (regexp-quote label-prompt) label-name))
                                   filtered-labels)))
               (selected-label-name (if (= (length matching-labels) 1)
                                        (car matching-labels)
                                      (when matching-labels
                                        (completing-read "Select specific label: " matching-labels nil t))))
               (selected-type (when (and selected-label-name (not (string-empty-p selected-label-name)))
                                (cdr (assoc selected-label-name issue-types))))

               ;; Prepare mutation
               (query "mutation CreateIssue($input: IssueCreateInput!) {
                         issueCreate(input: $input) {
                           success
                           issue {
                             id
                             identifier
                             title
                           }
                         }
                       }")

               ;; Build input variables
               (input `(("title" . ,title)
                        ("description" . ,description)
                        ("teamId" . ,team-id)
                        ,@(when selected-state
                            `(("stateId" . ,selected-state)))
                        ,@(when selected-priority
                            `(("priority" . ,selected-priority)))
                        ,@(when selected-assignee
                            `(("assigneeId" . ,selected-assignee)))
                        ,@(when estimate-num
                            `(("estimate" . ,estimate-num)))
                        ,@(when selected-type
                            `(("labelIds" . [,selected-type])))))

               (variables `(("input" . ,input)))
               (response (linear--graphql-request query `(("input" . ,input)))))

          (if response
              (let ((issue-data (assoc 'issue (assoc 'issueCreate (assoc 'data response)))))
                (message "Created issue %s: %s"
                         (cdr (assoc 'identifier issue-data))
                         (cdr (assoc 'title issue-data)))
                issue-data)
            (message "Failed to create issue")))

      (message "No team selected"))))

;;;###autoload
(defun linear-toggle-debug ()
  "Toggle debug logging for Linear API requests."
  (interactive)
  (setq linear-debug (not linear-debug))
  (message "Linear debug mode %s" (if linear-debug "enabled" "disabled")))

;;;###autoload
(defun linear-check-setup ()
  "Check if Linear.el is properly set up."
  (interactive)
  (if linear-api-key
      (progn
        (message "API key is set (length: %d). Testing connection..." (length linear-api-key))
        (linear-test-connection))
    (message "Linear API key is not set. Use M-x customize-variable RET linear-api-key")))

;;;###autoload
(defun linear-load-api-key-from-env ()
  "Try to load Linear API key from environment variable."
  (interactive)
  (let ((env-key (getenv "LINEAR_API_KEY")))
    (if env-key
        (progn
          (setq linear-api-key env-key)
          (message "Loaded Linear API key from LINEAR_API_KEY environment variable"))
      (message "LINEAR_API_KEY environment variable not found or empty"))))

(provide 'linear)
;;; linear.el ends here
