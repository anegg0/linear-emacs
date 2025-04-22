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

(defvar linear--cache-issues nil
  "Cache for issues.")

(defvar linear--cache-teams nil
  "Cache for teams.")

(defun linear--headers ()
  "Return headers for Linear API requests."
  (unless linear-api-key
    (error "Linear API key not set. Use M-x customize-variable RET linear-api-key"))
  `(("Content-Type" . "application/json")
    ("Authorization" . ,(format "Bearer %s" linear-api-key))))

(defun linear--graphql-request (query &optional variables)
  "Make a GraphQL request to Linear API with QUERY and optional VARIABLES."
  (let ((response nil)
        (error-response nil))
    (request
     linear-graphql-url
     :type "POST"
     :headers (linear--headers)
     :data (json-encode `(("query" . ,query)
                          ,@(when variables `(("variables" . ,variables)))))
     :parser 'json-read
     :sync t
     :success (cl-function
               (lambda (&key data &allow-other-keys)
                 (setq response data)))
     :error (cl-function
             (lambda (&key error-thrown &allow-other-keys)
               (setq error-response error-thrown))))
    (if error-response
        (error "Linear API error: %s" error-response)
      response)))

(defun linear-get-teams ()
  "Get a list of teams from Linear."
  (let* ((query "query { teams { nodes { id name } } }")
         (response (linear--graphql-request query))
         (teams (cdr (assoc 'nodes (assoc 'teams (assoc 'data response))))))
    (setq linear--cache-teams teams)
    teams))

(defun linear-select-team ()
  "Prompt user to select a team."
  (let* ((teams (or linear--cache-teams (linear-get-teams)))
         (team-names (mapcar (lambda (team)
                               (cons (cdr (assoc 'name team)) team))
                             teams))
         (selected (completing-read "Select team: " team-names nil t)))
    (cdr (assoc selected team-names))))

(defun linear-get-issues ()
  "Get a list of issues assigned to the user."
  (let* ((query "query { viewer { assignedIssues { nodes { id identifier title state { name color } } } } }")
         (response (linear--graphql-request query))
         (issues (cdr (assoc 'nodes (assoc 'assignedIssues (assoc 'viewer (assoc 'data response)))))))
    (setq linear--cache-issues issues)
    issues))

(defun linear-create-issue (title description team-id)
  "Create a new issue with TITLE, DESCRIPTION, and TEAM-ID."
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
         (response (linear--graphql-request query variables))
         (issue-data (assoc 'issue (assoc 'issueCreate (assoc 'data response)))))
    (message "Created issue %s: %s"
             (cdr (assoc 'identifier issue-data))
             (cdr (assoc 'title issue-data)))
    issue-data))

;;;###autoload
(defun linear-list-issues ()
  "Display a list of the user's assigned Linear issues."
  (interactive)
  (let* ((issues (linear-get-issues))
         (buffer (get-buffer-create "*Linear Issues*")))
    (with-current-buffer buffer
      (erase-buffer)
      (dolist (issue issues)
        (let ((id (cdr (assoc 'id issue)))
              (identifier (cdr (assoc 'identifier issue)))
              (title (cdr (assoc 'title issue)))
              (state (cdr (assoc 'name (assoc 'state issue))))
              (color (cdr (assoc 'color (assoc 'state issue)))))
          (insert (format "%-10s %-12s %s\n" identifier state title))))
      (goto-char (point-min)))
    (switch-to-buffer buffer)))

;;;###autoload
(defun linear-new-issue ()
  "Create a new Linear issue."
  (interactive)
  (let* ((team (if linear-default-team-id
                   (list (cons 'id linear-default-team-id))
                 (linear-select-team)))
         (team-id (cdr (assoc 'id team)))
         (title (read-string "Issue title: "))
         (description (read-string "Description: ")))
    (linear-create-issue title description team-id)))

(provide 'linear)
;;; linear.el ends here