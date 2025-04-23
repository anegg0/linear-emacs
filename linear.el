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
(require 'url)

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
  (when (string-match-p "[\n\r\t ]" linear-api-key)
    (message "Warning: API key contains whitespace which may cause authentication issues"))

  ;; Ensure the API key is trimmed of any whitespace
  (let ((clean-key (string-trim linear-api-key)))
    `(("Content-Type" . "application/json")
      ;; Linear API uses direct token auth - not Bearer token
      ("Authorization" . ,clean-key))))

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

    ;; Log the actual headers being sent (with token partially redacted)
    (let ((headers (linear--headers)))
      (linear--log "Headers: %s"
                   (prin1-to-string
                    (mapcar (lambda (header)
                              (if (equal (car header) "Authorization")
                                  (cons "Authorization"
                                        (replace-regexp-in-string
                                         "\\(Bearer \\).+"
                                         "\\1[REDACTED]"
                                         (cdr header)))
                                header))
                            headers))))

    ;; Use url-retrieve-synchronously for a more direct HTTP request
    ;; that may provide better debugging information
    (let* ((url-request-method "POST")
           (url-request-extra-headers (linear--headers))
           (url-request-data request-data)
           (url-buffer (condition-case err
                           (url-retrieve-synchronously linear-graphql-url)
                         (error
                          (linear--log "URL retrieve error: %s" (error-message-string err))
                          nil))))

      (when url-buffer
        (with-current-buffer url-buffer
          (linear--log "Raw response buffer: %s" (buffer-string))

          ;; Check HTTP status code
          (goto-char (point-min))
          (if (not (re-search-forward "HTTP/[0-9.]+ \\([0-9]+\\)" nil t))
              (linear--log "Could not find HTTP status code")
            (let ((status-code (string-to-number (match-string 1))))
              (linear--log "HTTP status code: %d" status-code)

              (if (>= status-code 400)
                  (setq error-response (format "HTTP error %d" status-code))

                ;; Try to parse the JSON response
                (goto-char (point-min))
                (re-search-forward "\n\n" nil t) ; Skip headers
                (condition-case json-err
                    (let ((json-data (json-read)))
                      (linear--log "Response received: %s" (prin1-to-string json-data))
                      (setq response json-data))
                  (error
                   (linear--log "JSON parse error: %s" (error-message-string json-err))
                   (setq error-response (format "JSON parse error: %s" (error-message-string json-err))))))))))

      ;; Cleanup
      (when url-buffer
        (kill-buffer url-buffer)))

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
          (dolist (issue issues)
            (let ((id (cdr (assoc 'id issue)))
                  (identifier (cdr (assoc 'identifier issue)))
                  (title (cdr (assoc 'title issue)))
                  (state (cdr (assoc 'name (assoc 'state issue))))
                  (color (cdr (assoc 'color (assoc 'state issue)))))
              (insert (format "%-10s %-12s %s\n" identifier state title))))
          (goto-char (point-min))
          (switch-to-buffer buffer))
      (message "No issues found or failed to retrieve issues"))))

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
    (if team-id
        (linear-create-issue title description team-id)
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
        (message "API key is set. Testing connection...")
        (linear-test-connection))
    (message "Linear API key is not set. Use M-x customize-variable RET linear-api-key")))

;;;###autoload
(defun linear-debug-auth ()
  "Debug authentication issues with Linear API."
  (interactive)
  (setq linear-debug t)
  (linear--log "Starting authentication debug")

  ;; Check API key
  (if (null linear-api-key)
      (message "Linear API key is not set")
    (progn
      ;; Check for whitespace in key
      (when (string-match-p "[\n\r\t ]" linear-api-key)
        (message "Warning: API key contains whitespace which may cause authentication issues"))

      ;; Verify key format
      (let ((key-length (length linear-api-key)))
        (linear--log "API key length: %d" key-length)
        (if (< key-length 20)
            (message "API key seems too short (%d chars). Linear keys are typically longer" key-length)
          (message "API key length seems reasonable (%d chars)" key-length)))

      ;; Report key first/last chars (redacted middle)
      (when (> (length linear-api-key) 8)
        (let ((visible-prefix (substring linear-api-key 0 4))
              (visible-suffix (substring linear-api-key -4)))
          (linear--log "API key preview: %s...%s" visible-prefix visible-suffix)))

      ;; Try a minimal query with extra auth debugging
      (message "Testing authentication with minimal query...")
      (let* ((query "query { viewer { id } }")
             (url-request-method "POST")
             (url-request-extra-headers (linear--headers))
             (url-request-data (json-encode `(("query" . ,query)))))

        (linear--log "Request URL: %s" linear-graphql-url)
        (linear--log "Request method: %s" url-request-method)
        (linear--log "Request data: %s" url-request-data)

        (condition-case err
            (let ((url-buffer (url-retrieve-synchronously linear-graphql-url)))
              (when url-buffer
                (with-current-buffer url-buffer
                  (linear--log "Full response:\n%s" (buffer-string))
                  (message "Check *Messages* buffer for detailed authentication debug info"))))
          (error
           (message "Error during authentication test: %s" (error-message-string err))))))))

(provide 'linear)
;;; linear.el ends here
