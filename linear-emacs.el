;;; linear-emacs.el --- Linear.app integration -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Gael Blanchemain
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (request "0.3.0") (dash "2.17.0") (s "1.12.0"))
;; Keywords: tools
;; URL: https://github.com/anegg0/linear-emacs

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; linear-emacs.el provides an interface to Linear.app issue tracking from Emacs.
;; It allows you to view, create, and update issues without leaving your editor.

;;; Code:

;;
;; This file is organized into the following sections:
;;
;; - Dependencies and requirements
;; - Customization and variables
;; - Core API functions
;; - Team management functions
;; - Issue management functions
;; - Issue state management functions
;; - Org-mode integration functions
;; - Mapping functions (between Linear and org-mode)
;; - User-facing commands
;; - Org-mode sync hooks
;;

;; Dependencies
(require 'request)
(require 'json)
(require 'dash)
(require 's)
(require 'org)

;;; Customization and Variables
(defgroup linear-emacs nil
  "Integration with Linear issue tracking."
  :group 'tools
  :prefix "linear-emacs-")

(defcustom linear-emacs-api-key nil
  "API key for Linear.app.
Can be set manually or loaded from LINEAR_API_KEY environment variable
using `linear-emacs-load-api-key-from-env'."
  :type 'string
  :group 'linear-emacs)

(defcustom linear-emacs-graphql-url "https://api.linear.app/graphql"
  "GraphQL endpoint URL for Linear API."
  :type 'string
  :group 'linear-emacs)

(defcustom linear-emacs-default-team-id nil
  "Default team ID to use for creating issues.
When set, skips team selection prompt when creating new issues."
  :type 'string
  :group 'linear-emacs)

(defcustom linear-emacs-debug nil
  "Enable debug logging for Linear requests.
When enabled, detailed API request and response information will be
logged to the *Messages* buffer."
  :type 'boolean
  :group 'linear-emacs)

(defcustom linear-emacs-org-file-path (expand-file-name "gtd/linear.org" org-directory)
  "Path to the org file where Linear issues are stored.
  This file will be created/updated when running `linear-emacs-list-issues\\='.
  Defaults to \\='gtd/linear.org\\=' in your `org-directory\\='."
  :type 'file
  :group 'linear-emacs)

;; Cache variables
(defvar linear-emacs--cache-issues nil
  "Cache for issues.")

(defvar linear-emacs--cache-teams nil
  "Cache for teams.")

;;; Core API Functions

(defun linear-emacs--headers ()
  "Return headers for Linear API requests."
  (unless linear-emacs-api-key
    (error "Linear API key not set. Use M-x customize-variable RET linear-emacs-api-key"))

  ;; For personal API keys, the format is: "Authorization: <API_KEY>"
  ;; No "Bearer" prefix for personal API keys
  `(("Content-Type" . "application/json")
    ("Authorization" . ,linear-emacs-api-key)))

(defun linear-emacs--log (format-string &rest args)
  "Log message with FORMAT-STRING and ARGS if debug is enabled."
  (when linear-emacs-debug
    (apply #'message (concat "[Linear] " format-string) args)))

(defun linear-emacs--graphql-request (query &optional variables)
  "Make a GraphQL request to Linear API.
  QUERY is the GraphQL query string.
  VARIABLES is an optional alist of variables to include in the request.
  Returns the JSON response data or nil if the request failed."
  (linear-emacs--log "Making GraphQL request with query: %s" query)
  (when variables
    (linear-emacs--log "Variables: %s" (prin1-to-string variables)))

  (let ((response nil)
        (error-response nil)
        (request-data (json-encode `(("query" . ,query)
                                     ,@(when variables `(("variables" . ,variables)))))))
    (linear-emacs--log "Request payload: %s" request-data)

    (request
      linear-emacs-graphql-url
      :type "POST"
      :headers (linear-emacs--headers)
      :data request-data
      :parser 'json-read
      :sync t
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (linear-emacs--log "Response received: %s" (prin1-to-string data))
                  (setq response data)))
      :error (cl-function
              (lambda (&key error-thrown response data &allow-other-keys)
                (setq error-response error-thrown)
                (linear-emacs--log "Error: %s" error-thrown)
                (linear-emacs--log "Response status: %s" (request-response-status-code response))
                (when data
                  (linear-emacs--log "Error response: %s" (prin1-to-string data))))))

    (if error-response
        (progn
          (message "Linear API error: %s" error-response)
          nil)
      response)))

;;; Team Management

(defun linear-emacs-get-teams ()
  "Get a list of teams from Linear."
  (linear-emacs--log "Fetching teams")
  (let* ((query "query { teams { nodes { id name } } }")
         (response (linear-emacs--graphql-request query)))
    (if response
        (let ((teams (cdr (assoc 'nodes (assoc 'teams (assoc 'data response))))))
          (linear-emacs--log "Retrieved %d teams" (length teams))
          (setq linear-emacs--cache-teams teams)
          teams)
      (message "Failed to retrieve teams")
      nil)))

(defun linear-emacs-select-team ()
  "Prompt user to select a team."
  (let* ((teams (or linear-emacs--cache-teams (linear-emacs-get-teams)))
         (team-names (when teams
                       (mapcar (lambda (team)
                                 (cons (cdr (assoc 'name team)) team))
                               teams)))
         (selected (completing-read "Select team: " team-names nil t)))
    (cdr (assoc selected team-names))))

(defun linear-emacs--get-team-id-by-name (team-name)
  "Get the Linear team ID for the given TEAM-NAME."
  (linear-emacs--log "Looking up team ID for team %s" team-name)
  (let* ((query "query {
  teams {
  nodes {
  id
  name
  }
  }
  }")
         (response (linear-emacs--graphql-request query))
         (teams (and response
                     (assoc 'data response)
                     (assoc 'teams (assoc 'data response))
                     (cdr (assoc 'nodes (assoc 'teams (assoc 'data response))))))
         (team (and teams
                    (seq-find (lambda (tm)
                                (string= (cdr (assoc 'name tm)) team-name))
                              teams))))
    (if team
        (cdr (assoc 'id team))
      (progn
        ;; If we couldn't find the team by exact name, cache the team IDs for debugging
        (when teams
          (linear-emacs--log "Available teams: %s"
                             (mapconcat (lambda (tm)
                                          (format "%s (%s)"
                                                  (cdr (assoc 'name tm))
                                                  (cdr (assoc 'id tm))))
                                        teams
                                        ", ")))
        (message "Could not find team with name: %s" team-name)
        nil))))

(defun linear-emacs-get-team-members (team-id)
  "Get members for the given TEAM-ID."
  (linear-emacs--log "Fetching team members for team %s" team-id)
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
         (response (linear-emacs--graphql-request query variables)))
    (when response
      (let ((members (cdr (assoc 'nodes (assoc 'members (assoc 'team (assoc 'data response)))))))
        (linear-emacs--log "Retrieved %d team members" (length members))
        (let ((formatted-members
               (mapcar (lambda (member)
                         (cons (or (cdr (assoc 'displayName member))
                                   (cdr (assoc 'name member)))
                               (cdr (assoc 'id member))))
                       members)))
          (linear-emacs--log "Formatted team members: %s" (prin1-to-string formatted-members))
          formatted-members)))))

;;; Issue Management

(defun linear-emacs-get-issues-page (&optional after)
  "Get a page of issues assigned to the user starting AFTER the given cursor."
  (linear-emacs--log "Fetching assigned issues page %s" (if after (format "after %s" after) "first page"))
  (let* ((query "query GetAssignedIssues($first: Int!, $after: String) {
  viewer {
  assignedIssues(first: $first, after: $after) {
  nodes {
  id
  identifier
  title
  description
  priority
  state { name color }
  team { id name }
  labels {
  nodes {
  name
  }
  }
  project {
  name
  }
  }
  pageInfo {
  hasNextPage
  endCursor
  }
  }
  }
  }")
         (variables `(("first" . 100) ; Fetch 100 issues per page
                      ,@(when after `(("after" . ,after)))))
         (response (linear-emacs--graphql-request query variables)))
    (linear-emacs--log "Response: %s" (prin1-to-string response))
    (if response
        (if (assoc 'data response)
            (let* ((viewer (assoc 'viewer (assoc 'data response)))
                   (assigned-issues (and viewer (assoc 'assignedIssues viewer))))
              (if assigned-issues
                  (let* ((issues (cdr (assoc 'nodes assigned-issues)))
                         (page-info (cdr (assoc 'pageInfo assigned-issues)))
                         (has-next-page (and page-info (eq (cdr (assoc 'hasNextPage page-info)) t)))
                         (end-cursor (and page-info (cdr (assoc 'endCursor page-info)))))
                    (linear-emacs--log "Retrieved %d issues, has next page: %s, end cursor: %s"
                                       (length issues) has-next-page end-cursor)
                    (list :issues issues :has-next-page has-next-page :end-cursor end-cursor))
                (progn
                  (linear-emacs--log "No assignedIssues found in response")
                  (list :issues nil :has-next-page nil :end-cursor nil))))
          (progn
            (linear-emacs--log "Invalid response format from Linear API: %s" (prin1-to-string response))
            (message "Invalid response format from Linear API")
            nil))
      (progn
        (message "Failed to retrieve issues")
        nil))))

(defun linear-emacs-get-issues ()
  "Get a list of all issues assigned to the user with pagination."
  (linear-emacs--log "Fetching all assigned issues with pagination")

  (let ((all-issues '())
        (has-more t)
        (cursor nil)
        (page-num 1)
        (max-pages 10)) ;; Safety limit to prevent infinite loops

    ;; Loop through all pages
    (while (and has-more (< page-num max-pages))
      (linear-emacs--log "Fetching page %d of issues" page-num)
      (let ((page-result (linear-emacs-get-issues-page cursor)))
        (if page-result
            (let ((page-issues (plist-get page-result :issues))
                  (next-has-more (plist-get page-result :has-next-page))
                  (next-cursor (plist-get page-result :end-cursor)))

              ;; Only process if we have issues
              (if page-issues
                  (progn
                    ;; Append issues from this page
                    (when (vectorp page-issues)
                      (setq page-issues (append page-issues nil)))
                    (setq all-issues (append all-issues page-issues))

                    ;; Update pagination state
                    (setq has-more next-has-more)
                    (setq cursor next-cursor)
                    (setq page-num (1+ page-num))

                    (linear-emacs--log "Retrieved %d issues from page %d, has more: %s"
                                       (length page-issues) (1- page-num) has-more))

                ;; No issues on this page
                (progn
                  (linear-emacs--log "No issues found on page %d" page-num)
                  (setq has-more nil))))

          ;; Error occurred
          (progn
            (linear-emacs--log "Error fetching page %d" page-num)
            (setq has-more nil)))))

    (when (>= page-num max-pages)
      (linear-emacs--log "Reached maximum page limit (%d pages)" max-pages))

    ;; Return the aggregated issues
    (linear-emacs--log "Retrieved a total of %d issues across %d pages" (length all-issues) (1- page-num))
    (setq linear-emacs--cache-issues all-issues)
    all-issues))

(defun linear-emacs-create-issue (title description team-id)
  "Create a new issue with TITLE, DESCRIPTION, and TEAM-ID."
  (linear-emacs--log "Creating issue: %s" title)
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
         (response (linear-emacs--graphql-request query variables)))
    (if response
        (let ((issue-data (assoc 'issue (assoc 'issueCreate (assoc 'data response)))))
          (message "Created issue %s: %s"
                   (cdr (assoc 'identifier issue-data))
                   (cdr (assoc 'title issue-data)))
          issue-data)
      (message "Failed to create issue")
      nil)))

;;; Issue State Management

(defun linear-emacs-get-states (team-id)
  "Get workflow states for the given TEAM-ID."
  (linear-emacs--log "Fetching workflow states for team %s" team-id)
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
         (response (linear-emacs--graphql-request query variables)))
    (when response
      (cdr (assoc 'nodes (assoc 'states (assoc 'team (assoc 'data response))))))))

(defun linear-emacs--get-state-id-by-name (state-name team-id)
  "Get the Linear state ID for the given STATE-NAME in TEAM-ID."
  (linear-emacs--log "Looking up state ID for %s in team %s" state-name team-id)
  (let* ((query "query GetTeamWorkflowStates($teamId: String!) {
  team(id: $teamId) {
  states {
  nodes {
  id
  name
  }
  }
  }
  }")
         (variables `(("teamId" . ,team-id)))
         (response (linear-emacs--graphql-request query variables))
         (states (and response
                      (assoc 'data response)
                      (assoc 'team (assoc 'data response))
                      (assoc 'states (assoc 'team (assoc 'data response)))
                      (cdr (assoc 'nodes (assoc 'states (assoc 'team (assoc 'data response)))))))
         (state (and states
                     (seq-find (lambda (s)
                                 (string= (downcase (cdr (assoc 'name s)))
                                          (downcase state-name)))
                               states))))
    (if state
        (cdr (assoc 'id state))
      (progn
        (message "Could not find state with name: %s in team %s" state-name team-id)
        nil))))

(defun linear-emacs-update-issue-state (issue-id state-name team-id)
  "Update the state of Linear issue with ISSUE-ID to STATE-NAME for TEAM-ID."
  (linear-emacs--log "Updating issue %s state to %s for team %s" issue-id state-name team-id)
  (let* ((query "mutation UpdateIssueState($issueId: String!, $stateId: String!) {
  issueUpdate(id: $issueId, input: {stateId: $stateId}) {
  success
  issue {
  id
  identifier
  state {
  id
  name
  }
  }
  }
  }")
         ;; First, we need to get the state ID from the state name and team ID
         (state-id (linear-emacs--get-state-id-by-name state-name team-id))
         (variables `(("issueId" . ,issue-id)
                      ("stateId" . ,state-id)))
         (response (linear-emacs--graphql-request query variables)))
    (if response
        (let ((success (and (assoc 'data response)
                            (assoc 'issueUpdate (assoc 'data response))
                            (cdr (assoc 'success (assoc 'issueUpdate (assoc 'data response)))))))
          (if success
              (message "Updated issue %s state to %s" issue-id state-name)
            (linear-emacs--log "Failed to update issue state: %s" (prin1-to-string response))
            (message "Failed to update issue %s state" issue-id)))
      (message "Failed to update issue %s state: API error" issue-id))))

(defun linear-emacs--graphql-request-async (query variables success-fn error-fn)
  "Make an asynchronous GraphQL request to Linear API.
  QUERY is the GraphQL query string.
  VARIABLES is an alist of variables to include in the request.
  SUCCESS-FN is a function to call on successful response.
  ERROR-FN is a function to call on error."
  (linear-emacs--log "Making async GraphQL request with query: %s" query)
  (linear-emacs--log "Variables: %s" (prin1-to-string variables))

  (let ((request-data (json-encode `(("query" . ,query)
                                     ("variables" . ,variables)))))
    (linear-emacs--log "Async request payload: %s" request-data)

    (request
      linear-emacs-graphql-url
      :type "POST"
      :headers (linear-emacs--headers)
      :data request-data
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (linear-emacs--log "Async response received: %s" (prin1-to-string data))
                  (funcall success-fn data)))
      :error (cl-function
              (lambda (&key error-thrown response data &allow-other-keys)
                (linear-emacs--log "Async error: %s" error-thrown)
                (linear-emacs--log "Async response status: %s" (request-response-status-code response))
                (when data
                  (linear-emacs--log "Async error response: %s" (prin1-to-string data)))
                (funcall error-fn error-thrown response data))))))

(defun linear-emacs--update-issue-state-async (issue-id state-name team-id)
  "Asynchronously update the state of Linear issue.
  ISSUE-ID is the Linear issue ID.
  STATE-NAME is the target state name to set.
  TEAM-ID is the team ID of the issue.
  This function updates the UI immediately and performs the API update in the background."
  (linear-emacs--log "Asynchronously updating issue %s state to %s for team %s" issue-id state-name team-id)

  ;; Show immediate feedback to the user
  (message "Updating issue state to %s... (in background)" state-name)

  ;; First, we need to get the state ID from the state name and team ID
  (let* ((state-id (linear-emacs--get-state-id-by-name state-name team-id))
         (query "mutation UpdateIssueState($issueId: String!, $stateId: String!) {
  issueUpdate(id: $issueId, input: {stateId: $stateId}) {
  success
  issue {
  id
  identifier
  state {
  id
  name
  }
  }
  }
  }")
         (variables `(("issueId" . ,issue-id)
                      ("stateId" . ,state-id))))

    ;; Define success handler
    (let ((success-handler (lambda (data)
                             (let ((success (and (assoc 'data data)
                                                 (assoc 'issueUpdate (assoc 'data data))
                                                 (cdr (assoc 'success (assoc 'issueUpdate (assoc 'data data)))))))
                               (if success
                                   (message "Successfully updated issue %s state to %s" issue-id state-name)
                                 (linear-emacs--log "Failed to update issue state asynchronously: %s" (prin1-to-string data))
                                 (message "Failed to update issue %s state in Linear" issue-id)))))
          ;; Define error handler
          (error-handler (lambda (error-thrown _response _data)
                           (message "Error updating issue %s state in Linear: %s" issue-id error-thrown))))

      ;; Make the async request
      (linear-emacs--graphql-request-async query variables success-handler error-handler))))

;;; Other Issue Attributes

(defun linear-emacs-get-priorities ()
  "Get priority options for Linear issues."
  ;; Linear uses integers for priorities: 0 (No priority), 1 (Urgent), 2 (High), 3 (Medium), 4 (Low)
  '(("No priority" . 0)
    ("Urgent" . 1)
    ("High" . 2)
    ("Medium" . 3)
    ("Low" . 4)))

(defun linear-emacs-get-issue-types (team-id)
  "Get issue types for the given TEAM-ID."
  (linear-emacs--log "Fetching issue types for team %s" team-id)
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
         (response (linear-emacs--graphql-request query variables)))
    (when response
      (let ((labels (cdr (assoc 'nodes (assoc 'labels (assoc 'team (assoc 'data response)))))))
        (mapcar (lambda (label)
                  (cons (cdr (assoc 'name label))
                        (cdr (assoc 'id label))))
                labels)))))

;;; Org Mode Integration

(defun linear-emacs-org-hook-function ()
  "Hook function to run when linear.org is modified."
  (when (and buffer-file-name
             (string-match-p "linear\\.org$" buffer-file-name))
    (linear-emacs--log "linear.org was modified, syncing changes to Linear API")
    (linear-emacs-sync-org-to-linear)))

(defun linear-emacs--extract-org-heading-properties ()
  "Extract issue properties from org heading at point.
  Returns a plist with :todo-state, :issue-id, :issue-identifier, and :team-id."
  (let ((todo-state nil)
        (issue-id nil)
        (issue-identifier nil)
        (team-id nil))
    
    ;; Extract TODO state
    (when (looking-at "^\\*\\*\\* \\(TODO\\|IN-PROGRESS\\|IN-REVIEW\\|BACKLOG\\|BLOCKED\\|DONE\\)")
      (setq todo-state (match-string 1))
      
      ;; Get issue ID, identifier, and team ID from properties
      (save-excursion
        (forward-line)
        (when (looking-at ":PROPERTIES:")
          (forward-line)
          (while (and (not (looking-at ":END:"))
                      (not (eobp)))
            (cond
             ((looking-at ":ID:\\s-+\\(.+\\)")
              (setq issue-id (match-string 1)))
             ((looking-at ":ID-LINEAR:\\s-+\\(.+\\)")
              (setq issue-identifier (match-string 1)))
             ;; Extract team ID from the TEAM property
             ((looking-at ":TEAM:\\s-+\\(.+\\)")
              ;; Fetch the actual team ID based on team name
              (let ((team-name (match-string 1)))
                (setq team-id (linear-emacs--get-team-id-by-name team-name)))))
            (forward-line)))))
    
    ;; Return properties as plist
    (list :todo-state todo-state
          :issue-id issue-id
          :issue-identifier issue-identifier
          :team-id team-id)))

(defun linear-emacs--map-org-state-to-linear (todo-state)
  "Map \='org-mode\=' TODO state to Linear state name.
  TODO-STATE is the \='org-mode\=' state string."
  (cond
   ((string= todo-state "TODO") "Todo")
   ((string= todo-state "IN-PROGRESS") "In Progress")
   ((string= todo-state "IN-REVIEW") "In Review")
   ((string= todo-state "BACKLOG") "Backlog")
   ((string= todo-state "BLOCKED") "Blocked")
   ((string= todo-state "DONE") "Done")
   (t nil)))

(defun linear-emacs--process-heading-at-point ()
  "Process the Linear issue at the current org heading."
  (let* ((props (linear-emacs--extract-org-heading-properties))
         (todo-state (plist-get props :todo-state))
         (issue-id (plist-get props :issue-id))
         (issue-identifier (plist-get props :issue-identifier))
         (team-id (plist-get props :team-id)))
    
    ;; If we found an issue ID, state, and team ID, update the Linear API
    (when (and issue-id issue-identifier team-id)
      ;; Map org TODO state to Linear state
      (let ((linear-state (linear-emacs--map-org-state-to-linear todo-state)))
        (when linear-state
          ;; Use async request to update Linear API in the background
          (linear-emacs--update-issue-state-async issue-id linear-state team-id))))))

(defun linear-emacs-sync-org-to-linear ()
  "Syncs change from linear.org to Linear API."
  (interactive)
  ;; If called from org-after-todo-state-change-hook, just process the current heading
  (if (eq this-command 'org-todo)
      (linear-emacs-sync-current-heading-to-linear)
    ;; Otherwise, scan the entire file
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\*\\*\\* \\(TODO\\|IN-PROGRESS\\|IN-REVIEW\\|BACKLOG\\|BLOCKED\\|DONE\\)" nil t)
        (beginning-of-line)
        (linear-emacs--process-heading-at-point)))))

(defun linear-emacs-sync-current-heading-to-linear ()
  "Sync the current org heading's TODO state to Linear API.
  Used when directly changing a TODO state in the org buffer."
  (save-excursion
    ;; Move to the beginning of the current heading
    (org-back-to-heading t)
    (linear-emacs--process-heading-at-point)))

;;; Mapping Functions

(defun linear-emacs--map-linear-state-to-org (state)
  "Map Linear state name to \='org-mode\=' TODO state string.
  STATE is the Linear state string."
  (cond
   ((string-equal state "Done") "DONE")
   ((string-equal state "Todo") "TODO")
   ((string-equal state "In Progress") "IN-PROGRESS")
   ((string-equal state "In Review") "IN-REVIEW")
   ((string-equal state "Backlog") "BACKLOG")
   ((string-equal state "Blocked") "BLOCKED")
   (t "TODO")))

(defun linear-emacs--map-linear-priority-to-org (priority-num)
  "Convert Linear priority number to \='org-mode\=' priority string.
  PRIORITY-NUM is the Linear priority number (0=None, 1=Urgent, 2=High, 3=Medium, 4=Low)."
  (cond
   ((eq priority-num 1) "[#A]") ; Urgent -> A
   ((eq priority-num 2) "[#B]") ; High -> B
   ((eq priority-num 3) "[#C]") ; Medium -> C
   ((eq priority-num 4) "[#D]") ; Low -> D
   (t "[#C]")))            ; Default -> C

(defun linear-emacs--get-linear-priority-name (priority-num)
  "Convert Linear priority number to readable name.
  PRIORITY-NUM is the Linear priority number (0=None, 1=Urgent, 2=High, 3=Medium, 4=Low)."
  (cond
   ((eq priority-num 1) "Urgent")
   ((eq priority-num 2) "High")
   ((eq priority-num 3) "Medium")
   ((eq priority-num 4) "Low")
   (t "Medium")))

(defun linear-emacs--format-issue-as-org-entry (issue)
  "Format a Linear ISSUE as an \='org-mode\=' entry."
  (let* ((id (cdr (assoc 'id issue)))
         (identifier (cdr (assoc 'identifier issue)))
         (title (cdr (assoc 'title issue)))
         (description (or (cdr (assoc 'description issue)) ""))
         (priority-num (cdr (assoc 'priority issue)))
         (state-assoc (assoc 'state issue))
         (state (and state-assoc (cdr (assoc 'name state-assoc))))
         (todo-state (linear-emacs--map-linear-state-to-org state))
         (priority (linear-emacs--map-linear-priority-to-org priority-num))
         (team-assoc (assoc 'team issue))
         (team-name (or (and team-assoc (cdr (assoc 'name team-assoc))) ""))
         (project-assoc (assoc 'project issue))
         (project-name (or (and project-assoc (cdr (assoc 'name project-assoc))) ""))
         (labels-assoc (assoc 'labels issue))
         (labels-nodes (and labels-assoc (cdr (assoc 'nodes labels-assoc))))
         (labels (if (and labels-nodes (not (eq labels-nodes 'null)))
                     (progn
                       (when (vectorp labels-nodes)
                         (setq labels-nodes (append labels-nodes nil)))
                       (mapconcat (lambda (label)
                                    (cdr (assoc 'name label)))
                                  labels-nodes ", "))
                   ""))
         (link (format "https://linear.app/issue/%s" identifier))
         (result ""))

    ;; Build the org entry as a string
    ;; Sanitize title to ensure org compatibility
    (let ((sanitized-title (replace-regexp-in-string "\\[\\|\\]" "" title)))
      (setq result (concat result (format "*** %s %s %s\n" todo-state priority sanitized-title))))

    (setq result (concat result ":PROPERTIES:\n"))
    (setq result (concat result (format ":ID:       %s\n" id)))
    (setq result (concat result (format ":ID-LINEAR: %s\n" identifier)))
    (setq result (concat result (format ":TEAM: %s\n" team-name)))

    ;; Format description properly for multi-line content
    (setq result (concat result ":DESCRIPTION: |\n"))
    ;; Process description to ensure proper org formatting
    (let ((desc-lines (split-string description "\n")))
      (dolist (line desc-lines)
        (setq result (concat result (format "  %s\n" (replace-regexp-in-string "\"" "'" line))))))

    (setq result (concat result (format ":PRIORITY: %s\n" (linear-emacs--get-linear-priority-name priority-num))))

    ;; Format labels properly
    (if (string-empty-p labels)
        (setq result (concat result ":LABELS: []\n"))
      (setq result (concat result (format ":LABELS: [%s]\n"
                                          (replace-regexp-in-string "," ", " labels)))))

    (setq result (concat result (format ":PROJECT: %s\n" project-name)))
    (setq result (concat result (format ":LINK: %s\n" link)))
    (setq result (concat result ":END:\n"))

    result))

;;; User-facing Commands

;;;###autoload
(defun linear-emacs-list-issues ()
  "Update linear.org file with assigned Linear issues and display it.
  Only shows issues with statuses TODO, IN-PROGRESS, IN-REVIEW, BACKLOG, and BLOCKED."
  (interactive)
  (linear-emacs--log "Executing linear-emacs-list-issues")
  (let* ((issues (linear-emacs-get-issues))
         (org-file-path linear-emacs-org-file-path)
         ;; Define the list of statuses to include (case insensitive)
         (include-statuses '("todo" "in progress" "in review" "backlog" "blocked")))
    (linear-emacs--log "Retrieved %d total issues before filtering" (length issues))
    (if (and issues (> (length issues) 0))
        (progn
          ;; Make sure issues is a list, not a vector
          (when (vectorp issues)
            (setq issues (append issues nil)))

          ;; Filter issues based on status
          (setq issues
                (seq-filter
                 (lambda (issue)
                   (let* ((state-assoc (assoc 'state issue))
                          (state (and state-assoc (cdr (assoc 'name state-assoc))))
                          (state-lower (and state (downcase state))))
                     (linear-emacs--log "Issue %s has state: %s (include: %s)"
                                        (cdr (assoc 'identifier issue))
                                        state-lower
                                        (if (member state-lower include-statuses) "yes" "no"))
                     (member state-lower include-statuses)))
                 issues))

          (linear-emacs--log "After filtering by status, have %d issues" (length issues))

          ;; Update org file
          (condition-case err
              (progn
                (with-temp-buffer
                  ;; Insert header
                  (insert ":PROPERTIES:\n")
                  (insert ":ID:       a12acb12-8a69-4d15-a846-21e20ed2f3ae\n")
                  (insert "#+title: Linear issues assigned to me\n")
                  (insert "#+TAGS: :\n")
                  (insert "#+filetags: :twai:b:\n")
                  (insert "#+STARTUP: overview\n")
                  (insert "#+TODO: TODO IN-PROGRESS IN-REVIEW BACKLOG BLOCKED | DONE\n")
                  (insert ":END:\n\n")

                  ;; Insert issues
                  (dolist (issue issues)
                    (insert (linear-emacs--format-issue-as-org-entry issue)))

                  ;; Write to file
                  (linear-emacs--log "Writing %d issues to %s" (length issues) org-file-path)
                  (make-directory (file-name-directory org-file-path) t) ;; Ensure directory exists
                  (write-region (point-min) (point-max) org-file-path nil 'quiet))

                ;; Open the org file
                (find-file org-file-path)
                (message "Updated Linear issues in %s with %d active issues"
                         org-file-path (length issues)))

            ;; Handle errors
            (error (progn
                     (linear-emacs--log "Error updating linear.org: %s" (error-message-string err))
                     (message "Error updating linear.org: %s" (error-message-string err))))))

      (message "No issues found or failed to retrieve issues"))))

;;;###autoload
(defun linear-emacs-new-issue ()
  "Create a new Linear issue with additional attributes."
  (interactive)
  ;; Select team first (needed for states, members, etc.)
  (let* ((team (if linear-emacs-default-team-id
                   (list (cons 'id linear-emacs-default-team-id))
                 (linear-emacs-select-team)))
         (team-id (cdr (assoc 'id team))))

    (if team-id
        (let* ((title (read-string "Issue title: "))
               (description (read-string "Description: "))

               ;; Get workflow states
               (states (linear-emacs-get-states team-id))
               (state-options (when states
                                (mapcar (lambda (state)
                                          (cons (cdr (assoc 'name state))
                                                (cdr (assoc 'id state))))
                                        states)))
               (selected-state (when state-options
                                 (cdr (assoc (completing-read "State: " state-options nil t)
                                             state-options))))

               ;; Get priorities
               (priority-options (linear-emacs-get-priorities))
               (selected-priority (cdr (assoc (completing-read "Priority: " priority-options nil t)
                                              priority-options)))

               ;; Get team members for assignee
               (members (linear-emacs-get-team-members team-id))
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
               (issue-types (linear-emacs-get-issue-types team-id))
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

               (response (linear-emacs--graphql-request query `(("input" . ,input)))))

          (if response
              (let ((issue-data (assoc 'issue (assoc 'issueCreate (assoc 'data response)))))
                (message "Created issue %s: %s"
                         (cdr (assoc 'identifier issue-data))
                         (cdr (assoc 'title issue-data)))
                issue-data)
            (message "Failed to create issue")))

      (message "No team selected"))))

;;;###autoload
(defun linear-emacs-test-connection ()
  "Test the connection to Linear API."
  (interactive)
  (linear-emacs--log "Testing connection to Linear API")
  (let* ((query "query { viewer { id name } }")
         (response (linear-emacs--graphql-request query)))
    (if response
        (let ((viewer (assoc 'viewer (assoc 'data response))))
          (message "Connected to Linear as: %s" (cdr (assoc 'name viewer)))
          t)
      (message "Failed to connect to Linear API")
      nil)))

;;;###autoload
(defun linear-emacs-toggle-debug ()
  "Toggle debug logging for Linear API requests."
  (interactive)
  (setq linear-emacs-debug (not linear-emacs-debug))
  (message "Linear debug mode %s" (if linear-emacs-debug "enabled" "disabled")))

;;;###autoload
(defun linear-emacs-check-setup ()
  "Check if Linear.el is properly set up."
  (interactive)
  (if linear-emacs-api-key
      (progn
        (message "API key is set (length: %d). Testing connection..." (length linear-emacs-api-key))
        (linear-emacs-test-connection))
    (message "Linear API key is not set. Use M-x customize-variable RET linear-emacs-api-key")))

;;;###autoload
(defun linear-emacs-load-api-key-from-env ()
  "Try to load Linear API key from environment variable."
  (interactive)
  (let ((env-key (getenv "LINEAR_API_KEY")))
    (if env-key
        (progn
          (setq linear-emacs-api-key env-key)
          (message "Loaded Linear API key from LINEAR_API_KEY environment variable"))
      (message "LINEAR_API_KEY environment variable not found or empty"))))

;;; Org Mode Sync Hooks

;;;###autoload
(defun linear-emacs-enable-org-sync ()
  "Enable synchronization between org mode and Linear."
  (interactive)
  (add-hook 'after-save-hook #'linear-emacs-org-hook-function nil t)
  (add-hook 'org-after-todo-state-change-hook #'linear-emacs-sync-org-to-linear nil t)
  (message "Linear-org synchronization enabled"))

;;;###autoload
(defun linear-emacs-disable-org-sync ()
  "Disable synchronization between org mode and Linear."
  (interactive)
  (remove-hook 'after-save-hook #'linear-emacs-org-hook-function t)
  (remove-hook 'org-after-todo-state-change-hook #'linear-emacs-sync-org-to-linear t)
  (message "Linear-org synchronization disabled"))

(provide 'linear-emacs)
;;; linear-emacs.el ends here
