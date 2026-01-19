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
;; - Core API functions (async-first)
;; - Team management functions
;; - Issue management functions
;; - Issue state management functions
;; - Org-mode integration functions
;; - Mapping functions (between Linear and org-mode)
;; - User-facing commands
;; - Org-mode sync hooks
;; - Backward compatibility functions
;;

;; Dependencies
(require 'request)
(require 'json)
(require 'dash)
(require 's)
(require 'org)
(require 'cl-lib)

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

(defcustom linear-emacs-issues-state-mapping
  '(("Todo" . "TODO")
    ("Backlog" . "TODO")
    ("In Progress" . "NEXT")
    ("In Review" . "WAITING")
    ("Blocked" . "WAITING")
    ("Done" . "DONE"))
  "Mapping between Linear state names and org-mode TODO states.
Each element is a cons cell (LINEAR-STATE . ORG-STATE).
This mapping is used for:
- Converting Linear states to org TODO states
- Converting org TODO states to Linear states
- Filtering which issues to include in the org file
Only issues with Linear states listed here will be included
when running `linear-emacs-list-issues'."
  :type '(alist :key-type string :value-type string)
  :group 'linear-emacs)

(defcustom linear-emacs-async-default t
  "Use async API calls by default.
When t, all API calls will be asynchronous unless explicitly overridden.
Set to nil to use synchronous calls by default for backward compatibility."
  :type 'boolean
  :group 'linear-emacs)

(defcustom linear-emacs-progress-messages t
  "Show progress messages during long operations.
When enabled, displays messages about ongoing API operations."
  :type 'boolean
  :group 'linear-emacs)

(defvar linear-emacs-todo-states-pattern nil
  "Cached regex pattern for matching org-mode TODO states.
This pattern is generated from `linear-emacs-issues-state-mapping'.
Use `linear-emacs--get-todo-states-pattern' to get the pattern.")

;; Cache variables
(defvar linear-emacs--cache-issues nil
  "Cache for issues.")

(defvar linear-emacs--cache-teams nil
  "Cache for teams.")

;; Progress tracking variables
(defvar linear-emacs--active-requests 0
  "Number of currently active API requests.")

(defvar linear-emacs--request-queue nil
  "Queue of pending requests to manage concurrent API calls.")

;;; Core API Functions (Async-First Architecture)

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

(defun linear-emacs--progress (format-string &rest args)
  "Show progress message with FORMAT-STRING and ARGS if progress messages are enabled."
  (when linear-emacs-progress-messages
    (apply #'message (concat "[Linear] " format-string) args)))

(defun linear-emacs--graphql-request-async (query &optional variables success-fn error-fn)
  "Make an asynchronous GraphQL request to Linear API.
QUERY is the GraphQL query string.
VARIABLES is an optional alist of variables to include in the request.
SUCCESS-FN is called with the response data on success.
ERROR-FN is called with error information on failure.
If SUCCESS-FN or ERROR-FN are not provided, default handlers will be used."
  (linear-emacs--log "Making async GraphQL request with query: %s" query)
  (when variables
    (linear-emacs--log "Variables: %s" (prin1-to-string variables)))

  ;; Track active requests
  (setq linear-emacs--active-requests (1+ linear-emacs--active-requests))

  ;; Default handlers if not provided
  (unless success-fn
    (setq success-fn (lambda (data)
                       (linear-emacs--log "Request completed: %s" (prin1-to-string data)))))

  (unless error-fn
    (setq error-fn (lambda (error-thrown response data)
                     (message "Linear API error: %s" error-thrown)
                     (linear-emacs--log "Error response: %s" (prin1-to-string data)))))

  (let ((request-data (json-encode `(("query" . ,query)
                                     ,@(when variables `(("variables" . ,variables)))))))
    (linear-emacs--log "Request payload: %s" request-data)

    (request
      linear-emacs-graphql-url
      :type "POST"
      :headers (linear-emacs--headers)
      :data request-data
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (setq linear-emacs--active-requests (1- linear-emacs--active-requests))
                  (linear-emacs--log "Response received: %s" (prin1-to-string data))
                  (funcall success-fn data)))
      :error (cl-function
              (lambda (&key error-thrown response data &allow-other-keys)
                (setq linear-emacs--active-requests (1- linear-emacs--active-requests))
                (linear-emacs--log "Error: %s" error-thrown)
                (linear-emacs--log "Response status: %s" (request-response-status-code response))
                (when data
                  (linear-emacs--log "Error response: %s" (prin1-to-string data)))
                (funcall error-fn error-thrown response data))))))

(defun linear-emacs--graphql-request (query &optional variables)
  "Synchronous wrapper for GraphQL requests (backward compatibility).
QUERY is the GraphQL query string.
VARIABLES is an optional alist of variables.
Returns the response data or nil on error.
This function blocks until the request completes."
  (linear-emacs--log "Making synchronous GraphQL request (backward compatibility mode)")
  (let ((response nil)
        (error-response nil)
        (completed nil))

    ;; Make async request but wait for completion
    (linear-emacs--graphql-request-async
     query
     variables
     (lambda (data)
       (setq response data)
       (setq completed t))
     (lambda (error-thrown _response _data)
       (setq error-response error-thrown)
       (setq completed t)))

    ;; Busy wait for completion (with timeout)
    (let ((timeout 30) ; 30 second timeout
          (start-time (current-time)))
      (while (and (not completed)
                  (< (float-time (time-subtract (current-time) start-time)) timeout))
        (sleep-for 0.1)))

    (if error-response
        (progn
          (message "Linear API error: %s" error-response)
          nil)
      response)))

;;; Team Management (Async)

(defun linear-emacs-get-teams-async (&optional callback)
  "Asynchronously get a list of teams from Linear.
CALLBACK is called with the list of teams on success."
  (linear-emacs--log "Fetching teams asynchronously")
  (linear-emacs--progress "Fetching teams...")

  (let* ((query "query { teams { nodes { id name } } }")
         (success-fn (lambda (response)
                       (if response
                           (let ((teams (cdr (assoc 'nodes (assoc 'teams (assoc 'data response))))))
                             (linear-emacs--log "Retrieved %d teams" (length teams))
                             (setq linear-emacs--cache-teams teams)
                             (when callback
                               (funcall callback teams)))
                         (message "Failed to retrieve teams")
                         (when callback
                           (funcall callback nil)))))
         (error-fn (lambda (_error _response _data)
                     (message "Failed to retrieve teams")
                     (when callback
                       (funcall callback nil)))))

    (linear-emacs--graphql-request-async query nil success-fn error-fn)))

(defun linear-emacs-get-teams ()
  "Get a list of teams from Linear (synchronous for backward compatibility)."
  (let ((teams nil)
        (completed nil))

    (linear-emacs-get-teams-async
     (lambda (result)
       (setq teams result)
       (setq completed t)))

    ;; Wait for completion
    (while (not completed)
      (sleep-for 0.1))

    teams))

(defun linear-emacs-select-team-async (callback)
  "Asynchronously prompt user to select a team.
CALLBACK is called with the selected team."
  (if linear-emacs--cache-teams
      ;; Use cached teams
      (let* ((team-names (mapcar (lambda (team)
                                   (cons (cdr (assoc 'name team)) team))
                                 linear-emacs--cache-teams))
             (selected (completing-read "Select team: " team-names nil t)))
        (funcall callback (cdr (assoc selected team-names))))
    ;; Fetch teams first
    (linear-emacs-get-teams-async
     (lambda (teams)
       (if teams
           (let* ((team-names (mapcar (lambda (team)
                                        (cons (cdr (assoc 'name team)) team))
                                      teams))
                  (selected (completing-read "Select team: " team-names nil t)))
             (funcall callback (cdr (assoc selected team-names))))
         (funcall callback nil))))))

(defun linear-emacs-select-team ()
  "Prompt user to select a team (synchronous for backward compatibility)."
  (let ((result nil)
        (completed nil))

    (linear-emacs-select-team-async
     (lambda (team)
       (setq result team)
       (setq completed t)))

    ;; Wait for completion
    (while (not completed)
      (sleep-for 0.1))

    result))

;;; Issue Management (Async with Progress Reporting)

(defun linear-emacs-get-issues-page-async (callback &optional after project-id)
  "Asynchronously get a page of issues.
CALLBACK is called with a plist containing :issues, :has-next-page, and :end-cursor.
AFTER is the cursor for pagination.
PROJECT-ID optionally filters by project."
  (linear-emacs--log "Fetching assigned issues page %s%s"
                     (if after (format "after %s" after) "first page")
                     (if project-id (format " for project %s" project-id) ""))

  (let* ((query (if project-id
                    "query GetAssignedIssues($first: Int!, $after: String, $projectId: ID!) {
  viewer {
  assignedIssues(first: $first, after: $after, filter: { project: { id: { eq: $projectId } } }) {
  nodes {
  id
  identifier
  title
  description
  priority
  dueDate
  startedAt
  createdAt
  cycle { id name number startsAt endsAt }
  state { name color }
  team { id name }
  labels {
  nodes {
  name
  }
  }
  project {
  id
  name
  }
  }
  pageInfo {
  hasNextPage
  endCursor
  }
  }
  }
  }"
                  "query GetAssignedIssues($first: Int!, $after: String) {
  viewer {
  assignedIssues(first: $first, after: $after) {
  nodes {
  id
  identifier
  title
  description
  priority
  dueDate
  startedAt
  createdAt
  cycle { id name number startsAt endsAt }
  state { name color }
  team { id name }
  labels {
  nodes {
  name
  }
  }
  project {
  id
  name
  }
  }
  pageInfo {
  hasNextPage
  endCursor
  }
  }
  }
  }"))
         (variables `(("first" . 100) ; Fetch 100 issues per page
                      ,@(when after `(("after" . ,after)))
                      ,@(when project-id `(("projectId" . ,project-id)))))

         (success-fn (lambda (response)
                       (linear-emacs--log "Response: %s" (prin1-to-string response))
                       (if (and response (assoc 'data response))
                           (let* ((viewer (assoc 'viewer (assoc 'data response)))
                                  (assigned-issues (and viewer (assoc 'assignedIssues viewer))))
                             (if assigned-issues
                                 (let* ((issues (cdr (assoc 'nodes assigned-issues)))
                                        (page-info (cdr (assoc 'pageInfo assigned-issues)))
                                        (has-next-page (and page-info (eq (cdr (assoc 'hasNextPage page-info)) t)))
                                        (end-cursor (and page-info (cdr (assoc 'endCursor page-info)))))
                                   (linear-emacs--log "Retrieved %d issues, has next page: %s"
                                                      (length issues) has-next-page)
                                   (funcall callback (list :issues issues
                                                          :has-next-page has-next-page
                                                          :end-cursor end-cursor)))
                               (progn
                                 (linear-emacs--log "No assignedIssues found in response")
                                 (funcall callback (list :issues nil :has-next-page nil :end-cursor nil)))))
                         (progn
                           (linear-emacs--log "Invalid response format from Linear API")
                           (message "Invalid response format from Linear API")
                           (funcall callback nil)))))

         (error-fn (lambda (_error _response _data)
                     (message "Failed to retrieve issues")
                     (funcall callback nil))))

    (linear-emacs--graphql-request-async query variables success-fn error-fn)))

(defun linear-emacs-get-issues-async (callback &optional project-id)
  "Asynchronously get all issues with pagination and progress reporting.
CALLBACK is called with the complete list of issues.
PROJECT-ID optionally filters by project."
  (linear-emacs--log "Fetching all assigned issues with async pagination%s"
                     (if project-id (format " for project %s" project-id) ""))

  (let ((all-issues '())
        (page-num 1)
        (max-pages 10) ; Safety limit
        (total-fetched 0))

    (linear-emacs--progress "Fetching issues... (page 1)")

    ;; Recursive function to fetch all pages
    (cl-labels ((fetch-next-page (cursor)
                  (if (>= page-num max-pages)
                      (progn
                        (linear-emacs--log "Reached maximum page limit (%d pages)" max-pages)
                        (linear-emacs--progress "Retrieved %d issues (reached page limit)" total-fetched)
                        (setq linear-emacs--cache-issues all-issues)
                        (funcall callback all-issues))

                    (linear-emacs-get-issues-page-async
                     (lambda (page-result)
                       (if page-result
                           (let ((page-issues (plist-get page-result :issues))
                                 (has-more (plist-get page-result :has-next-page))
                                 (next-cursor (plist-get page-result :end-cursor)))

                             (if page-issues
                                 (progn
                                   ;; Convert vector to list if needed
                                   (when (vectorp page-issues)
                                     (setq page-issues (append page-issues nil)))

                                   ;; Append issues
                                   (setq all-issues (append all-issues page-issues))
                                   (setq total-fetched (length all-issues))

                                   ;; Update progress
                                   (linear-emacs--progress
                                    "Retrieved %d issues%s..."
                                    total-fetched
                                    (if has-more
                                        (format " (fetching page %d)" (1+ page-num))
                                      ""))

                                   ;; Continue or finish
                                   (if has-more
                                       (progn
                                         (setq page-num (1+ page-num))
                                         (fetch-next-page next-cursor))
                                     (progn
                                       (linear-emacs--progress "Retrieved %d issues (complete)" total-fetched)
                                       (setq linear-emacs--cache-issues all-issues)
                                       (funcall callback all-issues))))

                               ;; No issues on this page
                               (progn
                                 (linear-emacs--log "No issues found on page %d" page-num)
                                 (linear-emacs--progress "Retrieved %d issues (complete)" total-fetched)
                                 (setq linear-emacs--cache-issues all-issues)
                                 (funcall callback all-issues))))

                         ;; Error occurred
                         (progn
                           (linear-emacs--log "Error fetching page %d" page-num)
                           (message "Error fetching issues, returning %d issues fetched so far" total-fetched)
                           (setq linear-emacs--cache-issues all-issues)
                           (funcall callback all-issues))))
                     cursor project-id))))

      ;; Start fetching from the first page
      (fetch-next-page nil))))

(defun linear-emacs-get-issues (&optional project-id)
  "Get all issues (synchronous wrapper for backward compatibility).
PROJECT-ID optionally filters by project."
  (let ((issues nil)
        (completed nil))

    (linear-emacs-get-issues-async
     (lambda (result)
       (setq issues result)
       (setq completed t))
     project-id)

    ;; Wait for completion
    (while (not completed)
      (sleep-for 0.1))

    issues))

(defun linear-emacs-create-issue-async (title description team-id callback)
  "Asynchronously create a new issue.
TITLE is the issue title.
DESCRIPTION is the issue description.
TEAM-ID is the team to create the issue in.
CALLBACK is called with the created issue data."
  (linear-emacs--log "Creating issue: %s" title)
  (linear-emacs--progress "Creating issue...")

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

         (success-fn (lambda (response)
                       (if response
                           (let ((issue-data (assoc 'issue (assoc 'issueCreate (assoc 'data response)))))
                             (message "Created issue %s: %s"
                                      (cdr (assoc 'identifier issue-data))
                                      (cdr (assoc 'title issue-data)))
                             (when callback
                               (funcall callback issue-data)))
                         (message "Failed to create issue")
                         (when callback
                           (funcall callback nil)))))

         (error-fn (lambda (_error _response _data)
                     (message "Failed to create issue")
                     (when callback
                       (funcall callback nil)))))

    (linear-emacs--graphql-request-async query variables success-fn error-fn)))

(defun linear-emacs-create-issue (title description team-id)
  "Create a new issue (synchronous wrapper for backward compatibility).
TITLE is the issue title.
DESCRIPTION is the issue description.
TEAM-ID is the team to create the issue in."
  (let ((issue nil)
        (completed nil))

    (linear-emacs-create-issue-async
     title description team-id
     (lambda (result)
       (setq issue result)
       (setq completed t)))

    ;; Wait for completion
    (while (not completed)
      (sleep-for 0.1))

    issue))

;;; Issue State Management (Async)

(defun linear-emacs-get-states-async (team-id callback)
  "Asynchronously get workflow states for TEAM-ID.
CALLBACK is called with the list of states."
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

         (success-fn (lambda (response)
                       (when response
                         (let ((states (cdr (assoc 'nodes (assoc 'states (assoc 'team (assoc 'data response)))))))
                           (when callback
                             (funcall callback states))))))

         (error-fn (lambda (_error _response _data)
                     (when callback
                       (funcall callback nil)))))

    (linear-emacs--graphql-request-async query variables success-fn error-fn)))

(defun linear-emacs-get-states (team-id)
  "Get workflow states for TEAM-ID (synchronous wrapper)."
  (let ((states nil)
        (completed nil))

    (linear-emacs-get-states-async
     team-id
     (lambda (result)
       (setq states result)
       (setq completed t)))

    ;; Wait for completion
    (while (not completed)
      (sleep-for 0.1))

    states))

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

;;; Team Member and Project Management

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

(defun linear-emacs-get-projects (team-id)
  "Get a list of projects for the given TEAM-ID."
  (linear-emacs--log "Fetching projects for team %s" team-id)
  (let* ((query "query GetProjects($teamId: String!) {
  team(id: $teamId) {
    projects {
      nodes {
        id
        name
        description
        state
      }
    }
  }
}")
         (variables `(("teamId" . ,team-id)))
         (response (linear-emacs--graphql-request query variables)))
    (when response
      (let ((projects (cdr (assoc 'nodes (assoc 'projects (assoc 'team (assoc 'data response)))))))
        ;; Convert vector to list if needed
        (when (vectorp projects)
          (setq projects (append projects nil)))
        (linear-emacs--log "Retrieved %d projects" (length projects))
        projects))))

(defun linear-emacs-select-project (team-id)
  "Prompt user to select a project from TEAM-ID."
  (let* ((projects (linear-emacs-get-projects team-id))
         (project-names (when projects
                          (mapcar (lambda (project)
                                    (cons (cdr (assoc 'name project)) project))
                                  projects)))
         (selected (when project-names
                     (completing-read "Select project (optional): "
                                      (cons "None" project-names) nil t nil nil "None"))))
    (unless (string= selected "None")
      (cdr (assoc selected project-names)))))

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
  Returns a plist with :todo-state, :issue-id, :issue-identifier, and :team-id.
  Uses org-mode APIs for robust property extraction."
  (let* ((todo-state (org-get-todo-state))
         (valid-states (mapcar #'cdr linear-emacs-issues-state-mapping))
         (issue-id nil)
         (issue-identifier nil)
         (team-id nil))

    ;; Only extract properties if this heading has a valid Linear TODO state
    (when (and todo-state (member todo-state valid-states))
      (setq issue-id (org-entry-get nil "ID"))
      (setq issue-identifier (org-entry-get nil "ID-LINEAR"))
      (let ((team-name (org-entry-get nil "TEAM")))
        (when team-name
          (setq team-id (linear-emacs--get-team-id-by-name team-name)))))

    ;; Return properties as plist
    (list :todo-state todo-state
          :issue-id issue-id
          :issue-identifier issue-identifier
          :team-id team-id)))


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
      (let ((todo-states-pattern (linear-emacs--get-todo-states-pattern)))
        (while (re-search-forward (format "^\\*\\*\\* \\(%s\\)" todo-states-pattern) nil t)
          (beginning-of-line)
          (linear-emacs--process-heading-at-point)
          (forward-line))))))

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
  (or (cdr (assoc state linear-emacs-issues-state-mapping))
      "TODO"))  ; Default fallback

(defun linear-emacs--map-org-state-to-linear (todo-state)
  "Map \='org-mode\=' TODO state to Linear state name.
  TODO-STATE is the \='org-mode\=' state string."
  (or (car (rassoc todo-state linear-emacs-issues-state-mapping))
      nil))


(defun linear-emacs--get-included-linear-states ()
  "Get list of Linear states to include based on the mapping.
  Returns a list of lowercased state names for case-insensitive comparison."
  (mapcar (lambda (mapping)
            (downcase (car mapping)))
          linear-emacs-issues-state-mapping))

(defun linear-emacs--get-todo-states-pattern ()
  "Get or generate the regex pattern for matching org-mode TODO states.
  The pattern is built from the org-mode states in `linear-emacs-issues-state-mapping'.
  The result is cached in `linear-emacs-todo-states-pattern' for performance."
  (or linear-emacs-todo-states-pattern
      (let ((org-states (mapcar #'cdr linear-emacs-issues-state-mapping)))
        ;; Generate pattern like "TODO\\|IN-PROGRESS\\|IN-REVIEW\\|BACKLOG\\|BLOCKED\\|DONE"
        (setq linear-emacs-todo-states-pattern
              (mapconcat #'regexp-quote org-states "\\|")))))

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

(defun linear-emacs--format-date-as-org (date-string)
  "Convert Linear DATE-STRING (YYYY-MM-DD) to org date format.
Returns nil if DATE-STRING is nil or invalid."
  (when (and date-string (not (eq date-string 'null)) (stringp date-string))
    (ignore-errors
      (format-time-string "<%Y-%m-%d %a>" (date-to-time date-string)))))

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
         (project-id (or (and project-assoc (cdr (assoc 'id project-assoc))) ""))
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
         (cycle-data (cdr (assoc 'cycle issue)))
         (cycle-end (and cycle-data
                         (not (eq cycle-data 'null))
                         (cdr (assoc 'endsAt cycle-data))))
         (deadline (linear-emacs--format-date-as-org cycle-end))
         (scheduled (or (linear-emacs--format-date-as-org (cdr (assoc 'startedAt issue)))
                        (format-time-string "<%Y-%m-%d %a>")))
         (result ""))

    ;; Build the org entry as a string
    ;; Sanitize title to ensure org compatibility
    (let ((sanitized-title (replace-regexp-in-string "\\[\\|\\]" "" title)))
      (setq result (concat result (format "*** %s %s %s\n" todo-state priority sanitized-title))))

    ;; Add planning line (SCHEDULED and/or DEADLINE)
    (when (or scheduled deadline)
      (setq result (concat result
                           (when scheduled (format "SCHEDULED: %s" scheduled))
                           (when (and scheduled deadline) " ")
                           (when deadline (format "DEADLINE: %s" deadline))
                           "\n")))

    (setq result (concat result ":PROPERTIES:\n"))
    (setq result (concat result (format ":ID:       %s\n" id)))
    (setq result (concat result (format ":ID-LINEAR: %s\n" identifier)))
    (setq result (concat result (format ":TEAM: %s\n" team-name)))

    ;; Format description as single line (org properties must be single-line)
    ;; Replace newlines with spaces and clean up for org compatibility
    (let ((clean-desc (replace-regexp-in-string "[\n\r]+" " " description)))
      (setq clean-desc (replace-regexp-in-string "\"" "'" clean-desc))
      (setq clean-desc (string-trim clean-desc))
      (setq result (concat result (format ":DESCRIPTION: %s\n" clean-desc))))

    (setq result (concat result (format ":PRIORITY: %s\n" (linear-emacs--get-linear-priority-name priority-num))))

    ;; Format labels properly
    (if (string-empty-p labels)
        (setq result (concat result ":LABELS: []\n"))
      (setq result (concat result (format ":LABELS: [%s]\n"
                                          (replace-regexp-in-string "," ", " labels)))))

    (setq result (concat result (format ":PROJECT: %s\n" project-name)))
    (setq result (concat result (format ":LINK: %s\n" link)))
    (setq result (concat result (format ":PROJECT-ID: %s\n" project-id)))
    (setq result (concat result ":END:\n"))

    result))

;;; User-facing Commands (Async)

;;;###autoload
(defun linear-emacs-list-issues (&optional project-id)
  "Update linear.org file with assigned Linear issues and display it.
Only shows issues with statuses TODO, IN-PROGRESS, IN-REVIEW, BACKLOG, and BLOCKED.
Optionally filter by PROJECT-ID.
This is now async and shows progress during fetching."
  (interactive)
  (linear-emacs--log "Executing linear-emacs-list-issues")

  (linear-emacs--progress "Fetching issues from Linear...")

  (linear-emacs-get-issues-async
   (lambda (issues)
     (let* ((org-file-path linear-emacs-org-file-path)
            (include-statuses (linear-emacs--get-included-linear-states)))

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
             (linear-emacs--progress "Writing %d issues to org file..." (length issues))

             ;; Update org file
             (condition-case err
                 (progn
                   (with-temp-buffer
                     ;; Insert header - keywords must be OUTSIDE the PROPERTIES drawer
                     (insert "#+title: Linear issues assigned to me\n")
                     (insert "#+STARTUP: overview\n")
                     (insert "#+TODO: TODO NEXT WAITING | DONE\n")
                     (insert "#+filetags: :twai:b:\n\n")

                     ;; Insert issues
                     (dolist (issue issues)
                       (insert (linear-emacs--format-issue-as-org-entry issue)))

                     ;; Write to file
                     (linear-emacs--log "Writing %d issues to %s" (length issues) org-file-path)
                     (make-directory (file-name-directory org-file-path) t) ;; Ensure directory exists
                     (write-region (point-min) (point-max) org-file-path nil 'quiet))

                   ;; Open the org file
                   ;; (find-file org-file-path)  ; Commented out to avoid buffer switching before org-todo-list
                   (message "Updated Linear issues in %s with %d active issues"
                            org-file-path (length issues)))

               ;; Handle errors
               (error (progn
                        (linear-emacs--log "Error updating linear.org: %s" (error-message-string err))
                        (message "Error updating linear.org: %s" (error-message-string err))))))

         (message "No issues found or failed to retrieve issues"))))
   project-id))

;;;###autoload
(defun linear-emacs-list-issues-by-project ()
  "List Linear issues filtered by a selected project.
Uses async API for better performance."
  (interactive)
  ;; First select team
  (let* ((team (if linear-emacs-default-team-id
                   (list (cons 'id linear-emacs-default-team-id))
                 (linear-emacs-select-team)))
         (team-id (cdr (assoc 'id team))))
    (if team-id
        (let* ((project (linear-emacs-select-project team-id))
               (project-id (and project (cdr (assoc 'id project)))))
          (if project-id
              (progn
                (message "Fetching issues for project: %s" (cdr (assoc 'name project)))
                (linear-emacs-list-issues project-id))
            (message "No project selected")))
      (message "No team selected"))))

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

               ;; Get project
               (selected-project (linear-emacs-select-project team-id))
               (selected-project-id (and selected-project (cdr (assoc 'id selected-project))))

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
                            `(("labelIds" . [,selected-type])))
                        ,@(when selected-project-id
                            `(("projectId" . ,selected-project-id)))))

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
  (linear-emacs--progress "Testing Linear API connection...")

  (let* ((query "query { viewer { id name } }"))
    (linear-emacs--graphql-request-async
     query
     nil
     (lambda (response)
       (if response
           (let ((viewer (assoc 'viewer (assoc 'data response))))
             (message "Connected to Linear as: %s" (cdr (assoc 'name viewer))))
         (message "Failed to connect to Linear API")))
     (lambda (_error _response _data)
       (message "Failed to connect to Linear API")))))

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
