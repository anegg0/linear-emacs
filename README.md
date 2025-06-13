# Linear.app for Emacs

This package provides integration between Emacs and Linear.app, allowing you to view and manage your Linear issues without leaving Emacs.

## Features

- List, view, and create Linear issues directly from Emacs
- Bi-directional synchronization between Linear.app and org-mode
- Map Linear workflow states to org-mode TODO states
- Update issue details from either Linear or Emacs
- Track issue priorities, labels, and assignments
- Support for issue filtering and organization
- Automatically sync changes between systems

## Installation

### Manual Installation

1. Clone this repository:
   ```
   git clone ssh://git@codeberg.org/anegg0/linear-emacs.git
   ```

2. Add the following to your Emacs configuration:
   ```elisp
   (add-to-list 'load-path "/path/to/linear-emacs")
   (require 'linear)
   ```

3. Install the required dependencies:
   - request.el
   - dash.el
   - s.el

### Using Doom Emacs

```elisp
(package! linear
  :recipe (:host github :repo "anegg0/linear-emacs" :files ("*.el")))
```

## Configuration

### Basic Setup

There are several ways to set your Linear API key:

#### Direct Setting (Not Recommended)

```elisp
(setq linear-api-key "your-api-key-here")
```

#### Environment Variable (Better)

Set the `LINEAR_API_KEY` environment variable and load it with:

```elisp
(linear-load-api-key-from-env)
```

#### Using auth-source (Recommended)

For secure credential storage, use Emacs' built-in auth-source package:

```elisp
(defun my/linear-load-api-key-from-auth-source ()
  "Load Linear API key from auth-source."
  (interactive)
  (require 'auth-source)
  (let* ((auth-info (auth-source-search :host "api.linear.app" :user "apikey" :max 1))
         (secret (when auth-info
                   (funcall (plist-get (car auth-info) :secret)))))
    (if secret
        (progn
          (setq linear-api-key secret)
          (message "Successfully loaded Linear API key from auth-source"))
      (message "Failed to retrieve Linear API key from auth-source"))))

;; Call this function when linear loads
(after! linear
  (my/linear-load-api-key-from-auth-source))
```

To set up your auth-source entry:

1. For macOS Keychain users, run:
   ```
   security add-generic-password -a apikey -s api.linear.app -w YOUR_API_KEY
   ```

2. For `~/.authinfo.gpg` users, add this line to your file:
   ```
   machine api.linear.app login apikey password YOUR_API_KEY
   ```

Note: The hostname (`api.linear.app`) and username (`apikey`) in your auth-source entry must match exactly what you're using in the `auth-source-search` function.

You can get your API key from Linear.app under Settings > Account > API > Personal API Keys.

Optionally, set a default team ID to streamline issue creation:

```elisp
(setq linear-default-team-id "your-team-id")
```

### Org-Mode Integration

To enable bidirectional synchronization with org-mode in Doom Emacs:

```elisp
;; In your config.el
(after! linear
  (linear-load-api-key-from-env)
  
  ;; Automatically enable two-way sync when linear.org is opened
  (defun my/enable-linear-org-sync ()
    "Enable Linear-org synchronization when linear.org is opened."
    (when (and buffer-file-name
               (string-match-p "linear\\.org$" buffer-file-name))
      (when (fboundp 'linear-enable-org-sync)
        (linear-enable-org-sync)
        (message "Linear-org synchronization enabled for this buffer"))))
  
  ;; Add hook to auto-enable sync when linear.org is opened
  (add-hook 'find-file-hook #'my/enable-linear-org-sync)
  
  ;; Enable sync for org-after-todo-state-change-hook
  (add-hook 'org-after-todo-state-change-hook
            (lambda ()
              (when (and buffer-file-name
                         (string-match-p "linear\\.org$" buffer-file-name)
                         (fboundp 'linear-sync-org-to-linear))
                (linear-sync-org-to-linear)))))
```

When you run `M-x linear-list-issues`, the package will create a `linear.org` file in your org-directory with all your assigned issues. The default location is:

```elisp
(expand-file-name "gtd/linear.org" org-directory)
```

You can customize this location in the `linear-list-issues` function if needed.

The org file will have the following structure:

```org
:PROPERTIES:
:ID:       a12acb12-8a69-4d15-a846-21e20ed2f3ae
#+title: Linear issues assigned to me
#+TAGS: :
#+filetags: :twai:b:
#+STARTUP: overview
#+TODO: TODO IN-PROGRESS IN-REVIEW BACKLOG BLOCKED | DONE
:END:

*** TODO [#B] Issue Title
:PROPERTIES:
:ID:       issue-unique-id
:ID-LINEAR: TEAM-123
:TEAM: Team Name
:DESCRIPTION: |
  Issue description goes here
:PRIORITY: High
:LABELS: [label1, label2]
:PROJECT: Project Name
:LINK: https://linear.app/issue/TEAM-123
:END:
```

When you change the TODO state of an issue in the org file, it will automatically synchronize with Linear.

### Priority Mapping

Map Linear priorities to org priorities:

```elisp
(setq linear-org-priority-mapping
      '((0 . nil)   ; No priority
        (1 . "A")   ; Urgent
        (2 . "B")   ; High
        (3 . "C")   ; Medium
        (4 . "D"))) ; Low
```

## Usage

### Commands

- `M-x linear-list-issues` - Display your assigned issues
- `M-x linear-new-issue` - Create a new issue
- `M-x linear-test-connection` - Test your Linear API connection
- `M-x linear-toggle-debug` - Toggle debug mode for troubleshooting
- `M-x linear-check-setup` - Verify your API key is loaded correctly

### Org-Mode Integration Commands

- `M-x linear-list-issues` - Pull issues from Linear into org-mode
- `M-x linear-sync-org-to-linear` - Sync the current org entry to Linear
- `M-x linear-enable-org-sync` - Enable automatic synchronization
- `M-x linear-disable-org-sync` - Disable automatic synchronization

#### Improved Synchronization Commands

The following commands are available in the optimized configuration:

- `M-x my/linear-sync-single-issue-at-point` - Sync only the current issue at point
- `M-x my/toggle-linear-auto-sync` - Toggle automatic syncing before showing todo list

### Recommended Keybindings

For Doom Emacs, you can set up convenient keybindings:

```elisp
(map! :leader
      (:prefix ("l" . "Linear")
       :desc "Sync all Linear issues" "s" #'linear-list-issues
       :desc "New issue" "n" #'linear-new-issue
       :desc "Toggle Linear auto-sync" "t" #'my/toggle-linear-auto-sync
       :desc "Test connection" "c" #'linear-test-connection
       :desc "Toggle debug" "d" #'linear-toggle-debug))
```

In the optimized configuration, these keybindings are already set up for you:

```elisp
(map! :leader
      :prefix "l"
      :desc "Sync all Linear issues" "s" #'linear-list-issues
      :desc "Toggle Linear auto-sync" "t" #'my/toggle-linear-auto-sync)
```

## Performance Optimization

To avoid synchronization performance issues, the optimized configuration:

1. Only synchronizes the current issue at point instead of all issues
2. Makes automatic synchronization optional with a toggle
3. Adds convenient keybindings for managing synchronization

```elisp
;; In your config.el
(after! linear
  ;; Improved synchronization function that only updates the changed issue
  (defun my/linear-sync-single-issue-at-point ()
    "Sync only the current issue at point to Linear API."
    (interactive)
    (save-excursion
      ;; Move to the beginning of the current heading
      (org-back-to-heading t)
      ;; Check if this is a Linear issue heading
      (when (looking-at "^\\*\\*\\* \\(TODO\\|IN-PROGRESS\\|IN-REVIEW\\|BACKLOG\\|BLOCKED\\|DONE\\)")
        ;; [Implementation details omitted for brevity]
        )))

  ;; Override linear-sync-org-to-linear to only sync the current issue
  (defun linear-sync-org-to-linear ()
    "Sync only the current issue to Linear API."
    (interactive)
    (my/linear-sync-single-issue-at-point))

  ;; Add convenient keybinding for manually syncing all issues
  (map! :leader
        :prefix "l"
        :desc "Sync all Linear issues" "s" #'linear-list-issues
        :desc "Toggle Linear auto-sync" "t" #'my/toggle-linear-auto-sync))
```

## Customization

Enable debug logging to troubleshoot issues:

```elisp
(setq linear-debug t)
```

## Troubleshooting

1. Check your connection:
   ```
   M-x linear-test-connection
   ```

2. Enable debug mode:
   ```
   M-x linear-toggle-debug
   ```

3. Verify your API key is loaded correctly:
   ```
   M-x linear-check-setup
   ```

4. Check the `*Messages*` buffer for detailed error information when debug mode is enabled

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

This project is licensed under the MIT License - see the [LICENSE.txt](LICENSE.txt) file for details.
