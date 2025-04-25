# Linear.app for Emacs

This package provides integration between Emacs and Linear.app, allowing you to view and manage your Linear issues without leaving Emacs.

## Features

- List, view, and create Linear issues
- Bi-directional synchronization between Linear.app and org-mode
- Map Linear workflow states to org-mode TODO states
- Update issue details from either Linear or Emacs
- Automatically sync changes between systems

## Installation

### Manual Installation

1. Clone this repository:
   ```
   git clone https://github.com/anegg0/linearel.git
   ```

2. Add the following to your Emacs configuration:
   ```elisp
   (add-to-list 'load-path "/path/to/linearel")
   (require 'linear)
   ```

3. Install the required dependencies:
   - request.el
   - dash.el
   - s.el

### Using use-package and straight.el

```elisp
(use-package linear
  :straight (:host github :repo "anegg0/linearel")
  :config
  (setq linear-api-key "your-api-key-here"))
```

### Using Doom Emacs

```elisp
;; In packages.el
(package! linear
  :recipe (:host github :repo "anegg0/linearel" :files ("*.el")))

;; In config.el
(use-package linear
  :config
  (setq linear-api-key "your-api-key-here"))
```

## Configuration

### Basic Setup

Set your Linear API key:

```elisp
(setq linear-api-key "your-api-key-here")
```

You can get your API key from Linear.app under Settings > Account > API > Personal API Keys.

Alternatively, set the `LINEAR_API_KEY` environment variable and load it with:

```elisp
(linear-load-api-key-from-env)
```

Optionally, set a default team ID to streamline issue creation:

```elisp
(setq linear-default-team-id "your-team-id")
```

### Org-Mode Integration

To enable bidirectional synchronization with org-mode:

```elisp
(require 'linear-org)
(setq linear-org-file (expand-file-name "linear.org" org-directory))
```

To customize the mapping between Linear states and org TODO keywords:

```elisp
(setq linear-org-state-mapping
      '(("Todo" . "TODO")
        ("In Progress" . "IN-PROGRESS")
        ("In Review" . "IN-REVIEW")
        ("Backlog" . "BACKLOG")
        ("Blocked" . "BLOCKED")
        ("Done" . "DONE")
        ("Canceled" . "CANCELED")
        ("Duplicate" . "DUPLICATE")))
```

Set up the matching TODO keywords in your linear.org file:

```org
#+TITLE: Linear Tasks
#+FILETAGS: :linear:
#+TODO: TODO IN-PROGRESS IN-REVIEW BACKLOG BLOCKED | DONE CANCELED DUPLICATE
```

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

### Org-Mode Integration Commands

- `M-x linear-org-sync-from-linear` - Pull issues from Linear into org-mode
- `M-x linear-org-sync-to-linear` - Push current org entry to Linear
- `M-x linear-org-capture-to-linear` - Create a new Linear issue from org capture
- `M-x linear-org-open-issue` - Open the current issue in your browser
- `M-x linear-org-start-auto-sync` - Start automatic synchronization
- `M-x linear-org-stop-auto-sync` - Stop automatic synchronization

### Recommended Keybindings

For Doom Emacs, you can set up convenient keybindings:

```elisp
(map! :leader
      (:prefix ("L" . "Linear")
       :desc "List issues" "l" #'linear-list-issues
       :desc "New issue" "n" #'linear-new-issue
       :desc "Sync from Linear" "s" #'linear-org-sync-from-linear
       :desc "Test connection" "t" #'linear-test-connection
       :desc "Toggle debug" "d" #'linear-toggle-debug))

(map! :map org-mode-map
      :localleader
      (:prefix ("L" . "Linear")
       :desc "Sync from Linear" "s" #'linear-org-sync-from-linear
       :desc "Sync to Linear" "p" #'linear-org-sync-to-linear
       :desc "Capture to Linear" "c" #'linear-org-capture-to-linear
       :desc "Open in browser" "o" #'linear-org-open-issue))
```

## Customization

Enable debug logging to troubleshoot issues:

```elisp
(setq linear-debug t)
```

Set the synchronization interval (in seconds):

```elisp
(setq linear-org-sync-interval 3600) ; Sync every hour
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

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.
