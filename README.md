# Linear.app for Emacs

This package provides integration between Emacs and Linear.app, allowing you to view and manage your Linear issues without leaving Emacs.

## Installation

### Manual Installation

1. Clone this repository:
   ```
   git clone https://github.com/anegg0/linearel.git
   ```

2. Add the following to your Emacs configuration:
   ```elisp
   (add-to-list 'load-path "/path/to/linear.el")
   (require 'linear)
   ```

3. Install the required dependencies:
   - request.el
   - dash.el
   - s.el

### With use-package and straight.el

```elisp
(use-package linear
  :straight (:host github :repo "yourusername/linear.el")
  :config
  (setq linear-api-key "your-api-key-here"))
```

## Configuration

Set your Linear API key:

```elisp
(setq linear-api-key "your-api-key-here")
```

You can get your API key from Linear.app under Settings > Account > API > Personal API Keys.

Optionally, set a default team ID to streamline issue creation:

```elisp
(setq linear-default-team-id "your-team-id")
```

## Usage

### Commands

- `M-x linear-list-issues` - Display your assigned issues
- `M-x linear-new-issue` - Create a new issue

## Roadmap

Planned features:

- Issue detail view
- Commenting on issues
- Changing issue status
- Keyboard shortcuts for navigation
- Real-time notifications
- Team and project filtering

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.
