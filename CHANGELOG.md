# Changelog

All notable changes to linear-emacs will be documented in this file.

## [1.0.0] - 2025-01-20

### Initial Release

Linear-emacs is an Emacs/Linear.app integration package to manage Linear issues without leaving Emacs (or close).

### Features

#### Core Functionality
- **Full Linear API Integration**: Interact with Linear's GraphQL API directly from Emacs
- **Issue Management**: List, view, and create Linear issues without leaving Emacs
- **Team Support**: Select teams, view team members, and manage team-specific workflows
- **Rich Issue Creation**: Create issues with titles, descriptions, priorities, assignees, estimates, and labels
- **Workflow State Management**: Support for all Linear workflow states (Todo, In Progress, In Review, Backlog, Blocked, Done)

#### Org-Mode Integration
- **Bidirectional Synchronization**: Sync between Linear.app and org-mode
- **TODO State Mapping**: Automatic mapping between Linear workflow states and org-mode TODO states
- **Live Updates**: Changes in org-mode automatically propagate to Linear (and vice versa)
- **Filtering**: Only syncs active issues (excludes completed issues for better focus)
- **Asynchronous Updates**: Background synchronization for improved performance

#### User Experience
- **Secure API Key Management**: Support for environment variables and auth-source integration
- **Customizable Output**: Configure where Linear issues are stored in your org directory
- **Debug Mode**: Built-in debugging for troubleshooting API interactions
- **Pagination Support**: Handles large numbers of issues
- **Priority Mapping**: Maps Linear priorities to org-mode priorities (Urgent→A, High→B, Medium→C, Low→D)

### Technical Details

#### Commands
- `linear-list-issues` - Fetch and display assigned issues in org format
- `linear-new-issue` - Create new issues with full attribute support
- `linear-test-connection` - Verify API connectivity
- `linear-toggle-debug` - Enable/disable debug logging
- `linear-check-setup` - Validate configuration
- `linear-enable-org-sync` - Enable automatic synchronization
- `linear-disable-org-sync` - Disable automatic synchronization
- `linear-load-api-key-from-env` - Load API key from environment

#### Dependencies
- Emacs 27.1+
- request.el 0.3.0+
- dash.el 2.17.0+
- s.el 1.12.0+

### 📝 Configuration

Basic setup:
```elisp
(require 'linear)
(setq linear-api-key "your-api-key")  ; Or use auth-source/env variable
(setq linear-org-file-path (expand-file-name "gtd/linear.org" org-directory))
```

### Getting Started

1. Install the package and its dependencies
2. Set up your Linear API key (via customization, environment variable, or auth-source)
3. Run `M-x linear-test-connection` to verify setup
4. Use `M-x linear-list-issues` to fetch your assigned issues
5. Enable `linear-enable-org-sync` in your linear.org buffer for bidirectional sync

### Acknowledgments

Created by Gael Blanchemain who'd rather stay in Emacs than deal with other sleazy-corporate "Apps".

### 📄 License

Released under the GNU General Public License v3.0 or later.

---

For more information, bug reports, and contributions, visit:
- GitHub: https://github.com/anegg0/linear-emacs
- Codeberg: https://codeberg.org/anegg0/linear-emacs
