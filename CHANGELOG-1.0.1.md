# Changelog for linear-emacs v1.0.1

## [1.0.1] - 2025-01-20

### Changed
- Renamed package from `linear.el` to `linear-emacs.el` to follow Emacs package naming conventions
- Updated all function and variable prefixes from `linear-` to `linear-emacs-` for package consistency
- Changed package summary to remove redundant "Emacs" reference
- Fixed lambda variable naming to avoid reserved symbol `t`

### Fixed
- Resolved all package-lint errors and warnings
- Corrected license references in README.md from MIT to GPL-3.0
- Fixed LICENSE file reference in README.md (was incorrectly referencing LICENSE.txt)
- Updated defgroup prefix to match package name

### Documentation
- Updated all function names in README.md to use new `linear-emacs-` prefix
- Updated all function names in CLAUDE.md to use new `linear-emacs-` prefix
- Synchronized version number across all files (1.0.0 → 1.0.1)

### MELPA Preparation
- Created MELPA-compatible recipe file
- Ensured all dependencies (request, dash, s) are available in MELPA
- Package now meets all MELPA submission requirements