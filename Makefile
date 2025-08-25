# Makefile for enkan-repl
EMACS ?= emacs
BATCH = $(EMACS) --batch -Q

.PHONY: help test test-docs test-all compile checkdoc lint check clean install-deps format docs extract-api generate-template

help: ## Show this help message
	@echo "enkan-repl Quality Check Commands"
	@echo "========================================"
	@echo ""
	@echo "Available commands:"
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-15s\033[0m %s\n", $$1, $$2}'
	@echo ""
	@echo "Setup (run once):"
	@echo "  make install-deps"
	@echo ""
	@echo "Quick start:"
	@echo "  make install-deps && make check"

install-deps: ## Install required dependencies (package-lint)
	$(BATCH) --eval "(progn (require 'package) (add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\")) (package-refresh-contents) (package-install 'package-lint))"

test: ## Run ERT tests
	$(BATCH) --eval "(add-to-list 'load-path \".\")" -l scripts/load-all-tests.el

test-docs: ## Run generate-docs tests
	$(BATCH) --eval "(add-to-list 'load-path \".\")" -l scripts/load-all-tests.el

test-all: test test-docs ## Run all tests (core package and development tools)

compile: ## Byte compile with warnings as errors
	$(BATCH) --eval "(setq byte-compile-error-on-warn t)" --eval "(byte-compile-file \"enkan-repl.el\")"

checkdoc: ## Check documentation format
	$(BATCH) --eval "(checkdoc-file \"enkan-repl.el\")"

lint: ## Run package-lint checks
	$(BATCH) --eval "(progn (package-initialize) (require 'package-lint) (with-temp-buffer (insert-file-contents \"enkan-repl.el\") (emacs-lisp-mode) (package-lint-current-buffer)))"

format: ## Auto-format elisp files using built-in indent-region (spaces only)
	$(BATCH) --eval "(progn (find-file \"enkan-repl.el\") (setq-local indent-tabs-mode nil) (untabify (point-min) (point-max)) (mark-whole-buffer) (indent-region (point-min) (point-max)) (save-buffer))"
	$(BATCH) --eval "(progn (find-file \"test/enkan-repl-test.el\") (setq-local indent-tabs-mode nil) (untabify (point-min) (point-max)) (mark-whole-buffer) (indent-region (point-min) (point-max)) (save-buffer))"

check: test compile checkdoc lint format ## Run all quality checks including formatting

check-ci: test compile checkdoc format ## Run quality checks for CI (without package-lint)

clean: ## Remove compiled files
	rm -f *.elc

extract-api: ## Extract public API documentation as org file
	$(BATCH) -l scripts/generate-docs.el --eval "(extract-public-api)"

generate-template: ## Generate default template file
	$(BATCH) -l scripts/generate-docs.el --eval "(generate-default-template)"

generate-constants: ## Generate precompiled constants for cheat-sheet performance
	$(BATCH) -l scripts/generate-constants.el --eval "(generate-cheat-sheet-constants)"

docs: ## Generate all documentation files (constants first, then docs)
	$(BATCH) -l scripts/generate-constants.el --eval "(generate-cheat-sheet-constants)"
	$(BATCH) -l scripts/generate-docs.el --eval "(generate-all-docs)"
