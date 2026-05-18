CASK ?= $(shell command -v cask 2>/dev/null || printf '%s/.cask/bin/cask' "$(HOME)")

.PHONY: test

test:
	$(CASK) exec buttercup -L . tests
