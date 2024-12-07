# Name of the executable
EXEC = myAdventure

# List of source files
LISP_SRC = game.lisp textAdventure.lisp

# SBCL command to create the executable
SBCL_CMD = (sb-ext:save-lisp-and-die \"$(EXEC)\" :executable t :toplevel 'start)

# Build the executable
$(EXEC):
	sbcl --no-userinit --non-interactive \
		--load textAdventure.lisp \
		--load game.lisp \
		--eval "$(SBCL_CMD)"

# Clean build files
clean:
	rm -f $(EXEC)

# Phony targets
.PHONY: clean
