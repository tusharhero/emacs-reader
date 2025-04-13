# Makefile for Emacs Reader’s dynamic module

# --- Configuration ---
# Try to find emacs-module.h - users might need to adjust this
# Common location if emacs source is available:

# EMACS_SRC_DIR ?= /path/to/emacs/source
# EMACS_MODULE_HEADER_DIR = $(EMACS_SRC_DIR)/src

# Or, if installed via package manager (example for some systems):
EMACS_MODULE_HEADER_DIR ?= /usr/include/

# If you are on a Guix or Nix system, your package declaration should substitute this path
# accordingly. In Guix one can use substitute*.

# Use pkg-config for MuPDF if available, otherwise manual paths are needed
MUPDF_LIBS  ?= -lmupdf

# Compiler and flags
CC = gcc
CFLAGS ?= -Wall -g

# Add Emacs module header path if found/set
ifdef EMACS_MODULE_HEADER_DIR
  CFLAGS += -I$(EMACS_MODULE_HEADER_DIR)
endif
CFLAGS += -fPIC
LDFLAGS ?= -shared
LDLIBS ?= $(MUPDF_LIBS)

# --- Files ---
TARGET_MODULE = render-core.so
SOURCES = render/render-pdf.c render/helpers.c
OBJECTS = $(SOURCES:.c=.o)

# --- Rules ---
.PHONY: all clean

all: $(TARGET_MODULE)

$(TARGET_MODULE): $(OBJECTS)
	$(CC) $(LDFLAGS) $(OBJECTS) $(LDLIBS) -o $@
	@echo "Module $(TARGET_MODULE) built successfully."
	@echo "Ensure Emacs module header path and MuPDF paths were correct."
	@echo "If EMACS_MODULE_HEADER_DIR was not found/set, compilation might fail."

%.o: %.c render/helpers.h Makefile
	$(CC) $(CFLAGS) -c $< -o $@

render/%.o: render/%.c render/helpers.h Makefile
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f $(OBJECTS) $(TARGET_MODULE)
	@echo "Cleaned object files and module."

# --- Help for User ---
# Add a check or warning if essential paths seem missing
# (This is a basic example)
check-config:
ifndef EMACS_MODULE_HEADER_DIR
	@echo "Warning: EMACS_MODULE_HEADER_DIR is not set. Set it if compilation fails."
	@echo "         Example: make EMACS_MODULE_HEADER_DIR=/usr/include/emacs"
endif

ifeq ($(strip $(MUPDF_LIBS)),)
	@echo "Warning: Could not determine MuPDF LIBS (via pkg-config)."
	@echo "         Set MUPDF_LIBS manually if compilation fails."
endif

# Optional: Rule for package.el integration (e.g., for MELPA)
# This target might be called by package-build.el or similar tools.
build: all
