### Makefile for Emacs Reader’s dynamic module
##
## Copyright (C) 2025 Divya Ranjan Pattanaik
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <https://www.gnu.org/licenses/>.

# Detect Guix or Nix environment
HAVE_GUIX := $(shell command -v guix >/dev/null 2>&1 && echo yes || echo no)
HAVE_NIX  := $(shell command -v nix >/dev/null 2>&1 && echo yes || echo no)
USE_PKGCONFIG := yes

ifeq ($(HAVE_GUIX),yes)
  $(info Guix detected: skipping pkg-config checks.)
  USE_PKGCONFIG := no
else ifeq ($(HAVE_NIX),yes)
  $(info Nix detected: skipping pkg-config checks.)
  USE_PKGCONFIG := no
endif

# Compiler and base flags
CC := gcc
CFLAGS += -Wall -Wextra -fPIC
LDFLAGS :=

# Platform-specific flags
OS_NAME := $(shell uname)
ifeq ($(OS_NAME),Darwin)
  CFLAGS += -DMACOS
  LDFLAGS += -dynamiclib
else
  CFLAGS += -DLINUX -pthread
  LDFLAGS += -shared -lpthread
endif

# Required MuPDF version
REQUIRED_VERSION := 1.26.0

# pkg-config handling (unless inside Guix/Nix)
ifeq ($(USE_PKGCONFIG),yes)
  PKG_EXISTS := $(shell pkg-config --exists mupdf && echo yes || echo no)
  ifeq ($(PKG_EXISTS),no)
    $(error "MuPDF not found via pkg-config. Please install it or set PKG_CONFIG_PATH.")
  endif

  MUPDF_VERSION := $(shell pkg-config --modversion mupdf)
  VER_OK := $(shell [ "$$(printf '%s\n' $(REQUIRED_VERSION) $(MUPDF_VERSION) | sort -V | head -n1)" = "$(REQUIRED_VERSION)" ] && echo yes || echo no)
  ifeq ($(VER_OK),no)
    $(error "MuPDF version $(MUPDF_VERSION) too old. Require ≥ $(REQUIRED_VERSION).")
  endif

  CFLAGS += $(shell pkg-config --cflags mupdf)
  LDFLAGS += $(shell pkg-config --libs mupdf)
else
  # Trust the environment to provide MuPDF correctly (Guix/Nix shell)
  CFLAGS += -DMUPDF_NO_PKGCONFIG
  LDFLAGS += -lmupdf
endif

# Output and source files
LIB_NAME := render-core.so
SRCS := render/elisp-helpers.c render/mupdf-helpers.c render/render-threads.c render/render-core.c render/render-theme.c
OBJS := $(SRCS:%.c=%.o)

.PHONY: all clean

# Default target
all: $(LIB_NAME)

# Link shared library
$(LIB_NAME): $(OBJS)
	$(CC) $(LDFLAGS) -o $@ $^

# Compile .c to .o
%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

# Clean build artifacts
clean:
	rm -f $(OBJS) $(LIB_NAME)
