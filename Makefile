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

# Detect platform
OS_NAME := $(shell uname)
ifeq ($(OS),Windows_NT)
  PLATFORM := windows
else ifeq ($(OS_NAME),Darwin)
  PLATFORM := macos
else
  PLATFORM := linux
endif

# Shared library extension and object file extension per platform
ifeq ($(PLATFORM),macos)
  OBJ_EXT := .o
  CC := gcc
  ifneq (,$(shell which port 2>/dev/null))
    CFLAGS += -DMACOS -I/opt/local/include
    LDFLAGS := -dynamiclib -L/opt/local/lib -lmupdf
  else ifneq (,$(shell which brew 2>/dev/null))
    HOMEBREW_PREFIX := $(shell brew --prefix)
    CFLAGS += -DMACOS -I/$(HOMEBREW_PREFIX)/include
    LDFLAGS := -dynamiclib -L$(HOMEBREW_PREFIX)/lib -lmupdf
  endif
else
  OBJ_EXT := .o
  CC := gcc
  CFLAGS += -DLINUX -pthread
  LDFLAGS := -shared -lmupdf -lpthread
endif

# Module filenames and library paths
LIB_NAME := render-core.so

# Compiler and headers
CFLAGS += -Wall -Wextra -fPIC

# Source files and object targets
SRCS := render/elisp-helpers.c render/mupdf-helpers.c render/render-threads.c render/render-core.c render/render-theme.c
OBJS := $(SRCS:%.c=%$(OBJ_EXT))

.PHONY: all clean submodule

# Default build target
all: $(LIB_NAME)

# Build render-core.so
$(LIB_NAME): $(OBJS)
	$(CC) $(LDFLAGS) -o $@ $^

# Compile C sources into platform-specific object files
%$(OBJ_EXT): %.c
	$(CC) $(CFLAGS) -c $< -o $@

# Clean artifacts
clean:
	rm -f $(OBJS) $(LIB_NAME)
