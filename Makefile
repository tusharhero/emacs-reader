# Makefile for Emacs Reader’s dynamic module

SUBMODULE_DIR := $(CURDIR)/dep
MUPDF_DIR := $(SUBMODULE_DIR)/mupdf
LIBMUPDF := $(MUPDF_DIR)/build/shared-release/libmupdf.so.26.0

CC = gcc
MUPDF_HEADERS = $(MUPDF_DIR)/include/
CFLAGS = -Wall -Wextra -fPIC -I$(dir $(MUPDF_HEADERS))
LDFLAGS = -L$(dir $(LIBMUPDF)) -lmupdf
RPATHS = -Wl,-rpath,$(MUPDF_DIR)/build/shared-release/

SRCS = render/helpers.c render/render-core.c
OBJS = $(SRCS:.c=.o)

LIB_NAME = render-core.so

.PHONY: all clean submodule

# Top-level build target
all: $(LIB_NAME)

# Build render-core.so, ensure libmupdf.so is built first
$(LIB_NAME): $(OBJS) $(LIBMUPDF)
	$(CC) -shared -o $@ $^ $(LDFLAGS) $(RPATHS)

# Rule to build the submodule shared library
$(LIBMUPDF):
	git submodule update --init --recursive
	$(MAKE) -C $(MUPDF_DIR) shared USE_SYSTEM_LIBS=no XCFLAGS="-DLCMS2MT_PREFIX=lcms2mt_"

# Rule to compile C source files into object files
%.o: %.c render/helpers.h
	$(CC) $(CFLAGS) -c $< -o $@

# Clean up build artifacts
clean:
	rm -f $(OBJS) $(LIB_NAME)
