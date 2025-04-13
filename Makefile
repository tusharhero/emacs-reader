# Makefile for Emacs Reader’s dynamic module

CC = gcc
CFLAGS = -Wall -Wextra -fPIC -I/usr/include/
LDFLAGS = -lmupdf

SRCS = helpers.c render-pdf.c
OBJS = $(SRCS:.c=.o)

LIB_NAME = render-core.so

all: render-core.so

$(LIB_NAME): $(OBJS)
        $(CC) -shared -o $@ $^ $(LDFLAGS)

%.o: %.c helpers.h
        $(CC) $(CFLAGS) -c $< -o $@

clean:
        rm -f $(OBJS) $(LIB_NAME)
