//
// Copyright (C) 2025  Divya Ranjan Pattanaik
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

#ifndef RENDER_CORE_H
#define RENDER_CORE_H

#include "emacs-module.h"
#include <pthread.h>
#include <mupdf/fitz.h>

#define MAX_CACHE_SIZE 11
#define MAX_CACHE_WINDOW (MAX_CACHE_SIZE / 2)

typedef enum {
    PAGE_STATUS_EMPTY,
    PAGE_STATUS_RENDERING,
    PAGE_STATUS_READY,
    PAGE_STATUS_ERROR
} PageStatus;

typedef struct {
    int page_num;
    char *svg_data;
    size_t svg_size;
    PageStatus status;
    pthread_mutex_t mutex;
} CachedPage;

// DocState
typedef struct {
  fz_context *ctx;
  fz_document *doc;

  CachedPage **cached_pages_pool;
  CachedPage *cache_window[MAX_CACHE_SIZE];

  char *path;
  int pagecount;
  char *svg_background;
  char *svg_foreground;
  int prev_page_number;
  int current_page_number;
  int next_page_number;
  char *current_svg_data;
  size_t current_svg_size;
  char *next_svg_data;
  size_t next_svg_size;
  char *prev_svg_data;
  size_t prev_svg_size;
  fz_page *current_page;
  fz_page *next_page;
  fz_page *prev_page;
  fz_rect page_bbox;
  fz_outline *outline;
} DocState;


typedef struct {
  DocState *state;
  CachedPage *cp;
} render_thread_args;

// Function declarations
int load_mupdf_doc(DocState *state);
int load_pages(DocState *state, int page_number);
int render_pages(DocState *state, int page_number);

// Emacs module functions
emacs_value emacs_load_doc(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                           void *data);
emacs_value emacs_next_page(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                            void *data);
emacs_value emacs_prev_page(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                            void *data);
emacs_value emacs_first_page(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                             void *data);
emacs_value emacs_last_page(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                            void *data);
emacs_value emacs_goto_page(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                            void *data);
emacs_value emacs_doc_scale_page(emacs_env *env, ptrdiff_t nargs,
                                 emacs_value *args, void *data);

#endif // RENDER_CORE_H
