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

#ifndef MUPDF_HELPERS_H
#define MUPDF_HELPERS_H

#include "render-core.h"
#include <mupdf/fitz.h>
#include <stdbool.h>
#include <stddef.h>

int
doc_page_width(EmacsWinState *win_state);
int
doc_page_height(EmacsWinState *win_state);

void
fail(const char *msg);
void
lock_mutex(void *user, int lock);
void
unlock_mutex(void *user, int lock);
int
init_main_ctx(DocState *state);
int
load_mupdf_doc(DocState *state);

void
reset_doc_state(DocState *state);
void
free_cached_page(DocState *state, CachedPage *cp);
void
free_cache_window(DocState *state);
void
free_cached_pages_pool(DocState *state);

#endif
