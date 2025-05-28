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

bool create_buffers(fz_context *ctx, fz_buffer **curr_buf, fz_buffer **prev_buf,
                    fz_buffer **next_buf);
bool create_outputs(fz_context *ctx, fz_output **curr_out, fz_output **prev_out,
                    fz_output **next_out, fz_buffer *curr_buf,
                    fz_buffer *prev_buf, fz_buffer *next_buf);
void drop_all_buffers(fz_context *ctx, fz_buffer *curr, fz_buffer *prev,
                      fz_buffer *next);
void drop_all_outputs(fz_context *ctx, fz_output *curr, fz_output *prev,
                      fz_output *next);
void drop_all_devices(fz_context *ctx, fz_device *curr, fz_device *prev,
                      fz_device *next);
void close_all_outputs(fz_context *ctx, fz_output *curr, fz_output *prev,
                       fz_output *next);
void close_all_devices(fz_context *ctx, fz_device *curr, fz_device *prev,
                       fz_device *next);

int doc_page_width(DocState *state);
int doc_page_length(DocState *state);


void fail(const char *msg);
void lock_mutex(void *user, int lock);
void unlock_mutex(void *user, int lock);
void init_main_ctx(DocState *state);
void open_document(DocState *state);


void clean_up_svg_data(DocState *state);
void drop_all_doc_pages(fz_context *ctx, DocState *state);
void reset_doc_state(DocState *state);

#endif
