// Helper functions that complement the MuPDF C API

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

#include "mupdf-helpers.h"
#include "render-core.h"

pthread_mutex_t g_mupdf_mutex[FZ_LOCK_MAX];

/**
 * create_buffers - Creates mupdf buffers (wrappers around dynamic arrays) to be
 * used with outputs.
 * @ctx: mupdf context for which the buffers will be created
 * @curr_buff:
 * @prev_buff:
 * @next_buff:
 * The buffers are to be provided for operations that are local to a function or
 * sub-routine.  The default capacity of these buffers is set to 1024 bytes.
 * Return: 1 if success, 0 if error
 */

bool create_buffers(fz_context *ctx, fz_buffer **curr_buf, fz_buffer **prev_buf,
                    fz_buffer **next_buf) {
  fz_try(ctx) {
    *curr_buf = fz_new_buffer(ctx, 1024);
    *prev_buf = fz_new_buffer(ctx, 1024);
    *next_buf = fz_new_buffer(ctx, 1024);
  }

  fz_catch(ctx) {
    fprintf(stderr, "Cannot create buffer: %s\n", fz_caught_message(ctx));

    return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
}

/**
 * create_ouputs - Create mupdf output objects to be associated with each
buffer.
 * @ctx: mupdf context
 * @curr_out, @next_out, @prev_out: Three externally defined outputs (fz_output)
 * @curr_buff, @net_buff, @prev_buff: Corresponding buffers they’ll be
 * associated to.
 * It is good if the outputs to be associated are previously instantiated with
 * NULL.
 * Return: 1 if success, 0 if error
 */

bool create_outputs(fz_context *ctx, fz_output **curr_out, fz_output **prev_out,
                    fz_output **next_out, fz_buffer *curr_buf,
                    fz_buffer *prev_buf, fz_buffer *next_buf) {
  fz_try(ctx) {
    *curr_out = fz_new_output_with_buffer(ctx, curr_buf);
    *prev_out = fz_new_output_with_buffer(ctx, prev_buf);
    *next_out = fz_new_output_with_buffer(ctx, next_buf);
  }

  fz_catch(ctx) {
    fprintf(stderr, "Cannot create output: %s\n", fz_caught_message(ctx));
    return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
}

/*
  The following functions below are useful for extensive mupdf operations, as
  hapens in `load_doc' and `render_page' in render-core.c.
 */

/**
 * drop_all_buffers - Drop (free) all given fz_buffer objects.
 * @ctx: mupdf context
 * @curr: Current buffer to drop
 * @prev: Previous buffer to drop
 * @next: Next buffer to drop
 *
 * Drops all non-NULL buffers using the mupdf `fz_drop_buffer()` function.
 * Buffers that are NULL are safely ignored.
 */

void drop_all_buffers(fz_context *ctx, fz_buffer *curr, fz_buffer *prev,
                      fz_buffer *next) {
  if (curr)
    fz_drop_buffer(ctx, curr);
  if (prev)
    fz_drop_buffer(ctx, prev);
  if (next)
    fz_drop_buffer(ctx, next);
}

/**
 * drop_all_outputs - Drop (free) all given fz_output objects.
 * @ctx: mupdf context
 * @curr: Current output to drop
 * @prev: Previous output to drop
 * @next: Next output to drop
 *
 * Drops all non-NULL outputs using the mupdf `fz_drop_output()` function.
 * Outputs that are NULL are safely ignored.
 */

void drop_all_outputs(fz_context *ctx, fz_output *curr, fz_output *prev,
                      fz_output *next) {
  if (curr)
    fz_drop_output(ctx, curr);
  if (prev)
    fz_drop_output(ctx, prev);
  if (next)
    fz_drop_output(ctx, next);
}

/**
 * drop_all_devices - Drop (free) all given fz_device objects.
 * @ctx: mupdf context
 * @curr: Current device to drop
 * @prev: Previous device to drop
 * @next: Next device to drop
 *
 * Drops all non-NULL devices using the mupdf `fz_drop_device()` function.
 * Devices that are NULL are safely ignored.
 */

void drop_all_devices(fz_context *ctx, fz_device *curr, fz_device *prev,
                      fz_device *next) {
  if (curr)
    fz_drop_device(ctx, curr);
  if (prev)
    fz_drop_device(ctx, prev);
  if (next)
    fz_drop_device(ctx, next);
}

/**
 * close_all_outputs - Close all given fz_output objects.
 * @ctx: mupdf context
 * @curr: Current output to close
 * @prev: Previous output to close
 * @next: Next output to close
 *
 * Closes all non-NULL outputs using the mupdf `fz_close_output()` function.
 * Outputs that are NULL are safely ignored.
 */

void close_all_outputs(fz_context *ctx, fz_output *curr, fz_output *prev,
                       fz_output *next) {
  if (curr)
    fz_close_output(ctx, curr);
  if (prev)
    fz_close_output(ctx, prev);
  if (next)
    fz_close_output(ctx, next);
}

/**
 * close_all_devices - Close all given fz_device objects.
 * @ctx: mupdf context
 * @curr: Current device to close
 * @prev: Previous device to close
 * @next: Next device to close
 *
 * Closes all non-NULL devices using the mupdf `fz_close_device()` function.
 * Devices that are NULL are safely ignored.
 */

void close_all_devices(fz_context *ctx, fz_device *curr, fz_device *prev,
                       fz_device *next) {
  if (curr)
    fz_close_device(ctx, curr);
  if (prev)
    fz_close_device(ctx, prev);
  if (next)
    fz_close_device(ctx, next);
}

/**
 * doc_page_width - Provide integer width of the current document
 * @state: Pointer to DocState
 * Return: Integer width
 * It calculates the width of the mupdf page, has nothing to do with the Emacs
 * image for the document.
 */

int doc_page_width(DocState *state) {
  int width = (int)(state->page_bbox.x1 - state->page_bbox.x0);

  return width;
}

/**
 * doc_page_length - Provide integer length of the current document
 * @state: Pointer to DocState
 * Return: Integer length
 * It calculates the length of the mupdf page, has nothing to do with the Emacs
 * image for the document.
 */

int doc_page_length(DocState *state) {
  int length = (int)(state->page_bbox.y1 - state->page_bbox.y0);
  return length;
}

/**
 * clean_up_svg_data - Free and reset all SVG data buffers in the DocState.
 * @state: Pointer to the DocState whose SVG data will be cleaned up.
 *
 * This function checks each of the SVG data pointers (current, next, prev)
 * in the provided DocState. If a pointer is non-NULL, it frees the
 * allocated memory, sets the pointer to NULL, and resets the corresponding
 * size field to 0. Safely ignores any already-NULL pointers.
 */

void clean_up_svg_data(DocState *state) {
  if (state->current_svg_data) {
    free(state->current_svg_data);
    state->current_svg_data = NULL;
    state->current_svg_size = 0;
  }

  if (state->next_svg_data) {
    free(state->next_svg_data);
    state->next_svg_data = NULL;
    state->next_svg_size = 0;
  }

  if (state->prev_svg_data) {
    free(state->prev_svg_data);
    state->prev_svg_data = NULL;
    state->prev_svg_size = 0;
  }
}

/**
 * drop_all_doc_pages - Drop (release) all loaded pages in the DocState.
 * @ctx: mupdf context used to drop pages.
 * @state: Pointer to the DocState containing the pages to drop.
 *
 * Drops each of the page objects (prev, current, next) stored in the
 * DocState by calling `fz_drop_page()`. Any NULL page pointers are
 * safely ignored.
 */

void drop_all_doc_pages(fz_context *ctx, DocState *state) {
  if (state->prev_page)
    fz_drop_page(ctx, state->prev_page);
  if (state->current_page)
    fz_drop_page(ctx, state->current_page);
  if (state->next_page)
    fz_drop_page(ctx, state->next_page);
}

/**
 * reset_doc_state - Reset all fields of a DocState to their initial values.
 * @state: Pointer to the DocState to be reset.
 *
 * Clears out any existing data in the DocState by overwriting it with
 * an initializer that sets all pointers to NULL, integer/page-number
 * fields to zero, and the page bounding box to zero coordinates.
 * Use this after dropping pages or closing the document to ensure the
 * state is clean before reuse.
 */

void reset_doc_state(DocState *state) {
  fprintf(stderr, "Freeing the existing DocState\n");
  *state = (DocState){.ctx = NULL,
                      .locks = NULL,
                      .doc = NULL,

                      .cached_pages_pool = NULL,
                      .cache_window = NULL,
                      .current_cached_page = NULL,

                      .path = NULL,
                      .pagecount = 0,
                      .svg_background = "white",
                      .svg_foreground = "black",
                      .current_page_number = 0,
                      .next_page_number = 0,
                      .prev_page_number = 0,
                      .current_svg_data = NULL,
                      .current_svg_size = 0,
                      .next_svg_data = NULL,
                      .next_svg_size = 0,
                      .prev_svg_data = NULL,
                      .prev_svg_size = 0,
                      .current_page = NULL,
                      .prev_page = NULL,
                      .next_page = NULL,
                      .page_bbox =
                          {
                              .x0 = 0.0f,
                              .y0 = 0.0f,
                          },
                      .outline = NULL};
}

void fail(const char *msg) {
  fprintf(stderr, "%s\n", msg);
  abort();
}

void lock_mutex(void *user, int lock) {
  pthread_mutex_t *mutexes = (pthread_mutex_t *)user;
  if (pthread_mutex_lock(&mutexes[lock]) != 0)
    fail("pthread_mutex_lock()");
}

void unlock_mutex(void *user, int lock) {
  pthread_mutex_t *mutexes = (pthread_mutex_t *)user;
  if (pthread_mutex_unlock(&mutexes[lock]) != 0)
    fail("pthread_mutex_unlock()");
}

void init_main_ctx(DocState *state) {
  for (int i = 0; i < FZ_LOCK_MAX; ++i) {
    pthread_mutex_init(&g_mupdf_mutex[i], NULL);
  }

  state->locks.user = g_mupdf_mutex;
  state->locks.lock = lock_mutex;
  state->locks.unlock = unlock_mutex;

  state->ctx = fz_new_context(NULL, &state->locks, FZ_STORE_UNLIMITED);
  if (!state->ctx) {
    fprintf(stderr, "Cannot create MuPDF context\n");
    exit(1);
  }

  fz_register_document_handlers(state->ctx);
}

void open_document(DocState *state) {
  state->doc = fz_open_document(state->ctx, state->path);
  state->outline = fz_load_outline(state->ctx, state->doc);
  state->pagecount = fz_count_pages(state->ctx, state->doc);

  if (!state->doc) {
    fprintf(stderr, "Could not open document\n");
    exit(1);
  }
}
