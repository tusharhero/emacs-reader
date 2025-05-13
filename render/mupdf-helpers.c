// Helper functions that complement the MuPDF C API

#include "mupdf-helpers.h"
#include <assert.h>
#include <mupdf/fitz.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>

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
