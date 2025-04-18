// Helper functions for various purposes.
#include "helpers.h"
#include <assert.h>
#include <emacs-module.h>
#include <mupdf/fitz.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>

// Change an Emacs string to a C string
bool emacs_2_c_str(emacs_env *env, emacs_value value, char **buffer,
                   size_t *size) {
  ptrdiff_t buffer_size;
  if (!env->copy_string_contents(env, value, NULL, &buffer_size))
    return false;
  assert(env->non_local_exit_check(env) == emacs_funcall_exit_return);
  assert(buffer_size > 0);
  *buffer = malloc((size_t)buffer_size);
  if (*buffer == NULL) {
    env->non_local_exit_signal(env, env->intern(env, "memory-full"),
                               env->intern(env, "nil"));
    return false;
  }
  ptrdiff_t old_buffer_size = buffer_size;
  if (!env->copy_string_contents(env, value, *buffer, &buffer_size)) {
    free(*buffer);
    *buffer = NULL;
    return false;
  }
  assert(env->non_local_exit_check(env) == emacs_funcall_exit_return);
  assert(buffer_size == old_buffer_size);
  *size = (size_t)(buffer_size - 1);
  return true;
}

// Create output buffers for the page
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

// Create Outputs
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

// Dropping and closing functions
void drop_all_buffers(fz_context *ctx, fz_buffer *curr, fz_buffer *prev,
                      fz_buffer *next) {
  if (curr)
    fz_drop_buffer(ctx, curr);
  if (prev)
    fz_drop_buffer(ctx, prev);
  if (next)
    fz_drop_buffer(ctx, next);
}

void drop_all_outputs(fz_context *ctx, fz_output *curr, fz_output *prev,
                      fz_output *next) {
  if (curr)
    fz_drop_output(ctx, curr);
  if (prev)
    fz_drop_output(ctx, prev);
  if (next)
    fz_drop_output(ctx, next);
}

void drop_all_devices(fz_context *ctx, fz_device *curr, fz_device *prev,
                      fz_device *next) {
  if (curr)
    fz_drop_device(ctx, curr);
  if (prev)
    fz_drop_device(ctx, prev);
  if (next)
    fz_drop_device(ctx, next);
}

void close_all_outputs(fz_context *ctx, fz_output *curr, fz_output *prev,
                       fz_output *next) {
  if (curr)
    fz_close_output(ctx, curr);
  if (prev)
    fz_close_output(ctx, prev);
  if (next)
    fz_close_output(ctx, next);
}

void close_all_devices(fz_context *ctx, fz_device *curr, fz_device *prev,
                       fz_device *next) {
  if (curr)
    fz_close_device(ctx, curr);
  if (prev)
    fz_close_device(ctx, prev);
  if (next)
    fz_close_device(ctx, next);
}

// C interface for (provide) in Elisp
void provide(emacs_env *env, const char *value) {
  emacs_value Qvalue = env->intern(env, value);
  emacs_value Qprovide = env->intern(env, "provide");
  emacs_value args[] = {Qvalue};

  env->funcall(env, Qprovide, 1, args);
}
