#include <assert.h>
#include <emacs-module.h>
#include <mupdf/fitz.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include "helpers.h"

int plugin_is_GPL_compatible;

typedef struct {
  fz_context *ctx;
  fz_document *doc;
  int pagecount;
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
} PdfState;

PdfState state = {
  .ctx = NULL,
  .doc = NULL,
  .pagecount = 0,
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
};

// Loading a PDF
int load_pdf(PdfState *state, char *input_file) {

  state->ctx = fz_new_context(NULL, NULL, FZ_STORE_UNLIMITED);
  if (!state->ctx) {
    fprintf(stderr, "Cannot create MuPDF context\n");
    return EXIT_FAILURE;
  }
  fz_try(state->ctx) { fz_register_document_handlers(state->ctx); }
  fz_catch(state->ctx) {
    fprintf(stderr, "Cannot register document handlers: %s\n",
            fz_caught_message(state->ctx));
    fz_drop_context(state->ctx);
    state->ctx = NULL;
    return EXIT_FAILURE;
  }
  fz_try(state->ctx) { state->doc = fz_open_document(state->ctx, input_file); }
  fz_catch(state->ctx) {
    fprintf(stderr, "Cannot open document '%s': %s\n", input_file,
            fz_caught_message(state->ctx));
    fz_drop_context(state->ctx);
    state->ctx = NULL;
    return EXIT_FAILURE;
  }
  fz_try(state->ctx) {
    state->pagecount = fz_count_pages(state->ctx, state->doc);
  }
  fz_catch(state->ctx) {
    fprintf(stderr, "Cannot get page count: %s\n",
            fz_caught_message(state->ctx));
    fz_drop_document(state->ctx, state->doc);
    fz_drop_context(state->ctx);
    state->doc = NULL;
    state->ctx = NULL;
    return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
}

// Load the page, and it’s adjacent ones.
int load_pages(PdfState *state, int page_number) {
  fz_try(state->ctx) {
    state->current_page_number = page_number;
    state->current_page = fz_load_page(state->ctx, state->doc, page_number);

    if (0 < page_number) {
      state->prev_page_number = page_number - 1;
      state->prev_page =
	fz_load_page(state->ctx, state->doc, state->prev_page_number);
    }

    if (page_number < (state->pagecount - 1)) {
      state->next_page_number = page_number + 1;
      state->next_page =
	fz_load_page(state->ctx, state->doc, state->next_page_number);
    }
  }
  fz_catch(state->ctx) {
    fprintf(stderr, "Cannot load pages: %s\n", fz_caught_message(state->ctx));
    fz_drop_document(state->ctx, state->doc);
    fz_drop_context(state->ctx);
    return EXIT_FAILURE;
  }
  return 0;
}

// Clean up previous SVG data if any
void clean_up_svg_data(PdfState *state) {
  if (state->current_svg_data) {
    free(state->current_svg_data);
    state->current_svg_data = NULL;
    state->current_svg_size = 0;
  }
}

// Drop all PDF pages
void drop_all_pdf_pages(fz_context *ctx, PdfState *state) {
  if (state->prev_page)
    fz_drop_page(ctx, state->prev_page);
  if (state->current_page)
    fz_drop_page(ctx, state->current_page);
  if (state->next_page)
    fz_drop_page(ctx, state->next_page);
}

// Rendering the page
int render_page(PdfState *state, int page_number) {
  fz_device *prev_dev = NULL;
  fz_output *prev_out = NULL;
  fz_buffer *prev_buf = NULL;

  fz_device *curr_dev = NULL;
  fz_output *curr_out = NULL;
  fz_buffer *curr_buf = NULL;

  fz_device *next_dev = NULL;
  fz_output *next_out = NULL;
  fz_buffer *next_buf = NULL;

  // Check valid state
  if (!state->ctx || !state->doc || page_number < 0 ||
      page_number >= state->pagecount) {
    fprintf(stderr, "Invalid state or page number for rendering SVG.\n");
    return EXIT_FAILURE;
  }

  // Clean up existing data
  clean_up_svg_data(state);

  // Load the page, and it’s adjacent ones.
  load_pages(state, page_number);

  // Get the page bounding box
  state->page_bbox = fz_bound_page(state->ctx, state->current_page);
  float page_width = state->page_bbox.x1 - state->page_bbox.x0;
  float page_height = state->page_bbox.y1 - state->page_bbox.y0;

  // Create buffers
  if (create_buffers(state->ctx, &curr_buf, &prev_buf, &next_buf) == EXIT_FAILURE) {
    fz_drop_document(state->ctx, state->doc);
    fz_drop_context(state->ctx);

    fprintf(stderr, "Creating buffers failed.\n");
    return EXIT_FAILURE;
  }

  // Create an output object associated with each of the buffers
  if (create_outputs(state->ctx, &curr_out, &prev_out, &next_out, curr_buf,
		     prev_buf, next_buf) == EXIT_FAILURE) {
    drop_all_buffers(state->ctx, curr_buf, prev_buf, next_buf);
    drop_all_pdf_pages(state->ctx, state);
    fz_drop_document(state->ctx, state->doc);
    fz_drop_context(state->ctx);

    fprintf(stderr, "Creating outputs failed.\n");
    return EXIT_FAILURE;
  }

  // Create an SVG device
  fz_try(state->ctx) {
    curr_dev =
      fz_new_svg_device(state->ctx, curr_out, page_width, page_height, 0, 1);
    prev_dev =
      fz_new_svg_device(state->ctx, prev_out, page_width, page_height, 0, 1);
    next_dev =
      fz_new_svg_device(state->ctx, next_out, page_width, page_height, 0, 1);
  }
  fz_catch(state->ctx) {
    fprintf(stderr, "Cannot create SVG device: %s\n",
            fz_caught_message(state->ctx));
    close_all_outputs(state->ctx, curr_out, prev_out, next_out);
    drop_all_outputs(state->ctx, curr_out, prev_out, next_out);
    drop_all_buffers(state->ctx, curr_buf, prev_buf, next_buf);
    drop_all_pdf_pages(state->ctx, state);
    fz_drop_document(state->ctx, state->doc);
    fz_drop_context(state->ctx);
    return EXIT_FAILURE;
  }

  // Run the page through the device
  fz_try(state->ctx) {
    fz_run_page(state->ctx, state->current_page, curr_dev, fz_identity, NULL);
    fz_run_page(state->ctx, state->prev_page, prev_dev, fz_identity, NULL);
    fz_run_page(state->ctx, state->next_page, next_dev, fz_identity, NULL);
  }
  fz_catch(state->ctx) {
    fprintf(stderr, "Cannot run page: %s\n", fz_caught_message(state->ctx));
    close_all_devices(state->ctx, curr_dev, prev_dev, next_dev);
    drop_all_devices(state->ctx, curr_dev, prev_dev, next_dev);
    close_all_outputs(state->ctx, curr_out, prev_out, next_out);
    drop_all_outputs(state->ctx, curr_out, prev_out, next_out);
    drop_all_buffers(state->ctx, curr_buf, prev_buf, next_buf);
    drop_all_pdf_pages(state->ctx, state);
    fz_drop_document(state->ctx, state->doc);
    fz_drop_context(state->ctx);
    return EXIT_FAILURE;
  }

  // Close and drop the devices
  close_all_devices(state->ctx, curr_dev, prev_dev, next_dev);
  drop_all_devices(state->ctx, curr_dev, prev_dev, next_dev);

  // Close the output to finalize the buffer content
  fz_try(state->ctx) {
    close_all_outputs(state->ctx, curr_out, prev_out, next_out);
  }
  fz_catch(state->ctx) {
    fprintf(stderr, "Cannot close output: %s\n", fz_caught_message(state->ctx));

    drop_all_outputs(state->ctx, curr_out, prev_out, next_out);
    drop_all_buffers(state->ctx, curr_buf, prev_buf, next_buf);
    drop_all_pdf_pages(state->ctx, state);
    fz_drop_document(state->ctx, state->doc);
    fz_drop_context(state->ctx);

    return EXIT_FAILURE;
  }

  // Allocate memory for SVG data
  state->current_svg_size = curr_buf->len;
  state->current_svg_data = (char *)malloc(state->current_svg_size + 1);

  state->prev_svg_size = prev_buf->len;
  state->prev_svg_data = (char *)malloc(state->prev_svg_size + 1);

  state->next_svg_size = next_buf->len;
  state->next_svg_data = (char *)malloc(state->next_svg_size + 1);

  if (state->current_svg_data == NULL || state->prev_svg_data == NULL ||
      state->next_svg_data == NULL) {
    fprintf(stderr, "Cannot allocate memory for SVG data\n");

    drop_all_outputs(state->ctx, curr_out, prev_out, next_out);
    drop_all_buffers(state->ctx, curr_buf, prev_buf, next_buf);
    drop_all_pdf_pages(state->ctx, state);
    fz_drop_document(state->ctx, state->doc);
    fz_drop_context(state->ctx);

    return EXIT_FAILURE;
  }

  // Copy the data and null-terminate
  memcpy(state->current_svg_data, curr_buf->data, state->current_svg_size);
  state->current_svg_data[state->current_svg_size] = '\0';
  memcpy(state->prev_svg_data, prev_buf->data, state->prev_svg_size);
  state->prev_svg_data[state->prev_svg_size] = '\0';
  memcpy(state->next_svg_data, next_buf->data, state->next_svg_size);
  state->next_svg_data[state->next_svg_size] = '\0';

  // Clean up
  drop_all_outputs(state->ctx, curr_out, prev_out, next_out);
  drop_all_buffers(state->ctx, curr_buf, prev_buf, next_buf);
  drop_all_pdf_pages(state->ctx, state);

  return EXIT_SUCCESS;
}

emacs_value emacs_load_pdf(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                           void *data) {
  (void)nargs;
  (void)data;
  char *file;
  size_t str_length = 0;
  emacs_value result = env->intern(env, "nil"); // Initialize result to nil

  if (!emacs_2_c_str(env, args[0], &file, &str_length)) {
    fprintf(stderr, "Failed to convert Emacs string to C string.\n");
    return env->intern(env, "nil");
  }

  fprintf(stderr, "Attempting to load: %s\n", file);

  if (load_pdf(&state, file) == EXIT_SUCCESS) {
    fprintf(stderr, "PDF loaded successfully with %d pages.\n",
            state.pagecount);
    fprintf(
	    stderr,
	    "State after load_pdf: ctx=%p, doc=%p, pagecount=%d, current_page=%d\n",
	    state.ctx, state.doc, state.pagecount, state.current_page_number);

    if (render_page(&state, state.current_page_number) == EXIT_SUCCESS) {
      emacs_value svg_string =
	env->make_string(env, state.current_svg_data, state.current_svg_size);
      emacs_value file_name_string = env->make_string(env, file, strlen(file));
      emacs_value file_name = env->funcall(env, env->intern(env, "file-name-base"), 1, &file_name_string);
      emacs_value buffer = env->funcall(
					env, env->intern(env, "generate-new-buffer"), 1, &file_name);
      env->funcall(env, env->intern(env, "switch-to-buffer"), 1, &buffer);
      emacs_value image_args[3] = {svg_string, env->intern(env, "svg"),
                                   env->intern(env, "t")};
      emacs_value image_data =
	env->funcall(env, env->intern(env, "create-image"), 3, image_args);
      env->funcall(env, env->intern(env, "insert-image"), 1, &image_data);
    } else {
      fprintf(stderr, "Rendering initial page failed.\n");
    }
  } else {
    fprintf(stderr, "Loading PDF failed.\n");
  }

  free(file);
  return result;
}

emacs_value emacs_next_page(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                            void *data) {
  (void)nargs;
  (void)args;
  (void)data;

  if (state.current_page_number < (state.pagecount - 1)) {
    emacs_value next_svg_string =
      env->make_string(env, state.next_svg_data, state.next_svg_size);
    env->funcall(env, env->intern(env, "erase-buffer"), 0, NULL);
    emacs_value image_args[3] = {next_svg_string, env->intern(env, "svg"),
                                 env->intern(env, "t")};
    emacs_value image_data =
      env->funcall(env, env->intern(env, "create-image"), 3, image_args);
    env->funcall(env, env->intern(env, "insert-image"), 1, &image_data);

    if (state.current_page_number == (state.pagecount - 2)) {
      fprintf(stderr, "Already at the last page.\n");
    } else {
      render_page(&state, state.next_page_number);
    }
    return env->intern(env, "t");
  } else {
    fprintf(stderr, "Already at the last page.\n");
    return env->intern(env, "nil");
  }
  return env->intern(env, "t");
}

emacs_value emacs_prev_page(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                            void *data) {
  (void)nargs;
  (void)args;
  (void)data;

  if (state.current_page_number > 0) {
    emacs_value prev_svg_string =
      env->make_string(env, state.prev_svg_data, state.prev_svg_size);
    env->funcall(env, env->intern(env, "erase-buffer"), 0, NULL);
    emacs_value image_args[3] = {prev_svg_string, env->intern(env, "svg"),
                                 env->intern(env, "t")};
    emacs_value image_data =
      env->funcall(env, env->intern(env, "create-image"), 3, image_args);
    env->funcall(env, env->intern(env, "insert-image"), 1, &image_data);
    render_page(&state, state.prev_page_number);
    return env->intern(env, "t");

  } else {
    fprintf(stderr, "Already at the first page.\n");
    return env->intern(env, "nil");
  }
  return env->intern(env, "t");
}

emacs_value emacs_first_page(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                            void *data) {
  (void)nargs;
  (void)args;
  (void)data;

  if (state.current_page_number > 0) {
    emacs_value prev_svg_string =
      env->make_string(env, state.prev_svg_data, state.prev_svg_size);
    env->funcall(env, env->intern(env, "erase-buffer"), 0, NULL);
    emacs_value image_args[3] = {prev_svg_string, env->intern(env, "svg"),
                                 env->intern(env, "t")};
    emacs_value image_data =
      env->funcall(env, env->intern(env, "create-image"), 3, image_args);
    env->funcall(env, env->intern(env, "insert-image"), 1, &image_data);
    render_page(&state, 0);
    return env->intern(env, "t");

  } else {
    fprintf(stderr, "Already at the first page.\n");
    return env->intern(env, "nil");
  }
  return env->intern(env, "t");
}

emacs_value emacs_last_page(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                            void *data) {
  (void)nargs;
  (void)args;
  (void)data;

  last_page_number = state.pagecount + 1;

  if (state.current_page_number > 0) {
    emacs_value prev_svg_string =
      env->make_string(env, state.prev_svg_data, state.prev_svg_size);
    env->funcall(env, env->intern(env, "erase-buffer"), 0, NULL);
    emacs_value image_args[3] = {prev_svg_string, env->intern(env, "svg"),
                                 env->intern(env, "t")};
    emacs_value image_data =
      env->funcall(env, env->intern(env, "create-image"), 3, image_args);
    env->funcall(env, env->intern(env, "insert-image"), 1, &image_data);
    render_page(&state, last_page_number);
    return env->intern(env, "t");

  } else {
    fprintf(stderr, "Already at the first page.\n");
    return env->intern(env, "nil");
  }
  return env->intern(env, "t");
}

int emacs_module_init(struct emacs_runtime *runtime) {
  emacs_env *env = runtime->get_environment(runtime);
  if (!env) {
    fprintf(stderr, "Failed to get Emacs environment.\n");
    return 1;
  }
  emacs_value fset = env->intern(env, "fset");

  // Register load-pdf
  emacs_value load_func_symbol = env->intern(env, "load-pdf");
  emacs_value load_func =
    env->make_function(env, 1, 1, emacs_load_pdf, "Loads a PDF file.", NULL);
  emacs_value load_args[2] = {load_func_symbol, load_func};
  env->funcall(env, fset, 2, load_args);

  // Register next-pdf-page
  emacs_value next_func_symbol = env->intern(env, "next-pdf-page");
  emacs_value next_func = env->make_function(env, 0, 0, emacs_next_page,
                                             "Go to the next PDF page.", NULL);
  emacs_value next_args[2] = {next_func_symbol, next_func};
  env->funcall(env, fset, 2, next_args);

  // Register previous-pdf-page
  emacs_value prev_func_symbol = env->intern(env, "previous-pdf-page");
  emacs_value prev_func = env->make_function(
					     env, 0, 0, emacs_prev_page, "Go to the previous PDF page.", NULL);
  emacs_value prev_args[2] = {prev_func_symbol, prev_func};
  env->funcall(env, fset, 2, prev_args);

  provide(env, "render-core");
  fprintf(stderr, "Emacs module initialized successfully.\n");
  return 0;
}
