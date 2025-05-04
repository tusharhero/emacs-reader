#include "helpers.h"
#include <assert.h>
#include <emacs-module.h>
#include <mupdf/fitz.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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

// Clean up previous SVG data if any
void clean_up_svg_data(PdfState *state) {
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

// Drop all PDF pages
void drop_all_pdf_pages(fz_context *ctx, PdfState *state) {
  if (state->prev_page)
    fz_drop_page(ctx, state->prev_page);
  if (state->current_page)
    fz_drop_page(ctx, state->current_page);
  if (state->next_page)
    fz_drop_page(ctx, state->next_page);
}

// Reset the PdfState
void reset_pdf_state(PdfState *state) {
  fprintf(stderr, "Freeing the existing PdfState\n");
  clean_up_svg_data(state);
  drop_all_pdf_pages(state->ctx, state);
  memset(state, 0, sizeof(PdfState));
}

// Fetch pointer to PdfState from Emacs Environment to a C pointer
PdfState *get_pdf_state_ptr(emacs_env *env) {
  emacs_value ptr_sym = env->intern(env, "pdf-state-ptr");
  emacs_value ptr =
    env->funcall(env, env->intern(env, "symbol-value"), 1, &ptr_sym);
  PdfState *state = env->get_user_ptr(env, ptr);

  return state;
}

// Fetches the current page number of the PDF
emacs_value get_current_page_number(emacs_env *env, ptrdiff_t nargs,
				    emacs_value *args, void *data) {
  (void)nargs;
  (void)data;
  (void)args;

  PdfState *state = get_pdf_state_ptr(env);
  return env->make_integer(env, state->current_page_number);
}

// Set the rendering status of the buffer to be true
void set_current_render_status(emacs_env *env) {
  emacs_value render_status_var = env->intern(env, "page-render-status");
  env->funcall(env, env->intern(env, "set"), 2,
               (emacs_value[]){render_status_var, env->intern(env, "t")});
}

// Exposing the pagecount of the PDF to an Elisp variable
void set_current_page_number(emacs_env *env, PdfState *state) {
  emacs_value pagecount_args[2] = {env->intern(env, "current-pdf-pagecount"),
				   env->make_integer(env, state->pagecount)};
  env->funcall(env, env->intern(env, "set"), 2, pagecount_args);
}

// Initializes the overlay and stores it in a buffer-local variable
void init_overlay(emacs_env *env) {
  emacs_value start = env->funcall(env, env->intern(env, "point-min"), 0, NULL);
  emacs_value end = env->funcall(env, env->intern(env, "point-max"), 0, NULL);
  emacs_value overlay = env->funcall(env, env->intern(env, "make-overlay"), 2,
                                     (emacs_value[]){start, end});

  emacs_value current_overlay_sym = env->intern(env, "current-svg-overlay");

  env->funcall(env, env->intern(env, "set"), 2, (emacs_value[]){current_overlay_sym, overlay});
}

// Fetches the overlay object from the current-svg-overlay
emacs_value get_current_svg_overlay(emacs_env *env) {

  emacs_value current_overlay_sym = env->intern(env, "current-svg-overlay");
  emacs_value current_overlay = env->funcall(env, env->intern(env, "symbol-value"), 1, &current_overlay_sym);

  return current_overlay;
}

// loading a PDF
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
  if (create_buffers(state->ctx, &curr_buf, &prev_buf, &next_buf) ==
      EXIT_FAILURE) {
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

// Emacs command to load a PDF file and then render it
emacs_value emacs_load_pdf(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                           void *data) {
  (void)nargs;
  (void)data;
  char *file;
  size_t str_length = 0;

  if (!emacs_2_c_str(env, args[0], &file, &str_length)) {
    fprintf(stderr, "Failed to convert Emacs string to C string.\n");
    return env->intern(env, "nil");
  }

  PdfState *state = malloc(sizeof(PdfState));
  *state = (PdfState){
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

  reset_pdf_state(state);
  fprintf(stderr, "Attempting to load: %s\n", file);

  if (load_pdf(state, file) == EXIT_SUCCESS) {
    fprintf(stderr, "PDF loaded successfully with %d pages.\n",
            state->pagecount);
    fprintf(
	    stderr,
	    "State after load_pdf: ctx=%p, doc=%p, pagecount=%d, current_page=%d\n",
	    state->ctx, state->doc, state->pagecount, state->current_page_number);

    set_current_page_number(env, state);
    set_current_render_status(env);
    init_overlay(env);
    emacs_value current_svg_overlay = get_current_svg_overlay(env);

    if (render_page(state, state->current_page_number) == EXIT_SUCCESS) {

      emacs_value svg_string =
	env->make_string(env, state->current_svg_data, state->current_svg_size);
      emacs_value image_args[3] = {svg_string, env->intern(env, "svg"),
                                   env->intern(env, "t")};
      emacs_value image_data =
	env->funcall(env, env->intern(env, "create-image"), 3, image_args);
      emacs_value overlay_put_args[3] = {
	current_svg_overlay, env->intern(env, "display"), image_data};
      env->funcall(env, env->intern(env, "overlay-put"), 3, overlay_put_args);

      // Create a user pointer and expose it to Emacs in a buffer-local fashion
      emacs_value user_ptr = env->make_user_ptr(env, NULL, state);
      emacs_value pdf_state_ptr_sym = env->intern(env, "pdf-state-ptr");
      env->funcall(env, env->intern(env, "set"), 2, (emacs_value[]){pdf_state_ptr_sym, user_ptr});
    } else {
      fprintf(stderr, "Rendering initial page failed.\n");
    }
  } else {
    fprintf(stderr, "Loading PDF failed.\n");
  }

  free(file);
  return env->intern(env, "t");
}

// Emacs command to render the next page
emacs_value emacs_next_page(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                            void *data) {
  (void)nargs;
  (void)args;
  (void)data;

  PdfState *state = get_pdf_state_ptr(env);

  emacs_value current_svg_overlay = get_current_svg_overlay(env);

  emacs_value next_svg_string =
    env->make_string(env, state->next_svg_data, state->next_svg_size);
  emacs_value image_args[3] = {next_svg_string, env->intern(env, "svg"),
                               env->intern(env, "t")};
  emacs_value image_data =
    env->funcall(env, env->intern(env, "create-image"), 3, image_args);
  emacs_value overlay_put_args[3] = {current_svg_overlay, env->intern(env, "display"),
                                     image_data};
  env->funcall(env, env->intern(env, "overlay-put"), 3, overlay_put_args);

  if (state->current_page_number < (state->pagecount - 2)) {
    render_page(state, state->next_page_number);
  } else if (state->current_page_number == (state->pagecount - 2)) {
    // At n-2 it makes sure state’s pages are restructured according to next
    // page but not rendered.
    load_pages(state, state->next_page_number);
  } else if (state->current_page_number == (state->pagecount - 1)) {
    fprintf(stderr, "Already at the last page.\n");
  }

  return env->intern(env, "t");
}

// Emacs command to render the previous page
emacs_value emacs_prev_page(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                            void *data) {
  (void)nargs;
  (void)args;
  (void)data;

  PdfState *state = get_pdf_state_ptr(env);
  emacs_value current_svg_overlay = get_current_svg_overlay(env);

  if (state->current_page_number == 0) {
    fprintf(stderr, "Already at the first page.\n");
    return env->intern(env, "nil");
  }

  if (state->current_page_number < (state->pagecount - 1)) {

    emacs_value prev_svg_string =
      env->make_string(env, state->prev_svg_data, state->prev_svg_size);
    emacs_value image_args[3] = {prev_svg_string, env->intern(env, "svg"),
                                 env->intern(env, "t")};
    emacs_value image_data =
      env->funcall(env, env->intern(env, "create-image"), 3, image_args);
    emacs_value overlay_put_args[3] = {current_svg_overlay,
                                       env->intern(env, "display"), image_data};
    env->funcall(env, env->intern(env, "overlay-put"), 3, overlay_put_args);

    if (state->current_page_number > 0) {
      render_page(state, state->prev_page_number);
      return env->intern(env, "t");
    } else {
      fprintf(stderr, "Already at the first page.\n");
      return env->intern(env, "nil");
    }
  } else {
    render_page(state, (state->pagecount - 2));
    emacs_value current_svg_string =
      env->make_string(env, state->current_svg_data, state->current_svg_size);
    emacs_value image_args[3] = {current_svg_string, env->intern(env, "svg"),
                                 env->intern(env, "t")};
    emacs_value image_data =
      env->funcall(env, env->intern(env, "create-image"), 3, image_args);
    emacs_value overlay_put_args[3] = {
      current_svg_overlay,
      env->intern(env, "display"), image_data};
    env->funcall(env, env->intern(env, "overlay-put"), 3, overlay_put_args);
  }

  return env->intern(env, "t");
}

// Emacs commad to the first page of the PDF
emacs_value emacs_first_page(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                             void *data) {
  (void)nargs;
  (void)args;
  (void)data;

  PdfState *state = get_pdf_state_ptr(env);
  emacs_value current_svg_overlay = get_current_svg_overlay(env);

  if (state->current_page_number == 0) {
    fprintf(stderr, "Already at the first page.\n");
    return env->intern(env, "nil");
  }

  // Can’t set to zero beacuse render_page needs to render previous or next page
  state->current_page_number = 1;

  if (render_page(state, state->current_page_number) == EXIT_SUCCESS) {
    emacs_value svg_string =
      env->make_string(env, state->prev_svg_data, state->prev_svg_size);
    emacs_value image_args[3] = {svg_string, env->intern(env, "svg"),
                                 env->intern(env, "t")};
    emacs_value image_data =
      env->funcall(env, env->intern(env, "create-image"), 3, image_args);
    emacs_value overlay_put_args[3] = {current_svg_overlay,
                                       env->intern(env, "display"), image_data};
    env->funcall(env, env->intern(env, "overlay-put"), 3, overlay_put_args);
  } else {
    fprintf(stderr, "Failed to render the first page.\n");
  }

  state->current_page_number = 0;
  return env->intern(env, "t");
}

// Emacs command to go to the last page of the PDF
emacs_value emacs_last_page(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                            void *data) {
  (void)nargs;
  (void)args;
  (void)data;

  PdfState *state = get_pdf_state_ptr(env);
  emacs_value current_svg_overlay = get_current_svg_overlay(env);

  if (render_page(state, (state->pagecount - 2)) == EXIT_SUCCESS) {
    state->current_page_number = (state->pagecount - 1);
    emacs_value svg_string =
      env->make_string(env, state->next_svg_data, state->next_svg_size);
    emacs_value image_args[3] = {svg_string, env->intern(env, "svg"),
                                 env->intern(env, "t")};
    emacs_value image_data =
      env->funcall(env, env->intern(env, "create-image"), 3, image_args);
    emacs_value overlay_put_args[3] = {current_svg_overlay,
                                       env->intern(env, "display"), image_data};
    env->funcall(env, env->intern(env, "overlay-put"), 3, overlay_put_args);
  } else {
    fprintf(stderr, "Failed to render the last page.\n");
  }

  return env->intern(env, "t");
}

// Emacs command to go to a specific page of the PDF
emacs_value emacs_goto_page(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                            void *data) {
  (void)nargs;
  (void)data;
  int page_number = env->extract_integer(env, args[0]);

  PdfState *state = get_pdf_state_ptr(env);
  emacs_value current_svg_overlay = get_current_svg_overlay(env);

  if (page_number > 1 && page_number < (state->pagecount - 1)) {
    state->current_page_number = page_number;
    if (render_page(state, state->current_page_number) == EXIT_SUCCESS) {
      emacs_value svg_string =
	env->make_string(env, state->current_svg_data, state->current_svg_size);
      emacs_value image_args[3] = {svg_string, env->intern(env, "svg"),
                                   env->intern(env, "t")};
      emacs_value image_data =
	env->funcall(env, env->intern(env, "create-image"), 3, image_args);
      emacs_value overlay_put_args[3] = {
	current_svg_overlay, env->intern(env, "display"), image_data};
      env->funcall(env, env->intern(env, "overlay-put"), 3, overlay_put_args);
    } else {
      fprintf(stderr, "Page number out of bounds");
    }
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

  // Register first-pdf-page
  emacs_value first_page_func_symbol = env->intern(env, "first-pdf-page");
  emacs_value first_page_func = env->make_function(
						   env, 0, 0, emacs_first_page, "Go to the PDF's first page.", NULL);
  emacs_value first_page_args[2] = {first_page_func_symbol, first_page_func};
  env->funcall(env, fset, 2, first_page_args);

  // Register last-pdf-page
  emacs_value last_page_func_symbol = env->intern(env, "last-pdf-page");
  emacs_value last_page_func = env->make_function(
						  env, 0, 0, emacs_last_page, "Go to the PDF's first page.", NULL);
  emacs_value last_page_args[2] = {last_page_func_symbol, last_page_func};
  env->funcall(env, fset, 2, last_page_args);

  // Register goto-pdf-page
  emacs_value goto_page_func_symbol = env->intern(env, "goto-pdf-page");
  emacs_value goto_page_func = env->make_function(
						  env, 1, 1, emacs_goto_page, "Go to the PDF's first page.", NULL);
  emacs_value goto_page_args[2] = {goto_page_func_symbol, goto_page_func};
  env->funcall(env, fset, 2, goto_page_args);

  // Register get-current-pdf-page-number
  emacs_value get_current_pdf_page_number_symbol =
    env->intern(env, "get-current-pdf-pagenumber");
  emacs_value get_current_pdf_page_number_func =
    env->make_function(env, 0, 0, get_current_page_number,
		       "Get current page number for the visiting PDF", NULL);
  emacs_value get_current_pdf_page_number_args[2] = {
    get_current_pdf_page_number_symbol, get_current_pdf_page_number_func};
  env->funcall(env, fset, 2, get_current_pdf_page_number_args);

  // Register the buffer-local page number
  env->funcall(env, env->intern(env, "make-variable-buffer-local"), 1, (emacs_value[]){env->intern(env, "current-pdf-pagecount")});

  // Register the buffer-local variable to indicate whether a buffer has been
  // rendered
  emacs_value page_render_status_sym = env->intern(env, "page-render-status");
  env->funcall(env, env->intern(env, "make-variable-buffer-local"), 1,
               &page_render_status_sym);
  env->funcall(
	       env, env->intern(env, "set"), 2,
	       (emacs_value[]){page_render_status_sym, env->intern(env, "nil")});
  // Ensure that the variable stays buffer-local and doesn’t get overriden
  env->funcall(env, env->intern(env, "put"), 3,
               (emacs_value[]){page_render_status_sym,
                               env->intern(env, "permanent-local"),
                               env->intern(env, "t")});

  // Register the buffer-local user pointer for PdfState
  emacs_value pdf_state_ptr_sym = env->intern(env, "pdf-state-ptr");
  env->funcall(env, env->intern(env, "make-variable-buffer-local"), 1,
	       &pdf_state_ptr_sym);
  env->funcall(env, env->intern(env, "put"), 3,
               (emacs_value[]){pdf_state_ptr_sym,
                               env->intern(env, "permanent-local"),
                               env->intern(env, "t")});

  // Register the buffer-local svg overlay for the PDF
  emacs_value svg_overlay_sym = env->intern(env, "current-svg-overlay");
  env->funcall(env, env->intern(env, "make-variable-buffer-local"), 1,
	       &svg_overlay_sym);

  // Provide the current dynamic module as a feature to Emacs
  provide(env, "render-pdf");
  fprintf(stderr, "Emacs module initialized successfully.\n");
  return 0;
}
