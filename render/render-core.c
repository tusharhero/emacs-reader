#include "elisp-helpers.h"
#include "mupdf-helpers.h"
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
  char *path;
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
  fz_outline *outline;
} DocState;

// Clean up previous SVG data if any
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

// Drop all pages of the document from the context
void drop_all_doc_pages(fz_context *ctx, DocState *state) {
  if (state->prev_page)
    fz_drop_page(ctx, state->prev_page);
  if (state->current_page)
    fz_drop_page(ctx, state->current_page);
  if (state->next_page)
    fz_drop_page(ctx, state->next_page);
}

// Reset the state of the document in heap memory
void reset_doc_state(DocState *state) {
  fprintf(stderr, "Freeing the existing DocState\n");
  *state = (DocState){.ctx = NULL,
                      .doc = NULL,
                      .path = NULL,
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
                      .outline = NULL};
}

// Fetch pointer to DocState from Emacs Environment to a C pointer
DocState *get_doc_state_ptr(emacs_env *env) {
  emacs_value ptr_sym = env->intern(env, "doc-state-ptr");
  emacs_value ptr =
      env->funcall(env, env->intern(env, "symbol-value"), 1, &ptr_sym);
  DocState *state = env->get_user_ptr(env, ptr);

  return state;
}

// Fetches the current page number of the document that’s open
emacs_value get_current_page_number(emacs_env *env, ptrdiff_t nargs,
                                    emacs_value *args, void *data) {
  (void)nargs;
  (void)data;
  (void)args;

  DocState *state = get_doc_state_ptr(env);
  return env->make_integer(env, state->current_page_number);
}

// Set the rendering status of the document in the buffer buffer to be true
void set_current_render_status(emacs_env *env) {
  emacs_value render_status_var = env->intern(env, "doc-render-status");
  env->funcall(env, env->intern(env, "set"), 2,
               (emacs_value[]){render_status_var, env->intern(env, "t")});
}

// Exposing the pagecount of the document to an Elisp variable
void set_current_pagecount(emacs_env *env, DocState *state) {
  emacs_value pagecount_args[2] = {env->intern(env, "current-doc-pagecount"),
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

  env->funcall(env, env->intern(env, "set"), 2,
               (emacs_value[]){current_overlay_sym, overlay});
}

// Fetches the overlay object from the current-svg-overlay
emacs_value get_current_svg_overlay(emacs_env *env) {

  emacs_value current_overlay_sym = env->intern(env, "current-svg-overlay");
  emacs_value current_overlay = env->funcall(
      env, env->intern(env, "symbol-value"), 1, &current_overlay_sym);

  return current_overlay;
}

// Loading a document into MuPDF context, and respective document handlers
int load_doc(DocState *state) {
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
  fz_try(state->ctx) { state->doc = fz_open_document(state->ctx, state->path); }

  fz_catch(state->ctx) {
    fprintf(stderr, "Cannot open document '%s': %s\n", state->path,
            fz_caught_message(state->ctx));
    fz_drop_context(state->ctx);
    state->ctx = NULL;
    return EXIT_FAILURE;
  }

  fz_try(state->ctx) {
    state->outline = fz_load_outline(state->ctx, state->doc);
  }

  fz_catch(state->ctx) {
    fprintf(stderr, "Cannot create outline for the document '%s': %s\n",
            state->path, fz_caught_message(state->ctx));
    fz_drop_context(state->ctx);
    state->outline = NULL;
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
int load_pages(DocState *state, int page_number) {
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
int render_page(DocState *state, int page_number) {
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
    drop_all_doc_pages(state->ctx, state);
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
    drop_all_doc_pages(state->ctx, state);
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
    drop_all_doc_pages(state->ctx, state);
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
    drop_all_doc_pages(state->ctx, state);
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
    drop_all_doc_pages(state->ctx, state);
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
  drop_all_doc_pages(state->ctx, state);

  return EXIT_SUCCESS;
}

// Emacs command to load a document file and then render it
emacs_value emacs_load_doc(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                           void *data) {
  (void)nargs;
  (void)data;
  size_t str_length = 0;
  DocState *state = malloc(sizeof(DocState));
  reset_doc_state(state);

  if (!elisp_2_c_str(env, args[0], &state->path, &str_length)) {
    fprintf(stderr, "Failed to convert Emacs string to C string.\n");
    return env->intern(env, "nil");
  }

  if (load_doc(state) == EXIT_SUCCESS) {
    fprintf(stderr, "%s loaded successfully with %d pages.\n", state->path,
            state->pagecount);
    fprintf(stderr,
            "State after loading the document: ctx=%p, doc=%p, pagecount=%d, "
            "current_page=%d\n",
            state->ctx, state->doc, state->pagecount,
            state->current_page_number);

    set_current_pagecount(env, state);
    set_current_render_status(env);
    init_overlay(env);
    emacs_value current_svg_overlay = get_current_svg_overlay(env);

    if (render_page(state, state->current_page_number) == EXIT_SUCCESS) {

      // Take the SVG data from DocState and create an SVG image of it as a Lisp
      // Object.
      emacs_value svg_string = env->make_string(env, state->current_svg_data,
                                                state->current_svg_size);
      emacs_value image_args[4] = {svg_string, env->intern(env, "svg"),
                                   env->intern(env, "t")};
      emacs_value image_data =
          env->funcall(env, env->intern(env, "create-image"), 3, image_args);

      // Render the created image on the buffer’s overlay
      emacs_value overlay_put_args[3] = {
          current_svg_overlay, env->intern(env, "display"), image_data};
      env->funcall(env, env->intern(env, "overlay-put"), 3, overlay_put_args);

      // Create a user pointer and expose it to Emacs in a buffer-local fashion
      emacs_value user_ptr = env->make_user_ptr(env, NULL, state);
      emacs_value doc_state_ptr_sym = env->intern(env, "doc-state-ptr");
      env->funcall(env, env->intern(env, "set"), 2,
                   (emacs_value[]){doc_state_ptr_sym, user_ptr});
    } else {
      fprintf(stderr, "Rendering initial page failed.\n");
    }
  } else {
    fprintf(stderr, "Loading document failed.\n");
  }

  return env->intern(env, "t");
}

// Emacs command to render the next page
emacs_value emacs_next_page(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                            void *data) {
  (void)nargs;
  (void)args;
  (void)data;

  DocState *state = get_doc_state_ptr(env);
  emacs_value current_svg_overlay = get_current_svg_overlay(env);

  emacs_value next_svg_string =
      env->make_string(env, state->next_svg_data, state->next_svg_size);
  emacs_value image_args[3] = {next_svg_string, env->intern(env, "svg"),
                               env->intern(env, "t")};
  emacs_value image_data =
      env->funcall(env, env->intern(env, "create-image"), 3, image_args);
  emacs_value overlay_put_args[3] = {current_svg_overlay,
                                     env->intern(env, "display"), image_data};
  env->funcall(env, env->intern(env, "overlay-put"), 3, overlay_put_args);

  if (state->current_page_number < (state->pagecount - 2)) {
    render_page(state, state->next_page_number);
  } else if (state->current_page_number == (state->pagecount - 2)) {
    // At n-2 it makes sure state’s pages are restructured according to
    // next page but not rendered.
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

  DocState *state = get_doc_state_ptr(env);
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
    emacs_value overlay_put_args[3] = {current_svg_overlay,
                                       env->intern(env, "display"), image_data};
    env->funcall(env, env->intern(env, "overlay-put"), 3, overlay_put_args);
  }

  return env->intern(env, "t");
}

// Emacs commad to the first page of the document
emacs_value emacs_first_page(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                             void *data) {
  (void)nargs;
  (void)args;
  (void)data;

  DocState *state = get_doc_state_ptr(env);
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

// Emacs command to go to the last page of the document
emacs_value emacs_last_page(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                            void *data) {
  (void)nargs;
  (void)args;
  (void)data;

  DocState *state = get_doc_state_ptr(env);
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

// Emacs command to go to a specific page of the document
emacs_value emacs_goto_page(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                            void *data) {
  (void)nargs;
  (void)data;
  int page_number = env->extract_integer(env, args[0]);

  DocState *state = get_doc_state_ptr(env);
  emacs_value current_svg_overlay = get_current_svg_overlay(env);

  if (page_number > 1 && page_number < (state->pagecount - 1)) {
    state->current_page_number = page_number;
    if (render_page(state, state->current_page_number) == EXIT_SUCCESS) {
      emacs_value svg_string = env->make_string(env, state->current_svg_data,
                                                state->current_svg_size);
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

emacs_value emacs_doc_change_page_size(emacs_env *env, ptrdiff_t nargs,
                                       emacs_value *args, void *data) {
  (void)nargs;
  (void)data;

  emacs_value scale_factor = args[0];

  DocState *state = get_doc_state_ptr(env);
  emacs_value current_svg_overlay = get_current_svg_overlay(env);

  emacs_value svg_string =
      env->make_string(env, state->current_svg_data, state->current_svg_size);
  emacs_value image_args[3] = {svg_string, env->intern(env, "svg"),
                               env->intern(env, "t")};
  emacs_value image_data =
      env->funcall(env, env->intern(env, "create-image"), 3, image_args);

  emacs_value cdr_image_data =
      env->funcall(env, env->intern(env, "cdr"), 1, &image_data);

  emacs_value modified_cdr =
      env->funcall(env, env->intern(env, "plist-put"), 3,
                   (emacs_value[]){cdr_image_data, env->intern(env, ":scale"),
                                   scale_factor});
  env->funcall(env, env->intern(env, "setcdr"), 2,
               (emacs_value[]){image_data, modified_cdr});
  emacs_value overlay_put_args[3] = {current_svg_overlay,
                                     env->intern(env, "display"), image_data};
  env->funcall(env, env->intern(env, "overlay-put"), 3, overlay_put_args);

  return env->intern(env, "t");
}

int emacs_module_init(struct emacs_runtime *runtime) {
  emacs_env *env = runtime->get_environment(runtime);
  if (!env) {
    fprintf(stderr, "Failed to get Emacs environment.\n");
    return 1;
  }

  // Register load-doc
  register_module_func(env, emacs_load_doc, "load-doc", 1, 1,
                       "Opens a document in Emacs.");

  // Register next-doc-page
  register_module_func(env, emacs_next_page, "next-doc-page", 0, 0,
                       "Go to the next page of the document.");

  // Register previous-doc-page
  register_module_func(env, emacs_prev_page, "previous-doc-page", 0, 0,
                       "Go to the previous page of the document.");

  // Register first-doc-page
  register_module_func(env, emacs_first_page, "first-doc-page", 0, 0,
                       "Go to the document's first page.");

  // Register last-doc-page
  register_module_func(env, emacs_last_page, "last-doc-page", 0, 0,
                       "Go to the document's last page.");

  // Register goto-doc-page
  register_module_func(env, emacs_goto_page, "goto-doc-page", 1, 1,
                       "Go to page number N of the document");

  // Register get-current-doc-page-number
  register_module_func(env, get_current_page_number,
                       "get-current-doc-pagenumber", 0, 0,
                       "Get current page number for the visiting document");

  // Register the buffer-local page number
  env->funcall(env, env->intern(env, "make-variable-buffer-local"), 1,
               (emacs_value[]){env->intern(env, "current-doc-pagecount")});

  // Register the buffer-local variable to indicate whether a buffer has been
  // rendered
  emacs_value page_render_status_sym = env->intern(env, "doc-render-status");
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

  // Register the buffer-local user pointer for DocState
  emacs_value doc_state_ptr_sym = env->intern(env, "doc-state-ptr");
  env->funcall(env, env->intern(env, "make-variable-buffer-local"), 1,
               &doc_state_ptr_sym);
  env->funcall(env, env->intern(env, "put"), 3,
               (emacs_value[]){doc_state_ptr_sym,
                               env->intern(env, "permanent-local"),
                               env->intern(env, "t")});

  // Register the buffer-local variable current-svg-overlay
  emacs_value svg_overlay_sym = env->intern(env, "current-svg-overlay");
  env->funcall(env, env->intern(env, "make-variable-buffer-local"), 1,
               &svg_overlay_sym);

  // Register the doc-change-page-size function
  register_module_func(
      env, emacs_doc_change_page_size, "doc-change-page-size", 1, 1,
      "Scales the current page of the document by a given FACTOR");

  // Provide the current dynamic module as a feature to Emacs
  provide(env, "render-core");
  fprintf(stderr, "Emacs module initialized successfully.\n");
  return 0;
}
