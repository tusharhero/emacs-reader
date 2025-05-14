#include "elisp-helpers.h"
#include <assert.h>
#include <emacs-module.h>
#include <mupdf/fitz.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int plugin_is_GPL_compatible;
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
    state->current_page =
        fz_load_page(state->ctx, state->doc, state->current_page_number);
  }
  fz_catch(state->ctx) {
    fprintf(stderr, "Cannot load pages: %s\n", fz_caught_message(state->ctx));
    fz_drop_document(state->ctx, state->doc);
    fz_drop_context(state->ctx);
    return EXIT_FAILURE;
  }

  if (page_number > 0) {
    fz_try(state->ctx) {
      state->prev_page_number = page_number - 1;
      state->prev_page =
          fz_load_page(state->ctx, state->doc, state->prev_page_number);
    }
    fz_catch(state->ctx) {
      fprintf(stderr, "Cannot load pages: %s\n", fz_caught_message(state->ctx));
      fz_drop_document(state->ctx, state->doc);
      fz_drop_context(state->ctx);
      return EXIT_FAILURE;
    }
  } else {
    state->prev_page = NULL;
  }

  if (page_number < state->pagecount - 1) {
    fz_try(state->ctx) {
      state->next_page_number = page_number + 1;
      state->next_page =
          fz_load_page(state->ctx, state->doc, state->next_page_number);
    }
    fz_catch(state->ctx) {
      fprintf(stderr, "Cannot load pages: %s\n", fz_caught_message(state->ctx));
      fz_drop_document(state->ctx, state->doc);
      fz_drop_context(state->ctx);
      return EXIT_FAILURE;
    }
  } else {
    state->next_page = NULL;
  }

  return EXIT_SUCCESS;
}

// Rendering the current page (n) and it’s adjacent ones (n-1, n+1)

int render_pages(DocState *state, int page_number) {
  fz_device *prev_dev = NULL;
  fz_output *prev_out = NULL;
  fz_buffer *prev_buf = NULL;

  fz_device *curr_dev = NULL;
  fz_output *curr_out = NULL;
  fz_buffer *curr_buf = NULL;

  fz_device *next_dev = NULL;
  fz_output *next_out = NULL;
  fz_buffer *next_buf = NULL;

  // Check whether the page number and state are valid
  if (!state || !state->ctx || !state->doc || page_number < 0 ||
      page_number >= state->pagecount) {
    fprintf(stderr, "Invalid state or page number for rendering SVG.\n");
    return EXIT_FAILURE;
  }

  clean_up_svg_data(state);

  load_pages(state, page_number);

  // Get the page bounding box
  state->page_bbox = fz_bound_page(state->ctx, state->current_page);
  float page_width = state->page_bbox.x1 - state->page_bbox.x0;
  float page_height = state->page_bbox.y1 - state->page_bbox.y0;

  // Create buffer, output and SVG device for the current page
  fz_try(state->ctx) {
    curr_buf = fz_new_buffer(state->ctx, 1024);
    curr_out = fz_new_output_with_buffer(state->ctx, curr_buf);
    curr_dev =
        fz_new_svg_device(state->ctx, curr_out, page_width, page_height, 0, 1);
  }
  fz_catch(state->ctx) {
    fprintf(stderr, "Failed to create resources for current page: %s\n",
            fz_caught_message(state->ctx));
    if (curr_dev)
      fz_drop_device(state->ctx, curr_dev);
    if (curr_out)
      fz_drop_output(state->ctx, curr_out);
    if (curr_buf)
      fz_drop_buffer(state->ctx, curr_buf);
    fz_drop_document(state->ctx, state->doc);
    fz_drop_context(state->ctx);
    return EXIT_FAILURE;
  }

  // If the previous page is available, create resources for that.
  if (state->prev_page) {
    fz_try(state->ctx) {
      prev_buf = fz_new_buffer(state->ctx, 1024);
      prev_out = fz_new_output_with_buffer(state->ctx, prev_buf);
      prev_dev = fz_new_svg_device(state->ctx, prev_out, page_width,
                                   page_height, 0, 1);
    }
    fz_catch(state->ctx) {
      fprintf(stderr, "Failed to create resources for current page: %s\n",
              fz_caught_message(state->ctx));
      if (prev_dev)
        fz_drop_device(state->ctx, prev_dev);
      if (prev_out)
        fz_drop_output(state->ctx, prev_out);
      if (prev_buf)
        fz_drop_buffer(state->ctx, prev_buf);
      if (curr_dev)
        fz_drop_device(state->ctx, curr_dev);
      if (curr_out)
        fz_drop_output(state->ctx, curr_out);
      if (curr_buf)
        fz_drop_buffer(state->ctx, curr_buf);
      fz_drop_document(state->ctx, state->doc);
      fz_drop_context(state->ctx);
      return EXIT_FAILURE;
    }
  }

  // If the next page is available, create resources for that.
  if (state->next_page) {
    fz_try(state->ctx) {
      next_buf = fz_new_buffer(state->ctx, 1024);
      next_out = fz_new_output_with_buffer(state->ctx, next_buf);
      next_dev = fz_new_svg_device(state->ctx, next_out, page_width,
                                   page_height, 0, 1);
    }
    fz_catch(state->ctx) {
      fprintf(stderr, "Failed to create resources for current page: %s\n",
              fz_caught_message(state->ctx));
      if (next_dev)
        fz_drop_device(state->ctx, next_dev);
      if (next_out)
        fz_drop_output(state->ctx, next_out);
      if (next_buf)
        fz_drop_buffer(state->ctx, next_buf);
      if (prev_dev)
        fz_drop_device(state->ctx, prev_dev);
      if (prev_out)
        fz_drop_output(state->ctx, prev_out);
      if (prev_buf)
        fz_drop_buffer(state->ctx, prev_buf);
      if (curr_dev)
        fz_drop_device(state->ctx, curr_dev);
      if (curr_out)
        fz_drop_output(state->ctx, curr_out);
      if (curr_buf)
        fz_drop_buffer(state->ctx, curr_buf);
      fz_drop_document(state->ctx, state->doc);
      fz_drop_context(state->ctx);
      return EXIT_FAILURE;
    }
  }

  // Run the current page through it’s corresponding SVG device and if other
  // pages are also available, do the same for them.

  fz_try(state->ctx) {
    fz_run_page(state->ctx, state->current_page, curr_dev, fz_identity, NULL);
    if (state->prev_page && prev_dev) {
      fz_run_page(state->ctx, state->prev_page, prev_dev, fz_identity, NULL);
    }
    if (state->next_page && next_dev) {
      fz_run_page(state->ctx, state->next_page, next_dev, fz_identity, NULL);
    }
  }
  fz_catch(state->ctx) {
    fprintf(stderr, "Cannot run page: %s\n", fz_caught_message(state->ctx));
    if (next_dev)
      fz_drop_device(state->ctx, next_dev); // Close not called, just drop
    if (next_out)
      fz_drop_output(state->ctx, next_out);
    if (next_buf)
      fz_drop_buffer(state->ctx, next_buf);
    if (prev_dev)
      fz_drop_device(state->ctx, prev_dev);
    if (prev_out)
      fz_drop_output(state->ctx, prev_out);
    if (prev_buf)
      fz_drop_buffer(state->ctx, prev_buf);
    fz_drop_device(state->ctx, curr_dev);
    fz_drop_output(state->ctx, curr_out);
    fz_drop_buffer(state->ctx, curr_buf);
    return EXIT_FAILURE;
  }

  // Close and drop devices
  fz_try(state->ctx) {
    fz_close_device(state->ctx, curr_dev);
    if (prev_dev)
      fz_close_device(state->ctx, prev_dev);
    if (next_dev)
      fz_close_device(state->ctx, next_dev);
  }
  fz_catch(state->ctx) {
    fprintf(stderr, "Cannot close device: %s\n", fz_caught_message(state->ctx));
    // Devices are already broken, just drop everything
    if (next_dev)
      fz_drop_device(state->ctx, next_dev);
    if (next_out)
      fz_drop_output(state->ctx, next_out);
    if (next_buf)
      fz_drop_buffer(state->ctx, next_buf);
    if (prev_dev)
      fz_drop_device(state->ctx, prev_dev);
    if (prev_out)
      fz_drop_output(state->ctx, prev_out);
    if (prev_buf)
      fz_drop_buffer(state->ctx, prev_buf);
    fz_drop_device(state->ctx, curr_dev);
    fz_drop_output(state->ctx, curr_out);
    fz_drop_buffer(state->ctx, curr_buf);
    return EXIT_FAILURE;
  }

  fz_drop_device(state->ctx, curr_dev);
  curr_dev = NULL;
  if (prev_dev) {
    fz_drop_device(state->ctx, prev_dev);
    prev_dev = NULL;
  }
  if (next_dev) {
    fz_drop_device(state->ctx, next_dev);
    next_dev = NULL;
  }

  // Close the outputs to finalize the buffer contents.
  fz_try(state->ctx) {
    fz_close_output(state->ctx, curr_out);
    if (prev_out)
      fz_close_output(state->ctx, prev_out);
    if (next_out)
      fz_close_output(state->ctx, next_out);
  }
  fz_catch(state->ctx) {
    fprintf(stderr, "Cannot close output: %s\n", fz_caught_message(state->ctx));
    // Outputs are already broken, just drop everything else
    if (next_out)
      fz_drop_output(state->ctx, next_out);
    if (next_buf)
      fz_drop_buffer(state->ctx, next_buf);
    if (prev_out)
      fz_drop_output(state->ctx, prev_out);
    if (prev_buf)
      fz_drop_buffer(state->ctx, prev_buf);
    fz_drop_output(state->ctx, curr_out);
    fz_drop_buffer(state->ctx, curr_buf);
    return EXIT_FAILURE;
  }

  // Allocate memory for the SVG data and copy them to DocState’s respective
  // fields
  state->current_svg_size = curr_buf->len;
  state->current_svg_data = (char *)malloc(state->current_svg_size + 1);

  memcpy(state->current_svg_data, curr_buf->data, state->current_svg_size);
  state->current_svg_data[state->current_svg_size] = '\0';

  if (state->prev_page && prev_buf) {
    state->prev_svg_size = prev_buf->len;
    state->prev_svg_data = (char *)malloc(state->prev_svg_size + 1);

    memcpy(state->prev_svg_data, prev_buf->data, state->prev_svg_size);
    state->prev_svg_data[state->prev_svg_size] = '\0';
  } else {
    state->prev_svg_data = NULL;
    state->prev_svg_size = 0;
  }

  if (state->next_page && next_buf) {
    state->next_svg_size = next_buf->len;
    state->next_svg_data = (char *)malloc(state->next_svg_size + 1);

    memcpy(state->next_svg_data, next_buf->data, state->next_svg_size);
    state->next_svg_data[state->next_svg_size] = '\0';
  } else {
    state->next_svg_data = NULL;
    state->next_svg_size = 0;
  }

  // Clean up
  fz_drop_output(state->ctx, curr_out);
  curr_out = NULL;
  if (prev_out) {
    fz_drop_output(state->ctx, prev_out);
    prev_out = NULL;
  }
  if (next_out) {
    fz_drop_output(state->ctx, next_out);
    next_out = NULL;
  }

  fz_drop_buffer(state->ctx, curr_buf);
  curr_buf = NULL;
  if (prev_buf) {
    fz_drop_buffer(state->ctx, prev_buf);
    prev_buf = NULL;
  }
  if (next_buf) {
    fz_drop_buffer(state->ctx, next_buf);
    next_buf = NULL;
  }

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

    if (render_pages(state, state->current_page_number) == EXIT_SUCCESS) {

      // Take the SVG data from DocState and create an SVG image of it as a Lisp
      // Object.
      emacs_value current_image_data = svg2elisp_image(
          env, state, state->current_svg_data, state->current_svg_size);

      // Render the created image on the buffer’s overlay
      emacs_value overlay_put_args[3] = {
          current_svg_overlay, env->intern(env, "display"), current_image_data};
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

  emacs_value next_image_data =
      svg2elisp_image(env, state, state->next_svg_data, state->next_svg_size);
  emacs_value overlay_put_args[3] = {
      current_svg_overlay, env->intern(env, "display"), next_image_data};
  env->funcall(env, env->intern(env, "overlay-put"), 3, overlay_put_args);

  if (state->current_page_number < (state->pagecount - 1)) {
    render_pages(state, state->next_page_number);
  } else {
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
    emacs_value prev_image_data =
        svg2elisp_image(env, state, state->prev_svg_data, state->prev_svg_size);

    emacs_value overlay_put_args[3] = {
        current_svg_overlay, env->intern(env, "display"), prev_image_data};
    env->funcall(env, env->intern(env, "overlay-put"), 3, overlay_put_args);

    if (state->current_page_number > 0) {
      render_pages(state, state->prev_page_number);
      return env->intern(env, "t");
    } else {
      fprintf(stderr, "Already at the first page.\n");
      return env->intern(env, "nil");
    }
  } else {
    render_pages(state, (state->pagecount - 2));
    emacs_value current_image_data = svg2elisp_image(
        env, state, state->current_svg_data, state->current_svg_size);
    emacs_value overlay_put_args[3] = {
        current_svg_overlay, env->intern(env, "display"), current_image_data};
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

  state->current_page_number = 0;

  if (render_pages(state, state->current_page_number) == EXIT_SUCCESS) {
    emacs_value prev_image_data = svg2elisp_image(
        env, state, state->current_svg_data, state->current_svg_size);
    emacs_value overlay_put_args[3] = {
        current_svg_overlay, env->intern(env, "display"), prev_image_data};
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

  if (state->current_page_number == state->pagecount - 1) {
    fprintf(stderr, "Already at the last page\n");
    return env->intern(env, "nil");
  }

  state->current_page_number = state->pagecount - 1;

  if (render_pages(state, state->current_page_number) == EXIT_SUCCESS) {
    emacs_value current_page_data = svg2elisp_image(
        env, state, state->current_svg_data, state->current_svg_size);
    emacs_value overlay_put_args[3] = {
        current_svg_overlay, env->intern(env, "display"), current_page_data};
    env->funcall(env, env->intern(env, "overlay-put"), 3, overlay_put_args);
  } else {
    fprintf(stderr, "Failed to render the last page.\n");
  }
  state->current_page_number = state->pagecount - 1;
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
    if (render_pages(state, state->current_page_number) == EXIT_SUCCESS) {
      emacs_value current_image_data = svg2elisp_image(
          env, state, state->current_svg_data, state->current_svg_size);
      emacs_value overlay_put_args[3] = {
          current_svg_overlay, env->intern(env, "display"), current_image_data};
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

  emacs_value current_image_data = svg2elisp_image(
      env, state, state->current_svg_data, state->current_svg_size);
  emacs_value cdr_current_image_data =
      env->funcall(env, env->intern(env, "cdr"), 1, &current_image_data);

  emacs_value updated_width =
      env->funcall(env, env->intern(env, "*"), 2,
                   (emacs_value[]){env->make_float(env, doc_page_width(state)),
                                   scale_factor});
  emacs_value updated_length =
      env->funcall(env, env->intern(env, "*"), 2,
                   (emacs_value[]){env->make_float(env, doc_page_length(state)),
                                   scale_factor});

  env->funcall(env, env->intern(env, "plist-put"), 3,
               (emacs_value[]){cdr_current_image_data,
                               env->intern(env, ":width"), updated_width});

  env->funcall(env, env->intern(env, "plist-put"), 3,
               (emacs_value[]){cdr_current_image_data,
                               env->intern(env, ":length"), updated_length});

  emacs_value modified_cdr =
      env->funcall(env, env->intern(env, "plist-put"), 3,
                   (emacs_value[]){cdr_current_image_data,
                                   env->intern(env, ":scale"), scale_factor});

  env->funcall(env, env->intern(env, "setcdr"), 2,
               (emacs_value[]){current_image_data, modified_cdr});
  emacs_value overlay_put_args[3] = {
      current_svg_overlay, env->intern(env, "display"), current_image_data};
  env->funcall(env, env->intern(env, "overlay-put"), 3, overlay_put_args);

  return env->intern(env, "t");
}

int emacs_module_init(struct emacs_runtime *runtime) {
  emacs_env *env = runtime->get_environment(runtime);
  if (!env) {
    fprintf(stderr, "Failed to get Emacs environment.\n");
    return 1;
  }

  // Registrations for the required functions and variables

  register_module_func(env, emacs_load_doc, "load-doc", 1, 1,
                       "Opens a document in Emacs.");

  register_module_func(env, emacs_next_page, "next-doc-page", 0, 0,
                       "Go to the next page of the document.");

  register_module_func(env, emacs_prev_page, "previous-doc-page", 0, 0,
                       "Go to the previous page of the document.");

  register_module_func(env, emacs_first_page, "first-doc-page", 0, 0,
                       "Go to the document's first page.");

  register_module_func(env, emacs_last_page, "last-doc-page", 0, 0,
                       "Go to the document's last page.");

  register_module_func(env, emacs_goto_page, "goto-doc-page", 1, 1,
                       "Go to page number N of the document");

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

  emacs_value doc_state_ptr_sym = env->intern(env, "doc-state-ptr");
  env->funcall(env, env->intern(env, "make-variable-buffer-local"), 1,
               &doc_state_ptr_sym);
  env->funcall(env, env->intern(env, "put"), 3,
               (emacs_value[]){doc_state_ptr_sym,
                               env->intern(env, "permanent-local"),
                               env->intern(env, "t")});

  emacs_value svg_overlay_sym = env->intern(env, "current-svg-overlay");
  env->funcall(env, env->intern(env, "make-variable-buffer-local"), 1,
               &svg_overlay_sym);

  register_module_func(
      env, emacs_doc_change_page_size, "doc-change-page-size", 1, 1,
      "Scales the current page of the document by a given FACTOR");

  // Provide the current dynamic module as a feature to Emacs
  provide(env, "render-core");
  fprintf(stderr, "Emacs module initialized successfully.\n");
  return 0;
}
