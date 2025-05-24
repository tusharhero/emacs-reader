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

#include "elisp-helpers.h"
#include "mupdf-helpers.h"
#include "render-core.h"

int plugin_is_GPL_compatible;

/**
 * load_mupdf_doc - Initialize MuPDF context and other document essentials
 * @state: Pointer to a DocState with `.path` set to the document file path.
 *
 * Creates a new MuPDF context, registers document handlers, opens the
 * document at `state->path`, loads its outline (table of contents), and
 * counts its pages. On success, populates `state->ctx`, `state->doc`,
 * `state->outline`, and `state->pagecount`.
 *
 * On any failure, prints an error to stderr, tears down any partially
 * created MuPDF objects, and leaves `state` in a clean (NULL)
 * context/document state.
 *
 * Return: EXIT_SUCCESS on success; EXIT_FAILURE if any step fails.
 */

int load_mupdf_doc(DocState *state) {
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

/**
 * load_pages - Load the current page (n) and its adjacent pages (n-1, n+1)
 * into the DocState.
 * @state:        Pointer to an initialized DocState with a valid context and
 * open document.
 * @page_number:  Zero-based index of the page to load as the
 * `state->current_page`.
 *
 * If `page_number > 0`, also loads the previous page into `state->prev_page`;
 * otherwise sets `state->prev_page` to NULL. If `page_number < state->pagecount
 * - 1`, loads the next page into `state->next_page`; otherwise sets
 * `state->next_page` to NULL.
 *
 * On any failure during loading, prints an error via stderr containing the
 * MuPDF caught message, drops the document and context, and returns
 * EXIT_FAILURE. On success, updates:
 *   - state->current_page_number, state->current_page
 *   - state->prev_page_number,    state->prev_page    (or NULL)
 *   - state->next_page_number,    state->next_page    (or NULL)
 *
 * Return: EXIT_SUCCESS if all requested pages are loaded (or skipped at edges);
 *         EXIT_FAILURE if any fz_load_page call fails.
 */

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

/**
 * render_pages - Render the current page and its adjacent pages into SVG
 * buffers.
 * @state:        Pointer to an initialized DocState containing context,
 * document, and pagecount.
 * @page_number:  Zero-based index of the page to render as the
 * state->current_page.
 *
 * Validates the DocState and page_number, then:
 *   1. Cleans up any existing SVG data in state.
 *   2. Loads the requested page and its neighbors (prev/next) via load_pages().
 *   3. Computes the bounding box and dimensions for rendering.
 *   4. Allocates MuPDF buffers, outputs, and SVG devices for each available
 * page.
 *   5. Runs each loaded page through its SVG device to generate SVG content.
 *   6. Closes and drops all devices and outputs in proper order.
 *   7. Copies generated SVG bytes into heap-allocated C strings in state:
 *        - state->current_svg_data / size
 *        - state->prev_svg_data    / size (or NULL/0 at document start)
 *        - state->next_svg_data    / size (or NULL/0 at document end)
 *   8. Cleans up temporary buffers, outputs, and loaded pages.
 *
 * On any failure during setup, rendering, or teardown, prints a descriptive
 * error to stderr (including fz_caught_message), drops any allocated resources,
 * and returns EXIT_FAILURE. On success, returns EXIT_SUCCESS.
 */

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

/**
 * emacs_load_doc - Load a document from Emacs, initialize state, and render
 * first page.
 * @env:   The Emacs environment pointer.
 * @nargs: Number of arguments passed from Elisp (should be 1).
 * @args:  Array of Elisp argument values; args[0] is the file path string.
 * @data:  User-supplied callback data (ignored).
 *
 * Allocates and resets a new DocState, converts the provided Elisp string for
 * the file path in args[0] to a C string and stores it in state->path. Attempts
 * to open the document via load_mupdf_doc(); on success:
 *   1. Emits debug messages to stderr about load status and page count.
 *   2. Exposes the total page count to Elisp via set_current_pagecount().
 *   3. Marks the render status true in Elisp via set_current_render_status().
 *   4. Creates a buffer-local SVG overlay via init_overlay().
 *   5. Renders the current page (and neighbors) to SVG via render_pages().
 *   6. Converts the SVG to an Emacs image object (svg2elisp_image), and
 * displays it in the overlay using overlay-put.
 *   7. Wraps the DocState in a user pointer and stores it in the Elisp variable
 *      `reader-current-doc-state-ptr` for later access.
 *
 * If the path conversion fails, prints an error and returns `nil`.  Any other
 * failures during loading or rendering emit an error to stderr but still return
 * `t` to Elisp (the render step logs its own failure).
 *
 * Return: Elisp `t` on completion (or `nil` if path conversion failed).
 */

emacs_value emacs_load_doc(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                           void *data) {
  (void)nargs;
  (void)data;
  size_t str_length = 0;
  DocState *state = malloc(sizeof(DocState));

  if (!state) {
    emacs_message(env, "Document cannot be loaded into memory due to "
                       "unsupported format or some other reason.");
    return EMACS_NIL;
  }

  reset_doc_state(state);

  if (!elisp_2_c_str(env, args[0], &state->path, &str_length)) {
    emacs_message(env, "Failed to convert Emacs string to C string.");
    return EMACS_NIL;
  }

  if (load_mupdf_doc(state) == EXIT_SUCCESS) {
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
      emacs_value doc_state_ptr_sym =
          env->intern(env, "reader-current-doc-state-ptr");
      env->funcall(env, env->intern(env, "set"), 2,
                   (emacs_value[]){doc_state_ptr_sym, user_ptr});
    } else {
      emacs_message(env, "Rendering initial page failed.");
      return EMACS_NIL;
    }
  } else {
    emacs_message(env, "Loading document failed.");
    return EMACS_NIL;
  }

  return EMACS_T;
}

/**
 * emacs_next_page - Move to and display the next page in the overlay.
 * @env:   The Emacs environment pointer.
 * @nargs: (ignored).
 * @args:  (ignored).
 * @data:  (ignored).
 *
 * Retrieves the current DocState and overlay, converts the next-page's SVG
 * data (`state->next_svg_data`) into an Emacs image, and updates the overlay to
 * display it. If not already at the last page, also advances the DocState’s
 * page index by calling render_pages() on the next page.
 *
 * Return: Elisp `t` on completion.
 */

emacs_value emacs_next_page(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                            void *data) {
  (void)nargs;
  (void)args;
  (void)data;

  DocState *state = get_doc_state_ptr(env);
  emacs_value current_svg_overlay = get_current_svg_overlay(env);

  if (state) {
    if (state->current_page_number == (state->pagecount - 1)) {
      emacs_message(env, "Already last page!");
      return EMACS_NIL;
    }

    emacs_value next_image_data =
        svg2elisp_image(env, state, state->next_svg_data, state->next_svg_size);
    emacs_value overlay_put_args[3] = {
        current_svg_overlay, env->intern(env, "display"), next_image_data};
    env->funcall(env, env->intern(env, "overlay-put"), 3, overlay_put_args);

    if (state->current_page_number < (state->pagecount - 1)) {
      render_pages(state, state->next_page_number);
    }
  } else {
    return EMACS_NIL;
  }

  return EMACS_T;
}

/**
 * emacs_prev_page - Move to and display the previous page in the overlay.
 * @env:   The Emacs environment pointer.
 * @nargs: (ignored).
 * @args:  (ignored).
 * @data:  (ignored).
 *
 * Retrieves the current DocState and overlay. If on page > 0, converts the
 * previous-page's SVG data (`state->prev_svg_data`) into an Emacs image,
 * updates the overlay, and re-renders pages at state->prev_page_number. If
 * already at the first page, emits a warning and returns nil.
 *
 * Return: Elisp `t` if page moved, `nil` if already at first page.
 */

emacs_value emacs_prev_page(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                            void *data) {
  (void)nargs;
  (void)args;
  (void)data;

  DocState *state = get_doc_state_ptr(env);
  emacs_value current_svg_overlay = get_current_svg_overlay(env);

  if (state) {
    if (state->current_page_number == 0) {
      emacs_message(env, "Already first page!");
      return EMACS_NIL;
    }

    if (state->current_page_number < (state->pagecount - 1)) {
      emacs_value prev_image_data = svg2elisp_image(
          env, state, state->prev_svg_data, state->prev_svg_size);

      emacs_value overlay_put_args[3] = {
          current_svg_overlay, env->intern(env, "display"), prev_image_data};
      env->funcall(env, env->intern(env, "overlay-put"), 3, overlay_put_args);

      if (state->current_page_number > 0) {
        render_pages(state, state->prev_page_number);
        return EMACS_T;
      } else {
        emacs_message(env, "Already first page!");
        return EMACS_NIL;
      }
    } else {
      render_pages(state, (state->pagecount - 2));
      emacs_value current_image_data = svg2elisp_image(
          env, state, state->current_svg_data, state->current_svg_size);
      emacs_value overlay_put_args[3] = {
          current_svg_overlay, env->intern(env, "display"), current_image_data};
      env->funcall(env, env->intern(env, "overlay-put"), 3, overlay_put_args);
      return EMACS_T;
    }
  } else {
    return EMACS_NIL;
  }

  return EMACS_T;
}

/**
 * emacs_first_page - Jump to and display the first page of the document.
 * @env:   The Emacs environment pointer.
 * @nargs: (ignored).
 * @args:  (ignored).
 * @data:  (ignored).
 *
 * If not already on page zero, resets state->current_page_number to 0,
 * re-renders the pages, converts the current-page SVG into an Emacs image,
 * and updates the overlay. Warns if the first page is already displayed.
 *
 * Return: Elisp `t` on success, `nil` if already at the first page.
 */

emacs_value emacs_first_page(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                             void *data) {
  (void)nargs;
  (void)args;
  (void)data;

  DocState *state = get_doc_state_ptr(env);
  emacs_value current_svg_overlay = get_current_svg_overlay(env);

  if (state) {

    if (state->current_page_number == 0) {
      emacs_message(env, "Already first page!");
      return EMACS_NIL;
    }

    state->current_page_number = 0;

    if (render_pages(state, state->current_page_number) == EXIT_SUCCESS) {
      emacs_value prev_image_data = svg2elisp_image(
          env, state, state->current_svg_data, state->current_svg_size);
      emacs_value overlay_put_args[3] = {
          current_svg_overlay, env->intern(env, "display"), prev_image_data};
      env->funcall(env, env->intern(env, "overlay-put"), 3, overlay_put_args);
    } else {
      emacs_message(env, "Already first page!");
      return EMACS_NIL;
    }
    state->current_page_number = 0;
  } else {
    return EMACS_NIL;
  }

  return EMACS_T;
}

/**
 * emacs_last_page - Jump to and display the last page of the document.
 * @env:   The Emacs environment pointer.
 * @nargs: (ignored).
 * @args:  (ignored).
 * @data:  (ignored).
 *
 * If not already on the last page, sets state->current_page_number to
 * pagecount–1, re-renders, converts the SVG, and updates the overlay.
 * Warns and returns nil if already at end.
 *
 * Return: Elisp `t` on success, `nil` if already at the last page.
 */

emacs_value emacs_last_page(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                            void *data) {
  (void)nargs;
  (void)args;
  (void)data;

  DocState *state = get_doc_state_ptr(env);
  emacs_value current_svg_overlay = get_current_svg_overlay(env);

  if (state) {
    if (state->current_page_number == state->pagecount - 1) {
      emacs_message(env, "Already first page!");
      return EMACS_NIL;
    }

    state->current_page_number = state->pagecount - 1;

    if (render_pages(state, state->current_page_number) == EXIT_SUCCESS) {
      emacs_value current_page_data = svg2elisp_image(
          env, state, state->current_svg_data, state->current_svg_size);
      emacs_value overlay_put_args[3] = {
          current_svg_overlay, env->intern(env, "display"), current_page_data};
      env->funcall(env, env->intern(env, "overlay-put"), 3, overlay_put_args);
    } else {
      emacs_message(env, "Failed to render the last page");
      return EMACS_NIL;
    }
    state->current_page_number = state->pagecount - 1;
  } else {
    return EMACS_NIL;
  }

  return EMACS_T;
}

/**
 * emacs_goto_page - Jump to a specific page number and display it.
 * @env:   The Emacs environment pointer.
 * @nargs: Number of Elisp args (should be 1).
 * @args:  Array of Elisp argument values; args[0] is desired page index.
 * @data:  (ignored).
 *
 * Extracts the integer page_number from args[0]. If within (1..pagecount-2),
 * updates state->current_page_number, re-renders that page, converts SVG
 * to an image, and updates the overlay. Otherwise emits a bounds warning.
 *
 * Return: Elisp `t` on success or out-of-bounds (always `t`).
 */

emacs_value emacs_goto_page(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                            void *data) {
  (void)nargs;
  (void)data;
  int page_number = env->extract_integer(env, args[0]);

  DocState *state = get_doc_state_ptr(env);
  emacs_value current_svg_overlay = get_current_svg_overlay(env);

  if (state) {
    if (page_number >= 0 && page_number <= (state->pagecount - 1)) {
      state->current_page_number = page_number;
      if (render_pages(state, state->current_page_number) == EXIT_SUCCESS) {
        emacs_value current_image_data = svg2elisp_image(
            env, state, state->current_svg_data, state->current_svg_size);
        emacs_value overlay_put_args[3] = {current_svg_overlay,
                                           env->intern(env, "display"),
                                           current_image_data};
        env->funcall(env, env->intern(env, "overlay-put"), 3, overlay_put_args);
      } else {
        emacs_message(env, "Page cannot be rendered");
        return EMACS_NIL;
      }
    } else {
      emacs_message(env, "Provided page number is out of bounds!");
      return EMACS_NIL;
    }
  } else {
    return EMACS_NIL;
  }

  return EMACS_T;
}

/**
 * emacs_doc_change_page_size - Scale the displayed SVG to a new size.
 * @env:   The Emacs environment pointer.
 * @nargs: Number of Elisp args (should be 1).
 * @args:  Array of Elisp argument values; args[0] is the float scale factor.
 * @data:  (ignored).
 *
 * Retrieves the current DocState and overlay, then:
 *   - Rebuilds the SVG image plist’s :width, :length, and :scale entries
 *     by multiplying the original page dimensions by the given factor.
 *   - Updates the image object via plist-put and setcdr.
 *   - Re-displays the modified image in the overlay.
 *
 * Return: Elisp `t` on completion.
 */

emacs_value emacs_doc_scale_page(emacs_env *env, ptrdiff_t nargs,
                                 emacs_value *args, void *data) {
  (void)nargs;
  (void)data;

  emacs_value scale_factor = args[0];
  DocState *state = get_doc_state_ptr(env);
  emacs_value current_svg_overlay = get_current_svg_overlay(env);

  if (state) {
    emacs_value current_image_data = svg2elisp_image(
        env, state, state->current_svg_data, state->current_svg_size);
    emacs_value cdr_current_image_data =
        env->funcall(env, env->intern(env, "cdr"), 1, &current_image_data);

    emacs_value updated_width = env->funcall(
        env, env->intern(env, "*"), 2,
        (emacs_value[]){env->make_float(env, doc_page_width(state)),
                        scale_factor});
    emacs_value updated_length = env->funcall(
        env, env->intern(env, "*"), 2,
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
  } else {
    return EMACS_NIL;
  }

  return EMACS_T;
}

// Entrypoint for the dynamic module
int emacs_module_init(struct emacs_runtime *runtime) {
  emacs_env *env = runtime->get_environment(runtime);
  if (!env) {
    fprintf(stderr, "Failed to get Emacs environment.\n");
    return 1;
  }

  // Registrations for the required functions and variables

  register_module_func(
      env, emacs_load_doc, "reader-dyn--load-doc", 1, 1,
      "Loads a DOC to be rendered in Emacs.  It is wrapped around the Elisp "
      "function `reader-open-doc'. The function does the following:\n 1. "
      "Allocates and resets a new DocState\n 2. Attempts to open the document "
      "through the internal (non-registered) `load_mupdf_doc'.\n 3. Exposes "
      "the total page count to Elisp\n 4. Create a bufer-local SVG overlay "
      "through `init_overlay'\n 5. Calls the main `render_page' function to "
      "render the current page and its adjacent ones.\n 6. Converts the SVG to "
      "an Emacs image object and displays it in the overlay through "
      "`overlay-put'.\n 7. Wraps the C pointer for DocState as an user pointer "
      "and stores it in `reader-current-doc-state-ptr'.");

  register_module_func(
      env, emacs_next_page, "reader-dyn--next-page", 0, 0,
      "Loads and renders the next page of the document.  It is wrapped around "
      "the Elisp function `reader-next-page'.  Since DocState stores SVG data "
      "for the previous and next page, all this does is render the data for "
      "the next page that was rendered and stored in memory previously.");

  register_module_func(
      env, emacs_prev_page, "reader-dyn--prev-page", 0, 0,
      "Loads and renders the previous page of the document.  It is wrapped "
      "around the Elisp function `reader-prev-page'.  Since DocState stores "
      "SVG data for the previous and next page, all this does is render the "
      "data for the previous page that was rendered and stored in memory "
      "previously.");

  register_module_func(
      env, emacs_first_page, "reader-dyn--first-page", 0, 0,
      "Loads and renders the first page of the document. It is wrapped around "
      "`reader-first-page'. It calls `render_pages' with 0 as the argument, "
      "since MuPDF does 0-indexing.");

  register_module_func(env, emacs_last_page, "reader-dyn--last-page", 0, 0,
                       "Loads and renders the last page of the document. It is "
                       "wrapped around `reader-last-page'. It calls "
                       "`render_pages' with (pagecount - 1) as the argument");

  register_module_func(
      env, emacs_goto_page, "reader-dyn--goto-page", 1, 1,
      "Loads and renders the N page number. It is wrapped around "
      "`reader-goto-page'. It calls `render_pages' with N - 1 as the argument");

  register_module_func(env, get_current_page_number,
                       "reader-dyn--current-doc-pagenumber", 0, 0,
                       "Returns the current page number the document is at "
                       "from DocState as an Elisp value.");

  // Register the buffer-local page number
  permanent_buffer_local_var(env, "reader-current-doc-pagecount");

  // Register the buffer-local variable to indicate whether a buffer has been
  // rendered
  permanent_buffer_local_var(env, "reader-current-doc-render-status");

  // Register the buffer-local value for the DocState user pointer
  permanent_buffer_local_var(env, "reader-current-doc-state-ptr");

  // Register the buffer-local value for reader-current-svg-overlay
  permanent_buffer_local_var(env, "reader-current-svg-overlay");

  register_module_func(
      env, emacs_doc_scale_page, "reader-dyn--scale-page", 1, 1,
      "Scales the current page of the document by a given FACTOR. It "
      "multiplies the FACTOR with the :width, :height and :scale properties of "
      "the image, and then renders the scaled image through `overlay-put'.");

  // Provide the current dynamic module as a feature to Emacs
  provide(env, "render-core");
  fprintf(stderr, "Emacs module initialized successfully.\n");

  return 0;
}
