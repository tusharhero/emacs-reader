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

#include "render-core.h"
#include "elisp-helpers.h"
#include "emacs-module.h"
#include "mupdf-helpers.h"
#include "render-theme.h"

int plugin_is_GPL_compatible;

int load_page_dl(DocState *state, CachedPage *cp) {
  fz_device *dl_dev = NULL;
  fz_page *loaded_page = NULL;

  fz_context *ctx = fz_clone_context(state->ctx);

  clock_t start = clock();

  if (cp->status == PAGE_STATUS_EMPTY) {
    cp->status = PAGE_STATUS_RENDERING;

    fz_try(ctx) {
      loaded_page = fz_load_page(ctx, state->doc, cp->page_num);
      state->page_bbox = fz_bound_page(ctx, loaded_page);

      cp->display_list = fz_new_display_list(ctx, state->page_bbox);
      dl_dev = fz_new_list_device(ctx, cp->display_list);
      fz_run_page(ctx, loaded_page, dl_dev, fz_identity, NULL);
    }
    fz_always(ctx) {
      fz_drop_device(state->ctx, dl_dev);
      fz_drop_page(state->ctx, loaded_page);
    }
    fz_catch(ctx) fz_rethrow(ctx);
  }

  clock_t end = clock();

  double duration = (double)(end - start) / CLOCKS_PER_SEC;
  fprintf(stderr, "Took %f to load\n", duration);
  return EXIT_SUCCESS;
}

void *draw_page_thread(void *arg) {
  fz_output *out = NULL;
  fz_buffer *buf = NULL;
  fz_device *dev = NULL;

  RenderThreadArgs *args = (RenderThreadArgs *)arg;
  DocState *state = args->state;
  CachedPage *cp = args->cp;

  clock_t start = clock();
  fz_context *ctx = fz_clone_context(state->ctx);

  fz_try(ctx) {
    cp->pixmap = fz_new_pixmap_with_bbox(
        ctx, fz_device_rgb(ctx), fz_round_rect(state->page_bbox), NULL, 0);
    fz_clear_pixmap_with_value(ctx, cp->pixmap, 0xff);

    dev = fz_new_draw_device(ctx, fz_identity, cp->pixmap);
    fz_run_display_list(ctx, cp->display_list, dev, fz_identity,
                        state->page_bbox, NULL);

    fz_close_device(ctx, dev);
  }
  fz_always(ctx) fz_drop_device(ctx, dev);
  fz_catch(ctx) cp->status = PAGE_STATUS_ERROR;

  fz_try(ctx) {
    buf = fz_new_buffer(ctx, 1024);
    out = fz_new_output_with_buffer(ctx, buf);

    fz_write_pixmap_as_png(ctx, out, cp->pixmap);
    fz_drop_pixmap(ctx, cp->pixmap);
  }
  fz_catch(ctx) {
    fz_close_output(ctx, out);
    fz_drop_output(ctx, out);
    fz_drop_buffer(ctx, buf);
  }

  fz_close_output(ctx, out);

  cp->svg_size = buf->len;
  cp->svg_data = (char *)malloc(cp->svg_size);

  if (cp->svg_data == NULL) {
    fprintf(stderr, "Cannot allocate memory for SVG data\n");
    fz_close_output(ctx, out);
    fz_drop_buffer(ctx, buf);
    return NULL;
  }

  // Copy the data and null-terminate
  memcpy(cp->svg_data, buf->data, cp->svg_size);

  fz_drop_output(ctx, out);
  fz_drop_buffer(ctx, buf);
  fz_drop_context(ctx);

  cp->status = PAGE_STATUS_READY;
  clock_t end = clock();

  double duration = (double)(end - start) / CLOCKS_PER_SEC;
  fprintf(stderr, "Took %f to render\n", duration);

  return NULL;
}

void async_render(DocState *state, CachedPage *cp) {
  /* if (!cp) */
  /* return false; */
  /* pthread_t th; */

  RenderThreadArgs *render_args = malloc(sizeof(RenderThreadArgs));
  render_args->state = state;
  render_args->cp = cp;

  draw_page_thread(render_args);
  /* int err = pthread_create(&th, NULL, render_page_thread, render_args); */
  /* if (err) { */
  /*   fprintf(stderr, "Failed to create render thread: %d\n", err); */
  /*   free(render_args); */
  /* return false; */
  /* } */
  /* pthread_join(th, NULL); */

  /* return th; */
}

void build_cache_window(DocState *state, int n) {
  int start, end;
  int pagecount = state->pagecount;

  if (n < MAX_CACHE_WINDOW) {
    start = 0;
    end = n + MAX_CACHE_WINDOW;
    if (end >= pagecount)
      end = pagecount - 1;
  } else if (n > (pagecount - 1) - MAX_CACHE_WINDOW) {
    end = pagecount - 1;
    start = n - MAX_CACHE_WINDOW;
    if (start < 0)
      start = 0;
  } else {
    start = n - MAX_CACHE_WINDOW;
    end = n + MAX_CACHE_WINDOW;
  }

  state->current_page_number = n;
  state->current_window_index = n - start;

  /* pthread_t threads[MAX_CACHE_WINDOW]; */
  /* int thread_count = 0; */

  for (int i = 0; i < MAX_CACHE_SIZE; ++i) {
    int idx = start + i;
    if (idx < pagecount && idx <= end) {
      CachedPage *cp = state->cached_pages_pool[idx];
      state->cache_window[i] = cp;

      if (cp->status == PAGE_STATUS_EMPTY) {
        load_page_dl(state, cp);
        /* pthread_t th = async_render(state, cp); */
        /* if (th != 0) { */
        /*   threads[thread_count++] = th; */
        /* } */
        async_render(state, cp);
      }
    } else {
      state->cache_window[i] = NULL;
    }
  }

  /* for (int j = 0; j < thread_count; j++) { */
  /*   pthread_join(threads[j], NULL); */
  /* } */

  /* for (int k = 0; k < thread_count; k++) { */
  /*   CachedPage *cp = state->cache_window[k]; */
  /* } */

  state->current_cached_page = state->cache_window[state->current_window_index];
}

bool slide_cache_window_forward(DocState *state) {
  int n = ++state->current_page_number;
  int pagecount = state->pagecount;
  /* pthread_t th; */

  if (state->current_page_number + 1 >= state->pagecount) {
    fprintf(stderr,
            "slide_cache_window_right: cannot slide past end (page %d)\n", n);
    return false;
  }

  if (n - MAX_CACHE_WINDOW > 0 && n + MAX_CACHE_WINDOW < pagecount) {
    memmove(&state->cache_window[0], &state->cache_window[1],
            sizeof(state->cache_window[0]) * (MAX_CACHE_SIZE - 1));

    CachedPage *cp = state->cached_pages_pool[n + MAX_CACHE_WINDOW];
    state->cache_window[MAX_CACHE_SIZE - 1] = cp;

    if (cp->status == PAGE_STATUS_EMPTY) {
      load_page_dl(state, cp);
      async_render(state, cp);
    }

    state->current_window_index = MAX_CACHE_WINDOW;
  } else {
    build_cache_window(state, n);
  }

  state->current_cached_page = state->cache_window[state->current_window_index];

  return true;
}

bool slide_cache_window_backward(DocState *state) {
  int n = --state->current_page_number;
  int pagecount = state->pagecount;
  /* pthread_t th; */

  if (n < 0) {
    fprintf(stderr, "slide_window_left: cannot slide past start (page %d)\n",
            state->current_page_number);
    return false;
  }

  if (n - MAX_CACHE_WINDOW > 0 && n + MAX_CACHE_WINDOW < pagecount) {

    memmove(&state->cache_window[1], &state->cache_window[0],
            sizeof(state->cache_window[0]) * (MAX_CACHE_SIZE - 1));

    CachedPage *cp = state->cached_pages_pool[n - MAX_CACHE_WINDOW];
    state->cache_window[0] = cp;

    if (cp->status == PAGE_STATUS_EMPTY) {
      load_page_dl(state, cp);
      async_render(state, cp);
    }

    state->current_window_index = MAX_CACHE_WINDOW;
  } else {
    build_cache_window(state, n);
  }

  state->current_cached_page = state->cache_window[state->current_window_index];
  return true;
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

  init_main_ctx(state);  // Creates mupdf context with locks
  load_mupdf_doc(state); // Opens the doc and sets pagecount

  state->cached_pages_pool =
      malloc(state->pagecount * sizeof(*state->cached_pages_pool));
  CachedPage *block = calloc(state->pagecount, sizeof *block);

  for (int i = 0; i < state->pagecount; ++i) {
    state->cached_pages_pool[i] = &block[i];
    state->cached_pages_pool[i]->page_num = i;
    pthread_mutex_init(&state->cached_pages_pool[i]->mutex, NULL);
  }

  build_cache_window(state, state->current_page_number);

  set_current_pagecount(env, state);
  set_current_render_status(env);
  init_overlay(env);
  emacs_value current_svg_overlay = get_current_svg_overlay(env);

  CachedPage *cp = state->current_cached_page;

  emacs_value current_image_data =
      svg2elisp_image(env, state, cp->svg_data, cp->svg_size);

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

    // Get the pointer for the next CachedPage in the window
    CachedPage *next_cp = state->cache_window[state->current_window_index + 1];
    fprintf(stderr, "Loading next page %d\n", next_cp->page_num);

    emacs_value next_image_data =
        svg2elisp_image(env, state, next_cp->svg_data, next_cp->svg_size);

    emacs_value overlay_put_args[3] = {
        current_svg_overlay, env->intern(env, "display"), next_image_data};
    env->funcall(env, env->intern(env, "overlay-put"), 3, overlay_put_args);

    slide_cache_window_forward(state);
    return EMACS_T;
  } else {
    return EMACS_NIL;
  }

  /* sleep(1); */
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

    CachedPage *prev_cp = state->cache_window[state->current_window_index - 1];

    emacs_value prev_image_data =
        svg2elisp_image(env, state, prev_cp->svg_data, prev_cp->svg_size);

    emacs_value overlay_put_args[3] = {
        current_svg_overlay, env->intern(env, "display"), prev_image_data};
    env->funcall(env, env->intern(env, "overlay-put"), 3, overlay_put_args);

    slide_cache_window_backward(state);

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

    build_cache_window(state, state->current_page_number);
    CachedPage *first_cp = state->current_cached_page;

    emacs_value first_image_data =
        svg2elisp_image(env, state, first_cp->svg_data, first_cp->svg_size);

    emacs_value overlay_put_args[3] = {
        current_svg_overlay, env->intern(env, "display"), first_image_data};
    env->funcall(env, env->intern(env, "overlay-put"), 3, overlay_put_args);

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
      emacs_message(env, "Already the last page!");
      return EMACS_NIL;
    }

    state->current_page_number = state->pagecount - 1;
    build_cache_window(state, state->current_page_number);

    CachedPage *last_cp = state->current_cached_page;
    emacs_value last_image_data =
        svg2elisp_image(env, state, last_cp->svg_data, last_cp->svg_size);
    emacs_value overlay_put_args[3] = {
        current_svg_overlay, env->intern(env, "display"), last_image_data};
    env->funcall(env, env->intern(env, "overlay-put"), 3, overlay_put_args);
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
      build_cache_window(state, state->current_page_number);

      CachedPage *cp = state->current_cached_page;
      emacs_value current_image_data =
          svg2elisp_image(env, state, cp->svg_data, cp->svg_size);
      emacs_value overlay_put_args[3] = {
          current_svg_overlay, env->intern(env, "display"), current_image_data};
      env->funcall(env, env->intern(env, "overlay-put"), 3, overlay_put_args);
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
    CachedPage *cp = state->current_cached_page;

    emacs_value current_image_data =
        svg2elisp_image(env, state, cp->svg_data, cp->svg_size);
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

  register_module_func(env, set_doc_theme, "reader-dyn--set-doc-theme", 2, 2,
                       "Sets the the theme for the documents rendered by Emacs "
                       "reader. Currently only sets their FOREGROUND (first "
                       "arg) and BACKGROUND (second arg).");

  // Provide the current dynamic module as a feature to Emacs
  provide(env, "render-core");
  fprintf(stderr, "Emacs module initialized successfully.\n");

  return 0;
}
