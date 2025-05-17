// Helper functions that complement the C API of the Emacs Module

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
#include <assert.h>
#include "emacs-module.h"
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

/**
 * elisp_2_c_str - Convert an Elisp string to a C string.
 * @env: Pointer (of type `emacs_env') to the environment of the Emacs runtime
 * @value: The value of the Elisp string that is to be converted.  It will only
 * the value (in Elisp, (symbol-value str)) of the string, not the symbol.
 * @buffer: The C character array that is going to store the converted string.
 * @size: Pointer to the length of the string after being converted, as
 * type size_t.
 *
 * @Return: true on success, false on error
 */

bool elisp_2_c_str(emacs_env *env, emacs_value value, char **buffer,
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

/**
 * provide - Calls the Elisp (provide) function to provide a feature to the
 * module.
 * @env: Pointer (of type `emacs_env') to the environment of the Emacs runtime
 * @value: String of the symbol that is to be provid-ed.
 *
 * It doesn’t store the symbol or value of the feature anywhere, only calls the
 * provide function with the symbol from the string.
 *
 * It’s equivalent to doing the following in Elisp:
 * (provide 'feature)
 */

void provide(emacs_env *env, const char *value) {
  emacs_value Qvalue = env->intern(env, value);
  emacs_value Qprovide = env->intern(env, "provide");
  emacs_value args[] = {Qvalue};

  env->funcall(env, Qprovide, 1, args);
}

/**
 * register_module_func - Register a function from the dynamic module into the
 * Emacs environment.  It makes the function available to be directly usable
 * from Emacs as any other Elisp function.
 * @env: Pointer to the Emacs environment for the runtime (emacs_env).
 * @module_func: Pointer to the function (emacs_value) that needs to be
registered.
 * @symbol: The symbol for which the function is to be registered.
 * @min_args: The minimum number of arguments this function must be provided
 with.
 * @max_args: The maximum number of arguments this function can take.
 * @docstring: The string which would act as the docstring for the module
 function. Equivalent to the docstring in Elisp functions, and visible from
 C-h f.
 *
 * The @module_func must be not only compile-able but also it must respect the
 * Elisp runtime of Emacs, any sort of undefined or unpredictable behavior would
 * not be found during compile-time, but rather would result in a segfault of
 * Emacs.
 *
 * Returns: nothing
 */

void register_module_func(
    emacs_env *env,
    emacs_value (*module_func)(emacs_env *env, ptrdiff_t nargs,
                               emacs_value *args, void *data),
    char *symbol, int min_args, int max_args, char *docstring) {

  emacs_value elisp_func_symbol = env->intern(env, symbol);
  emacs_value elisp_func =
      env->make_function(env, min_args, max_args, module_func, docstring, NULL);

  emacs_value elisp_func_args[2] = {elisp_func_symbol, elisp_func};
  env->funcall(env, env->intern(env, "fset"), 2, elisp_func_args);
}

/**
 * svg2elisp_image - Create an Elisp image object from raw SVG data
 * @env: Emacs environment pointer
 * @svg_data: SVG data that is to be rendered as image, should usually be from
 * the DocState.
 * @svg_size: The size of the SVG data that is going to be rendered, should
 * usually be from the DocState.
 *
 * One needs to be extremely about handling the SVG data, and make sure it later
 * gets dropped and freed accordingly.
 *
 * Returns: The Elisp image object, which will be a specific plist, and can be
rendered in Emacs with `insert-image' or other means.

 */

emacs_value svg2elisp_image(emacs_env *env, DocState *state, char *svg_data,
                            size_t svg_size) {
  emacs_value image_width = env->make_integer(env, doc_page_width(state));
  emacs_value image_length = env->make_integer(env, doc_page_length(state));
  emacs_value image_background =
      env->make_string(env, "white", strlen("white"));
  emacs_value image_foreground =
      env->make_string(env, "black", strlen("black"));

  emacs_value svg_string = env->make_string(env, svg_data, svg_size);
  emacs_value image_args[11] = {svg_string,
                                env->intern(env, "svg"),
                                env->intern(env, "t"),
                                env->intern(env, ":width"),
                                image_width,
                                env->intern(env, ":length"),
                                image_length,
                                env->intern(env, ":background"),
                                image_background,
                                env->intern(env, ":foreground"),
                                image_foreground};
  emacs_value image_data =
      env->funcall(env, env->intern(env, "create-image"), 11, image_args);

  return image_data;
}

/**
 * get_doc_state_ptr - Retrieve the DocState pointer stored in Elisp.
 * @env:    The Emacs environment pointer.
 *
 * Looks up the Elisp symbol `doc-state-ptr`, obtains its value,
 * and converts that Lisp object into a native C pointer to `DocState`.
 * Returns NULL if the symbol is unbound or not a valid user pointer.
 *
 * Return: A pointer to the current `DocState`.
 */

DocState *get_doc_state_ptr(emacs_env *env) {
  emacs_value ptr_sym = env->intern(env, "reader-current-doc-state-ptr");
  emacs_value ptr =
      env->funcall(env, env->intern(env, "symbol-value"), 1, &ptr_sym);
  DocState *state = env->get_user_ptr(env, ptr);

  return state;
}

/**
 * get_current_page_number - Elisp-callable wrapper to fetch page number.
 * @env:    The Emacs environment pointer.
 * @nargs:  Number of arguments passed by Elisp (ignored).
 * @args:   Argument values passed by Elisp (ignored).
 * @data:   Callback data (ignored).
 *
 * Reads the `current_page_number` from the `DocState` and returns it
 * as an Elisp integer. Intended to be registered as an interactive
 * Elisp primitive.
 *
 * Return: An Elisp integer representing the current page number.
 */

emacs_value get_current_page_number(emacs_env *env, ptrdiff_t nargs,
                                    emacs_value *args, void *data) {
  (void)nargs;
  (void)data;
  (void)args;

  DocState *state = get_doc_state_ptr(env);
  if (state) {
    emacs_value page_number =
        env->make_integer(env, state->current_page_number);
    return page_number;
  } else {
    return EMACS_NIL;
  }
}

/**
 * set_current_render_status - Mark the buffer’s render status as true.
 * @env:    The Emacs environment pointer.
 *
 * Interns the Elisp variable `doc-render-status` and sets it to `t`
 * (true) in the current buffer, indicating that the SVG rendering
 * is up to date.
 */

void set_current_render_status(emacs_env *env) {
  emacs_value render_status_var = env->intern(env, "reader-doc-render-status");
  env->funcall(env, env->intern(env, "set"), 2,
               (emacs_value[]){render_status_var, env->intern(env, "t")});
}

/**
 * set_current_pagecount - Expose the document’s total page count to Elisp.
 * @env:    The Emacs environment pointer.
 * @state:  Pointer to the `DocState` containing the pagecount.
 *
 * Sets the Elisp variable `current-doc-pagecount` in the current buffer
 * to the integer value of `state->pagecount`, so that Elisp code can
 * inspect how many pages the document has.
 */

void set_current_pagecount(emacs_env *env, DocState *state) {
  emacs_value pagecount_args[2] = {
      env->intern(env, "reader-current-doc-pagecount"),
      env->make_integer(env, state->pagecount)};
  env->funcall(env, env->intern(env, "set"), 2, pagecount_args);
}

/**
 * init_overlay - Create and register an overlay covering the whole buffer.
 * @env:    The Emacs environment pointer.
 *
 * Calls `point-min` and `point-max` to get buffer bounds, then
 * makes an overlay spanning the entire buffer. Stores the overlay
 * object in the Elisp variable `reader-current-svg-overlay` for later use.
 */

void init_overlay(emacs_env *env) {
  emacs_value start = env->funcall(env, env->intern(env, "point-min"), 0, NULL);
  emacs_value end = env->funcall(env, env->intern(env, "point-max"), 0, NULL);
  emacs_value overlay = env->funcall(env, env->intern(env, "make-overlay"), 2,
                                     (emacs_value[]){start, end});
  emacs_value current_overlay_sym =
      env->intern(env, "reader-current-svg-overlay");

  env->funcall(env, env->intern(env, "set"), 2,
               (emacs_value[]){current_overlay_sym, overlay});
}

/**
 * get_current_svg_overlay - Retrieve the stored SVG overlay object.
 * @env:    The Emacs environment pointer.
 *
 * Looks up the Elisp variable `reader-current-svg-overlay` and returns its
 * value, which should be the overlay previously created by `init_overlay`.
 *
 * Return: The Elisp overlay object, or an unbound value if not set.
 */

emacs_value get_current_svg_overlay(emacs_env *env) {

  emacs_value current_overlay_sym =
      env->intern(env, "reader-current-svg-overlay");
  emacs_value current_overlay = env->funcall(
      env, env->intern(env, "symbol-value"), 1, &current_overlay_sym);

  return current_overlay;
}

/**
 * emacs_message - Print a message in Emacs with the given string
 * @env:    The Emacs environment pointer.
 * @str:    Null-terminated C string containing the message.
 */

void emacs_message(emacs_env *env, char *str) {
  emacs_value el_string = env->make_string(env, str, strlen(str));
  env->funcall(env, env->intern(env, "message"), 1, &el_string);
}
