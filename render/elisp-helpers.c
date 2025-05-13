// Helper functions that complement the C API of the Emacs Module

#include "elisp-helpers.h"
#include <assert.h>
#include <emacs-module.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>

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
