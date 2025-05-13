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
