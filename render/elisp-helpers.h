#ifndef EMACS_HELPERS_H
#define EMACS_HELPERS_H

#include <emacs-module.h>
#include <stdbool.h>
#include <stddef.h>

bool elisp_2_c_str(emacs_env *env, emacs_value value, char **buffer,
                   size_t *size);
void provide(emacs_env *env, const char *value);
void register_module_func(
    emacs_env *env,
    emacs_value (*module_func)(emacs_env *env, ptrdiff_t nargs,
                               emacs_value *args, void *data),
    char *symbol, int min_args, int max_args, char *docstring);
emacs_value svg2elisp_image(emacs_env *env, char *svg_data, size_t svg_size);

#endif
