#ifndef EMACS_HELPERS_H
#define EMACS_HELPERS_H

#include <emacs-module.h>
#include <stdbool.h>
#include <stddef.h>

bool elisp_2_c_str(emacs_env *env, emacs_value value, char **buffer,
                   size_t *size);
void provide(emacs_env *env, const char *value);

#endif
