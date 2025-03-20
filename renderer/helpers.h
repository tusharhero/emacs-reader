#ifndef HELPERS_H
#define HELPERS_H

#include <stddef.h>
#include <stdbool.h>
#include <emacs-module.h>

bool emacs_2_c_str(emacs_env *env, emacs_value value, char **buffer, size_t *size);

#endif
