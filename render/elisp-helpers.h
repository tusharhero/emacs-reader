#ifndef EMACS_HELPERS_H
#define EMACS_HELPERS_H

#include "mupdf-helpers.h"
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
emacs_value svg2elisp_image(emacs_env *env, DocState *state, char *svg_data,
                            size_t svg_size);

DocState *get_doc_state_ptr(emacs_env *env);
emacs_value get_current_page_number(emacs_env *env, ptrdiff_t nargs,
                                    emacs_value *args, void *data);
void set_current_render_status(emacs_env *env);
void set_current_pagecount(emacs_env *env, DocState *state);
void init_overlay(emacs_env *env);
emacs_value get_current_svg_overlay(emacs_env *env);

#endif
