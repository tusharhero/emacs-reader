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

#include "render-theme.h"
#include "elisp-helpers.h"
#include "render-core.h"

emacs_value
emacs_set_dark_theme(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
		     void *data)
{
  (void)data;
  (void)nargs;
  (void)args;
  DocState *state = get_doc_state_ptr(env);
  emacs_value current_doc_overlay = get_current_doc_overlay(env);
  if (state)
  {
    state->invert ^= 1;
    CachedPage *cp = state->current_cached_page;
    DrawThreadArgs *draw_args
      = malloc(sizeof(DrawThreadArgs));
    draw_args->state = state;
    draw_args->cp = cp;
    draw_page_thread(draw_args);
    display_img_to_overlay(env, state, cp->img_data,
			   cp->img_size,
			   current_doc_overlay);
  }
  else
    {
      return EMACS_NIL;
    }

  return EMACS_T;
}
