// Helper functions that complement the MuPDF C API

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

#include "mupdf-helpers.h"
#include "render-core.h"

pthread_mutex_t g_mupdf_mutex[FZ_LOCK_MAX];

/**
 * doc_page_width - Provide integer width of the current document
 * @state: Pointer to DocState
 * Return: Integer width
 */

int
doc_page_width(DocState *state)
{
	CachedPage *cp = state->current_cached_page;
	int width = cp->imgw;
	return width;
}

/**
 * doc_page_height - Provide integer height of the current document
 * @state: Pointer to DocState
 * Return: Integer height
 */

int
doc_page_height(DocState *state)
{
	CachedPage *cp = state->current_cached_page;
	int height = cp->imgh;
	return height;
}

/**
 * reset_doc_state - Reset all fields of a DocState to their initial values.
 * @state: Pointer to the DocState to be reset.
 *
 * Clears out any existing data in the DocState by overwriting it with
 * an initializer that sets all pointers to NULL, integer/page-number
 * fields to zero, and the page bounding box to zero coordinates.
 * Use this after dropping pages or closing the document to ensure the
 * state is clean before reuse.
 */

void
reset_doc_state(DocState *state)
{
	fprintf(stderr, "Freeing the existing DocState\n");
	*state = (DocState){
		.ctx = NULL,
                      .locks = NULL,
                      .doc = NULL,
                      .cached_pages_pool = NULL,
                      .cache_window = NULL,
                      .current_window_index = 0,
                      .current_cached_page = NULL,
                      .current_page_number = 0,
                      .page_bbox =
                          {
                              .x0 = 0.0f,
                              .y0 = 0.0f,
                          },
		      .path = NULL,
		      .pagecount = 0,
                      .outline = NULL,
                      .resolution = 72,
                      .rotate = 0,
		      .invert = 0};
}

void
fail(const char *msg)
{
	fprintf(stderr, "%s\n", msg);
	abort();
}

void
lock_mutex(void *user, int lock)
{
	pthread_mutex_t *mutexes = (pthread_mutex_t *)user;
	if (pthread_mutex_lock(&mutexes[lock]) != 0)
		fail("pthread_mutex_lock()");
}

void
unlock_mutex(void *user, int lock)
{
	pthread_mutex_t *mutexes = (pthread_mutex_t *)user;
	if (pthread_mutex_unlock(&mutexes[lock]) != 0)
		fail("pthread_mutex_unlock()");
}

int
init_main_ctx(DocState *state)
{
	for (int i = 0; i < FZ_LOCK_MAX; ++i)
	{
		pthread_mutex_init(&g_mupdf_mutex[i], NULL);
	}
	state->locks.user = g_mupdf_mutex;
	state->locks.lock = lock_mutex;
	state->locks.unlock = unlock_mutex;
	state->ctx = fz_new_context(NULL, &state->locks, FZ_STORE_DEFAULT);
	fz_register_document_handlers(state->ctx);

	if (!state->ctx)
	{
		fprintf(stderr, "Cannot create MuPDF context\n");
		return EXIT_FAILURE;
	}

	return EXIT_SUCCESS;
}

int
load_mupdf_doc(DocState *state)
{

	fz_try(state->ctx)
	{
		state->doc = fz_open_document(state->ctx, state->path);
		state->outline = fz_load_outline(state->ctx, state->doc);
		state->pagecount = fz_count_pages(state->ctx, state->doc);
	}
	fz_catch(state->ctx)
	{
		fprintf(stderr, "Could not open document\n");
		fz_drop_context(state->ctx);
		return EXIT_FAILURE;
	}

	return EXIT_SUCCESS;
}

void
free_cached_page(DocState *state, CachedPage *cp)
{
	if (cp->status != PAGE_STATUS_READY)
		return; // already evicted or never loaded

	cp->status = PAGE_STATUS_EMPTY;
	fz_drop_display_list(state->ctx, cp->display_list);
	cp->display_list = NULL;
	free(cp->img_data);
	cp->img_data = NULL;
	cp->img_size = 0;
}

void
free_cache_window(DocState *state)
{
	fprintf(stderr, "Freeing the cache window for current document...\n");
	for (int i = 0; i < MAX_CACHE_SIZE; i++)
	{
		if (state->cache_window[i]->status == PAGE_STATUS_READY)
			free_cached_page(state, state->cache_window[i]);
	}
	*state->cache_window = NULL;
}

void
free_cached_pages_pool(DocState *state)
{
	fprintf(stderr, "Freeing the entire cached pages pool for current "
			"document...\n");
	for (int i = 0; i < state->pagecount; i++)
	{
		pthread_mutex_destroy(&state->cached_pages_pool[i]->mutex);
		pthread_cond_destroy(&state->cached_pages_pool[i]->cond);
	}
	free(state->cached_pages_pool[0]);
	free(state->cached_pages_pool);
}
