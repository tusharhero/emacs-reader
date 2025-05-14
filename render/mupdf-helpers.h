#ifndef MUDPF_HELPERS_H
#define MUPDF_HELPERS_H

#include <mupdf/fitz.h>
#include <stdbool.h>
#include <stddef.h>

bool create_buffers(fz_context *ctx, fz_buffer **curr_buf, fz_buffer **prev_buf,
                    fz_buffer **next_buf);
bool create_outputs(fz_context *ctx, fz_output **curr_out, fz_output **prev_out,
                    fz_output **next_out, fz_buffer *curr_buf,
                    fz_buffer *prev_buf, fz_buffer *next_buf);
void drop_all_buffers(fz_context *ctx, fz_buffer *curr, fz_buffer *prev,
                      fz_buffer *next);
void drop_all_outputs(fz_context *ctx, fz_output *curr, fz_output *prev,
                      fz_output *next);
void drop_all_devices(fz_context *ctx, fz_device *curr, fz_device *prev,
                      fz_device *next);
void close_all_outputs(fz_context *ctx, fz_output *curr, fz_output *prev,
                       fz_output *next);
void close_all_devices(fz_context *ctx, fz_device *curr, fz_device *prev,
                       fz_device *next);

typedef struct {
  fz_context *ctx;
  fz_document *doc;
  char *path;
  int pagecount;
  int prev_page_number;
  int current_page_number;
  int next_page_number;
  char *current_svg_data;
  size_t current_svg_size;
  char *next_svg_data;
  size_t next_svg_size;
  char *prev_svg_data;
  size_t prev_svg_size;
  fz_page *current_page;
  fz_page *next_page;
  fz_page *prev_page;
  fz_rect page_bbox;
  fz_outline *outline;
} DocState;

int doc_page_width(DocState *state);
int doc_page_length(DocState *state);

void clean_up_svg_data(DocState *state);
void drop_all_doc_pages(fz_context *ctx, DocState *state);
void reset_doc_state(DocState *state);

#endif
