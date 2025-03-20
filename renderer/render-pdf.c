#include <assert.h>
#include <emacs-module.h>
#include <mupdf/fitz.h>
#include <stdio.h>
#include <stddef.h>
#include <stdbool.h>
#include <stdlib.h>
#include "helpers.h"

int plugin_is_GPL_compatible;

int render_pdf(const char *input_file, char **svg_data, size_t *svg_size)
{
    fz_context *ctx = NULL;
    fz_document *doc = NULL;
    fz_page *page = NULL;
    fz_device *dev = NULL;
    fz_output *out = NULL;
    fz_buffer *buf = NULL;
    fz_rect rect;
    int page_number = 0;

    // Initialize context
    ctx = fz_new_context(NULL, NULL, FZ_STORE_UNLIMITED);
    if (!ctx)
    {
        fprintf(stderr, "Cannot create MuPDF context\n");
        return EXIT_FAILURE;
    }

    // Register document handlers
    fz_try(ctx)
    {
        fz_register_document_handlers(ctx);
    }
    fz_catch(ctx)
    {
        fprintf(stderr, "Cannot register document handlers: %s\n", fz_caught_message(ctx));
        fz_drop_context(ctx);
        return EXIT_FAILURE;
    }

    // Open the document
    fz_try(ctx)
    {
        doc = fz_open_document(ctx, input_file);
    }
    fz_catch(ctx)
    {
        fprintf(stderr, "Cannot open document '%s': %s\n", input_file, fz_caught_message(ctx));
        fz_drop_context(ctx);
        return EXIT_FAILURE;
    }

    // Load the specified page
    fz_try(ctx)
    {
        page = fz_load_page(ctx, doc, page_number);
    }
    fz_catch(ctx)
    {
        fprintf(stderr, "Cannot load page %d: %s\n", page_number + 1, fz_caught_message(ctx));
        fz_drop_document(ctx, doc);
        fz_drop_context(ctx);
        return EXIT_FAILURE;
    }

    // Get the page bounding box
    rect = fz_bound_page(ctx, page);
    float page_width = rect.x1 - rect.x0;
    float page_height = rect.y1 - rect.y0;

    // Create a buffer to hold the SVG data
    fz_try(ctx)
    {
        buf = fz_new_buffer(ctx, 1024); // Initial capacity; will grow as needed
    }
    fz_catch(ctx)
    {
        fprintf(stderr, "Cannot create buffer: %s\n", fz_caught_message(ctx));
        fz_drop_page(ctx, page);
        fz_drop_document(ctx, doc);
        fz_drop_context(ctx);
        return EXIT_FAILURE;
    }

    // Create an output object associated with the buffer
    fz_try(ctx)
    {
        out = fz_new_output_with_buffer(ctx, buf);
    }
    fz_catch(ctx)
    {
        fprintf(stderr, "Cannot create output: %s\n", fz_caught_message(ctx));
        fz_drop_buffer(ctx, buf);
        fz_drop_page(ctx, page);
        fz_drop_document(ctx, doc);
        fz_drop_context(ctx);
        return EXIT_FAILURE;
    }

    // Create an SVG device
    fz_try(ctx)
    {
      dev = fz_new_svg_device(ctx, out, page_width, page_height, 0, 1);
    }
    fz_catch(ctx)
    {
        fprintf(stderr, "Cannot create SVG device: %s\n", fz_caught_message(ctx));
        fz_close_output(ctx, out);
        fz_drop_output(ctx, out);
        fz_drop_buffer(ctx, buf);
        fz_drop_page(ctx, page);
        fz_drop_document(ctx, doc);
        fz_drop_context(ctx);
        return EXIT_FAILURE;
    }

    // Run the page through the device
    fz_try(ctx)
    {
        fz_run_page(ctx, page, dev, fz_identity, NULL);
    }
    fz_catch(ctx)
    {
        fprintf(stderr, "Cannot run page: %s\n", fz_caught_message(ctx));
        fz_close_device(ctx, dev);
        fz_drop_device(ctx, dev);
        fz_close_output(ctx, out);
        fz_drop_output(ctx, out);
        fz_drop_buffer(ctx, buf);
        fz_drop_page(ctx, page);
        fz_drop_document(ctx, doc);
        fz_drop_context(ctx);
        return EXIT_FAILURE;
    }

    // Close and drop the device
    fz_close_device(ctx, dev);
    fz_drop_device(ctx, dev);

    // Close the output to finalize the buffer content
    fz_try(ctx)
    {
        fz_close_output(ctx, out);
    }
    fz_catch(ctx)
    {
        fprintf(stderr, "Cannot close output: %s\n", fz_caught_message(ctx));
        fz_drop_output(ctx, out);
        fz_drop_buffer(ctx, buf);
        fz_drop_page(ctx, page);
        fz_drop_document(ctx, doc);
        fz_drop_context(ctx);
        return EXIT_FAILURE;
    }

    // Allocate memory for SVG data
    *svg_size = buf->len;
    *svg_data = (char *)malloc(*svg_size + 1); // +1 for null terminator
    if (*svg_data == NULL)
    {
        fprintf(stderr, "Cannot allocate memory for SVG data\n");
        fz_drop_output(ctx, out);
        fz_drop_buffer(ctx, buf);
        fz_drop_page(ctx, page);
        fz_drop_document(ctx, doc);
        fz_drop_context(ctx);
        return EXIT_FAILURE;
    }

    // Copy the data and null-terminate
    memcpy(*svg_data, buf->data, *svg_size);
    (*svg_data)[*svg_size] = '\0';

    // Clean up
    fz_drop_output(ctx, out);
    fz_drop_buffer(ctx, buf);
    fz_drop_page(ctx, page);
    fz_drop_document(ctx, doc);
    fz_drop_context(ctx);

    return EXIT_SUCCESS;
}

emacs_value load_pdf(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                     void *data) {
  (void)nargs;
  (void)data;

  char *file;
  size_t str_length = 0;
  char *svg_data = NULL;
  size_t svg_size = 0;

  emacs_value result = env->intern(env, "nil");

  if (!emacs_2_c_str(env, args[0], &file, &str_length)) {
    fprintf(stderr, "Failed to convert Emacs string to C string.\n");
    return env->intern(env, "nil");
  }

  fprintf(stderr, "Attempting to render: %s\n", file);

  if (render_pdf(file, &svg_data, &svg_size) == EXIT_SUCCESS) {
    emacs_value svg_string = env->make_string(env, svg_data, svg_size);
    emacs_value buffer_name = env->make_string(env, "*svg*", 5);
    emacs_value buffer = env->funcall(env, env->intern(env, "generate-new-buffer"), 1, &buffer_name);
    env->funcall(env, env->intern(env, "switch-to-buffer"), 1, &buffer);
    env->funcall(env, env->intern(env, "insert"), 1, &svg_string);
    env->funcall(env, env->intern(env, "image-mode"), 0, NULL);
    result = buffer;
  } else {
    fprintf(stderr, "Rendering failed.\n");
    /* return env->intern(env, "nil"); */
    return result;
  }

  free(file);
  free(svg_data);
  return env->intern(env, "t");
}

int emacs_module_init(struct emacs_runtime *runtime) {
  emacs_env *env = runtime->get_environment(runtime);
  if (!env) {
    fprintf(stderr, "Failed to get Emacs environment.\n");
    return 1;  // Nonzero means failure
  }
  emacs_value fset = env->intern(env, "fset");
  emacs_value func_symbol = env->intern(env, "load-pdf");
  emacs_value func =
    env->make_function(env, 1, 1, load_pdf, "Loads a PDF file.", NULL);
  emacs_value args[] = {func_symbol, func};
  env->funcall(env, fset, 2, args);
  fprintf(stderr, "Emacs module initialized successfully.\n");
  return 0;
}
