# Expose some required functions from the parent ggplot2 namespace
.getFunctions <- function(){

  wanted_gp <- c(
    'create_layout',
    'plot_theme',
    'element_render',
    'set_last_plot','make_labels',
    'add_ggplot','labelGrob',
    'is.layer','is.facet','is.Coord','GeomSegment',
    '.element_tree',
    'expand_limits_scale',
    'view_scale_primary','view_scale_secondary',
    'combine_elements','aes_to_scale',
    'scales_list','guides_list',
    'predictdf',
    'check_required_aesthetics','snake_class',
    'ggname','ggplot_gtable','camelize',
    'element_grob.element_line','element_grob.element_rect',
    'element_grob.element_text','element_grob.element_blank',
    'plot_clone','compute_just','labelGrob',
    'hexGrob','hex_binwidth','hexBinSummarise',
    'find_args','is.margin','justify_grobs',
    'attach_plot_env',
    'by_layer',
    'table_add_tag',
    'table_add_legends' # ok if missing; handled below
    # NOTE: 'update_guides' was removed in ggplot2 >= 3.5, so we don't demand it here.
  )

  safe_get <- function(pkg, fname) {
    ns <- asNamespace(pkg)
    if (exists(fname, envir = ns, inherits = FALSE)) {
      get(fname, envir = ns, inherits = FALSE)
    } else {
      NULL
    }
  }

  gp_list <- setNames(lapply(wanted_gp, function(f) safe_get("ggplot2", f)), wanted_gp)
  ge_list <- setNames(lapply("latticeGrob", function(f) safe_get("gridExtra", f)), "latticeGrob")

  # ---- Shims for removed internals (ggplot2 >= 3.5/4.0) ----
  # Some old ggtern code may call ggint$update_guides(); provide a no-op that returns an empty list.
  if (is.null(gp_list[["update_guides"]])) {
    gp_list[["update_guides"]] <- function(...) list()
  }
  # If table_add_legends disappeared, offer a pass-through that returns its input layout unchanged.
  if (is.null(gp_list[["table_add_legends"]])) {
    gp_list[["table_add_legends"]] <- function(plot_table, guides, theme, guides_position = "right", ...) {
      plot_table
    }
  }

  out <- c(gp_list, ge_list)
  class(out) <- "internal"
  out
}

ggint <- .getFunctions()
