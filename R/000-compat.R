# R/000-compat.R

# Fallback for scales::is.zero (no longer exported)
if (!exists("is.zero", inherits = FALSE)) {
  is.zero <- function(x, tol = sqrt(.Machine$double.eps)) {
    if (length(x) != 1L || is.na(x)) return(FALSE)
    x <- suppressWarnings(as.numeric(x)); if (is.na(x)) return(FALSE)
    isTRUE(all.equal(x, 0, tolerance = tol))
  }
}

# Fallback for find_global_tern so early callers won't die.
# It tries ggtern's own namespace first, then ggplot2, then a few safe defaults.
if (!exists("find_global_tern", inherits = FALSE)) {
  find_global_tern <- function(name) {
    # ggtern internals (if already defined)
    if (exists(name, envir = parent.env(environment()), inherits = FALSE))
      return(get(name, envir = parent.env(environment()), inherits = FALSE))
    # ggplot2 private constants (e.g., .pt)
    if (name == ".pt") return(72.27 / 25.4)        # points -> mm
    if (requireNamespace("ggplot2", quietly = TRUE) &&
        exists(name, envir = asNamespace("ggplot2"), inherits = FALSE))
      return(get(name, envir = asNamespace("ggplot2"), inherits = FALSE))
    stop("Global '", name, "' not found (check collate order).")
  }
}

# ---- ggplot2 4.0 theme element shims (inside ggtern namespace) ----
# Convert old 'size=' to 'linewidth=' and coerce logicals to numeric.
.ele_linewidth <- function(x, default = 0.5) {
  if (is.null(x)) return(NULL)
  if (is.logical(x)) return(if (isTRUE(x)) default else 0)
  as.numeric(x)
}

# Wrap element_rect: map size->linewidth, sanitize linewidth
element_rect <- local({
  # Keep a handle to ggplot2's real element_rect
  .gg <- get("element_rect", envir = asNamespace("ggplot2"))
  function(..., size = NULL, linewidth = NULL) {
    if (is.null(linewidth) && !is.null(size)) linewidth <- size
    linewidth <- .ele_linewidth(linewidth)
    .gg(..., linewidth = linewidth)
  }
})

# Wrap element_line similarly (drop unsupported 'arrow' if present)
element_line <- local({
  .gg <- get("element_line", envir = asNamespace("ggplot2"))
  function(..., size = NULL, linewidth = NULL) {
    if (is.null(linewidth) && !is.null(size)) linewidth <- size
    linewidth <- .ele_linewidth(linewidth)
    .gg(..., linewidth = linewidth)
  }
})
