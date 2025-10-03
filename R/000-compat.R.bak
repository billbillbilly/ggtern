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
