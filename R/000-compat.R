# R/000-compat.R
# Compatibility helper: replicate old scales:::is.zero
is.zero <- function(x, tol = .Machine$double.eps^0.5) {
  if (is.null(x) || length(x) == 0L || anyNA(x)) return(FALSE)
  # treat length-1 numeric (and things coercible to numeric) as "zero" within tolerance
  x <- suppressWarnings(as.numeric(x))
  if (length(x) != 1L || is.na(x)) return(FALSE)
  isTRUE(all.equal(x, 0, tolerance = tol))
}

