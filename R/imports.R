# ─────────────────────────────────────────────────────────────────────────────
# Global imports for the entire package
# ─────────────────────────────────────────────────────────────────────────────

#' @useDynLib sshist, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom grDevices adjustcolor hcl.colors rgb
#' @importFrom graphics abline axis box grid hist image lines par plot.new plot.window points polygon rect rug
#' @importFrom stats median na.omit
NULL
