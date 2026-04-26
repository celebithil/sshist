#' sshist: Optimal Density Estimation via Shimazaki-Shinomoto Method
#'
#' @section Main functions:
#' \describe{
#'   \item{\code{\link{sshist}}}{\cr Optimal 1D histogram binning.}
#'   \item{\code{\link{sshist_2d}}}{\cr Optimal 2D histogram binning.}
#'   \item{\code{\link{sskernel}}}{\cr Optimal 1D fixed-bandwidth kernel density estimation.}
#'   \item{\code{\link{sskernel2d}}}{\cr Optimal 2D fixed-bandwidth kernel density estimation.}
#'   \item{\code{\link{ssvkernel}}}{\cr Locally adaptive 1D kernel density estimation.}
#'   \item{\code{\link{ssvkernel2d}}}{\cr Locally adaptive 2D kernel density estimation.}
#' }
#'
#' @section References:
#' \itemize{
#'   \item Shimazaki, H. and Shinomoto, S. (2007). A method for selecting the
#'     bin size of a time histogram. \emph{Neural Computation}, \strong{19}(6),
#'     1503–1527. \doi{10.1162/neco.2007.19.6.1503}
#'   \item Shimazaki, H. and Shinomoto, S. (2010). Kernel bandwidth optimization
#'     in spike rate estimation. \emph{Journal of Computational Neuroscience},
#'     \strong{29}(1-2), 171–182. \doi{10.1007/s10827-009-0180-4}
#' }
#'
#' @keywords internal
#' @useDynLib sshist, .registration = TRUE
"_PACKAGE"
