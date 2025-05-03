#' Progress function
#'
#' plots the progress of an iterative procedure using "[=>   ]", "[==> ]", etc.
#'
#' @param p Value
#' @return NULL
#'
#' @importFrom utils flush.console
#'
#' @export
loader <- function(p) {
  if (p == 0) cat("0%                       50%                     100%\n")
  str <- paste0(rep(c("\r[", "=", ">", " ", "]"), c(1, floor(p * 50), 1, 50 - floor(p * 50), 1)), collapse = "")
  cat(str)
  flush.console()
  if (floor(p) == 1) cat("\n")
}
