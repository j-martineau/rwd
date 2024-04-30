.rd_filename <- function(file, type, .D) {
  if (base::is.null(file)) {
    Prompt <- uj::f0(.D == ",", "comma (,)", uj::f0(.D == "\t", "tab", uj::f0(.D == " ", "space", uj::f0(.D == ".", "dot (.)", uj::f0(.D == "|", "pipe (|)", uj::f0(.D == ":", "colon (:)", uj::f0(.D == ";", "semicolon (;)", uj::f0(.D == "`", "backtick (`)", uj::f0(.D == "^", "caret (^)", uj::f0(.D == "~", "tilde (~)", uj::g("`", .D, "`")))))))))))
    Prompt <- uj::paste0(type, "in rectangular", Prompt, "delimited text format", collapse = " ")
    file   <- dlg::choose_doc(Prompt)
  }
  if (!ppp::.cmp_chr_vec(file)) {ppp::stopperr("[file] must be NULL or a complete character vec (?cmp_chr_vec)", .pkg = "rwd")}
  file <- base::paste0(file, collapse = "")
  if (!base::file.exists(file)) {ppp::stopperr(base::paste0("the specified file ('", file, "') does not exist."), .pkg = "rwd")}
  file
}

#' @name rd
#' @encoding UTF-8
#' @family extensions
#' @family wraps
#' @family files
#' @title Thin and extended functionality wrappers of `readr` functions.
#' @description Read the clipboard and text files, but prompting user to choose a file if no file name/path is provided.
#' @details
#' \tabular{ll}{  `rd_clip`   \tab Thinly wraps \code{\link[readr]{clipboard}}         \cr   \tab   \cr
#'                `rd_csv`    \tab Extends \code{\link[readr]{read_csv}}\eqn{^{(1)}}   \cr
#'                `rd_tsv`    \tab Extends \code{\link[readr]{read_tsv}}\eqn{^{(1)}}   \cr
#'                `rd_xsv`    \tab Extends \code{\link[readr]{read_delim}}\eqn{^{(1)}} \cr   \tab     }
#'  \tabular{l}{  \eqn{^{(1)}} `file = NULL` prompts the user to select a file.}
#' @param .D Either `NULL` or a \link[=cmp_ch1_scl]{complete onechar scalar} text delimiter.
#' @param type A \link[=cmp_str_scl]{complete string scalar} describing the type of file to be read.
#' @param file Either `NULL` or a \link[=cmp_chr_vec]{complete character vector} that resolves to a file path. When `file = NULL` the user is asked to select a file using a system dialog box.
#' @return **Varies**       \cr\cr `rd_clip`
#' \cr\cr  **A data.frame** \cr\cr `rd_csv, rd_tsv, rd_xsv`
#' @export
rd <- function() {utils::help("rd", package = "rwd")}

#' @rdname rd
#' @export
rd_xsv <- function(.D, type = "data file", file = NULL, ...) {
  Errors <- NULL
  if (!ppp::.cmp_ch1_scl(.D)) {Errors <- base::c(Errors, "[.D] must be a complete onechar scalar (?cmp_ch1_scl).")}
  if (!ppp::.cmp_str_scl(type)) {Errors <- base::c(Errors, "[type] must be a complete string scalar (?cmp_str_scl).")}
  if (!base::is.null(file)) {if (!ppp::.cmp_str_scl(file)) {Errors <- base::c(Errors, "[file] must be a complete string scalar (?cmp_str_scl).")}}
  if (!base::is.null(Errors)) {ppp::stopperr(Errors, .pkg = "rwd")}
  readr::read_delim(rwd:::.rd_filename(type, file, .D), delim = .D, ...)
}

#' @rdname rd
#' @export
rd_csv <- function(type = "data file", file = NULL, ...) {
  Errors <- NULL
  if (!ppp::.cmp_str_scl(type)) {Errors <- base::c(Errors, "[type] must be a complete string scalar (?cmp_str_scl).")}
  if (!base::is.null(file)) {if (!ppp::.cmp_str_scl(file)) {Errors <- base::c(Errors, "[file] must be a complete string scalar (?cmp_str_scl).")}}
  if (!base::is.null(Errors)) {ppp::stopperr(Errors, .pkg = "rwd")}
  readr::read_csv(rwd:::.rd_filename(type, file, ","), ...)
}

#' @rdname rd
#' @export
rd_tsv <- function(type = "data file", file = NULL, ...) {
  Errors <- NULL
  if (!ppp::.cmp_str_scl(type)) {Errors <- base::c(Errors, "[type] must be a complete string scalar (?cmp_str_scl).")}
  if (!base::is.null(file)) {if (!ppp::.cmp_str_scl(file)) {Errors <- base::c(Errors, "[file] must be a complete string scalar (?cmp_str_scl).")}}
  if (!base::is.null(Errors)) {ppp::stopperr(Errors, .pkg = "rwd")}
  readr::read_tsv(rwd:::.rd_filename(type, file, "\t"), ...)
}

#' @rdname rd
#' @export
rd_clip <- function() {readr::clipboard()}
