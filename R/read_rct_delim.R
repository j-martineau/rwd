rd_filename <- function(file, type, d) {
  if (base::is.null(file)) {
    sep <- uj::f0(d == "," , "comma (,)"    ,
           uj::f0(d == "\t", "tab"          ,
           uj::f0(d == " " , "space"        ,
           uj::f0(d == "." , "dot (.)"      ,
           uj::f0(d == "|" , "pipe (|)"     ,
           uj::f0(d == ":" , "colon (:)"    ,
           uj::f0(d == ";" , "semicolon (;)",
           uj::f0(d == "`" , "backtick (`)" ,
           uj::f0(d == "^" , "caret (^)"    ,
           uj::f0(d == "~" , "tilde (~)"    , uj::g("`", d, "`")))))))))))
    prompt <- base::paste0(type, "in rectangular", sep, "delimited text format", collapse = " ")
    file   <- dlg::choose_doc(prompt)
  }
  if (!ppp::.cmp_chr_vec(file)) {ppp::stopperr("[file] must be NULL or a complete character vec (?cmp_chr_vec)", pkg = "uj")}
  file <- base::paste0(file, collapse = "")
  if (!base::file.exists(file)) {ppp::stopperr(base::paste0("the specified file ('", file, "') does not exist."), pkg = "uj")}
  file
}

#' @title Interactively Select and Read a Rectangular Delimited Text Data File
#' @param type A \link[ppp:cmp_str_scl]{complete string scalar} description of the type of data file to be read.
#' @param d A \link[ppp:cmp_ch1_scl]{complete onechar scalar} text delimiter.
#' @param clear A logical scalar indicating whether to clear the console to denote a new user interaction.
#' @return A data.frame.
#' @export
read_rct_delim <- function(type, d = ",", clear = TRUE) {
  base::on.exit(base::gc(verbose = F))
  errs <- NULL
  if (!ppp::cmp_str_scl(type )) {errs <- base::c(errs, "[type] must be a complete string scalar (?cmp_str_scl).")}
  if (!ppp::cmp_ch1_scl(d    )) {errs <- base::c(errs, "[d] must be a complete onechar scalar (?cmp_ch1_scl)."  )}
  if (!ppp::cmp_lgl_scl(clear)) {errs <- base::c(errs, "[clear] must be scalar TRUE or scalar FALSE."           )}
  if (!base::is.null(errs)) {ppp::stopperr(errs, pkg = "rwd")}
  sep <- uj::f0(uj::is_EQ(d, "," ), "comma"   ,
         uj::f0(uj::is_EQ(d, "\t"), "tab"     ,
         uj::f0(uj::is_EQ(d, "`" ), "backtick",
         uj::f0(uj::is_EQ(d, "~" ), "tilde"   ,
         uj::f0(uj::is_EQ(d, "^" ), "caret"   ,
         uj::f0(uj::is_EQ(d, "|" ), "pipe"    ,
         uj::p0("`", d, "`")))))))
  type <- uj::g0("rectangular ", sep, "-separated ", type, " data file")
  path <- dlg::choose_doc(type, clear = clear)
  data <- uj::rd_xsv(file = path, d = d)
  rwd::clean_data(data, clear = clear)
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
#' @param d Either `NULL` or a \link[ppp:cmp_ch1_scl]{complete onechar scalar} text delimiter.
#' @param type A \link[ppp:cmp_str_scl]{complete string scalar} describing the type of file to be read.
#' @param file Either `NULL` or a \link[=cmp_chr_vec]{complete character vector} that resolves to a file path. When `file = NULL` the user is asked to select a file using a system dialog box.
#' @return **Varies**       \cr\cr `rd_clip`
#' \cr\cr  **A data.frame** \cr\cr `rd_csv, rd_tsv, rd_xsv`
#' @export
rd <- function() {utils::help("rd", package = "uj")}

#' @rdname rd
#' @export
rd_xsv <- function(d, type = "data file", file = NULL, ...) {
  errs <- NULL
  if (!ppp::.cmp_ch1_scl(d            )) {errs <- base::c(errs, "[d] must be a complete onechar scalar (?cmp_ch1_scl).")}
  if (!ppp::.cmp_str_scl(type         )) {errs <- base::c(errs, "[type] must be a complete string scalar (?cmp_str_scl).")}
  if (!ppp::nll_or(file, "cmp_str_scl")) {errs <- base::c(errs, "[file] must be a complete string scalar (?cmp_str_scl).")}
  if (!base::is.null(errs             )) {ppp::stopperr(errs, pkg = "uj")}
  readr::read_delim(rwd:::rd_filename(type, file, d), delim = d, ...)
}

#' @rdname rd
#' @export
rd_csv <- function(type = "data file", file = NULL, ...) {
  errs <- NULL
  if (!ppp::.cmp_str_scl(type         )) {errs <- base::c(errs, "[type] must be a complete string scalar (?cmp_str_scl).")}
  if (!ppp::nll_or(file, "cmp_str_scl")) {errs <- base::c(errs, "[file] must be a complete string scalar (?cmp_str_scl).")}
  if (!base::is.null(errs             )) {ppp::stopperr(errs, pkg = "uj")}
  readr::read_csv(uj:::rd_filename(type, file, ","), ...)
}

#' @rdname rd
#' @export
rd_tsv <- function(type = "data file", file = NULL, ...) {
  errs <- NULL
  if (!ppp::.cmp_str_scl(type         )) {errs <- base::c(errs, "[type] must be a complete string scalar (?cmp_str_scl).")}
  if (!ppp::nll_or(file, "cmp_str_scl")) {errs <- base::c(errs, "[file] must be a complete string scalar (?cmp_str_scl).")}
  if (!base::is.null(errs             )) {ppp::stopperr(errs, pkg = "uj")}
  readr::read_tsv(uj:::rd_filename(type, file, "\t"), ...)
}

#' @rdname rd
#' @export
rd_clip <- function() {readr::clipboard()}
