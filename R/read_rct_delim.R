#' @title Interactively Select and Read a Rectangular Delimited Text Data File
#' @param type A \link[ppp:cmp_str_scl]{complete string scalar} description of the type of data file to be read.
#' @param .d A \link[ppp:cmp_ch1_scl]{complete onechar scalar} text delimiter.
#' @param .clear A logical scalar indicating whether to clear the console to denote a new user interaction.
#' @return A data.frame.
#' @export
read_rct_delim <- function(type, .d = ",", .clear = TRUE, ...) {
  base::on.exit(base::gc(verbose = F))
  Errors <- NULL
  if (!ppp::cmp_str_scl(type)) {Errors <- base::c(Errors, "[type] must be a complete string scalar (?cmp_str_scl).")}
  if (!ppp::cmp_ch1_scl(.d)) {Errors <- base::c(Errors, "[.d] must be a complete onechar scalar (?cmp_ch1_scl).")}
  if (!base::isTRUE(.clear) & !base::isFALSE(.clear)) {Errors <- base::c(Errors, "[.clear] must be scalar TRUE or scalar FALSE.")}
  if (!base::is.null(Errors)) {ppp::stopperr(Errors, .PKG = "rwd")}
  sep <- uj::f0(uj::is_EQ(.d, ","), "comma",
                uj::f0(uj::is_EQ(.d, "\t"), "tab",
                       uj::f0(uj::is_EQ(.d, "`"), "backtick",
                              uj::f0(uj::is_EQ(.d, "~"), "tilde",
                                     uj::f0(uj::is_EQ(.d, "^"), "caret",
                                            uj::f0(uj::is_EQ(.d, "|"), "pipe", uj::p0("`", .d, "`")))))))
  doc.type <- uj::g0("rectangular ", sep, "-separated ", type, " data file")
  doc.path <- dlg::choose_doc(doc.type, .clear = .clear)
  data     <- rwd::rd_xsv(file = doc.path, .d = .d)
  rwd::clean_data(data, .clear = .clear)
}
