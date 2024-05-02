#' @title Functions to Interactively Clean Data Frames
#' @description This family of functions clean a data frame as follows:
#' \tabular{ll}{  `clean_remode`        \tab Change variable modes.                      \cr
#'                `clean_recode`        \tab Recode variables.                           \cr
#'                `clean_rename`        \tab Rename variables.                           \cr
#'                `clean_select`        \tab Select variables to retain.                 \cr
#'                `clean_data`          \tab Select, rename, recode, and remode variables. }
#' @param x A data frame.
#' @param clear A logical scalar indicating whether to CLEAR the console at the start of each data cleaning step.
#' @return A data frame.
#' @export
clean_data <- function(x, clear = TRUE) {
  base::gc()
  errs <- NULL
  if (!ppp::.atm_rct_dtf(x    )) {errs <- base::c(errs, "[x] must have 2+ rows, 2+ columns, and all columns must be atomic.")}
  if (!ppp::.cmp_lgl_scl(clear)) {errs <- base::c(errs, "[clear] must TRUE or FALSE.")}
  if (!base::is.null(errs     )) {ppp::stopperr(errs, pkg = "rwd")}
  x <- rwd::clean_select(x, clear = clear)
  x <- rwd::clean_rename(x, clear = clear)
  x <- rwd::clean_recode(x, clear = clear)
  rwd::clean_remode(x)
}

#' @rdname clean_data
#' @export
clean_select <- function(x, clear = TRUE) {
  base::gc()
  errs <- NULL
  if (!ppp::.atm_rct_dtf(x    )) {errs <- base::c(errs, "[x] must have 2+ rows, 2+ columns, and all columns must be atomic.")}
  if (!ppp::.cmp_lgl_scl(clear)) {errs <- base::c(errs, "[clear] must TRUE or FALSE.")}
  if (!base::is.null(errs     )) {ppp::stopperr(errs, pkg = "rwd")}
  varsToKeep <- dlg::chooseN(uj::cn(x), "What variables do you want to retain?", clear = clear)
  x[ , varsToKeep]
}

#' @rdname clean_data
#' @export
clean_rename <- function(x, clear = TRUE) {
  base::gc()
  errs <- NULL
  if (!ppp::.atm_rct_dtf(x   )) {errs <- base::c(errs, "[x] must have 2+ rows, 2+ columns, and all columns must be atomic.")}
  if (!ppp::.cmp_lgl_scl(clear)) {errs <- base::c(errs, "[clear] must TRUE or FALSE.")}
  if (!base::is.null(errs     )) {ppp::stopperr(errs, pkg = "rwd")}
  oldNames <- dlg::chooseN(uj::cn(x), "What variables do you want to rename?", .none = T, clear = clear)
  nOld     <- uj::N(oldNames)
  if (uj::not_BL(oldNames) & nOld != 0) {
    newNames <- dlg::ask_new(oldNames, clear = clear)
    allNames <- uj::cn(x)
    for (i in 1:nOld) {allNames[allNames == oldNames[i]] <- newNames[i]}
    x <- uj::name_cols(x, allNames)
  }
  x
}

#' @rdname clean_data
#' @export
clean_recode <- function(x, clear = TRUE) {
  base::gc()
  errs <- NULL
  if (!ppp::.atm_rct_dtf(x    )) {errs <- base::c(errs, "[x] must have 2+ rows, 2+ columns, and all columns must be atomic.")}
  if (!ppp::.cmp_lgl_scl(clear)) {errs <- base::c(errs, "[clear] must TRUE or FALSE.")}
  if (!base::is.null(errs     )) {ppp::stopperr(errs, pkg = "rwd")}
  varsToRecode <- dlg::chooseN(uj::cn(x), "What variables do you want to recode?", .none = T, clear = clear)
  for (varToRecode in varsToRecode) {
    originalVar   <- uj::av(x[ , varToRecode])
    recodedVar    <- uj::r(uj::N(originalVar), NA)
    originalVals  <- uj::suv(originalVar)
    message       <- uj::p0("What values of [", varToRecode, "] do you want to keep, if any?")
    valsToKeep    <- dlg::chooseN(originalVals, message, none = T, clear = clear)
    availableVals <- originalVals[uj::not_in(originalVals, valsToKeep)]
    if (uj::n1p(availableVals)) {
      message      <- uj::p0("What values of [", varToRecode, "] do you want to recode?")
      valsToRecode <- dlg::chooseN(availableVals, message, none = uj::DEF(valsToKeep), clear = clear)
    } else {valsToRecode <- NULL}
    if (uj::n1p(valsToRecode)) {
      replacementVals <- dlg::ask_new(valsToRecode, u = F, clear = clear)
      if (uj::n1p(valsToRecode)) {
        for (i in 1:uj::N(valsToRecode)) {
          valToRecode    <- valsToRecode[i]
          replacementVal <- replacementVals[i]
          recodedVar[originalVals == valToRecode] <- replacementVal
        }
      }
    }
    if (uj::n1p(valsToKeep)) {
      for (i in 1:uj::N(valsToKeep)) {
        valToKeep <- valsToKeep[i]
        recodedVar[originalVals == valToKeep] <- valToKeep}
      }
    x[ , varToRecode] <- recodedVar
    x <- x[uj::ok(recodedVar), ]
  }
  x
}

#' @rdname clean_data
#' @export
clean_remode <- function(x, clear = TRUE) {
  base::gc()
  errs <- NULL
  if (!ppp::.atm_rct_dtf(x    )) {errs <- base::c(errs, "[x] must have 2+ rows, 2+ columns, and all columns must be atomic.")}
  if (!ppp::.cmp_lgl_scl(clear)) {errs <- base::c(errs, "[clear] must TRUE or FALSE.")}
  if (!base::is.null(errs     )) {ppp::stopperr(errs, pkg = "rwd")}
  toRemode <- dlg::chooseN(uj::cn(x), "What variables do you want to re-MODE?", .none = T, clear = clear)
  allModes <- c("character", "integer", "logical", "numeric")
  for (var in toRemode) {
    original <- uj::av(x[, var])
    newMode  <- dlg::choose1(allModes, "What should [", var, "]'s new mode be?", clear = clear)
    if      (newMode == "character") {remoded <- uj::as_chr(original)}
    else if (newMode == "integer"  ) {remoded <- uj::as_int(original)}
    else if (newMode == "logical"  ) {remoded <- uj::as_lgl(original)}
    else                             {remoded <- uj::as_num(original)}
    x[ , var] <- original
  }
  x
}
