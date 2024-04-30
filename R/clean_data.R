#' @title Functions to Interactively Clean Data Frames
#' @description This family of functions clean a data frame as follows:
#' \tabular{ll}{  `clean_remode`        \tab Change variable modes.                      \cr
#'                `clean_recode`        \tab Recode variables.                           \cr
#'                `clean_rename`        \tab Rename variables.                           \cr
#'                `clean_select`        \tab Select variables to retain.                 \cr
#'                `clean_data`          \tab Select, rename, recode, and remode variables. }
#' @param x A data frame.
#' @param .clear A logical scalar indicating whether to CLEAR the console at the start of each data cleaning step.
#' @return A data frame.
#' @export
clean_data <- function(x, .clear = TRUE) {
  base::gc()
  Errors <- NULL
  if (!ppp::.atm_rct_dtf(x)) {Errors <- base::c(Errors, "[x] must have 2+ rows, 2+ columns, and all columns must be atomic.")}
  if (!ppp::.cmp_lgl_scl(.clear)) {Errors <- base::c(Errors, "[.clear] must TRUE or FALSE.")}
  if (!base::is.null(Errors)) {ppp::stopperr(Errors, .pkg = "rwd")}
  x <- rwd::clean_select(x, .clear = .clear)
  x <- rwd::clean_rename(x, .clear = .clear)
  x <- rwd::clean_recode(x, .clear = .clear)
  rwd::clean_remode(x)
}

#' @rdname clean_data
#' @export
clean_select <- function(x, .clear = TRUE) {
  base::gc()
  Errors <- NULL
  if (!ppp::.atm_rct_dtf(x)) {Errors <- base::c(Errors, "[x] must have 2+ rows, 2+ columns, and all columns must be atomic.")}
  if (!ppp::.cmp_lgl_scl(.clear)) {Errors <- base::c(Errors, "[.clear] must TRUE or FALSE.")}
  if (!base::is.null(Errors)) {ppp::stopperr(Errors, .pkg = "rwd")}
  KeepVars <- dlg::chooseN(uj::cn(x), "What variables do you want to retain?", .clear = .clear)
  x[ , KeepVars]
}

#' @rdname clean_data
#' @export
clean_rename <- function(x, .clear = TRUE) {
  base::gc()
  Errors <- NULL
  if (!ppp::.atm_rct_dtf(x)) {Errors <- base::c(Errors, "[x] must have 2+ rows, 2+ columns, and all columns must be atomic.")}
  if (!ppp::.cmp_lgl_scl(.clear)) {Errors <- base::c(Errors, "[.clear] must TRUE or FALSE.")}
  if (!base::is.null(Errors)) {ppp::stopperr(Errors, .pkg = "rwd")}
  OldNames <- dlg::chooseN(uj::cn(x), "What variables do you want to rename?", .none = T, .clear = .clear)
  nOld <- uj::N(OldNames)
  if (uj::not_BL(OldNames) & nOld != 0) {
    NewNames <- dlg::ask_new(OldNames, .clear = .clear)
    AllNames <- uj::cn(x)
    for (i in 1:nOld) {AllNames[AllNames == OldNames[i]] <- NewNames[i]}
    x <- uj::name_cols(x, AllNames)
  }
  x
}

#' @rdname clean_data
#' @export
clean_recode <- function(x, .clear = TRUE) {
  base::gc()
  Errors <- NULL
  if (!ppp::.atm_rct_dtf(x)) {Errors <- base::c(Errors, "[x] must have 2+ rows, 2+ columns, and all columns must be atomic.")}
  if (!ppp::.cmp_lgl_scl(.clear)) {Errors <- base::c(Errors, "[.clear] must TRUE or FALSE.")}
  if (!base::is.null(Errors)) {ppp::stopperr(Errors, .pkg = "rwd")}
  VarsToRecode <- dlg::chooseN(uj::cn(x), "What variables do you want to recode?", .none = T, .clear = .clear)
  for (VarToRecode in VarsToRecode) {
    OriginalVar <- uj::av(x[ , VarToRecode])
    AllUniqueVals <- uj::suv(OriginalVar)
    RecodedVar <- uj::r(uj::N(OriginalVar), NA)
    Message <- uj::p0("What values of [", VarToRecode, "] do you want to keep, if any?")
    ValsToKeep <- dlg::chooseN(AllUniqueVals, Message, .none = T, .clear = .clear)
    AvailableVals <- AllUniqueVals[uj::not_in(AllUniqueVals, ValsToKeep)]
    if (uj::n1p(AvailableVals)) {
      Message <- uj::p0("What values of [", VarToRecode, "] do you want to recode?")
      ValsToRecode <- dlg::chooseN(AvailableVals, Message, .none = uj::DEF(ValsToKeep), .clear = .clear)
    } else {ValsToRecode <- NULL}
    if (uj::n1p(ValsToRecode)) {
      RecodedVals <- dlg::ask_new(ValsToRecode, .u = F, .clear = .clear)
      if (uj::n1p(ValsToRecode)) {for (i in 1:uj::N(ValsToRecode)) {
        OldVal <- ValsToRecode[i]
        NewVal <- RecodedVals[i]
        RecodedVar[OriginalVar == OldVal] <- NewVal
      }}}
    if (uj::n1p(ValsToKeep)) {for (i in 1:uj::N(ValsToKeep)) {
      KeepVal <- ValsToKeep[i]
      RecodedVar[OriginalVar == KeepVal] <- KeepVal}
    }
    x[ , VarToRecode] <- RecodedVar
    x <- x[uj::ok(RecodedVar), ]
  }
  x
}

#' @rdname clean_data
#' @export
clean_remode <- function(x, .clear = TRUE) {
  base::gc()
  Errors <- NULL
  if (!ppp::.atm_rct_dtf(x)) {Errors <- base::c(Errors, "[x] must have 2+ rows, 2+ columns, and all columns must be atomic.")}
  if (!ppp::.cmp_lgl_scl(.clear)) {Errors <- base::c(Errors, "[.clear] must TRUE or FALSE.")}
  if (!base::is.null(Errors)) {ppp::stopperr(Errors, .pkg = "rwd")}
  VarsToRemode <- dlg::chooseN(uj::cn(x), "What variables do you want to re-MODE?", .none = T, .clear = .clear)
  AllModes <- c("character", "integer", "logical", "numeric")
  for (VarToRemode in VarsToRemode) {
    OriginalVar <- uj::av(x[, VarToRemode])
    NewMode <- dlg::choose1(AllModes, "What should [", VarToRemode, "]'s new mode be?", .clear = .clear)
    if (NewMode == "character") {RemodedVar <- uj::as_chr(OriginalVar)}
    else if (NewMode == "integer") {RemodedVar <- uj::as_int(OriginalVar)}
    else if (NewMode == "logical") {RemodedVar <- uj::as_lgl(OriginalVar)}
    else {RemodedVar <- uj::as_num(OriginalVar)}
    x[ , VarToRemode] <- RemodedVar
  }
  x
}
