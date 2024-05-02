#' @title Map Variables in a Data Frame to Specified Roles
#' @description Utility to map variables in a data.frame to specific roles, with options to trim variables not assigned to roles, to rename variables to match variable roles (vs. attaching a variable map as an attribute), and to clear the console with each step in variable mapping.
#' @details Variable mapping is conducted interactively via the console for all variable roles in `roles` and `opt.roles` that do not have matching variable names in `x`. For variable roles in `roles` and `opt.roles` that have matching variable names in `x`, matching is done automatically.
#' @param x A \link[ppp:rct_atm_dtf]{rectangular atomic data.frame} whose variables are to be mapped.
#' @param roles \link[ppp::unq_str_mvc]{Unique string multivec} containing labels for *required* variable roles (that are also valid object names without back-quoting).
#' @param defs \link[ppp::unq_str_mvc]{Unique string multivec} with `length(defs) == length(roles)` containing definitions/descriptions associated with corresponding values of `roles`.
#' @param opt.roles Optional \link[ppp::unq_str_mvc]{unique string multivec} containing labels for *optional* variable roles (that are also valid object names without back-quoting).
#' @param opt.defs Optional \link[ppp::unq_str_mvc]{unique string multivec} with `length(opt.defs) == length(opt.roles)` containing definitions/descriptions associated with corresponding values of `opt.roles`.
#' @param trim,rename,clear Logical scalar indicating, respectively, whether to trim un-mapped variables from `x`, whether to rename mapped variables using the associated values of `roles` and `opt.roles`, and whether to clear the console with each stop in the variable mapping process.
#' @return A \link[ppp:rct_atm_dtf]{rectangular atomic data.frame}.
#' @examples
#' dg <- 1:9
#' ai <- letters[dg]
#' AI <- LETTERS[dg]
#' uf <- factor(ai)
#' of <- ordered(AI)
#' tf <- ai %in% c("a", "e", "i", "o", "u")
#'
#' dtf <- data.frame(dg = dg, ai = ai, AI = AI, uf = uf, of = of, tf = tf, stringsAsFactors = F)
#'
#' roles <- c("dg", "ai", "AI")
#' ROLES <- c("uf", "of", "tf")
#'
#' defs  <- c("digits 1 through 9", "lowercase a-i" , "uppercase A-I"         )
#' DEFS  <- c("unordered factor"  , "ordered factor", "sampled logical values")
#'
#' map_data(dtf, roles, defs)
#' map_data(dtf, roles, defs, trim = F)
#' map_data(dtf, roles, defs, opt.roles = ROLES, opt.defs = DEFS)
#' @export
map_data <- function(x, roles, defs, opt.roles = NULL, opt.defs = NULL, trim = TRUE, rename = TRUE, clear = TRUE) {
  errs <- NULL
  if (!ppp::.atm_rct_dtf(x     )) {errs <- base::c(errs, "[x] must be a rectangular atomic data.frame (?atm_rct_dtf).")}
  if (!ppp::.unq_str_vec(roles )) {errs <- base::c(errs, "[roles] must be a unique string vec (?unq_str_vec).")}
  if (!ppp::.unq_str_vec(defs  )) {errs <- base::c(errs, "[defs] must be a unique string vec (?unq_str_vec).")}
  if (!ppp::.cmp_lgl_scl(trim  )) {errs <- base::c(errs, "[trim] must be TRUE or FALSE.")}
  if (!ppp::.cmp_lgl_scl(rename)) {errs <- base::c(errs, "[rename] must be TRUE or FALSE.")}
  if (!ppp::.cmp_lgl_scl(clear )) {errs <- base::c(errs, "[clear] must be TRUE or FALSE.")}
  if (!ppp::nll_or(opt.roles, "unq_str_vec")) {errs <- base::c(errs, "[opt.roles] must be NULL or a unique string vec (?unq_str_vec).")}
  if (!ppp::nll_or(opt.defs , "unq_str_vec")) {errs <- base::c(errs, "[opt.defs] must be NULL or a unique string vec (?unq_str_vec).")}
  if (!base::is.null(errs)) {ppp::stopperr(errs, pkg = "rwd")}
  available <- uj::cnames(x)
  if (!ppp::unq_str_vec(available        )) {errs <- base::c(errs, "Names of variables in [x] must be unique and non-blank.")}
  if (uj::N(roles    ) >  uj::N(available)) {errs <- base::c(errs, "length(roles) > ncol(x).")}
  if (uj::N(roles    ) != uj::N(defs     )) {errs <- base::c(errs, "length(defs) != length(roles).")}
  if (uj::N(opt.roles) != uj::N(opt.defs )) {errs <- base::c(errs, "length(opt.defs) != length(opt.roles).")}
  if (!base::is.null(errs)) {ppp::stopperr(errs, pkg = "rwd")}
  map <- uj::tb(role = roles, name = NA)
  for (i in 1:uj::N(roles)) {
    role <- roles[i]
    if (uj::not_IN(role, available)) {
      def <- defs[i]
      name <- dlg::choose1(available, "What variable of [x] contains ", def, " x?", clear = clear)
      available <- available[available != name]
    } else {name <- role}
    map$name[i] <- name
  }
  if (uj::N(available) > 0 & uj::N(opt.roles) > 0) {for (i in 1:uj::N(opt.roles)) {
    opt.role <- opt.roles[i]
    if  (uj::not_IN(opt.role, available)) {
      opt.def <- opt.defs[i]
      if (dlg::YES("Does this dataset contain a variable filling the role ", opt.role, " >> ", opt.def, "?", clear = clear)) {
        opt.name <- dlg::choose1(available, "What variable of [x] contains ", opt.def, " x?", clear = clear)
        available <- available[available != opt.name]
      } else {opt.name <- NULL}
    } else {opt.name <- opt.role}
    if (uj::DEF(opt.name)) {map <- base::rbind(map, uj::tb(role = opt.role, name = opt.name))}
  }}
  if (trim) {x <- x[ , map$name]}
  if (rename) {
    names <- uj::cnames(x)
    for (i in 1:uj::nr(map)) {
      role <- map$role[i]
      name <- map$name[i]
      if (uj::is_IN(name, names)) {names[names == name] <- role}
    }
    x <- uj::name_cols(x, names)
  } else {x <- uj::set_att(x, map = map)}
  x
}
