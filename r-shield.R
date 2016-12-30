
Rshield.mixMessages <- function(custom.message, ...) if (!is.null(custom.message)) custom.message else paste0(...)


mustBeCharacter <- function(str, custom.message = NULL) {
  if (!is.character(str)) bang(Rshield.mixMessages(custom.message, str, " is not character (as it must)"))
  else str
}

mustBeString <- function(str, custom.message = NULL) {
  if (!is.character(str)) bang(Rshield.mixMessages(custom.message, str, " is not a string (as it must)"))
  else str
}


mustBeNumeric <- function(n, custom.message = NULL) {
  if (!is.numeric(n) || is.nan(n)) bang(Rshield.mixMessages(custom.message, n, " is not numeric (as it must)"))
  else n
}

mustBeInteger <- function(n, custom.message = NULL) {
  mustBeNumeric(n, Rshield.mixMessages(custom.message, n, " is not numeric, so it cannot be an integer"))
  if (round(n)!=n) bang(Rshield.mixMessages(custom.message, n, " is not an integer (as it must be)"))
  else n
}

mustBePositive <- function(x, custom.message = NULL) {
  mustBeNumeric(x, Rshield.mixMessages(custom.message, x, " is not numeric, so it cannot be positive"))
  if (x <= 0) bang( Rshield.mixMessages(custom.message, x, " is not positive (as it must be)") )
  else x
}

mustBeNatural <- function(n, custom.message = NULL) {
  msg <- paste0(n, " is not integer, so it cannot be a natural number")
  mustBeInteger(n, Rshield.mixMessages(custom.message, msg))
  mustBePositive(n, Rshield.mixMessages(custom.message, msg))
}



mustBeDataFrame <- function(M, custom.message = NULL) {
  if (!is.data.frame(M)) bang(Rshield.mixMessages(custom.message, M, " is not a data.frame (as it must)"))
  else M
}

mustBeNonEmptyDataFrame <- function(M, custom.message = NULL) {
  mustBeDataFrame(M, custom.message)
  if (nrow(M) == 0) bang(Rshield.mixMessages(custom.message, M, " is data.frame, but empty (no rows at all)"))
  else M
}