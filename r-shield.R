
bang <- function(...) {
  str <- paste0("R-Shield::Detected exception => ", ...)
  stop(str, call. = FALSE)
}

println <- function(...) cat(paste0(...,"\n"))

Rshield.mixMessages <- function(custom.message, ...) if (!is.null(custom.message)) custom.message else paste0(...)

mustBeNumeric <- function(n, custom.message = NULL) {
  if (!is.numeric(n)) bang(Rshield.mixMessages(custom.message, n, " is not numeric (as it must)"))
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

