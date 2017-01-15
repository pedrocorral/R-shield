
Rshield.mixMessages <- function(custom.message, ...) if (!is.null(custom.message)) custom.message else paste0(...)


mustBeType <- function(t, x, custom.message = NULL) {
  if (identical(Rshield.typeOf(x), x)) x
  else bang(Rshield.mixMessages(custom.message, "mustBeType: [", x, "] is NOT a ", Rshield.typeAsString(t), " (as it must) but a ", Rshield.typeAsString(Rshield.typeOf(x))))
}

mustBeTypeOrNA <- function(t, x, custom.message = NULL) {
  if (identical(Rshield.typeOf(x), x)) x
  else if (is.na(x)) x
  else bang(Rshield.mixMessages(custom.message, "mustBeType: [", x, "] is NEITHER a ", Rshield.typeAsString(t), " NOR a NA (as it must), but a ", Rshield.typeAsString(Rshield.typeOf(x))))
}

mustBeCharacter <- function(str, custom.message = NULL) {
  if (!is.character(str)) bang(Rshield.mixMessages(custom.message, str, " is not character (as it must)"))
  else str
}

mustBeString <- function(str, custom.message = NULL) {
  if (!is.character(str)) bang(Rshield.mixMessages(custom.message, str, " is not a string (as it must)"))
  else str
}

mustBeNonEmptyCharacter <- function(str, custom.message = NULL) {
  if (length(mustBeCharacter(str)) == 0) bang(Rshield.mixMessages(custom.message, "We have an empty character (but it must not)"))
  else str
}


mustBeNumeric <- function(n, custom.message = NULL) {
  if (!is.numeric(n) || is.nan(n)) bang(Rshield.mixMessages(custom.message, n, " is not numeric (as it must)"))
  else n
}

mustBeNumericOrNA <- function(n, custom.message = NULL) {
  type <- Rshield.typeOf(n)
  if (!identical(type,numeric) && !identical(type,NA)) bang(Rshield.mixMessages(custom.message, n, " is NEITHER an integer NOR a NA (as it must be)"))
  else n
}

mustBeNumericBetween <- function(n, xmin, xmax, custom.message = NULL) {
  mustBeNumeric(n, custom.message)
  if (x < xmin || x > xmax) bang(Rshield.mixMessages(custom.message, n, " is a numeric not in the range [", xmin, ", ", xmax, "] (as it must)"))
  else x
}

mustBeNumericOrNABetween <- function(n, xmin, xmax, custom.message = NULL) {
  if (is.na(n)) n
  else mustBeNumericBetween(n, xmin, xmax, custom.message)  
}

mustBeBinary <- function(b, custom.message = NULL) {
  mustBeNumeric(b, custom.message)
  if (b != 1 && b != 0) bang(Rshield.mixMessages(custom.message, n, " is a non binary numeric (as it must)"))
  else b
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

mustBeFunction <- function(f, custom.message = NULL) {
  if (!is.function(f)) bang(Rshield.mixMessages(custom.message, f, " is not a function (as it must)"))
  else f
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

mustBeDataTable <- function(M, custom.message = NULL) {
  if (!Rshield.isDataTable(M)) bang(Rshield.mixMessages(custom.message, M, " is not a data.table (as it must)"))
  else M
}

mustBeHash <- function(H, custom.message = NULL) {
  if (!Rshield.isHash(H)) bang(Rshield.mixMessages(custom.message, H, " is not a hash (as it must)"))
  else H
}

mustBeDate <- function(maybe_date, custom.message = NULL) {
  if (!Rshield.isDate(maybe_date)) bang(Rshield.mixMessages(custom.message, maybe_date, " is not a date (as it must)"))
  else maybe_date
}



typeSafe <- function(f) {
  mustBeFunction(f, custom.message = paste0("The provided argument is not a function (as it must)"))

  getVoidSymbol <- function() formals(function(x) {})$x
  args <- formals(f)
  args_names <- names(args)
  new_args <- lapply(args_names, function(arg_name) {
    arg_value <- args[[arg_name]]
    getVoidSymbol()
  } )
  names(new_args) <- args_names
  
  g <- f
  formals(g) <- new_args

  checkFunction <- function(s) {
    if (s == as.symbol("numeric") || s == as.symbol("mustBeNumeric")) as.symbol("mustBeNumeric")
    else if (s == as.symbol("character") || s == as.symbol("mustBeCharacter") || s == as.symbol("mustBeString")) as.symbol("mustBeCharacter")
    else if (s == as.symbol("data.frame") || s == as.symbol("mustBeDataFrame")) as.symbol("mustBeDataFrame")
    else if (s == as.symbol("data.table") || s == as.symbol("mustBeDataTable")) as.symbol("mustBeDataTable")
    else if (s == as.symbol("Rshield.type.function") || s == as.symbol("mustBeFunction")) as.symbol("mustBeFunction")
    else if (s == as.symbol("Date") || s == as.symbol("mustBeDate")) as.symbol("mustBeDate")
    else if (s == as.symbol("hash") || s == as.symbol("mustBeHash")) as.symbol("mustBeHash")
    else bang("typeSafe::The type is unknown => ", as.character(f))
  }
  
  body_f <- body(f)
  body_xs <- as.list(body_f)
  if (!identical(body_xs[[1]], as.symbol("{")))
      body_xs <- list(as.symbol("{"), body_f)
  i <- 1
  lapply(args_names, function(arg_name) {
    arg_value <- args[[arg_name]]
    body_xs <<- append(
      body_xs,
      substitute(f(x), list(f = checkFunction(arg_value), x = as.symbol(arg_name))),
      after = i
    )
    i <<- 1 + i  
  } )

  body(g) <- as.call(body_xs)

  g
}
