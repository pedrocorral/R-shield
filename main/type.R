
Rshield.typeOf <- function(x) suppressWarnings( {
    bangHere <- function() bang("This type is unkwnown => ", x)

    if (Rshield.isDataTable(x)) data.table
    else if (is.data.frame(x)) data.frame
    else if (is.list(x)) list
    else if (is.null(x)) NULL
    else if (is.na(x)) NA
    else if (length(x) == 1)
        if (is.factor(x)) factor
        else if (is.numeric(x)) numeric
        else if (is.function(x)) Rshield.type.function
        else if (is.logical(x)) logical
        else if (Rshield.isDate(x)) Date
        else bangHere()
    else if (is.logical(x)) logical
    else if (is.vector(x)) vector
    else if (is.factor(x)) vector
    else if (Rshield.isDate(x)) vector
    else if (Rshield.isHash(x)) hash
    else if (is.ts(x)) ts
    else bangHere()
} )


Rshield.type.function <- as.symbol("Rshield.type.function")
Rshield.type.voidSymbol <- function() formals(function(x) {})$x

Rshield.isDataTable <- function(x) if (exists("is.data.table")) is.data.table(x) else FALSE
Rshield.isHash <- function(x) if (exists("is.hash")) is.hash(x) else FALSE
Rshield.isDate <- function(x) if (exists("is.Date")) is.Date(x) else FALSE
