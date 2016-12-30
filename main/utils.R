
bang <- function(...) {
  str <- paste0("R-Shield::Detected exception => ", ...)
  stop(str, call. = FALSE)
}

bangWhen <- function( boolean, ... ) if (boolean) bang(...)

println <- function(...) cat(paste0(...,"\n"))

Rshield.require <- function( package_name ) {
  requirePackage <- function() eval(parse(text=paste0("require(\"", package_name, "\")")))
  if (!requirePackage()) {
    install.packages(package_name)
    bangWhen(!requirePackage(), "Sorry, but I am not able to install the required package ", package_name)
  }
}