
bang <- function(...) {
  str <- paste0("R-Shield::Detected exception => ", ...)
  stop(str, call. = FALSE)
}

println <- function(...) cat(paste0(...,"\n"))
