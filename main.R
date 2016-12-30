
Rshield.path <- dirname(rstudioapi::getActiveDocumentContext()$path)
Rshield.source <- function( script_name ) source( paste0(Rshield.path, "/", script_name) )

Rshield.source("main/utils.R")
Rshield.source("main/r-shield.R")

# If you do not believe, you can run the unitary tests
Rshield.tests <- function() {
  Rshield.source("test/all.R")
  Rshield.tests.all()
}

