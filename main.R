

Rshield.path <- getSrcDirectory(function() {})  # Thanks to rakensi => https://stackoverflow.com/questions/3452086/getting-path-of-an-r-script 
Rshield.source <- function( script_name ) source( paste0(Rshield.path, "/", script_name) )

sapply( c(
  "main/utils.R",
  "main/type.R",
  "main/r-shield.R"
), Rshield.source )


# If you do not believe, you can run the unitary tests
Rshield.tests <- function() {
  Rshield.source("test/all.R")
  Rshield.tests.all()
}

