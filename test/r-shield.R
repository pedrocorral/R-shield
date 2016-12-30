
# https://cran.r-project.org/web/packages/RUnit/RUnit.pdf
require("RUnit")

Rshield.tests <- function() {
  
  checkEqualsNumeric( 1, mustBeNumeric(1) )
  checkEqualsNumeric( 0, mustBeNumeric(0) )
  checkEqualsNumeric( -13.14, mustBeNumeric(-13.14) )
  checkException( mustBeNumeric(NA) )
  checkException( mustBeNumeric("13") )
  
  checkEqualsNumeric( 3.14, mustBePositive(3.14) )
  checkException( 0, mustBePositive(0) )
  checkException( -130, mustBePositive(-130) )

}