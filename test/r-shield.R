
# https://cran.r-project.org/web/packages/RUnit/RUnit.pdf
require("RUnit")

Rshield.tests <- function() {
  
  checkEqualsNumeric( 1, mustBeNumeric(1) )
  checkEqualsNumeric( 0, mustBeNumeric(0) )
  checkEqualsNumeric( -13.14, mustBeNumeric(-13.14) )
  checkException( mustBeNumeric(NA) )
  checkException( mustBeNumeric("13") )
  
  checkEqualsNumeric( 3.14, mustBePositive(3.14) )
  checkException( mustBePositive(0) )
  checkException( mustBePositive(-130) )

  checkEqualsNumeric( 314, mustBeInteger(314) )
  checkEqualsNumeric( 0, mustBeInteger(0) )
  checkEqualsNumeric( -10, mustBeInteger(-10) )
  checkException( mustBeInteger(3.14) )

  checkEqualsNumeric( 314, mustBeNatural(314) )
  checkException( mustBeNatural(0) )
  checkException( mustBeNatural(-10) )
  checkException( mustBeNatural(3.14) )

}