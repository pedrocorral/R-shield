
# https://cran.r-project.org/web/packages/RUnit/RUnit.pdf

Rshield.tests.all <- function() {

  Rshield.require("RUnit")

  checkEquals( "this is a string", mustBeString("this is a string") )
  checkException( mustBeString(13) )
  checkException( mustBeString(NA) )
  checkException( mustBeString(NaN) )
  checkException( mustBeString(NULL) )
  checkException( mustBeString(FALSE) )
  
  checkEqualsNumeric( 1, mustBeNumeric(1) )
  checkEqualsNumeric( 0, mustBeNumeric(0) )
  checkEqualsNumeric( -13.14, mustBeNumeric(-13.14) )
  checkException( mustBeNumeric(NA) )
  checkException( mustBeNumeric("13") )
  checkException( mustBeNumeric(NaN) )
  checkException( mustBeNumeric(NULL) )
  checkException( mustBeNumeric(FALSE) )
  
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

  M <- data.frame( a = c(1,2,3), b = c(4,5,6) )
  checkEquals( M, mustBeDataFrame(M) )
  checkEquals( data.frame(), mustBeDataFrame(data.frame()) )
  checkEquals( M, mustBeNonEmptyDataFrame(M) )
  checkException( mustBeNonEmptyDataFrame(data.frame()) )
  checkException( mustBeDataFrame("data.frame()") )
  checkException( mustBeDataFrame(13.14) )
  
  println( "\nR-Shield::All the tests passed! ;)" )
}