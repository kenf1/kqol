#' Extract Text from QR Code
#'
#' Uses [Quadrangle](https://github.com/brianwdavis/quadrangle) to read and extract text from qr code.
#'
#' @param imagePath Path of image to extract text from.
#' @param loadLibrary Will load required libraries (quadrangle & ggplot2) if set to `TRUE`. Default is `FALSE`.
#'
#' @return Extracted text in console.
#' @export
#'
#' @examples

qr_text <- function(imagePath,loadLibrary=FALSE){
  if(loadLibrary==TRUE){
    library(quadrangle)
    library(ggplot2)
  }
  img <- imagePath
  scan_result <- quadrangle::qr_scan(img)
  return(scan_result$values)
}
