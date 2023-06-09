#' Out of sight, out of mind (row version)
#'
#' Removes row(s) that are equal to/exceeds row missingness threshold.
#'
#' @param dfName Input DF.
#' @param comparisonSign Options are ">" and ">=".
#' @param threshold Row missingness threshold.
#'
#' @return DF where rows with threshold NA are removed.
#' @export
#'
#' @examples
#' #All rows with > 0.1 (10% missingness) will be removed.
#' rmSomeNA_row(df,">",0.1)

rmSomeNA_row <- function(dfName,comparisonSign,threshold){
  threshold <- 1-as.numeric(threshold)
  if(comparisonSign==">"){
    dfName[which(rowMeans(!is.na(dfName)) > threshold),]
  }else if(comparisonSign==">="){
    dfName[which(rowMeans(!is.na(dfName)) >= threshold),]
  }else{
    stop("comparisonSign argument is incorrect. Please try again.")
  }
}
