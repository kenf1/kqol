#' Out of sight, out of mind (col version)
#'
#' Removes col(s) that are equal to/exceeds col missingness threshold.
#'
#' @param dfName Input DF.
#' @param comparisonSign Options are ">" and ">=".
#' @param threshold Col missingness threshold.
#'
#' @return DF where columns with threshold NA are removed.
#' @export
#'
#' @examples
#' #All columns with > 0.1 (10% missingness) will be removed.
#' rmSomeNA_col(df,">",0.1)

rmSomeNA_col <- function(dfName,comparisonSign,threshold){
  threshold <- 1-as.numeric(threshold)
  if(comparisonSign==">"){
    dfName[,which(colMeans(!is.na(dfName)) > threshold)]
  }else if(comparisonSign==">="){
    dfName[,which(colMeans(!is.na(dfName)) >= threshold)]
  }else{
    stop("comparisonSign argument is incorrect. Please try again.")
  }
}
