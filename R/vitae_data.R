#' vitae_data: Vitae Data Template
#'
#' Creates dataframe to use in tandem with `Vitae Rmd` Rmarkdown template to semi-automate the resme and/or CV creation process.
#'
#' @return Dataframe template for user to export. The user can decide which package to use for exporting to `.xlsx` format.
#'
#' @export
#'
#' @examples

vitae_data <- function(){
  col_names <- c("what","when","with","where","why")
  temp <- data.frame(ColName=col_names,Entry=NA) %>%
    tidyr::pivot_wider(names_from=ColName,values_from=Entry)
  return(temp)
}
