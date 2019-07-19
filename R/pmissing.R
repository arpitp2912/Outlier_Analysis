#' @title Calculates Percentage of Missing Values
#'
#' @description This package calculates and returns a matrix of columns of a data frame and the percentage of missing values in it.
#'
#' @param data
#'
#' @return matrix of column names and the percentage of missing values
#'
#' @examples pmissing(Boston)
#'
#' @export pmissing

pmissing <- function(data)
{
  #creating a local function
  pmiss <- function(x)
  {
    return(round(sum(is.na(x))/length(x)*100,2))
  }

  miss <- apply(data, 2, pmiss)
  miss <- as.matrix(miss)
  colnames(miss) <- "% Missing"

  return(miss)
}
