#' @title Detects the Presence of Outlier(s) in all the Columns of Data
#'
#' @description This package helps in detecting the presence of outliers in data. It returns a matrix of column names and whether the outlier was detected in it or not.
#'
#' @param data
#'
#' @return matrix of column names and status of outliers
#'
#' @examples outlier_detect(Boston)
#'
#' @export outlier_detect


outlier_detect = function(data){
  outlier = function(x){
    flag = 0
    q3 = quantile(x,0.75,na.rm = TRUE)
    q1 = quantile(x,0.25,na.rm = TRUE)
    upper_limit = q3 + IQR(x,na.rm = TRUE) * 1.5
    lower_limit = q1 - IQR(x,na.rm = TRUE) * 1.5
    x = na.omit(x)
    if (sum(x>upper_limit) | sum(x<lower_limit)){
      return('Outlier Detected')
    }
    else{
      return('-')
    }

  }
  outliers = apply(data,2,outlier)
  outliers = noquote(as.matrix(outliers))
  colnames(outliers) = 'Outlier Status'
  return(outliers)
}
