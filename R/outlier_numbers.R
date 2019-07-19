#' @title Counts the No. of Outliers Before and After Log Transformation of Variables.
#'
#' @description This package accepts a data set as an argument and returns the outlier counts for each variables in the data set.
#'
#' @param data_frame
#'
#' @return matrix with column names and no. of outliers.
#'
#' @examples outlier_numbers(Boston)
#'
#' @export outlier_numbers



outlier_numbers = function(data){
  outlier = function(x){
    if(is.numeric(x)){
      x_log = log(x)
      q3 = quantile(x,0.75,na.rm = TRUE)
      q1 = quantile(x,0.25,na.rm = TRUE)
      q3_log = quantile(x_log,0.75,na.rm = TRUE)
      q1_log = quantile(x_log,0.25,na.rm = TRUE)
      upper_limit = q3 + IQR(x,na.rm = TRUE) * 1.5
      lower_limit = q1 - IQR(x,na.rm = TRUE) * 1.5
      upper_limit_log = q3_log + IQR(x_log,na.rm = TRUE) * 1.5
      lower_limit_log = q1_log - IQR(x_log,na.rm = TRUE) * 1.5

      x = na.omit(x)
      u = sum(x > upper_limit)
      l = sum(x < lower_limit)

      u_log = sum(x_log > upper_limit_log)
      l_log = sum(x_log < lower_limit_log)

      s = c(u,l,u_log,l_log)
      return(s)
    }
    else{
      s = c('-','-','-','-')
      return(s)
    }

  }
  out = noquote(sapply(data,outlier))
  out = t(out)
  colnames(out) = c('Upper_Count', 'Lower_Count', 'Upper_Count_After_Log',
                    'Lower_Count_After_Log' )
  return(out)
}
