#' Compute surprise index
#' 
#' 
#' @param datain numeric vector (possibly raw, standardize or normalized)
#' @param win numeric, width of the sliding window
#' @return numeric vector of surprise indices (standardize departures)
surprise <- function(datain, win = 30)
{
  nx = length(x)
  x <- seq_len(nx)
  dataout <- matrix(data=NA,nrow=length(datain),ncol=1)
  for (i in (win+1):length(datain))
  {
    IDX <- (i-win):(i-1)
    df <- data.frame(x = x[IDX], y = datain[IDX])
    fit <- lm(y ~ x, data=df)
    #dev <- datain[i] - (fit$coefficients[1] + fit$coefficients[2]*datain[i])
    dev <- datain[i] - predict(fit,newdata = data.frame(x=i))
    dataout[i] <- dev / sd(fit$residuals)
  }
  return(dataout)
}
