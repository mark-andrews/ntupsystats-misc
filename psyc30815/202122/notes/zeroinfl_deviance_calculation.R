library(pscl)

M1 <- zeroinfl(art ~ fem + mar, data = bioChemists)
M2 <- zeroinfl(art ~ fem + mar + phd, data = bioChemists)

# to be used for nested model comparison with zeroinfl models
zeroinfl_deviance_comparison <- function(m1, m2){
  deviance1 <- as.numeric(-2 * logLik(m1))
  deviance2 <- as.numeric(-2 * logLik(m2))
  df1 <- attr(logLik(m1), 'df')
  df2 <- attr(logLik(m2), 'df')
  
  result <- c(deviance_difference = deviance1 - deviance2, 
              df = abs(df1 - df2), 
              pvalue = 1 - pchisq(abs(deviance1 - deviance2), df = abs(df1 - df2)))
  
  result
}

# e.g.
zeroinfl_deviance_comparison(M1, M2)
