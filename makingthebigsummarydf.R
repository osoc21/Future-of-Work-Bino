#selectedrow <- infomat[1,]
attrlow <- 0.04
attrhigh <- 0.11
timescale <- 8

whatifdf(selectedrow = selectedrow, attrlow = 0.02, attrhigh = 0.09)



whatifdf <- function(selectedrow, attrlow, attrhigh, timescale = 8, nruns = 10000){
  
  profile = selectedrow$Profile
  nfte <- selectedrow$FTE
  MVT <- selectedrow$MVT
  
  df <- data.frame('Y0' = rep(nfte,nruns))
  dfreturn <- data.frame(matrix(ncol=timescale,nrow=0, dimnames=list(NULL, c(paste0('Y',c(1:timescale))))))
  
  for (attrP in seq(attrlow,attrhigh,0.01)){
    df2 <- data.frame(row.names = paste0(attrP*100,'%'))
    for(y in 1:timescale){
      col <- paste0('Y',y)
      df[[col]] = NA
      df[df[,y]-retiremat[retiremat$Profile==profile,col] < 1,y+1] = 0
      df[df[,y]-retiremat[retiremat$Profile==profile,col] >= 1,y+1] = df[df[,y]-retiremat[retiremat$Profile==profile,col] >= 1,y] - as.numeric(retiremat[retiremat$Profile==profile,col])  -  rbinom(length(df[df[,y]-retiremat[retiremat$Profile==profile,col] >= 1,y]),as.numeric(df[df[,y]-retiremat[retiremat$Profile==profile,col] >= 1,y] - retiremat[retiremat$Profile==profile,col]),attrP)
      colval <- sum(df[[col]] < MVT)/nruns
      df2[[col]] = colval
    }
    dfreturn <-rbind(dfreturn,df2)
  }
  
  dfreturn <- round(dfreturn, digits = 2)
  return(dfreturn)

}
