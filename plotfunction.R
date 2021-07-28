binplot <- function(df, ts, nrep, selrow){
  
  tdf2 <- df
  MVT <- selrow$MVT
  jobtitle <- selrow$Profile
  timescale <- ts
  RT <- selrow$RT
  
  layout(matrix(c(1,1,2), nrow = 3, ncol = 1, byrow = TRUE))
  
  plot(1:(timescale+1),tdf2$lb,lty=1,type='l',xlab = 'Years',ylab = 'FTE employees',main = paste0('Likely shortage scenarios for ',jobtitle), cex.main = 3, cex.axis = 2, cex.lab = 2.2)
  lines(1:(timescale+1),tdf2$ub,type='l',col='red')
  polygon(c((timescale+1):1,1:(timescale+1)),c(rev(tdf2$lb),(tdf2$ub)),col=c('lightgreen',alpha=0.5),lty=0)
  lines(1:(timescale+1),tdf2$ub,type='l',lwd=1.5)
  lines(1:(timescale+1),tdf2$lb,type='l',lwd=1.5)
  abline(h=MVT, lwd=2, col="bisque4")
  lines(1:(timescale+1),tdf2$naive,lty=1,col='coral3',lwd=2.1)
  
  plot(1:(timescale+1),tdf2$probcross,ylab='Probability of deficit',xlab='Years', type = 'h', lwd= 5,main = "Probability of shortage over years", col = "coral1",ylim=c(0,1), cex.main = 3, cex.axis = 2, cex.lab = 2.2)
  abline(h=RT, lwd=2, col="bisque4")
}



whatifdf <- function(selectedrow, retiremat, attrlow, attrhigh, timescale = 8, nruns = 10000){
  
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
