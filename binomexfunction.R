library(ggplot2)

binomexampleOSOC <- function(timescale = 8,
                             MVT = 10,
                             nruns = 1000,
                             lb = 0.05,
                             ub = 0.95,
                             attrP = 0.05,
                             nfte = 15,
                             jobtitle = 'QA engineers'){
  
  df <- data.frame('T0' = rep(nfte,nruns))
  df2 <- data.frame(row.names = c('ub','lb','probcross','naive'))
  
  df2$T0 <- c(nfte,nfte,0,nfte)
  for (t in 1:timescale){
    col <- paste0('T',t)
    df[[col]] = NA
    df[,t+1] = df[,t] - rbinom(nruns,df[,t],attrP)
    colval <- c(quantile(df[[col]],c(ub,lb)),
                sum(df[[col]] < MVT)/nruns,
                nfte*((1-attrP)^t))
    df2[[col]] = colval
  }
  
  
  tdf2 <- data.frame(t(df2))
  
  
  layout(matrix(c(1,1,2), nrow = 3, ncol = 1, byrow = TRUE))
  
  plot(1:(timescale+1),tdf2$lb,lty=1,type='l',xlab = 'Years',ylab = 'FTE employees',main = paste0('Likely shortage scenarios for ',jobtitle), cex.main = 3, cex.axis = 2, cex.lab = 2.2)
  lines(1:(timescale+1),tdf2$ub,type='l',col='red')
  polygon(c((timescale+1):1,1:(timescale+1)),c(rev(tdf2$lb),(tdf2$ub)),col=c('lightgreen',alpha=0.5),lty=0)
  lines(1:(timescale+1),tdf2$ub,type='l',lwd=1.5)
  lines(1:(timescale+1),tdf2$lb,type='l',lwd=1.5)
  abline(h=MVT,col='red')
  abline(h=MVT, lwd=2, col="bisque4")
  lines(1:(timescale+1),tdf2$naive,lty=1,col='coral3',lwd=2.1)
  
  plot(1:(timescale+1),tdf2$probcross,ylab='Probability of deficit',xlab='Years', type = 'h', lwd= 5,main = "Probability of shortage over years", col = "coral1",ylim=c(0,0.5), cex.main = 3, cex.axis = 2, cex.lab = 2.2)
  }

