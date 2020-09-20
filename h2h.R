h2h <- function(d){
  h2hdata <- data.frame(d$Season, d$HomeTeam, d$AwayTeam, d$FTHG, d$FTAG, d$FTR)
  h2hdata$home <- rep(0, 10424)
  h2hdata$away <- rep(0, 10424)
  h2hdata$draw <- rep(0, 10424)
  
  h2hdata <- within(h2hdata, h2hdata$home[d.FTR=="H"] <- 1)
  h2hdata <- within(h2hdata, h2hdata$away[d.FTR=="A"] <- 1)
  h2hdata <- within(h2hdata, h2hdata$draw[d.FTR=="D"] <- 1)
  
  #colnames(h2hdata) <- c("season", "HomeTeam", "AwayTeam", "FTR", "v", "v", "v", "v", "v", "v", "v", "Home", "Away", "Draw")
  a <- h2hdata$h2hdata
  a
}