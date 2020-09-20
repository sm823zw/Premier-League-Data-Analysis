stats <- function(d){
  teams <- as.data.frame(table(d$HomeTeam))$Var1
  season <- as.data.frame(table(d$Season))$Var1
  df <- data.frame(team=character(0), season=character(0), GP=numeric(0), HGGS=numeric(0), HGGC=numeric(0), HGW=numeric(0), HGD=numeric(0), AGGS=numeric(0), AGGC=numeric(0), AGW=numeric(0), AGD=numeric(0))
  for(t in teams){
    
    for(s in season){
      x <- subset(d, Season==s & HomeTeam==t)
      GP <- length(x$HomeTeam[x$HomeTeam==t])
      HGGS <- sum(x$FTHG)
      HGGC <- sum(x$FTAG)
      HGW <- length(x$FTR[x$FTR=="H"])
      HGD <- length(x$FTR[x$FTR=="D"])
      
      x <- subset(d, Season==s & AwayTeam==t)
      GP <- GP + length(x$AwayTeam[x$AwayTeam==t])
      AGGS <- sum(x$FTAG)
      AGGC <- sum(x$FTHG)
      AGW <- length(x$FTR[x$FTR=="A"])
      AGD <- length(x$FTR[x$FTR=="D"])
      
      
      df <- rbind(df, c(s, t, GP, HGGS, HGGC, HGW, HGD, AGGS, AGGC, AGW, AGD))
    }
  }
  colnames(df) <- c("season", "team", "GP", "HGGS", "HGGC", "HGW", "HGD", "AGGS", "AGGC", "AGW", "AGD")
  df
}