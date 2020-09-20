cumstats <- function(d){
  teams <- as.data.frame(table(d$HomeTeam))$Var1
  season <- as.data.frame(table(d$Season))$Var1
  df <- data.frame(team=character(0), season=character(0), HGP=numeric(0), HGGS=numeric(0), HGGC=numeric(0), HGW=numeric(0), HGD=numeric(0), AGP=numeric(0), AGGS=numeric(0), AGGC=numeric(0), AGW=numeric(0), AGD=numeric(0))
  for(t in teams){
    HGP <- 0
    HGGS <- 0
    HGGC <- 0
    HGW <- 0
    HGD <- 0
    AGP <- 0
    AGGS <- 0
    AGGC <- 0
    AGW <- 0
    AGD <- 0
    
    for(s in season){
      x <- subset(d, Season==s & HomeTeam==t)
      HGP <- HGP + length(x$HomeTeam[x$HomeTeam==t])
      HGGS <- HGGS + sum(x$FTHG)
      HGGC <- HGGC + sum(x$FTAG)
      HGW <- HGW + length(x$FTR[x$FTR=="H"])
      HGD <- HGD + length(x$FTR[x$FTR=="D"])
      
      x <- subset(d, Season==s & AwayTeam==t)
      AGP <- AGP + length(x$AwayTeam[x$AwayTeam==t])
      AGGS <- AGGS + sum(x$FTAG)
      AGGC <- AGGC + sum(x$FTHG)
      AGW <- AGW + length(x$FTR[x$FTR=="A"])
      AGD <- AGD + length(x$FTR[x$FTR=="D"])
      
      if(HGP==0 | AGP==0){
        df <- rbind(df, c(s, t, HGP, 1, 1, 0.3333, 0.3333, AGP, 1, 1, 0.3333, 0.3333))
      } else {
        df <- rbind(df, c(s, t, HGP, round(HGGS/HGP, 4), round(HGGC/HGP, 4), round(HGW/HGP, 4), round(HGD/HGP, 4), AGP, round(AGGS/AGP, 4), round(AGGC/AGP, 4), round(AGW/AGP, 4), round(AGD/AGP, 4)))
      }
    }
  }
  colnames(df) <- c("season", "team", "HGP", "HGGS", "HGGC", "HGW", "HGD", "AGP", "AGGS", "AGGC", "AGW", "AGD")
  df
}
