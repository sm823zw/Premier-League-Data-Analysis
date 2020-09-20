h2hAllTime <- function(d){
  
  teams <- as.data.frame(table(d$HomeTeam))$Var1
  season <- as.data.frame(table(d$Season))$Var1
  
  df <- data.frame(team1=character(0), team2=character(0), H=numeric(0), D=numeric(0), A=numeric(0), HH2HG=numeric(0), AH2HG=numeric(0))
  
  
  for(t1 in teams){
    for(t2 in teams){
      if(t1 != t2){
        m1 <- subset(d, HomeTeam==t1 & AwayTeam==t2)
        
        H <- length(m1$FTR[m1$FTR=="H"])
        D <- length(m1$FTR[m1$FTR=="D"])
        A <- length(m1$FTR[m1$FTR=="A"])
        total <- H + D + A
        HG <- sum(m1$FTHG)
        AG <- sum(m1$FTAG)
        if(total == 0){
          df <- rbind(df, c(t1, t2, 0.3333, 0.3333, 0.3333, 1, 1))
        } else {
          df <- rbind(df, c(t1, t2, round(H/total, 4), round(D/total, 4), round(A/total, 4), round(HG/total, 4), round(AG/total, 4)))
        }
      }
    }
  }
  
  colnames(df) <- c("Team1", "Team2", "Home", "Draw", "Away", "HH2HG", "AH2HG")
  df
}