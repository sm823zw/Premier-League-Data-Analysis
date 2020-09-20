cumh2h <- function(d, h2h){
  teams <- as.data.frame(table(d$HomeTeam))$Var1
  
  df <- data.frame(season=character(0), team1=character(0), team2=character(0), HGH2H=numeric(0), AGH2H=numeric(0), ft=character(0), H=numeric(0), A=numeric(0), D=numeric(0))
  for(t1 in teams){
    for(t2 in teams){
      g <- subset(h2h, d.HomeTeam==t1 & d.AwayTeam==t2)
      g$home <- cumsum(g$home)
      g$away <- cumsum(g$away)
      g$draw <- cumsum(g$draw)
      g$d.FTHG <- cumsum(g$d.FTHG)
      g$d.FTAG <- cumsum(g$d.FTAG)
      len <- dim(g)[1]
      if(len == 0){
      } else if (len == 1){
        g$home <- 0.3333
        g$away <- 0.3333
        g$draw <- 0.3333
        g$d.FTHG <- 1
        g$d.FTAG <- 1
      } else {
        g$home <- c(0.3333, g$home[1:len-1])
        g$away <- c(0.3333, g$away[1:len-1])
        g$draw <- c(0.3333, g$draw[1:len-1])
        g$d.FTHG <- c(1, g$d.FTHG[1:len-1])
        g$d.FTAG <- c(1, g$d.FTAG[1:len-1])
      }
      #if(len!=1){
      #  g <- g[1:len-1, ] 
      #}
      df <- rbind(df, g)
    }
  }
  
  colnames(df) <- c("season", "HomeTeam", "AwayTeam", "HGH2H", "AGH2H", "FTR", "H", "A", "D")
  df$T <- df$H + df$A + df$D
  df$H <- round(df$H/df$T, 4)
  df$A <- round(df$A/df$T, 4)
  df$D <- round(df$D/df$T, 4)
  df$HGH2H <- round(df$HGH2H/df$T, 4)
  df$AGH2H <- round(df$AGH2H/df$T, 4)
  df <- df %>% arrange(HomeTeam, AwayTeam, season)
  df
}