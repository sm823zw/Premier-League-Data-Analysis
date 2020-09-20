teamSeasonClustering <- function(stats){
  
  x <- data.frame(season=character(0), team=character(0), GP=numeric(0), HGGS=numeric(0), HGGC=numeric(0), HGW=numeric(0), HGD=numeric(0), AGGS=numeric(0), AGGC=numeric(0), AGW=numeric(0), AGD=numeric(0))
  x <- rbind(x, stats)
  x$TeamSeason <- paste(x$team, x$season)
  x$GP <- as.numeric(x$GP)
  #x <- subset(x, GP!=0 & season %in% c("2017-18", "2018-19", "2019-20"))
  x <- subset(x, GP==38 & as.numeric(AGGS)>30 & as.numeric(HGGC)<15)
  
  x$HGGS <- round(as.numeric(x$HGGS)/x$GP, 3)
  x$HGGC <- round(as.numeric(x$HGGC)/x$GP, 3)
  x$HGGS <- round(as.numeric(x$HGGS)/x$GP, 3)
  x$HGW <- round(as.numeric(x$HGW)/x$GP, 3)
  x$HGD <- round(as.numeric(x$HGD)/x$GP, 3)
  x$AGGS <- round(as.numeric(x$AGGS)/x$GP, 3)
  x$AGGC <- round(as.numeric(x$AGGC)/x$GP, 3)
  x$AGW <- round(as.numeric(x$AGW)/x$GP, 3)
  x$AGD <- round(as.numeric(x$AGD)/x$GP, 3)
  
  y <- x[4:11]
  
  #y <- x[c("AGGS", "AGGC")]
  
  k <- kmeans(y, centers=5, iter.max = 25)
  x$cluster <- k$cluster
  
  ggplot(x) + geom_point(aes(AGGS, HGGC)) + geom_text_repel(aes(AGGS, HGGC, label=TeamSeason), color=x$cluster, size=3, vjust=-0.5)
}