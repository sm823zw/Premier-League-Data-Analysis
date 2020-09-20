allTimeClustering <- function(cumstats){
  x1 <- subset(cumstats, season=="2019-20" & as.numeric(HGP)>100)
  x <- x1[2:12]
  y <- x[2]
  y <- cbind(y, x[3:6])
  y <- cbind(y, x[8:11])
  f <- y[c("HGGS", "HGGC", "HGW")]
  k <- kmeans(f, centers = 6, iter.max = 20)
  x1$cluster <- k$cluster
  home <- ggplot(x1) + geom_point(aes(HGGS, HGGC)) + geom_text(aes(HGGS, HGGC, label=team), color=x1$cluster, size=3, vjust=-0.5)
  home
}