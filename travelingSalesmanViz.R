#### Traveling Salesman Visualization #####

library(ggplot2)
library(gtools)
library(TSP)
library(gridExtra)

#make random table of x,y coordinates
etsp <- ETSP(data.frame(x = runif(7), y = runif(7)))

#solve traveling salesman problem
tour<-solve_TSP(etsp, start = 1)

#create data frame
etsp_df <- data.frame(x = etsp[,1], y = etsp[,2])

# Rearrange the data frame based on the order specified by tour
tour_order <- tour
etsp_df <- etsp_df[tour_order, ]
#calculate the distance of the tour and round to two decimals
dist1<-round(tour_length(tour), digits=2)

#plot shortest path
p1<-ggplot() +
  theme_classic()+
  #geom_point(data = etsp_df, aes(x, y), color = "blue", size = 3) +
  geom_path(data = etsp_df, aes(x, y), color = "darkgrey", linetype = "dotted") +
  geom_text(data = etsp_df, aes(x, y, label = tour_order), hjust=0, vjust = 0) +
  labs(title = "Shortest Path", subtitle = paste("Distance = ", dist1, sep=""))+
  xlab("")+
  ylab("")

#plot actual path
etsp_df2 <- data.frame(x = etsp[,1], y = etsp[,2])
#rearrange the path in sequential order
tour_order2 <- seq(1:7)
etsp_df2 <- etsp_df2[tour_order2, ]
#calculate the distance
dist2<-round(sum(dist(etsp_df2)[1:6]), digits=2)

p2<-ggplot() +
  theme_classic()+
  #geom_point(data = etsp_df, aes(x, y), color = "blue", size = 3) +
  geom_path(data = etsp_df2, aes(x, y), color = "darkgrey", linetype = "dotted") +
  geom_text(data = etsp_df2, aes(x, y, label = tour_order2), hjust=0, vjust = 0) +
  labs(title = "Actual Path", subtitle = paste("Distance = ", dist2, sep=""))+
  xlab("")+
  ylab("")

# Combine the two plots side by side
grid.arrange(p2, p1, ncol = 2)


