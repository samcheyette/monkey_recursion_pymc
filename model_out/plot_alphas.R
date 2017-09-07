library(ggplot2)
library(reshape)
library(grid)
library(dplyr)

##########READ FILE#########################################

file <- "alphas.csv"
data = read.csv(file)
data$who = factor(data$who,levels(data$who)[c(3,1,2)])

#m = melt(data, id=c("who", "mean"))
t1 <- 	theme(axis.text=element_text(size=18), 
		strip.text.x = element_text(size = 20),
		plot.title=element_text(size=18),
		axis.text.x=element_text(size=17),
		#axis.text.x=element_text(size=13, angle=45),
		axis.title.x=element_blank(),
		axis.text.y=element_text(size=17),
		axis.title.y=element_text(size=17),

		legend.title=element_blank(),
		legend.text=element_text(size=17),
		 legend.key.size = unit(4, 'lines'))
###############################################################


p.1 <- ggplot(data, aes(x=who,y=mean)) +
		geom_point(size=3.0) +
		geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.2)


p.1 <- p.1 + t1 + 
			ylab("Alpha Value") +
			 ggtitle("Alpha")

ggsave("alphas.png")