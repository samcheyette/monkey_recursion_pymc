library(ggplot2)
library(reshape)
library(grid)
library(dplyr)

##########READ FILE#########################################

file <- "noise.csv"
data = read.csv(file)
data$who = factor(data$who,levels(data$who)[c(3,1,2)])

#m = melt(data, id=c("who", "mean"))
t1 <- 	theme(axis.text=element_text(size=26), 
		strip.text.x = element_text(size = 26),
		plot.title=element_text(size=26),
		#axis.text.x=element_blank(),
		axis.text.x=element_text(size=26),
		axis.title.x=element_blank(),
		axis.title.y=element_text(size=26),
		axis.text.y=element_text(size=26),

		legend.title=element_blank(),
		legend.text=element_text(size=26),
		 legend.key.size = unit(4, 'lines'),
		 panel.background = element_blank(),
		 axis.line = element_line(colour = "black"))
###############################################################


p.1 <- ggplot(data, aes(x=who,y=mean)) +
		geom_bar(position='dodge', stat='identity') +

		#geom_point(size=3.0) +
		geom_errorbar(aes(ymin=mean-sds, 
			ymax=mean+sds), width=0.)


p.1 <- p.1 + t1 + 
			ylab("Noise Value") #+
			# ggtitle("Noise")

ggsave("noise.png", width=8, height=7)