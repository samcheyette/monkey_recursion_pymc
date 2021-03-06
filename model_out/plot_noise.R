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

paper_theme <- theme(#legend.title=element_text( size = 14, face="plain"), 
                     legend.text=element_text(size = 12),
                     legend.title=element_blank(),
                     #legend.text=element_blank(),

                     axis.title.x = element_text(size=0),
                     axis.text.x=element_text(colour="black", size = 14), 
                     axis.title.y = element_text(size = 14, vjust = 1),
                     axis.text.y  = element_text(size = 12),
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(), 
                     panel.background = element_blank(),
                     axis.line.x = element_line(colour = "black"), 
                     axis.line.y = element_line(colour = "black"))
###############################################################
data <- data %>%
		mutate(who=as.factor(gsub("monkeys","Monkeys",as.character(who)))) %>%
		mutate(who=as.factor(gsub("tsimane","Tsimane",as.character(who)))) %>%
		mutate(who=as.factor(gsub("kids","US Kids",as.character(who))))
data$who = factor(data$who,levels(data$who)[c(2,3,1)])

p.1 <- ggplot(data, aes(x=who,y=mean)) +
		geom_bar(position='dodge', stat='identity') +

		#geom_point(size=3.0) +
		geom_errorbar(aes(ymin=mean-sds, 
			ymax=mean+sds), width=0.05)


p.1 <- p.1 + paper_theme + 
			ylab(expression("Noise ("*eta*")"))
			 #paste("(",paste(expression(eta),")", sep=""), sep=""))) #+
			# ggtitle("Noise")

ggsave("noise.png", width=4, height=4)