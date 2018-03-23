library(ggplot2)
library(reshape)
library(grid)
library(dplyr)

##########READ FILE#########################################

file_monkey <- "RecursionMonkey.csv"
file_kids <- "RecursionKids.csv"
file_tsimane<- "RecursionTsimane.csv"

data_monkey = read.csv(file_monkey)
data_kids = read.csv(file_kids)
data_tsimane = read.csv(file_tsimane)

table(data_monkey$Exposure, data_monkey$Sub)

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
		legend.text=element_text(size=20),
		 legend.key.size = unit(3, 'lines'),
		 panel.background = element_blank(),
		 axis.line = element_line(colour = "black"))

