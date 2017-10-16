library(ggplot2)
library(reshape)
library(grid)
library(dplyr)

file_sim <- "similarity_matrix.csv"
data = read.csv(file_sim)
data$ID <- seq.int(nrow(data))


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

###############################################################################


data <- data %>%
		filter(part2 > part1)
	

mins <- data %>%
		group_by(who1) %>%
		mutate(min_who =min(part1)) %>%
		top_n(n=1,wt=ID) %>%
		ungroup

head(mins)

l_mins <- mins$min_who
head(l_mins)
p.1 <- ggplot(data=data, aes(x=part1,y=part2,fill=sim)) +
		geom_tile()+
		
		geom_hline(data=mins,yintercept = l_mins, size=1.25) +
				geom_vline(data=mins,xintercept = l_mins, size=1.25)

ggsave("similarity_matrix.png", width=4,height=4)