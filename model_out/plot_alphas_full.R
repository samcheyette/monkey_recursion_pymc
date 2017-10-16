library(ggplot2)
library(reshape)
library(grid)
library(dplyr)

##########READ FILE#########################################

file <- "alpha_full.csv"
data = read.csv(file)
#data$who = factor(data$who,levels(data$who)[c(3,1,2)])

#m = melt(data, id=c("who", "mean"))

paper_theme <- theme(#legend.title=element_text( size = 14, face="plain"), 
                     legend.text=element_text(size = 12),
                     legend.title=element_blank(),
                     #legend.text=element_blank(),

                     axis.title.x = element_text(size=0),
                     axis.text.x=element_text(colour="black", size = 12), 
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
		mutate(who=as.factor(gsub("kids","US Kids",as.character(who))))%>%
              mutate(who=as.factor(gsub("adults","US Adults",as.character(who))))
data$who = factor(data$who,levels(data$who)[c(1,4,2,3)])
#data$who = factor(data$who,levels(data$who)[c(2,3,1)])



p.1 <- ggplot(data=data, aes(x=value)) +
		geom_histogram(bins=8) +
		facet_wrap(~who,nrow=3)

p.1 <- p.1 + paper_theme + ylab("Alpha")
ggsave("alpha_histogram.png", width=4,height=4) 



############################

data <- data %>%
              group_by(who) %>% 
              mutate(ci_95=quantile(value,.83)) %>%
              mutate(ci_5=quantile(value,.17)) %>%
              mutate(av=mean(value)) %>%
              top_n(n=1,wt=sample)
 


 head(data)

p.1 <- ggplot(data, aes(x=who,y=av)) +
              geom_point(size=1.5,shape=4) +
              #geom_bar(position='dodge', stat='identity') +

              #geom_point(size=3.0) +
              geom_errorbar(aes(ymin=ci_5, 
                     ymax=ci_95), width=0.05)


p.1 <- p.1 + paper_theme + 
                     ylab(expression("Clustering ("*alpha*")")) +
                     scale_y_continuous(expand = c(0,0))#+ #+
                     #coord_cartesian(ylim = c(5, 15))#+ ylim(0,0.1)
                      #paste("(",paste(expression(eta),")", sep=""), sep=""))) #+
                     # ggtitle("Noise")

ggsave("alpha.png", width=4, height=4)