library(ggplot2)
library(reshape)
library(grid)
library(dplyr)

##########READ FILE#########################################

file <- "betas.csv"
data = read.csv(file)
data$ID <- seq.int(nrow(data))

data$who = factor(data$who,levels(data$who)[c(3,1,2)])
#m = melt(data, id=c("who", "mean"))


t1 <- 	theme(axis.text=element_text(size=26), 
		strip.text.x = element_text(size = 26),
		plot.title=element_text(size=26),
		#axis.text.x=element_blank(),
		axis.text.x=element_text(size=26),
		axis.title.x=element_text(size=26),
		axis.title.y=element_text(size=26),
		axis.text.y=element_text(size=26),

		legend.title=element_blank(),
		legend.text=element_text(size=26),
		 legend.key.size = unit(4, 'lines'),
		 panel.background = element_blank(),
		 axis.line = element_line(colour = "black"))

#t1 <- 	theme(axis.text=element_text(size=18), 
#		strip.text.x = element_text(size = 24),
	#	plot.title=element_text(size=26),
	#	axis.text.x=element_text(size=24),
		#axis.text.x=element_text(size=13, angle=45),
	#	axis.title.x=element_blank(),
		#axis.text.y=element_text(size=24),
		#axis.title.y=element_text(size=24),

		#legend.title=element_blank(),
		##legend.text=element_text(size=17),
		 #legend.key.size = unit(4, 'lines'))
###############################################################


x <- data %>%
	filter(grepl("tsimane", who))

#sum(x$sd)
#data <- data %>% 
		#group_by(ID) %>%
		#mutate(who = gsub("kids", "US Kids", who)) %>%
		#mutate(who = gsub("monkeys", "Monkeys", who))  %>%
		#mutate(who = gsub("tsimane", "Tsimane", who)) 


#data$who <- factor(data$who,levels(data$who)[c(2,1,3)])
#levels(data$who) <- levels(data$who)[2,3,1]


head(data)
###############################################################


m.1 <- data %>% 
		group_by(who) %>%
		mutate(sds = sum(sds)) %>%

		group_by(who, alg_type) %>%
		mutate(mean_type=sum(val)) %>%
		top_n(n=1, wt=ID) %>%
		filter(!grepl("Other", alg_type))

p.1 <- ggplot(m.1, aes(x=alg_type, y=mean_type, group=who)) +
		geom_point(size=4.0, aes(group=who, color=who)) +
		geom_errorbar(aes(ymin=mean_type-sds, 
				ymax=mean_type+sds, color=who), width=0, size=0.5)


p.1 <- p.1 +  t1 +
		ylab("Beta Value") +
		ggtitle("Beta") +
		ylim(0,0.5) +xlab("")

ggsave("betas1.png", width=12, height=9)


#################################################################




m.2 <- data %>% 
		filter(!grepl("Other", alg_type))

p.2 <- ggplot(m.2, aes(x=alg_name, y=val, group=who)) +
		geom_point(size=4.0, aes(group=who, color=who)) +
		geom_errorbar(aes(ymin=val-sds, 
				ymax=val+sds, color=who), width=0.05, size=0.4) +
		facet_wrap(~alg_type, nrow=3, scales="free")


p.2 <- p.2 +  t1 +
		ylab("Beta Value") + xlab("")+
		ggtitle("Beta")

ggsave("betas2.png", width=15,height=12)


#######################################################################



m.3 <- m.1 %>% 
		filter(grepl("Recursive", alg_type))  %>%
		mutate(mean_type=mean_type)


p.3 <- ggplot(m.3, aes(x=who, y=mean_type)) +
		geom_bar(position='dodge', stat='identity') +
		geom_hline(linetype="dashed",
			 aes(yintercept=0.02)) +

		#geom_point(size=1.0) +
		geom_errorbar(aes(ymin=mean_type-sds, 
				ymax=mean_type+sds), width=0., size=0.9) #+
		#facet_wrap(~alg_type, nrow=3, scales="free")


p.3 <- p.3 +  t1 +
		ylab("Beta") +
		ggtitle("Recursive") + xlab("")

ggsave("betas3.png", width=7,height=7)



m.3 <- m.1 %>% 
		filter(grepl("Tail", alg_type)) 


p.3 <- ggplot(m.3, aes(x=who, y=mean_type)) +
		geom_bar(position='dodge', stat='identity') +
		geom_hline(linetype="dashed",
			 aes(yintercept=0.032)) +

		#geom_point(size=1.0) +
		geom_errorbar(aes(ymin=mean_type-sds, 
				ymax=mean_type+sds), width=0., size=0.9) #+
		#facet_wrap(~alg_type, nrow=3, scales="free")


p.3 <- p.3 +  t1 +
		ylab("Beta") +
		ggtitle("Tail-Recursive") + xlab("")

ggsave("betas4.png", width=7,height=7)
