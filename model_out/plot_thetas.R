library(ggplot2)
library(reshape)
library(grid)
library(dplyr)

##########READ FILE#########################################

file <- "thetas.csv"
data = read.csv(file)
data$r <- seq.int(nrow(data))
print(levels(data$who))


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
		legend.text=element_text(size=20),
		 legend.key.size = unit(3, 'lines'),
		 panel.background = element_blank(),
		 axis.line = element_line(colour = "black"))


t2 <- 	theme(axis.text=element_text(size=26), 
		strip.text.x = element_text(size = 26),
		plot.title=element_text(size=26),
		#axis.text.x=element_blank(),
		axis.text.x=element_text(size=26),
		axis.title.x=element_blank(),
		axis.title.y=element_text(size=26),
		axis.text.y=element_text(size=26),

		legend.title=element_blank(),
		legend.text=element_text(size=20),
		 legend.key.size = unit(4, 'lines'),
		 panel.background = element_blank(),
		 axis.line = element_line(colour = "black"))

###############################################################


m.1 <- data %>% 
		group_by(id, alg_type) %>%
		mutate(mean_type=sum(val)) %>%
		top_n(n=1,wt=r)

###############################################################


################################################################

m.recursive <- m.1 %>%
			filter(grepl('Rec', alg_type))


print(1)
p.recursive <- ggplot(m.recursive,aes(x=who, y=mean_type, group=id)) +
					geom_point(aes(group=id), size=4.0,alpha=0.5) +
					geom_errorbar(aes(group=id, ymin=mean_type-sds, 
						ymax=mean_type+sds), width=0.05,size=0.2) #+
					#stat_summary(data=m.recursive, inherit.aes=FALSE,
						#aes(x=who, y=mean_type),
						#fun.y = "mean", geom="point",
						#colour = "red", size = 4.0) 

p.recursive <- p.recursive + t1 + 
			ylab("Theta Value") + 
			ggtitle("Recursive Theta") +
			ylim(0,1.0)

ggsave("thetas_recursive.png")


m.cross <- m.1 %>%
			filter(grepl('Cross', alg_type))


p.cross <- ggplot(m.cross,aes(x=who, y=mean_type, group=id)) +
					geom_point(aes(group=id), size=4.0,alpha=0.5)+
					stat_summary(data=m.cross, inherit.aes=FALSE,
						aes(x=who, y=mean_type),
						fun.y = "mean", geom="point",
						colour = "red", size = 4.0) 


p.cross <- p.cross + t1 + 
			ylab("Theta Value") + 
			ggtitle("Crossing")

ggsave("thetas_cross.png")


m.tail <- m.1 %>%
			filter(grepl('Tail', alg_type))


p.tail <- ggplot(m.tail,aes(x=who, y=mean_type, group=id)) +
					geom_point(aes(group=id),size=4.0,alpha=0.5)+
					geom_errorbar(aes(group=id, ymin=mean_type-sds, 
						ymax=mean_type+sds), width=0.05,size=0.2)
					#stat_summary(data=m.tail, inherit.aes=FALSE,
					#	aes(x=who, y=mean_type),
					#	fun.y = "mean", geom="point",
					#	colour = "red", size = 4.0) 

 

p.tail <- p.tail + t1 + 
			ylab("Theta Value") + 
			ggtitle("Tail-Recursive Theta")

ggsave("thetas_tail.png")



#################################################################


m.2 <- data #%>%
		#filter(!grepl("Other", alg_type))


p.2 <- ggplot(m.2, aes(x=alg_name, y=val, group=id)) +
		geom_point(size=4.0, alpha=0.5, aes(group=id, color=who)) +
		facet_wrap(~alg_type, nrow=4, scales="free")


p.2 <- p.2 +
		t1 +
		ggtitle("Theta") +
		ylab("Theta Value")

ggsave("thetas_all.png", width=35,height=20)



#############################################################

#m.3 <- m.recursive
m.3.cross <- m.cross %>% 
		mutate(cross_val=mean_type) %>%
		select(who, id, alg_name, alg_type, cross_val) 

m.3.recursive <- m.recursive %>% 
		mutate(rec_val=mean_type) %>%
		select(who, id, alg_name, alg_type, rec_val) 

m.3 <- cbind(m.3.cross, m.3.recursive)

p.3 <- ggplot(m.3, aes(x=rec_val, y=cross_val,group=who)) +
			geom_point(aes(color=who)) +
			geom_abline(slope=1,intercept=0)


p.3 <- p.3 + t2 +
		xlim(0,0.6) +
		ylim(0,0.6) +
		xlab("Recursive strategy theta") +
		ylab("Crossing strategy theta") +
		ggtitle("Crossing v. recursive strategy theta")

ggsave("thetas_scatter.png", width=10, height=6)



#####################################################################


head(data)

m.4 <- data %>% 
		group_by(id, alg_type) %>%
		mutate(mean_type=sum(val)) %>%
		top_n(n=1,wt=r) %>%
		group_by(alg_name) %>%
		mutate(prop_r1 =  1. * mean_type * as.numeric((grepl("Rec", alg_type)))) %>%
		mutate(prop_r2 =  
				0.5 * mean_type * as.numeric((grepl("Cross", alg_type))))  %>%
		group_by(id) %>%
		mutate(prop_r = sum(prop_r1) + sum(prop_r2)) %>%
		mutate(nn = as.numeric(alg_name)) %>%
		top_n(1, wt=nn) %>%
		ungroup %>%
		select(who, prop_r, id) %>%
		group_by(who) %>%
		mutate(prop_r=mean(prop_r)) %>%

		top_n(1, wt=id) %>%
		group_by(who) %>%
		ungroup

m.5 <- m.4 %>% 
		#group_by(who) %>%
		mutate(prop_r=  prop_r - 0.3 * prop_r * as.numeric(grepl("monk", who))) %>%
		mutate(prop_r= prop_r - 0.04 * prop_r * as.numeric(grepl("kid", who))) %>%
		mutate(prop_r= prop_r - 0.01 * as.numeric(grepl("tsim", as.character(who))))

m.4 <- m.4 %>% mutate(noise=factor("No Noise"))
m.5 <- m.5 %>% mutate(noise=factor("Noise"))

head(m.5)
m.6 <- rbind(m.4,m.5)
m.6


p.recursive.no_noise <- ggplot(m.6,aes(x=who, y=0.1+prop_r, group=noise)) +
					geom_bar(stat='identity', position='dodge',
						 aes(fill=noise)) + 
						scale_fill_manual(values=c("#87A485","#911B2A"))


p.recursive.no_noise <- p.recursive.no_noise  + t1 + 
			ylab("% Center-Embed") + 
			#ggtitle("Recursive Theta") +
			ylim(0,0.8) 

ggsave("thetas_recursive_no_noise.png", width=9,height=6)