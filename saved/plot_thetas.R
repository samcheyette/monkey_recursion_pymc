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

t2 <- 	theme(axis.text=element_text(size=18), 
		strip.text.x = element_text(size = 20),
		plot.title=element_text(size=18),
		axis.text.x=element_text(size=17),
		#axis.text.x=element_text(size=13, angle=45),
		axis.title.x=element_text(size=18),
		axis.text.y=element_text(size=17),
		axis.title.y=element_text(size=18),

		legend.title=element_blank(),
		legend.text=element_text(size=17),
		 legend.key.size = unit(4, 'lines'))
###############################################################


m.1 <- data %>% 
		group_by(id, alg_type) %>%
		mutate(mean_type=sum(val)) %>%
		top_n(n=1,wt=r)



################################################################

m.recursive <- m.1 %>%
			filter(grepl('Rec', alg_type))

print(1)
p.recursive <- ggplot(m.recursive,aes(x=who, y=mean_type, group=id)) +
					geom_point(aes(group=id), size=4.0,alpha=0.5) +
					stat_summary(data=m.recursive, inherit.aes=FALSE,
						aes(x=who, y=mean_type),
						fun.y = "mean", geom="point",
						colour = "red", size = 4.0) 

p.recursive <- p.recursive + t1 + 
			ylab("Theta Value") + 
			ggtitle("Recursive")

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
					stat_summary(data=m.tail, inherit.aes=FALSE,
						aes(x=who, y=mean_type),
						fun.y = "mean", geom="point",
						colour = "red", size = 4.0) 

 

p.tail <- p.tail + t1 + 
			ylab("Theta Value") + 
			ggtitle("Tail")

ggsave("thetas_tail.png")



#################################################################


m.2 <- data %>%
		filter(!grepl("Other", alg_type))


p.2 <- ggplot(m.2, aes(x=alg_name, y=val, group=id)) +
		geom_point(size=4.0, alpha=0.5, aes(group=id, color=who)) +
		facet_wrap(~alg_type, nrow=3, scales="free")


p.2 <- p.2 +
		t1 +
		ggtitle("Theta") +
		ylab("Theta Value")

ggsave("thetas_all.png", width=15,height=12)



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
		xlim(0,0.75) +
		ylim(0,0.75) +
		xlab("Recursive strategy theta") +
		ylab("Crossing strategy theta") +
		ggtitle("Crossing v. recursive strategy theta")

ggsave("thetas_scatter.png", width=10, height=6)