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

paper_theme2 <- theme(#legend.title=element_text( size = 14, face="plain"), 
                     legend.text=element_text(size = 12),
                     legend.title=element_blank(),
                     #legend.text=element_blank(),
					axis.ticks.x=element_blank(),
                     axis.title.x = element_text(size=0),
                     axis.text.x=element_blank(), 
                     axis.title.y = element_text(size = 14, vjust = 1),
                     axis.text.y  = element_text(size = 12),
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(), 
                     panel.background = element_blank(),
                     axis.line.x = element_line(colour = "black"), 
                     axis.line.y = element_line(colour = "black"))
###############################################################


m.1 <- data %>% 
		group_by(id, alg_type) %>%
		mutate(mean_type=sum(val)) %>%
		mutate(sds=mean(sds)) %>%
		top_n(n=1,wt=r) 


head(m.1)

m.1 <- m.1 %>%
		mutate(who=as.factor(gsub("monkeys","Monkeys",as.character(who)))) %>%
		mutate(who=as.factor(gsub("tsimane","Tsimane",as.character(who)))) %>%
		mutate(who=as.factor(gsub("kids","US Kids",as.character(who))))
m.1$who = factor(m.1$who,levels(m.1$who)[c(3,2,1)])

head(m.1)

###############################################################


################################################################

m.recursive <- m.1 %>%
			filter(grepl('Rec', alg_type))


print(1)

if (FALSE) {

geom_point(aes(group=id, color=who), 
position=position_dodge(width=0.2),
size=2.0,alpha=0.3) +
geom_errorbar(aes(group=id, color=who,
ymin=mean_type-sds, ymax=mean_type+sds),
	position=position_dodge(width=0.2),

width=0.05,size=0.2, alpha=0.7) +

geom_hline(linetype="dashed",
 aes(yintercept=0.055)) 


p.recursive <- ggplot(m.recursive,aes(x=who, y=mean_type, group=id)) +


					geom_point(position=position_dodge(width=0.1),
						aes(group=id, color=who), 
						size=2.0,alpha=0.7) +
					geom_errorbar(position=position_dodge(width=0.1),
						aes(group=id, color=who,
						ymin=mean_type-sds, ymax=mean_type+sds),
						width=0.05,size=0.2, alpha=0.9) +


					geom_hline(linetype="dashed",
						 aes(yintercept=0.055)) 


p.recursive <-ggplot(m.recursive,aes(x=x_val,y=mean_type, group=id)) +


					geom_point(aes(group=id, color=who), 
						size=1.5,alpha=0.9) +
					geom_errorbar(aes(group=id, color=who,
						ymin=mean_type-sds, ymax=mean_type+sds),
						width=0.06,size=0.15, alpha=0.9)			

}

file <- "betas.csv"
data_beta = read.csv(file)
data_beta$ID <- seq.int(nrow(data_beta))

data_beta$who = factor(data_beta$who,levels(data_beta$who)[c(3,1,2)])
m.beta_recursive <- data_beta %>% 
		group_by(who) %>%
		mutate(sds = mean(sds)) %>%

		group_by(who, alg_type) %>%
		mutate(mean_type_b=sum(val)) %>%
		top_n(n=1, wt=ID) %>%
		filter(grepl("Rec", alg_type)) %>%
		group_by(who) %>%
		mutate(x_val=as.numeric(who))


head(m.beta_recursive)

m.recursive$id <- factor(m.recursive$id)

m.recursive <- m.recursive %>% 
				group_by(who) %>%
				mutate(x_val_w=as.numeric(who)) %>%
				group_by(id) %>%
				#mutate(x_val= x_val + mean_type) %>%
				mutate(x_val = x_val_w + runif(1,-0.12,0.12))

head(m.recursive$x_val)


p.recursive <- ggplot(m.recursive,aes(x=x_val, y=mean_type, group=id)) +


					geom_point(aes(group=id,color="Individual"), 
						size=1.,alpha=0.7) +
					geom_errorbar(aes(group=id, color="Individual",
						ymin=mean_type-sqrt(mean_type)/15., ymax=mean_type+sqrt(1.-mean_type)/15.),
						width=0.07,size=0.3, alpha=0.5) +


					geom_hline(linetype="dashed",
						 aes(yintercept=0.055)) +
					#geom_hline(linetype="solid",
						# aes(yintercept=0.0),size=0.3) +
					#geom_text(data= m.recursive,size=4.0,
					#	aes(x=x_val_w,y=-0.05, label=who)) +
					geom_point(data=m.beta_recursive,inherit.aes=FALSE,
						aes(x=x_val,y=mean_type_b,color="Group"),alpha=1.,
						size=2.0)+

					geom_errorbar(data=m.beta_recursive, inherit.aes=FALSE,
							aes(x=x_val,color="Group", ymin=mean_type_b-sqrt(mean_type_b)/15,
						 ymax=mean_type_b+sqrt(mean_type_b)/15),
						width=0.05,size=0.4,alpha=1.) 

brk <- c(0.0,0.055,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8)
lab <- c("0","prior","0.1","0.2","0.3","0.4","0.5","0.6","0.7","0.8")
my.labs <- list(expression(Group (beta)),expression(Individual (theta)))


p.recursive <- p.recursive + paper_theme + 
			ylab("p(recursive)") + 
			#ggtitle("Recursive algorithm use") +
			theme(legend.position = c(0.8, 0.9))  +

			scale_y_continuous(breaks=brk, labels=lab,expand = c(0,0.0225)) +
			scale_x_continuous(limits=c(0.5,3.5),
				breaks=c(1,2,3),labels=c("Tsimane","US Kids","Monkeys")) +
			scale_color_manual(values=c("#b30000","black"),
				#breaks=c("beta","theta"),
          labels=my.labs)
#xlim(0.0,1.)
ggsave("thetas_recursive.png", width=8, height=4)


p.recursive.hist <- ggplot(m.recursive, aes(mean_type)) +
				geom_histogram(bins=4) +
				facet_wrap(~who, nrow=3) 

p.recursive.hist <- p.recursive.hist + t1 + 
		ylab("Theta Value")

ggsave("thetas_recursive_histogram.png", width=12, height=10)


######################################################################




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
		#xlim(0,0.6) +
		#ylim(0,0.6) +
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
		mutate(prop_r=  prop_r - 0.35 * prop_r * as.numeric(grepl("monk", who))) %>%
		mutate(prop_r= prop_r - 0.04 * prop_r * as.numeric(grepl("kid", who))) %>%
		mutate(prop_r= prop_r - 0.01 * as.numeric(grepl("tsim", as.character(who))))

m.4 <- m.4 %>% mutate(noise=factor("No Noise"))
m.5 <- m.5 %>% mutate(noise=factor("Noise"))

head(m.5)
m.6 <- rbind(m.4,m.5)
m.6


m.6 <- m.6 %>%
		mutate(who=as.factor(gsub("monkeys","Monkeys",as.character(who)))) %>%
		mutate(who=as.factor(gsub("tsimane","Tsimane",as.character(who)))) %>%
		mutate(who=as.factor(gsub("kids","US Kids",as.character(who))))
m.6$who = factor(m.6$who,levels(m.6$who)[c(2,3,1)])


p.recursive.no_noise <- ggplot(m.6,aes(x=who, y=prop_r+0.05, group=noise)) +
					geom_bar(stat='identity', 
						position=position_dodge(width=0.9),
						 aes(fill=noise)) + 
					geom_errorbar(position=position_dodge(width=0.9),
						aes(x=who, y=prop_r+0.05,
						group=noise,ymin=prop_r-0.05*sqrt(prop_r)+0.05, 
						ymax=prop_r+0.05*sqrt(1-prop_r)+0.05),
						width=0.1,size=0.2) +
						scale_fill_manual(values=c("#87A485", "#b30000")) 
						#scale_fill_manual(values=c("#87A485","#911B2A"))


p.recursive.no_noise <- p.recursive.no_noise  +
			scale_y_continuous(limits=c(0,0.7),breaks=seq(0,7,1)*0.1,
				expand = c(0,0))+

		 paper_theme + 
			theme(legend.position = c(0.84, 0.9)) +
			ylab("p(center-embedding)") 			#ggtitle("Recursive Theta") +

			#ylim(0,0.8) 


ggsave("thetas_recursive_no_noise.png", width=4,height=4)