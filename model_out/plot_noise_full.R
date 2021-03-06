library(ggplot2)
library(reshape)
library(grid)
library(dplyr)

##########READ FILE#########################################

file <- "noise_full.csv"
data = read.csv(file)
data$ID <- seq.int(nrow(data))

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
#x <- seq(0,0.1,length=100)
#db <- dbeta(x, 1, 9)
#dfbeta <- as.data.frame(cbind(x,db))


#head(data)
#max(data$ID)


x <- data %>%
	group_by(sample) %>%
	top_n(1,wt=ID) %>%
	ungroup %>%
	top_n(1,wt=-ID)

d.mod <- x$ID

d.mod


m.0 <- data %>%
		group_by(sample) %>%
		mutate(ID = ID %%d.mod) %>%
		mutate(err=1-(1-value)**4)  %>%
		group_by(ID) %>%
		mutate(mean_val=mean(value)) %>%
		mutate(CI_95_val=quantile(value,.95)) %>%
		mutate(CI_5_val=quantile(value,.5)) %>%
		mutate(mean_err=mean(err)) %>%
		mutate(CI_95_err=quantile(err,.95)) %>%
		mutate(CI_5_err=quantile(err,.5)) %>%
		top_n(n=1,wt=sample)

p.1 <- ggplot(data=m.0, aes(x=who, y=value)) + 
		geom_jitter(width=0.2,height=0.0)  +
		geom_errorbar(aes(ymin=CI_5_err, ymax=CI_95_err))
		

head(m.0)

p.1 <- p.1 +paper_theme

ggsave("noise_by_part.png", width=6,height=6)




take_after <- 0

data <- data %>%
           filter(sample > take_after)

N <- length(data$who)
N
db <- rbeta(N, 1,9)
length(which(db < 0.05))
dfbeta <- as.data.frame(cbind(who=as.factor(rep("prior",N)), 
			value=as.double(as.character(db))))

length(which(dfbeta$value < 0.05))
#length(dfbeta[dfbeta$value < 0.1,])
#length(dfbeta[dfbeta$value < 0.15,])


#data <- data %>%
	#	group_by(sample) %>%
		#mutate(beta=rbeta())
#dfbeta$value <- as.factor(dfbeta$value)

data <- data %>%
		mutate(who=as.factor(gsub("monkeys","Monkeys",as.character(who)))) %>%
		mutate(who=as.factor(gsub("tsimane","Tsimane",as.character(who)))) %>%
		mutate(who=as.factor(gsub("kids","US Kids",as.character(who)))) %>%
		mutate(who=as.factor(gsub("adults","US Adults",as.character(who))))

#data$who = factor(data$who,levels(data$who)[c(2,3,1)])
data$who = factor(data$who,levels(data$who)[c(1,4,2,3)])
data$who <- as.factor(data$who)
head(data)

head(dfbeta)

dfbeta$who <- as.factor(as.character(dfbeta$who))

typeof(data$who)
typeof(dfbeta$who)
typeof(data$value)
typeof(dfbeta$value)




p.1 <- ggplot(data=data, aes(x=value, group=who)) +
		geom_histogram(bins=8,
			aes(group=who, fill=who)) 

		#geom_density(alpha=0.2,adjust=1.5, aes(x=value, 
					#group=who, fill=who)) 
		#geom_line(data=dfbeta, inherit.aes=FALSE,aes(x,db)) +
		#geom_histogram(data=dfbeta, bins=8, inherit.aes=FALSE,
		#		aes(x=value)) +
		#geom_density(data=dfbeta, adjust=0.1, inherit.aes=FALSE,
				#aes(x=value)) +
		#facet_wrap(~who,nrow=4)

p.1 <- p.1 + paper_theme + ylab("Noise") #s+ xlim(0,0.15)
ggsave("noise_histogram.png", width=8,height=4) 




###############################################################

#quantile(data$value,.95)

data <- data %>%
		group_by(who) %>% 
		mutate(ci_95=quantile(value,.95)) %>%
		mutate(ci_5=quantile(value,.05)) %>%
		mutate(av=median(value)) %>%
		top_n(n=1,wt=sample)
 


 head(data)

p.1 <- ggplot(data, aes(x=who,y=av)) +
		#geom_point(size=1.5,shape=4) +
		geom_bar(position='dodge', stat='identity') +

		#geom_point(size=3.0) +
		geom_errorbar(aes(ymin=ci_5, 
			ymax=ci_95), width=0.45)


p.1 <- p.1 + paper_theme + 
			ylab(expression("Noise ("*eta*")")) +
 			scale_y_continuous(expand = c(0,0)) +
			coord_cartesian(ylim = c(0, 0.125))#+ ylim(0,0.1)
			 #paste("(",paste(expression(eta),")", sep=""), sep=""))) #+
			# ggtitle("Noise")

ggsave("noise.png", width=4, height=4)

###############################################################

error_f <- function (val) {
	return (1.-(1. - val)**4)
}

data.err <- data %>%	
		group_by(who) %>%
		mutate(ci_95 = error_f(ci_95)) %>%
		mutate(av = error_f(av)) %>%
		mutate(ci_5 = error_f(ci_5))


head(data.err)
p.1 <- ggplot(data.err, aes(x=who,y=av)) +
		#geom_point(size=1.5,shape=4) +
		geom_bar(position='dodge', stat='identity') +

		#geom_point(size=3.0) +
		geom_errorbar(aes(ymin=ci_5, 
			ymax=ci_95), width=0.45) 
		#geom_point(data=data, aes(x=who, y=av))


p.1 <- p.1 + paper_theme + 
			ylab(expression("p(error)")) +
			ylim(0,0.5) #+
 			#scale_y_continuous(expand = c(0,0)) +
			#coord_cartesian(ylim = c(0, 0.125))#+ 

ggsave("error.pdf", width=4,height=4)