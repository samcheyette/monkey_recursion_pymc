library(ggplot2)
library(reshape)
library(grid)
library(dplyr)
library(stringr)

file_sim <- "similarity_matrix.csv"
data = read.csv(file_sim)
#data$ID <- seq.int(nrow(data))


paper_theme <- theme(#legend.title=element_text( size = 14, face="plain"), 
                     legend.text=element_text(size = 12),
                     legend.title=element_blank(),
                     #legend.text=element_blank(),

                     axis.title.x = element_blank(),
                     axis.text.x=element_blank(),
                     axis.title.y = element_blank(),
                     axis.text.y  = element_blank(),
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(), 
                     panel.background = element_blank(),
                     axis.line.x = element_line(colour = "black"), 
                     axis.line.y = element_line(colour = "black"))

###############################################################################



row.names(data) <- data[, 1]
type <- row.names(data)
l <- length(type)
table(type)
types <- c()
for (x in 1:l) {
       #type_w <- gsub('[[:digit:]]+', '', type[x])
       types[x] <- gsub('[[:digit:]]+', '', type[x])
}
data <- data[, -1]

table(types)

fit <- cmdscale(data, eig = TRUE, k = 2)
x <- fit$points[, 1]
y <- fit$points[, 2]

data <- as.data.frame(cbind(x,y, types))
head(data)

p.1 <- ggplot(data=data, aes(x=x,y=y, color=types)) +
              geom_point()

p.1<- p.1 + paper_theme

ggsave("similarity_matrix.png", width=4,height=4)

if (FALSE) {

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

}