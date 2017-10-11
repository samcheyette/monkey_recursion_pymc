library(ggplot2)
library(reshape)
library(grid)
library(dplyr)

file <- "beta_full.csv"
data = read.csv(file)
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


############################READ FILE#########################################


m.rec <- data %>%
         filter(which %in% c("[()]", "([])", "OOMM")) %>%
         group_by(who, part, sample) %>%
         mutate(recursive=sum(value)) %>%
         top_n(n=1, wt=ID) %>%
         group_by(who) %>%
       mutate(ci_95=quantile(recursive,.75)) %>%
       mutate(ci_5=quantile(recursive,.25)) %>%
       mutate(av=mean(recursive)) %>%
       top_n(n=1,wt=sample)


head(m.rec)



p.1 <- ggplot(data=m.rec, aes(x=who, y=av)) +
             geom_point() +
             geom_errorbar(aes(ymin=ci_5, ymax=ci_95))

       
p.1 <- p.1 + paper_theme
       
ggsave("betas.png", width=5,height=5)
