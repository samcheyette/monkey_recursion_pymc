library(ggplot2)
library(reshape)
library(grid)
library(dplyr)
#library(HDInterval)

file_beta <- "beta_full.csv"
data_beta = read.csv(file_beta)
data_beta$ID <- seq.int(nrow(data_beta))

file_theta <- "theta_full.csv"
data_theta = read.csv(file_theta)
data_theta$ID <- seq.int(nrow(data_theta))

take_after <- 0


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

data_theta$who = factor(data_theta$who,levels(data_theta$who)[c(3,1,2)])
data_beta$who = factor(data_beta$who,levels(data_beta$who)[c(3,1,2)])
data_theta <- data_theta %>%
                    filter(sample > take_after)
data_beta <- data_beta %>%
                    filter(sample > take_after)


m.rec.betas <- data_beta %>%
         filter(which %in% c("[()]", "([])", "OOMM")) %>%
         group_by(who, part, sample) %>%
         mutate(recursive=sum(value)) %>%
         top_n(n=1, wt=ID) %>%
         group_by(who) %>%
       mutate(ci_95=quantile(recursive,.75)) %>%
       mutate(ci_5=quantile(recursive,.25)) %>%


       mutate(av=median(recursive)) %>%
       top_n(n=1,wt=sample) %>%
        mutate(x_val=as.numeric(who)) 
head(m.rec.betas)

m.rec.thetas <- data_theta %>%

         filter(which %in% c("[()]", "([])", "OOMM")) %>%
         group_by(who, part, sample) %>%
         mutate(recursive=sum(value)) %>%
         top_n(n=1, wt=ID) %>%
         group_by(part) %>%
       mutate(ci_95=quantile(recursive,.75)) %>%
       mutate(ci_5=quantile(recursive,.25)) %>%

       mutate(av=median(recursive)) %>%
       top_n(n=1,wt=sample) %>%
       mutate(x_val=as.numeric(who) + runif(1,-0.2,0.2))



p.1 <- ggplot(data=m.rec.thetas, aes(x=x_val, y=av)) +
            
             geom_point(alpha=0.5,
              aes(x=x_val, y=av, group=part, color="Individual")) +
             geom_errorbar(data=m.rec.thetas,alpha=0.3,
                aes(color="Individual",ymin=ci_5, ymax=ci_95, group=part),
              width=0.05, alpha=0.3)  +
             geom_point(data=m.rec.betas,
                size=1.5,aes(color="Group")) +
                     geom_errorbar(data=m.rec.betas,
                        aes(color="Group",ymin=ci_5, ymax=ci_95),
              width=0.1,size=0.8, alpha=0.8) +

            geom_hline(linetype="dashed",
             aes(yintercept=0.055)) 




brk <- c(0.0,0.055,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
lab <- c("0","prior","0.1","0.2","0.3","0.4","0.5","0.6","0.7","0.8", "0.9")
my.labs <- list(expression(Group (beta)),expression(Individual (theta)))
       
p.1 <- p.1 + paper_theme +
            ylab("p(recursive)") + 
            theme(legend.position = c(0.8, 0.9))  +

            scale_y_continuous(breaks=brk, labels=lab) +

                scale_x_continuous(limits=c(0.5,3.5),
                breaks=c(1,2,3),labels=c("Tsimane","US Kids","Monkeys")) +
            scale_color_manual(values=c("#b30000","black"),
          labels=my.labs)

ggsave("thetas_betas.png", width=8,height=4)



####################################################################


print(1)
m.rec.thetas <- data_theta %>%
        filter(sample > take_after) %>%
        select(-one_of("thin_sample")) %>%

         filter(which %in% c("[()]", "([])", "OOMM", "OOMC", "[OMM", "(OMM" ,
                        "[(MM", "[(MM", "([]M", "[()M", "([]C", "[()C",
                            "OOCC", "OOCM","([)]", "[(])", "(OCC", "[OCC", 
                        "(OCM", "[OCM", "([CC", "[(CC", "([CM", "[(CM")) %>%

         group_by(sample, which, part) %>%
         mutate(rec = 1.0 * which %in% c("[()]", "([])", "OOMM", "OOMC", "[OMM", "(OMM" ,
                        "[(MM", "[(MM", "([]M", "[()M", "([]C", "[()C")) %>%
         mutate(close=0.5 * which %in% c("OOCC", "OOCM","([)]", "[(])", "(OCC", "[OCC", 
                        "(OCM", "[OCM", "([CC", "[(CC", "([CM", "[(CM")) %>%
         mutate(prb=value*(rec + close)) %>%
         group_by(who,sample,part) %>%
         mutate(prb=sum(prb)) %>%
         top_n(n=1,wt=ID) %>%
         group_by(who,sample) %>%
         mutate(prb=mean(prb)) %>%
         top_n(n=1,wt=ID) %>%
         #top_n(n=1, wt=ID) %>%

         #top_n(n=1, wt=sample) %>%
         group_by(who) %>%
         mutate(ci_95=quantile(prb,.95)) %>%
         mutate(ci_5=quantile(prb,.05)) %>%
         mutate(prb=median(prb)) %>%
            #mutate(ci_95=mean(ci_95)) %>%
           # mutate(ci_5=mean(ci_5)) %>%

         #top_n(n=1, wt=ID) %>%
         top_n(n=1, wt=ID) %>%

         ungroup %>%
        select(-one_of("sample"))  %>%
        select(-one_of("rec")) %>%
        select(-one_of("close"))

head(m.rec.thetas)


m.rec.thetas.noise <- m.rec.thetas %>%
        mutate(prb=  prb - 0.3 * prb * as.numeric(grepl("monk", who))) %>%
        mutate(prb= prb - 0.04 * prb * as.numeric(grepl("kid", who))) %>%
        mutate(prb= prb - 0.01 * prb* as.numeric(grepl("tsim", as.character(who)))) %>%
        mutate(ci_95=  ci_95 - 0.3 * ci_95 * as.numeric(grepl("monk", who))) %>%
        mutate(ci_95= ci_95 - 0.04 * ci_95 * as.numeric(grepl("kid", who))) %>%
        mutate(ci_95= ci_95 - 0.01 *ci_95 * as.numeric(grepl("tsim", as.character(who)))) %>%
        mutate(ci_5=  ci_5 - 0.3 * ci_5 * as.numeric(grepl("monk", who))) %>%
        mutate(ci_5= ci_5 - 0.04 * ci_5 * as.numeric(grepl("kid", who))) %>%
        mutate(ci_5= ci_5 - 0.01 * ci_5* as.numeric(grepl("tsim", as.character(who)))) 


         #mutate(rec=rec * )


m.rec.thetas <- m.rec.thetas %>% mutate(noise=factor("No Noise"))
m.rec.thetas.noise <- m.rec.thetas.noise %>% mutate(noise=factor("Noise"))

m.2 <- rbind(m.rec.thetas,m.rec.thetas.noise)

off <- 0.04

p.1 <- ggplot(data=m.2, aes(x=who, y=prb+off, group=noise)) +
            geom_bar(stat='identity', position='dodge',
             aes(fill=noise,group=noise)) +
            geom_errorbar(aes(ymin=ci_5+off,ymax=ci_95+off, group=noise),
                    position='dodge',  size=0.3) 


brk <- seq(0,7)/10.

p.1 <- p.1 +paper_theme + 
        scale_y_continuous(breaks=brk,expand=c(0.05,0.05)) +
        scale_x_discrete(labels=c("Tsimane","US Kids","Monkeys"))

head(m.rec.thetas)
ggsave("thetas_recursive_no_noise.png",width=6,height=4)
