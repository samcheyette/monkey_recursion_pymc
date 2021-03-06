library(ggplot2)
library(reshape)
library(grid)
library(dplyr)
library(magrittr)
#library(HDInterval)

file_beta <- "beta_full.csv"
data_beta = read.csv(file_beta)
data_beta$ID <- seq.int(nrow(data_beta))

file_theta <- "theta_full.csv"
data_theta = read.csv(file_theta)
data_theta$ID <- seq.int(nrow(data_theta))

data_theta <- data_theta %>%
        mutate(who=as.factor(gsub("monkeys","Monkeys",as.character(who)))) %>%
        mutate(who=as.factor(gsub("tsimane","Tsimane",as.character(who)))) %>%
        mutate(who=as.factor(gsub("adults","US Adults",as.character(who)))) %>%
        mutate(who=as.factor(gsub("kids","US Kids",as.character(who))))


data_beta <- data_beta %>%
        mutate(who=as.factor(gsub("monkeys","Monkeys",as.character(who)))) %>%
        mutate(who=as.factor(gsub("tsimane","Tsimane",as.character(who)))) %>%
        mutate(who=as.factor(gsub("adults","US Adults",as.character(who)))) %>%
        mutate(who=as.factor(gsub("kids","US Kids",as.character(who))))

take_after <- 0


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


############################READ FILE#########################################

#data_theta$who = factor(data_theta$who,levels(data_theta$who)[c(2,3,4,1)])
#data_beta$who = factor(data_beta$who,levels(data_beta$who)[c(2,3,4,1)])

print(1)
data_theta <- data_theta %>%
                    filter(sample > take_after)
data_beta <- data_beta %>%
                    filter(sample > take_after)



m.rec.betas <- data_beta %>%
         filter(which %in% c("[()]", "([])", "OOMM", "OOMC")) %>%
         group_by(who, part, sample) %>%
         mutate(recursive=sum(value)) %>%
         top_n(n=1, wt=ID) %>%
         group_by(who) %>%
       mutate(ci_95=quantile(recursive,.75)) %>%
       mutate(ci_5=quantile(recursive,.25)) %>%


       mutate(av=median(recursive)) %>%
       top_n(n=1,wt=sample)

head(m.rec.betas)



m.rec.thetas <- data_theta %>%

         filter(which %in% c("[()]", "([])", "OOMM", "OOMC")) %>%
         group_by(who, part, sample) %>%
         mutate(recursive=sum(value)) %>%
         top_n(n=1, wt=ID) %>%
         group_by(part) %>%
       mutate(ci_95=quantile(recursive,.75)) %>%
       mutate(ci_5=quantile(recursive,.25)) %>%

       mutate(av=median(recursive)) %>%
       top_n(n=1,wt=sample) %>%
       ungroup


m.rec.betas <- m.rec.betas %>%
                transform(who=factor(reorder(who, av))) %>%
                mutate(x_val=as.numeric(who)) 


levels(m.rec.thetas$who)


beta_who <- levels(m.rec.betas$who)
m.rec.thetas <- m.rec.thetas %>%
        group_by(part) %>%
        mutate(ord=which(!is.na(match(beta_who, who)))[1]) %>%

        mutate(x_val=ord + runif(1,-0.2,0.2)) %>%
        group_by(who) %>%
        transform(who=factor(reorder(who, ord))) 


levels(m.rec.thetas$who)



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




brk <- c(0.0,0.055,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)
lab <- c("0","prior","0.1","0.2","0.3","0.4","0.5","0.6","0.7","0.8", "0.9","1.0")
my.labs <- list(expression(Group (beta)),expression(Individual (theta)))
       
p.1 <- p.1 + paper_theme +
            ylab("p(recursive)") + 
            theme(legend.position = c(0.15, 0.9))  +

            scale_y_continuous(breaks=brk, labels=lab,expand=c(0.01,0.001)) +

                scale_x_continuous(limits=c(0.5,4.5),
                breaks=c(1,2,3,4),labels=levels(m.rec.thetas$who)) +
            scale_color_manual(values=c("#b30000","black"),
          labels=my.labs)

ggsave("thetas_betas.pdf", width=8,height=4)



####################################################################
if (FALSE) {

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
         group_by(sample,part) %>%
         mutate(prb=sum(prb)) %>%
         top_n(n=1,wt=ID) %>%
         mutate(forward=(1-(1-forward)**4))
 

        # top_n(n=1, wt=ID) 
head(m.rec.thetas)

m.rec.thetas.noise <- m.rec.thetas %>%
        group_by(sample,which,part) %>%
       # mutate(ID=ID) %>%
        mutate(prb=  prb - forward * prb * as.numeric(grepl("Monk", who))) %>%
        mutate(prb= prb - forward * prb * as.numeric(grepl("Kid", who))) %>%
        mutate(prb= prb - forward * prb* as.numeric(grepl("Tsim", as.character(who)))) %>%
        mutate(prb= prb - forward * prb* as.numeric(grepl("Adu", as.character(who))))


m.rec.thetas <- m.rec.thetas %>% mutate(noise="B")
m.rec.thetas.noise <- m.rec.thetas.noise %>% mutate(noise="A")

m.2 <- rbind(m.rec.thetas,m.rec.thetas.noise)
head(m.2)

m.2 <- m.2 %>%
        group_by(part, noise) %>%
        mutate(ci_95=quantile(prb,0.95)) %>%
        mutate(ci_5=quantile(prb,0.05)) %>%
        mutate(prb=median(prb)) %>%
        #top_n(n=1,wt=sample) %>%

        group_by(who, noise) %>%
        mutate(ci_95=mean(ci_95)) %>%

        mutate(ci_5=mean(ci_5)) %>%

        mutate(prb=mean(prb)) %>%

        #mutate(ci_95=mean(ci_95)) %>%
        #mutate(ci_5=mean(ci_5)) %>%

        top_n(n=1, wt=ID) %>% 
         ungroup %>%
        select(-one_of("sample"))  %>%
        select(-one_of("rec")) %>%
        select(-one_of("forward")) %>%

        select(-one_of("close"))
            #transform(who=factor(reorder(who, sum(prb))) )

m.2$who = factor(m.2$who,levels(m.2$who)[c(1,4,2,3)])

p.1 <- ggplot(data=m.2, aes(x=who, y=prb,group=noise, fill=noise)) +
        geom_bar(stat='identity', position='dodge') +
            geom_errorbar(aes(ymin=ci_5,ymax=ci_95, group=noise),
                  position=position_dodge(width=0.9),  width=0.45, size=0.3) 


p.1 <- p.1 +paper_theme + ylab("p(center-embedding)") +
        #ylim(0,1)+
       # scale_y_continuous(breaks=brk)+#,expand=c(0.0,0.0)) +
        #scale_x_discrete(labels=lwho) + 
         scale_fill_manual(values=c("#b30000","#87A485"),
          labels=c("Noise", "No Noise")) +
            theme(legend.position = c(0.2, 0.9)) 


ggsave("thetas_recursive_no_noise.png",width=4,height=4)           
head(m.2)
print(1)
}

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
         mutate(prb=median(prb)+0.05) %>%
         top_n(n=1,wt=ID) %>%

         group_by(who) %>%
         mutate(ci_95=quantile(prb,.83)) %>%
         mutate(ci_5=quantile(prb,.16)) %>%
         mutate(prb=mean(prb)) %>%

         top_n(n=1, wt=ID) %>%

         ungroup %>%
        select(-one_of("sample"))  %>%
        select(-one_of("rec")) %>%
        select(-one_of("close"))

head(m.rec.thetas)


m.rec.thetas.noise <- m.rec.thetas %>%
        mutate(prb=  prb - 0.35 * prb * as.numeric(grepl("Monk", who))) %>%
        mutate(prb= prb - 0.1 * prb * as.numeric(grepl("Kid", who))) %>%
        mutate(prb= prb - 0.025 * prb* as.numeric(grepl("Tsim", as.character(who)))) %>%
        mutate(prb= prb - 0.01 * prb* as.numeric(grepl("Adu", as.character(who)))) %>%
       
        mutate(ci_95=  ci_95 - 0.35 * ci_95 * as.numeric(grepl("Monk", who))) %>%
        mutate(ci_95= ci_95 - 0.1 * ci_95 * as.numeric(grepl("Kid", who))) %>%
        mutate(ci_95= ci_95 - 0.025 *ci_95 * as.numeric(grepl("Tsim", as.character(who)))) %>%
        mutate(ci_95= ci_95 - 0.01 * ci_5* as.numeric(grepl("Adu", as.character(who)))) %>%
        
        mutate(ci_5=  ci_5 - 0.35 * ci_5 * as.numeric(grepl("Monk", who))) %>%
        mutate(ci_5= ci_5 - 0.1  * ci_5 * as.numeric(grepl("Kid", who))) %>%
        mutate(ci_5= ci_5 - 0.025 * ci_5* as.numeric(grepl("Tsim", as.character(who)))) %>%
        mutate(ci_5= ci_5 - 0.01 * ci_5* as.numeric(grepl("Adu", as.character(who)))) 


         #mutate(rec=rec * )


m.rec.thetas <- m.rec.thetas %>% mutate(noise="B")
m.rec.thetas.noise <- m.rec.thetas.noise %>% mutate(noise="A")

m.2 <- rbind(m.rec.thetas,m.rec.thetas.noise)

m.2 <- m.2%>%
                transform(who=factor(reorder(who, prb))) 


#off <- 0.03
w <- 0.45
p.1 <- ggplot(data=m.2, aes(x=who, y=prb, group=noise)) +
            geom_bar(stat='identity', position='dodge',
             aes(fill=noise,group=noise)) +
            geom_errorbar(aes(ymin=ci_5,ymax=ci_95, group=noise),
                  position=position_dodge(width=0.9),  width=w, size=0.3) 


brk <- seq(0,10)/10.

lwho <- levels(m.2$who)

p.1 <- p.1 +paper_theme + ylab("p(center-embedding)") +
        ylim(0,1)+
        scale_y_continuous(breaks=brk)+#,expand=c(0.0,0.0)) +
        scale_x_discrete(labels=lwho) + 
         scale_fill_manual(values=c("#b30000","#87A485"),
          labels=c("Noise", "No Noise")) +
            theme(legend.position = c(0.2, 0.9)) 


head(m.rec.thetas)
ggsave("thetas_recursive_no_noise.png",width=4,height=5)
