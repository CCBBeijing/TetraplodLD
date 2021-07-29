

source("util_PC_m.R")
source("util_PC.R")
source("LD.R")
load("dat.RData")



chr03a_i <- which(dat$info$chr=="Chr03a")
chr04a_i <- which(dat$info$chr=="Chr04a")

chr03a_mf <- dat$info[chr03a_i,]
chr04a_mf <- dat$info[chr04a_i,]
chr <- rbind(chr03a_mf,chr04a_mf)

chr03a_ml <- dat$low[,chr03a_i]
chr04a_ml <- dat$low[,chr04a_i]
chr_l <- cbind(chr03a_ml,chr04a_ml)

chr03a_mu <- dat$up[,chr03a_i]
chr04a_mu <- dat$up[,chr04a_i]
chr_u <- cbind(chr03a_mu,chr04a_mu)


LD_03_l <- work_test1_mix(M=chr03a_ml,mn=chr03a_mf[,1])
LD_04_l <- work_test1_mix(M=chr04a_ml,mn=chr04a_mf[,1])
LD_c_l <- work_test1_mix(M=chr_l,mn=chr[,1])


LD_03_u <- work_test1_mix(M=chr03a_mu,mn=chr03a_mf[,1])
LD_04_u <- work_test1_mix(M=chr04a_mu,mn=chr04a_mf[,1])
LD_c_u <- work_test1_mix(M=chr_u,mn=chr[,1])




#normalize
LDn2_03_ls <- LD_chr(dn=LD_03_l[,16:19],mi=chr03a_mf)
LDn2_04_ls <- LD_chr(dn=LD_04_l[,16:19],mi=chr04a_mf)

LDn2_c_ls <- LD_chr_inv(dn=LD_c_l[,16:19],mi=chr)
LDn2_c_ls1 <- Lcs(l1=LDn2_03_ls,l2=LDn2_04_ls,l3=LDn2_c_ls)


LDn2_03_us <- LD_chr(dn=LD_03_u[,16:19],mi=chr03a_mf)
LDn2_04_us <- LD_chr(dn=LD_04_u[,16:19],mi=chr04a_mf)

LDn2_c_us <- LD_chr_inv(dn=LD_c_u[,16:19],mi=chr)
LDn2_c_us1 <- Lcs(l1=LDn2_03_us,l2=LDn2_04_us,l3=LDn2_c_us)






y1=LDn2_03_ls
y2=LDn2_04_ls
y3=LDn2_c_ls1

x1=LDn2_03_us
x2=LDn2_04_us
x3=LDn2_c_us1



x1_1 <- x1[which(x1[,1]<1),];x1_2 <- x1[which(x1[,1]>1),];
x2_1 <- x2[which(x2[,1]<1),];x2_2 <- x2[which(x2[,1]>1),];

y1_1 <- y1[which(y1[,1]<1),];y1_2 <- y1[which(y1[,1]>1),];
y2_1 <- y2[which(y2[,1]<1),];y2_2 <- y2[which(y2[,1]>1),];

Deab_u <- c(mean(x1_1[,2]),mean(x1_2[,2]),mean(x3[,2]),mean(x2_1[,2]),mean(x2_2[,2]))
DAb_u <- c(mean(x1_1[,3]),mean(x1_2[,3]),mean(x3[,3]),mean(x2_1[,3]),mean(x2_2[,3]))
DaB_u <- c(mean(x1_1[,4]),mean(x1_2[,4]),mean(x3[,4]),mean(x2_1[,4]),mean(x2_2[,4]))
DAB_u <- c(mean(x1_1[,5]),mean(x1_2[,5]),mean(x3[,5]),mean(x2_1[,5]),mean(x2_2[,5]))

Deab_l <- c(mean(y1_1[,2]),mean(y1_2[,2]),mean(y3[,2]),mean(y2_1[,2]),mean(y2_2[,2]))
DAb_l <- c(mean(y1_1[,3]),mean(y1_2[,3]),mean(y3[,3]),mean(y2_1[,3]),mean(y2_2[,3]))
DaB_l <- c(mean(y1_1[,4]),mean(y1_2[,4]),mean(y3[,4]),mean(y2_1[,4]),mean(y2_2[,4]))
DAB_l <- c(mean(y1_1[,5]),mean(y1_2[,5]),mean(y3[,5]),mean(y2_1[,5]),mean(y2_2[,5]))


pplot <- list()
library(ggplot2)
lk7a1 <- c(1,5,1,5)
lk7a2 <- c(2,6,2,6)
lk7a3 <- c(3.5,3.5)

lk7b1<- c(Deab_u[1],Deab_u[4],-Deab_l[1],-Deab_l[4])
lk7b2 <- c(Deab_u[2],Deab_u[5],-Deab_l[2],-Deab_l[5])
lk7b3 <- c(Deab_u[3],-Deab_l[3])
dat1 <- data.frame(x=lk7a1,y=lk7b1)
dat2 <- data.frame(x=lk7a2,y=lk7b2)
dat3 <- data.frame(x=lk7a3,y=lk7b3)



tt <- ggplot()
tt <-tt + geom_bar(data = dat1 ,mapping=aes(x=x,y=y),stat = "identity",col="orange",fill="orange",width = 0.8)#+geom_text(data=dat1,aes(label=z),vjust=1.5)

tt <-tt + geom_bar(data = dat2 ,mapping=aes(x,y),stat = "identity",col="mediumpurple",fill="mediumpurple",width = 0.8)
tt <-tt + geom_bar(data = dat3 ,mapping=aes(x,y),stat = "identity",col="brown2",fill="brown2",width = 0.8)
tt <- tt+xlab(NULL)+ylab(NULL)
tt <- tt +scale_y_continuous(breaks = seq(-0.6,0.6,by = 0.2),expand = c(0,0), labels = c(seq(0.6,0,by = -0.2),seq(0.2,0.6,by = 0.2)),limits = c(-0.6, 0.6))
tt <-tt +scale_x_continuous(breaks = c(1.5,3.5,5.5),labels = NULL,limits = c(0.5, 6.5))
tt <- tt +annotate("segment",x=0.5,xend = 6.5,y=0,yend = 0,linetype="dashed",colour="black",size=1.5)

tt <- tt+ theme_bw() +
  theme(panel.grid.major=element_line(colour=NA),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank()
  )

tt <- tt+theme(axis.text.x = element_text( color="black", size=14),axis.text.y =element_text( color="black", size=20))+
  theme(axis.ticks.length.y = unit(0.3,"cm"))+theme(axis.ticks.length.x = unit(0.3,"cm"))


tt
pplot[[1]] <- tt

###############################################################################################
lk7a1 <- c(1,5,1,5)
lk7a2 <- c(2,6,2,6)
lk7a3 <- c(3.5,3.5)

lk7b1<- c(DAb_u[1],DAb_u[4],-DAb_l[1],-DAb_l[4])
lk7b2 <- c(DAb_u[2],DAb_u[5],-DAb_l[2],-DAb_l[5])
lk7b3 <- c(DAb_u[3],-DAb_l[3])
dat1 <- data.frame(x=lk7a1,y=lk7b1)
dat2 <- data.frame(x=lk7a2,y=lk7b2)
dat3 <- data.frame(x=lk7a3,y=lk7b3)



tt <- ggplot()
tt <-tt + geom_bar(data = dat1 ,mapping=aes(x=x,y=y),stat = "identity",col="orange",fill="orange",width = 0.8)
tt <-tt + geom_bar(data = dat2 ,mapping=aes(x,y),stat = "identity",col="mediumpurple",fill="mediumpurple",width = 0.8)
tt <-tt + geom_bar(data = dat3 ,mapping=aes(x,y),stat = "identity",col="brown2",fill="brown2",width = 0.8)
tt <- tt+xlab(NULL)+ylab(NULL)
tt <- tt +scale_y_continuous(breaks = seq(-0.4,0.4,by = 0.2),expand = c(0,0), labels = c(seq(0.4,0,by = -0.2),seq(0.2,0.4,by = 0.2)),limits = c(-0.4, 0.4))
tt <-tt +scale_x_continuous(breaks = c(1.5,3.5,5.5),labels = NULL,limits = c(0.5, 6.5))
tt <- tt +annotate("segment",x=0.5,xend = 6.5,y=0,yend = 0,linetype="dashed",colour="black",size=1.5)

tt <- tt+ theme_bw() +
  theme(panel.grid.major=element_line(colour=NA),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank()
  )

tt <- tt+theme(axis.text.x = element_text( color="black", size=14),axis.text.y =element_text( color="black", size=20))+
  theme(axis.ticks.length.y = unit(0.3,"cm"))+theme(axis.ticks.length.x = unit(0.3,"cm"))


tt
pplot[[2]] <- tt
##################################################
lk7a1 <- c(1,5,1,5)
lk7a2 <- c(2,6,2,6)
lk7a3 <- c(3.5,3.5)


lk7b1<- c(DaB_u[1],DaB_u[4],-DaB_l[1],-DaB_l[4])
lk7b2 <- c(DaB_u[2],DaB_u[5],-DaB_l[2],-DaB_l[5])
lk7b3 <- c(DaB_u[3],-DaB_l[3])
dat1 <- data.frame(x=lk7a1,y=lk7b1)
dat2 <- data.frame(x=lk7a2,y=lk7b2)
dat3 <- data.frame(x=lk7a3,y=lk7b3)



tt <- ggplot()
tt <-tt + geom_bar(data = dat1 ,mapping=aes(x=x,y=y),stat = "identity",col="orange",fill="orange",width = 0.8)
tt <-tt + geom_bar(data = dat2 ,mapping=aes(x,y),stat = "identity",col="mediumpurple",fill="mediumpurple",width = 0.8)
tt <-tt + geom_bar(data = dat3 ,mapping=aes(x,y),stat = "identity",col="brown2",fill="brown2",width = 0.8)
tt <- tt+xlab(NULL)+ylab(NULL)
tt <- tt +scale_y_continuous(breaks = seq(-0.4,0.4,by = 0.2),expand = c(0,0), labels = c(seq(0.4,0,by = -0.2),seq(0.2,0.4,by = 0.2)),limits = c(-0.4, 0.4))
tt <-tt +scale_x_continuous(breaks = c(1.5,3.5,5.5),labels = NULL,limits = c(0.5, 6.5))
tt <- tt +annotate("segment",x=0.5,xend = 6.5,y=0,yend = 0,linetype="dashed",colour="black",size=1.5)

tt <- tt+ theme_bw() +
  theme(panel.grid.major=element_line(colour=NA),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank()
  )

tt <- tt+theme(axis.text.x = element_text( color="black", size=14),axis.text.y =element_text( color="black", size=20))+
  theme(axis.ticks.length.y = unit(0.3,"cm"))+theme(axis.ticks.length.x = unit(0.3,"cm"))


tt
pplot[[3]] <- tt
############################################################
lk7a1 <- c(1,5,1,5)
lk7a2 <- c(2,6,2,6)
lk7a3 <- c(3.5,3.5)

lk7b1<- c(DAB_u[1],DAB_u[4],-DAB_l[1],-DAB_l[4])
lk7b2 <- c(DAB_u[2],DAB_u[5],-DAB_l[2],-DAB_l[5])
lk7b3 <- c(DAB_u[3],-DAB_l[3])
dat1 <- data.frame(x=lk7a1,y=lk7b1)
dat2 <- data.frame(x=lk7a2,y=lk7b2)
dat3 <- data.frame(x=lk7a3,y=lk7b3)



tt <- ggplot()
tt <-tt + geom_bar(data = dat1 ,mapping=aes(x=x,y=y),stat = "identity",col="orange",fill="orange",width = 0.8)
tt <-tt + geom_bar(data = dat2 ,mapping=aes(x,y),stat = "identity",col="mediumpurple",fill="mediumpurple",width = 0.8)
tt <-tt + geom_bar(data = dat3 ,mapping=aes(x,y),stat = "identity",col="brown2",fill="brown2",width = 0.8)
tt <- tt+xlab(NULL)+ylab(NULL)
tt <- tt +scale_y_continuous(breaks = seq(-0.4,0.4,by = 0.2),expand = c(0,0), labels = c(seq(0.4,0,by = -0.2),seq(0.2,0.4,by = 0.2)),limits = c(-0.4, 0.4))
tt <-tt +scale_x_continuous(breaks = c(1.5,3.5,5.5),labels = NULL,limits = c(0.5, 6.5))
tt <- tt +annotate("segment",x=0.5,xend = 6.5,y=0,yend = 0,linetype="dashed",colour="black",size=1.5)

tt <- tt+ theme_bw() +
  theme(panel.grid.major=element_line(colour=NA),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank()
  )

tt <- tt+theme(axis.text.x = element_text( color="black", size=14),axis.text.y =element_text( color="black", size=20))+
  theme(axis.ticks.length.y = unit(0.3,"cm"))+theme(axis.ticks.length.x = unit(0.3,"cm"))


tt
pplot[[4]] <- tt
library(patchwork)
pplot[[1]] + pplot[[2]]+pplot[[3]]+pplot[[4]]
