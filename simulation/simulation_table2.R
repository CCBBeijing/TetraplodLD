
#full_mix
pall <- c(0.6,0.45,0.12)
source("../util_FC.R")
source("../util_FC_m.R")
source("../FC_optim.R")
source("../FCm_optim.R")
library(Rsolnp)
#H_simulation
###############################100
t1<-Sys.time()
dih_100_em <- c()
for(ii in 1:20){
  cret_H_100_em <- c()
  for(i in 1:1000){
    geno <- sim_geno_auto_m(allpar=pall,n=100)
    cret_H_100_em <- rbind(cret_H_100_em,mix_est_f(geno=geno))
  }
  dih_100_em <- c(dih_100_em,length(which(cret_H_100_em[,2]>0.05))/1000)
  cat("ii=",ii,"\n")
}
mean(dih_100_em)
t2<-Sys.time()
t_em_100 <- t2-t1


t1<-Sys.time()
dih_100_optim <- c()
for(ii in 1:20){
  cret_H_100_optim <- c()
  for(i in 1:1000){
    geno <- sim_geno_auto_m(allpar=pall,n=100)
    cret_H_100_optim <- rbind(cret_H_100_optim,mix_est_f_Y(geno=geno))
  }
  dih_100_optim <- c(dih_100_optim,length(which(cret_H_100_optim[,2]>0.05))/1000)
  cat("ii=",ii,"\n")
}
mean(dih_100_optim)
t2<-Sys.time()
t_optim_100 <- t2-t1


################################200

t1<-Sys.time()
dih_200_em <- c()
for(ii in 1:20){
  cret_H_200_em <- c()
  for(i in 1:1000){
    geno <- sim_geno_auto_m(allpar=pall,n=200)
    cret_H_200_em <- rbind(cret_H_200_em,mix_est_f(geno=geno))
  }
  dih_200_em <- c(dih_200_em,length(which(cret_H_200_em[,2]>0.05))/1000)
  cat("ii=",ii,"\n")
}
mean(dih_200_em)
t2<-Sys.time()
t_em_200 <- t2-t1


t1<-Sys.time()
dih_200_optim <- c()
for(ii in 1:20){
  cret_H_200_optim <- c()
  for(i in 1:1000){
    geno <- sim_geno_auto_m(allpar=pall,n=200)
    cret_H_200_optim <- rbind(cret_H_200_optim,mix_est_f_Y(geno=geno))
  }
  dih_200_optim <- c(dih_200_optim,length(which(cret_H_200_optim[,2]>0.05))/1000)
  cat("ii=",ii,"\n")
}
mean(dih_200_optim)
t2<-Sys.time()
t_optim_200 <- t2-t1

########################################400
t1<-Sys.time()
dih_400_em <- c()
for(ii in 1:20){
  cret_H_400_em <- c()
  for(i in 1:1000){
    geno <- sim_geno_auto_m(allpar=pall,n=400)
    cret_H_400_em <- rbind(cret_H_400_em,mix_est_f(geno=geno))
  }
  dih_400_em <- c(dih_400_em,length(which(cret_H_400_em[,2]>0.05))/1000)
  cat("ii=",ii,"\n")
}
mean(dih_400_em)
t2<-Sys.time()
t_em_400 <- t2-t1


t1<-Sys.time()
dih_400_optim <- c()
for(ii in 1:20){
  cret_H_400_optim <- c()
  for(i in 1:1000){
    geno <- sim_geno_auto_m(allpar=pall,n=400)
    cret_H_400_optim <- rbind(cret_H_400_optim,mix_est_f_Y(geno=geno))
  }
  dih_400_optim <- c(dih_400_optim,length(which(cret_H_400_optim[,2]>0.05))/1000)
  cat("ii=",ii,"\n")
}
mean(dih_400_optim)
t2<-Sys.time()
t_optim_400 <- t2-t1




#D_simulation
pall <- c(0.6,0.45,0.12,0.12,0.02,0.02,0.02,0.01)
#source("../util_FC.R")
#source("../util_FC_m.R")
########################100
t1<-Sys.time()
did_100_em <- c()
for(ii in 1:20){
  cret_D_100_em <- c()
  for(i in 1:1000){
    geno <- sim_geno_auto(allpar=pall,n=100)
    cret_D_100_em <- rbind(cret_D_100_em,mix_est_f(geno=geno))
  }
  did_100_em <- c(did_100_em,length(which(cret_D_100_em[,2]>0.05))/1000)
  cat("ii=",ii,"\n")
}
mean(did_100_em)
t2<-Sys.time()
td_em_100 <- t2-t1


t1<-Sys.time()
did_100_optim <- c()
for(ii in 1:20){
  cret_D_100_optim <- c()
  for(i in 1:1000){
    geno <- sim_geno_auto(allpar=pall,n=100)
    cret_D_100_optim <- rbind(cret_D_100_optim,mix_est_f_Y(geno=geno))
  }
  did_100_optim <- c(did_100_optim,length(which(cret_D_100_optim[,2]>0.05))/1000)
  cat("ii=",ii,"\n")
}
mean(did_100_optim)
t2<-Sys.time()
td_optim_100 <- t2-t1





#####################################200

t1<-Sys.time()
did_200_em <- c()
for(ii in 1:20){
  cret_D_200_em <- c()
  for(i in 1:1000){
    geno <- sim_geno_auto(allpar=pall,n=200)
    cret_D_200_em <- rbind(cret_D_200_em,mix_est_f(geno=geno))
  }
  did_200_em <- c(did_200_em,length(which(cret_D_200_em[,2]>0.05))/1000)
  cat("ii=",ii,"\n")
}
mean(did_200_em)
t2<-Sys.time()
td_em_200 <- t2-t1


t1<-Sys.time()
did_200_optim <- c()
for(ii in 1:20){
  cret_D_200_optim <- c()
  for(i in 1:1000){
    geno <- sim_geno_auto(allpar=pall,n=200)
    cret_D_200_optim <- rbind(cret_D_200_optim,mix_est_f_Y(geno=geno))
  }
  did_200_optim <- c(did_200_optim,length(which(cret_D_200_optim[,2]>0.05))/1000)
  cat("ii=",ii,"\n")
}
mean(did_200_optim)
t2<-Sys.time()
td_optim_200 <- t2-t1


#####################################400

t1<-Sys.time()
did_400_em <- c()
for(ii in 1:20){
  cret_D_400_em <- c()
  for(i in 1:1000){
    geno <- sim_geno_auto(allpar=pall,n=400)
    cret_D_400_em <- rbind(cret_D_400_em,mix_est_f(geno=geno))
  }
  did_400_em <- c(did_400_em,length(which(cret_D_400_em[,2]>0.05))/1000)
  cat("ii=",ii,"\n")
}
mean(did_400_em)
t2<-Sys.time()
td_em_400 <- t2-t1


t1<-Sys.time()
did_400_optim <- c()
for(ii in 1:20){
  cret_D_400_optim <- c()
  for(i in 1:1000){
    geno <- sim_geno_auto(allpar=pall,n=400)
    cret_D_400_optim <- rbind(cret_D_400_optim,mix_est_f_Y(geno=geno))
  }
  did_400_optim <- c(did_400_optim,length(which(cret_D_400_optim[,2]>0.05))/1000)
  cat("ii=",ii,"\n")
}
mean(did_400_optim)
t2<-Sys.time()
td_optim_400 <- t2-t1


