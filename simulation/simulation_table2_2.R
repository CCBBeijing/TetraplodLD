source("../util_FC.R")
source("../util_FC_m.R")
source("../FC_optim.R")
source("../FCm_optim.R")
library(Rsolnp)



pall <- c(0.6,0.45,0.12)
#############EM###############
###########haplotype
####100
allgeno <-c()
for(i in 1:1000){
  geno <- sim_geno_auto_m(allpar=pall,n=100)
  allgeno <- rbind(allgeno,geno)
}
t1<-Sys.time()
allres1 <- c()
for (i in 1:1000) {
  res1 <- EM_Ym_FC(geno=allgeno[i,])
  allres1 <- rbind(allres1,res1)
}
t2<-Sys.time()
t_optim_100_1 <- t2-t1
t1<-Sys.time()
allres2 <- c()
for (i in 1:1000) {
  res2 <- EM_Y_FC(geno=allgeno[i,])
  allres2 <- rbind(allres2,res2)
}
t2<-Sys.time()
t_optim_100_2 <- t2-t1
all_select_res <- c()
for (i in 1:1000) {
  select_res <- get_select_res(geno=allgeno[i,],res1 = allres1[i,],res2 = allres2[i,])
  all_select_res <- rbind(all_select_res,select_res)
}
length(which(all_select_res[,2]>0.05))/1000

###############200

allgeno <-c()
for(i in 1:1000){
  geno <- sim_geno_auto_m(allpar=pall,n=200)
  allgeno <- rbind(allgeno,geno)
}
t1<-Sys.time()
allres1 <- c()
for (i in 1:1000) {
  res1 <- EM_Ym_FC(geno=allgeno[i,])
  allres1 <- rbind(allres1,res1)
}
t2<-Sys.time()
t_optim_200_1 <- t2-t1
t1<-Sys.time()
allres2 <- c()
for (i in 1:1000) {
  res2 <- EM_Y_FC(geno=allgeno[i,])
  allres2 <- rbind(allres2,res2)
}
t2<-Sys.time()
t_optim_200_2 <- t2-t1
all_select_res <- c()
for (i in 1:1000) {
  select_res <- get_select_res(geno=allgeno[i,],res1 = allres1[i,],res2 = allres2[i,])
  all_select_res <- rbind(all_select_res,select_res)
}
length(which(all_select_res[,2]>0.05))/1000
###########400
allgeno <-c()
for(i in 1:1000){
  geno <- sim_geno_auto_m(allpar=pall,n=400)
  allgeno <- rbind(allgeno,geno)
}
t1<-Sys.time()
allres1 <- c()
for (i in 1:1000) {
  res1 <- EM_Ym_FC(geno=allgeno[i,])
  allres1 <- rbind(allres1,res1)
}
t2<-Sys.time()
t_optim_400_1 <- t2-t1
t1<-Sys.time()
allres2 <- c()
for (i in 1:1000) {
  res2 <- EM_Y_FC(geno=allgeno[i,])
  allres2 <- rbind(allres2,res2)
}
t2<-Sys.time()
t_optim_400_2 <- t2-t1
all_select_res <- c()
for (i in 1:1000) {
  select_res <- get_select_res(geno=allgeno[i,],res1 = allres1[i,],res2 = allres2[i,])
  all_select_res <- rbind(all_select_res,select_res)
}
length(which(all_select_res[,2]>0.05))/1000


#########Diplotype

#######100
pall <- c(0.6,0.45,0.12,0.12,0.02,0.02,0.02,0.01)
allgeno <-c()
for(i in 1:1000){
  geno <- sim_geno_auto(allpar=pall,n=100)
  allgeno <- rbind(allgeno,geno)
}
t1<-Sys.time()
allres1 <- c()
for (i in 1:1000) {
  res1 <- EM_Ym_FC(geno=allgeno[i,])
  allres1 <- rbind(allres1,res1)
}
t2<-Sys.time()
td_optim_100_1 <- t2-t1
t1<-Sys.time()
allres2 <- c()
for (i in 1:1000) {
  res2 <- EM_Y_FC(geno=allgeno[i,])
  allres2 <- rbind(allres2,res2)
}
t2<-Sys.time()
td_optim_100_2 <- t2-t1
all_select_res <- c()
for (i in 1:1000) {
  select_res <- get_select_res(geno=allgeno[i,],res1 = allres1[i,],res2 = allres2[i,])
  all_select_res <- rbind(all_select_res,select_res)
}
length(which(all_select_res[,2]>0.05))/1000
############200
allgeno <-c()
for(i in 1:1000){
  geno <- sim_geno_auto(allpar=pall,n=200)
  allgeno <- rbind(allgeno,geno)
}
t1<-Sys.time()
allres1 <- c()
for (i in 1:1000) {
  res1 <- EM_Ym_FC(geno=allgeno[i,])
  allres1 <- rbind(allres1,res1)
}
t2<-Sys.time()
td_optim_200_1 <- t2-t1
t1<-Sys.time()
allres2 <- c()
for (i in 1:1000) {
  res2 <- EM_Y_FC(geno=allgeno[i,])
  allres2 <- rbind(allres2,res2)
}
t2<-Sys.time()
td_optim_200_2 <- t2-t1
all_select_res <- c()
for (i in 1:1000) {
  select_res <- get_select_res(geno=allgeno[i,],res1 = allres1[i,],res2 = allres2[i,])
  all_select_res <- rbind(all_select_res,select_res)
}
length(which(all_select_res[,2]>0.05))/1000
##########400
allgeno <-c()
for(i in 1:1000){
  geno <- sim_geno_auto(allpar=pall,n=400)
  allgeno <- rbind(allgeno,geno)
}
t1<-Sys.time()
allres1 <- c()
for (i in 1:1000) {
  res1 <- EM_Ym_FC(geno=allgeno[i,])
  allres1 <- rbind(allres1,res1)
}
t2<-Sys.time()
td_optim_400_1 <- t2-t1
t1<-Sys.time()
allres2 <- c()
for (i in 1:1000) {
  res2 <- EM_Y_FC(geno=allgeno[i,])
  allres2 <- rbind(allres2,res2)
}
t2<-Sys.time()
td_optim_400_2 <- t2-t1
all_select_res <- c()
for (i in 1:1000) {
  select_res <- get_select_res(geno=allgeno[i,],res1 = allres1[i,],res2 = allres2[i,])
  all_select_res <- rbind(all_select_res,select_res)
}
length(which(all_select_res[,2]>0.05))/1000
