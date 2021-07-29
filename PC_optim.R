DF10 <- function(pall){
  
  
  pA <- pall[1]
  pa <- 1-pA
  pB <- pall[2]
  pb <- 1-pB
  DA <- pall[3]
  DB <- pall[4]
  Deab <- pall[5]
  DAb <- pall[6]
  DaB <- pall[7]
  DAB <- pall[8]
  
  PAABB <- pA^2*pB^2+pA^2*DB+pB^2*DA+2*pA*pB*Deab+2*pB*DAb+2*pA*DaB+DA*DB+Deab^2+DAB
  
  PAABb <- 2*(pA^2*pB*pb-pA^2*DB+pB*pb*DA+(pA*pb-pA*pB)*Deab+(pb-pB)*DAb-2*pA*DaB)-2*(DA*DB+Deab^2+DAB)
  
  PAAbb <- pA^2*pb^2+pA^2*DB+pb^2*DA-2*pA*pb*Deab-2*pb*DAb+2*pA*DaB+DA*DB+Deab^2+DAB
  
  PAaBB <- 2*(pA*pa*pB^2+pA*pa*DB-pB^2*DA+(pa*pB-pA*pB)*Deab-2*pB*DAb+(pa-pA)*DaB)-2*(DA*DB+Deab^2+DAB)
  
  PAaBb <- 2*(pA*pa*pB*pb-pA*pa*DB-pB*pb*DA+(pA*pB+pa*pb)*Deab-(pA*pb+pa*pB)*Deab+(pB-pb)*DAb+(pA-pa)*DaB)+
    2*(pA*pa*pB*pb-pA*pa*DB-pB*pb*DA+(pB-pb)*DAb+(pA-pa)*DaB)+4*(DA*DB+Deab^2+DAB)
  
  PAabb <- 2*(pA*pa*pb^2+pA*pa*DB-pb^2*DA+(pA*pb-pa*pb)*Deab+2*pb*DAb+(pa-pA)*DaB)-2*(DA*DB+Deab^2+DAB)
  
  PaaBB <- pa^2*pB^2+pa^2*DB+pB^2*DA-2*pa*pB*Deab+2*pB*DAb-2*pa*DaB +DA*DB+Deab^2+DAB
  PaaBb <- 2*(pa^2*pB*pb-pa^2*DB+pB*pb*DA+(pa*pB-pa*pb)*Deab+(pb-pB)*DAb+2*pa*DaB)-2*(DA*DB+Deab^2+DAB)
  Paabb <- pa^2*pb^2+pb^2*DA+pa^2*DB+2*pa*pb*Deab-2*pb*DAb-2*pa*DaB+DA*DB+Deab^2+DAB
  
  tmp <- c(PAABB,PAABb,PAAbb,PAaBB,PAaBb,PAabb,PaaBB,PaaBb,Paabb)
  #sum(tmp)
  return(tmp)
}



autoP_DF10 <- function(pall){
  
  
  gp <- DF10(pall=pall)
  
  PAABB <- gp[1]
  PAABb <- gp[2]
  PAAbb <- gp[3] 
  PAaBB <- gp[4] 
  PAaBb <- gp[5] 
  PAabb <- gp[6] 
  PaaBB <- gp[7] 
  PaaBb <- gp[8] 
  Paabb <- gp[9] 
  
  zy1 <- (PAABB)^2 
  zy2 <- 2*PAABB*PAABb + 2*PAABB*PAAbb+(PAABb)^2 + 2*PAABb*PAAbb
  zy3 <- (PAAbb)^2
  
  zy4 <- 2*PAABB*PAaBB + (PAaBB)^2+2*PAABB*PaaBB + 2*PAaBB*PaaBB
  
  
  zy5 <- 2*PAABB*PAaBb+2*PAaBB*PAABb + 2*PAABb*PAaBb+2*PAaBB*PAAbb+2*PAabb*PAABB + 2*PAAbb*PAaBb+2*PAabb*PAABb+
    2*PAaBB*PAaBb+2*PAABB*PaaBb+2*PAABb*PaaBB+ (PAaBb)^2+2*PAABb*PaaBb+2*PAABB*Paabb+2*PAAbb*PaaBB+2*PAabb*PAaBB+
    2*PAabb*PAaBb+2*PAABb*Paabb+2*PAAbb*PaaBb+2*PaaBB*PAaBb+2*PaaBb*PAaBB+2*PaaBb*PAaBb+2*Paabb*PAaBB+2*PaaBB*PAabb+
    2*Paabb*PAaBb+2*PaaBb*PAabb
  
  
  zy6 <- 2*PAabb*PAAbb + (PAabb)^2+2*PAAbb*Paabb + 2*Paabb*PAabb
  
  zy7 <- (PaaBB)^2
  zy8 <- 2*PaaBB*PaaBb + (PaaBb)^2+2*PaaBB*Paabb + 2*PaaBb*Paabb
  zy9 <- (Paabb)^2
  
  tmp <- c(zy1,zy2,zy3,zy4,zy5,zy6,zy7,zy8,zy9)
  #sum(tmp)
  return(tmp)
}


autoP_DF10new <- function(pall){
  
  
  gp <- pall
  
  PAABB <- gp[1]
  PAABb <- gp[2]
  PAAbb <- gp[3] 
  PAaBB <- gp[4] 
  PAaBb <- gp[5] 
  PAabb <- gp[6] 
  PaaBB <- gp[7] 
  PaaBb <- gp[8] 
  Paabb <- gp[9] 
  
  zy1 <- (PAABB)^2 
  zy2 <- 2*PAABB*PAABb + 2*PAABB*PAAbb+(PAABb)^2 + 2*PAABb*PAAbb
  zy3 <- (PAAbb)^2
  
  zy4 <- 2*PAABB*PAaBB + (PAaBB)^2+2*PAABB*PaaBB + 2*PAaBB*PaaBB
  
  
  zy5 <- 2*PAABB*PAaBb+2*PAaBB*PAABb + 2*PAABb*PAaBb+2*PAaBB*PAAbb+2*PAabb*PAABB + 2*PAAbb*PAaBb+2*PAabb*PAABb+
    2*PAaBB*PAaBb+2*PAABB*PaaBb+2*PAABb*PaaBB+ (PAaBb)^2+2*PAABb*PaaBb+2*PAABB*Paabb+2*PAAbb*PaaBB+2*PAabb*PAaBB+
    2*PAabb*PAaBb+2*PAABb*Paabb+2*PAAbb*PaaBb+2*PaaBB*PAaBb+2*PaaBb*PAaBB+2*PaaBb*PAaBb+2*Paabb*PAaBB+2*PaaBB*PAabb+
    2*Paabb*PAaBb+2*PaaBb*PAabb
  
  
  zy6 <- 2*PAabb*PAAbb + (PAabb)^2+2*PAAbb*Paabb + 2*Paabb*PAabb
  
  zy7 <- (PaaBB)^2
  zy8 <- 2*PaaBB*PaaBb + (PaaBb)^2+2*PaaBB*Paabb + 2*PaaBb*Paabb
  zy9 <- (Paabb)^2
  
  tmp <- c(zy1,zy2,zy3,zy4,zy5,zy6,zy7,zy8,zy9)
  #sum(tmp)
  return(tmp)
}

autoP_DF10eqn <- function(pall){ sum(pall)}

sim_geno_autoP  <- function(allpar,n=1000){
  
  
  conp <- autoP_DF10(pall=allpar)
  
  id <- sample(1:9,n,replace = T,prob=conp)
  
  return(id)
}

DF10eqn <- function(pall){
  
  
  pA <- pall[1]
  pa <- 1-pA
  pB <- pall[2]
  pb <- 1-pB
  DA <- pall[3]
  DB <- pall[4]
  Deab <- pall[5]
  DAb <- pall[6]
  DaB <- pall[7]
  DAB <- pall[8]
  
  PAABB <- pA^2*pB^2+pA^2*DB+pB^2*DA+2*pA*pB*Deab+2*pB*DAb+2*pA*DaB+DA*DB+Deab^2+DAB
  
  PAABb <- 2*(pA^2*pB*pb-pA^2*DB+pB*pb*DA+(pA*pb-pA*pB)*Deab+(pb-pB)*DAb-2*pA*DaB)-2*(DA*DB+Deab^2+DAB)
  
  PAAbb <- pA^2*pb^2+pA^2*DB+pb^2*DA-2*pA*pb*Deab-2*pb*DAb+2*pA*DaB+DA*DB+Deab^2+DAB
  
  PAaBB <- 2*(pA*pa*pB^2+pA*pa*DB-pB^2*DA+(pa*pB-pA*pB)*Deab-2*pB*DAb+(pa-pA)*DaB)-2*(DA*DB+Deab^2+DAB)
  
  PAaBb <- 2*(pA*pa*pB*pb-pA*pa*DB-pB*pb*DA+(pA*pB+pa*pb)*Deab-(pA*pb+pa*pB)*Deab+(pB-pb)*DAb+(pA-pa)*DaB)+
    2*(pA*pa*pB*pb-pA*pa*DB-pB*pb*DA+(pB-pb)*DAb+(pA-pa)*DaB)+4*(DA*DB+Deab^2+DAB)
  
  PAabb <- 2*(pA*pa*pb^2+pA*pa*DB-pb^2*DA+(pA*pb-pa*pb)*Deab+2*pb*DAb+(pa-pA)*DaB)-2*(DA*DB+Deab^2+DAB)
  
  PaaBB <- pa^2*pB^2+pa^2*DB+pB^2*DA-2*pa*pB*Deab+2*pB*DAb-2*pa*DaB +DA*DB+Deab^2+DAB
  PaaBb <- 2*(pa^2*pB*pb-pa^2*DB+pB*pb*DA+(pa*pB-pa*pb)*Deab+(pb-pB)*DAb+2*pa*DaB)-2*(DA*DB+Deab^2+DAB)
  Paabb <- pa^2*pb^2+pb^2*DA+pa^2*DB+2*pa*pb*Deab-2*pb*DAb-2*pa*DaB+DA*DB+Deab^2+DAB
  
  tmp <- c(PAABB,PAABb,PAAbb,PAaBB,PAaBb,PAabb,PaaBB,PaaBb,Paabb)
  
  #sum(tmp)
  return(sum(tmp))
}
DF10ineqn <- function(pall){
  
  
  pA <- pall[1]
  pa <- 1-pA
  pB <- pall[2]
  pb <- 1-pB
  DA <- pall[3]
  DB <- pall[4]
  Deab <- pall[5]
  DAb <- pall[6]
  DaB <- pall[7]
  DAB <- pall[8]
  
  PAABB <- pA^2*pB^2+pA^2*DB+pB^2*DA+2*pA*pB*Deab+2*pB*DAb+2*pA*DaB+DA*DB+Deab^2+DAB
  
  PAABb <- 2*(pA^2*pB*pb-pA^2*DB+pB*pb*DA+(pA*pb-pA*pB)*Deab+(pb-pB)*DAb-2*pA*DaB)-2*(DA*DB+Deab^2+DAB)
  
  PAAbb <- pA^2*pb^2+pA^2*DB+pb^2*DA-2*pA*pb*Deab-2*pb*DAb+2*pA*DaB+DA*DB+Deab^2+DAB
  
  PAaBB <- 2*(pA*pa*pB^2+pA*pa*DB-pB^2*DA+(pa*pB-pA*pB)*Deab-2*pB*DAb+(pa-pA)*DaB)-2*(DA*DB+Deab^2+DAB)
  
  PAaBb <- 2*(pA*pa*pB*pb-pA*pa*DB-pB*pb*DA+(pA*pB+pa*pb)*Deab-(pA*pb+pa*pB)*Deab+(pB-pb)*DAb+(pA-pa)*DaB)+
    2*(pA*pa*pB*pb-pA*pa*DB-pB*pb*DA+(pB-pb)*DAb+(pA-pa)*DaB)+4*(DA*DB+Deab^2+DAB)
  
  PAabb <- 2*(pA*pa*pb^2+pA*pa*DB-pb^2*DA+(pA*pb-pa*pb)*Deab+2*pb*DAb+(pa-pA)*DaB)-2*(DA*DB+Deab^2+DAB)
  
  PaaBB <- pa^2*pB^2+pa^2*DB+pB^2*DA-2*pa*pB*Deab+2*pB*DAb-2*pa*DaB +DA*DB+Deab^2+DAB
  PaaBb <- 2*(pa^2*pB*pb-pa^2*DB+pB*pb*DA+(pa*pB-pa*pb)*Deab+(pb-pB)*DAb+2*pa*DaB)-2*(DA*DB+Deab^2+DAB)
  Paabb <- pa^2*pb^2+pb^2*DA+pa^2*DB+2*pa*pb*Deab-2*pb*DAb-2*pa*DaB+DA*DB+Deab^2+DAB
  
  tmp <- c(PAABB,PAABb,PAAbb,PAaBB,PAaBb,PAabb,PaaBB,PaaBb,Paabb)
  
  #sum(tmp)
  return(tmp)
}


LL2 <- function(pall){
  
  ALLP2 <- autoP_DF10new(pall)
  L2 <- -sum(nts*log(ALLP2))
  return(L2)
}

cons1 <- function(para){
  
  pA <- para[1]
  pB <- para[2]
  pa <- 1-pA
  pb <- 1-pB
  
  DAi <- c(max(c(-pA^2,-pa^2)),pA*pa)
  DAl <- LD_norm(para[3],conv=DAi)
  
  DBi <- c(max(c(-pB^2,-pb^2)),pB*pb)
  DBl <- LD_norm(para[4],conv=DBi)
  
  Deabi <- c(max(c(-2*pA*pB,-2*pa*pb)),
             min(2*pA*pb,2*pa*pB))
  Deabl <- LD_norm(para[5],conv=Deabi)
  
  pmAb <- expand.grid(DA=c(min(c(-pA^2,-pa^2)),pA*pa),Deab= c(min(c(-2*pA*pB,-2*pa*pb)),
                                                              max(2*pA*pb,2*pa*pB)))
  tmpDAbf <- function(DP,pA,pa,pB,pb){
    DA <- DP[1]
    Deab <- DP[2]
    c(-pA*Deab-pB*DA-pA^2*pB,-pA*Deab+pb*DA+pA^2*pb,-(pA-pa)/2*Deab-pB*DA+pA*pa*pB,
      -(pA-pa)/2*Deab+pb*DA-pA*pa*pb,pa*Deab-pB*DA-pa^2*pB,pa*Deab+pb*DA+pa^2*pb)
  }
  tmpDAb_1 <- (apply(pmAb,1,tmpDAbf,pA=pA,pa=pa,pB=pB,pb=pb))
  tmpDAb <- del_norm(ptt=tmpDAb_1,the=para[6])
  DAbi <- c(max(tmpDAb[which(tmpDAb<0)],na.rm=T),min(tmpDAb[which(tmpDAb>0)],na.rm=T))
  DAbl <- LD_norm(para[6],conv=DAbi)
  
  pmaB <- expand.grid(DB=c(min(c(-pB^2,-pb^2)),pB*pb),Deab= c(min(c(-2*pA*pB,-2*pa*pb)),
                                                              max(2*pA*pb,2*pa*pB)))
  tmpDaBf <- function(DP,pA,pa,pB,pb){
    DB <- DP[1]
    Deab <- DP[2]
    c(-pB*Deab-pA*DB-pA*pB^2,-pB*Deab+pa*DB+pa*pB^2,-(pB-pb)/2*Deab-pA*DB+pA*pB*pb,
      -(pB-pb)/2*Deab+pa*DB-pa*pB*pb,pb*Deab-pA*DB-pA*pb^2,pb*Deab+pa*DB+pa*pb^2)
  }
  tmpDaB_1 <- (apply(pmaB,1,tmpDaBf,pA=pA,pa=pa,pB=pB,pb=pb))
  tmpDaB <- del_norm(ptt=tmpDaB_1,the=para[7])
  DaBi <- c(max(tmpDaB[which(tmpDaB<0)],na.rm = T),min(tmpDaB[which(tmpDaB>0)],na.rm=T))
  DaBl <- LD_norm(para[7],conv=DaBi)
  
  pmAB <- expand.grid(DA=c(min(c(-pA^2,-pa^2)),pA*pa),DB=c(min(c(-pB^2,-pb^2)),pB*pb),
                      Deab= c(min(c(-2*pA*pB,-2*pa*pb)),max(2*pA*pb,2*pa*pB)),
                      DAb=c(min(tmpDAb[which(tmpDAb<0)],na.rm = T),max(tmpDAb[which(tmpDAb>0)],na.rm=T)),
                      DaB=c(min(tmpDaB[which(tmpDaB<0)],na.rm = T),max(tmpDaB[which(tmpDaB>0)],na.rm=T)))
  tmpDABf <- function(DP,pA,pa,pB,pb){
    DA <- DP[1]
    DB <- DP[2]
    Deab <- DP[3]
    DAb <- DP[4]
    DaB <- DP[5]
    c(-(pA^2*pB^2+pA^2*DB+pB^2*DA+2*pA*pB*Deab+2*pB*DAb+2*pA*DaB+DA*DB+Deab^2),
      pA^2*pB*pb-pA^2*DB+pB*pb*DA+(pA*pb-pA*pB)*Deab+(pb-pB)*DAb-2*pA*DaB-DA*DB-Deab^2,
      -(pA^2*pb^2+pA^2*DB+pb^2*DA-2*pA*pb*Deab-2*pb*DAb+2*pA*DaB+DA*DB+Deab^2),
      pA*pa*pB^2+pA*pa*DB-pB^2*DA+(pa*pB-pA*pB)*Deab-2*pB*DAb+(pa-pA)*DaB-DA*DB-Deab^2,
      (-(2*(pA*pa*pB*pb-pA*pa*DB-pB*pb*DA+(pA*pB+pa*pb)*Deab-(pA*pb+pa*pB)*Deab+(pB-pb)*DAb+(pA-pa)*DaB)+
           2*(pA*pa*pB*pb-pA*pa*DB-pB*pb*DA+(pB-pb)*DAb+(pA-pa)*DaB))-4*DA*DB-4*Deab^2)/4,
      pA*pa*pb^2+pA*pa*DB-pb^2*DA+(pA*pb-pa*pb)*Deab+2*pb*DAb+(pa-pA)*DaB-DA*DB-Deab^2,
      -(pa^2*pB^2+pa^2*DB+pB^2*DA-2*pa*pB*Deab+2*pB*DAb-2*pa*DaB +DA*DB+Deab^2),
      pa^2*pB*pb-pa^2*DB+pB*pb*DA+(pa*pB-pa*pb)*Deab+(pb-pB)*DAb+2*pa*DaB-DA*DB-Deab^2,
      -(pa^2*pb^2+pb^2*DA+pa^2*DB+2*pa*pb*Deab-2*pb*DAb-2*pa*DaB+DA*DB+Deab^2))
  }
  
  tmpDAB_1 <- (apply(pmAB,1,tmpDABf,pA=pA,pa=pa,pB=pB,pb=pb))
  tmpDAB <- del_norm(ptt=tmpDAB_1,the=para[8])
  DABi <- c(max(tmpDAB[which(tmpDAB<0)],na.rm=T),min(tmpDAB[which(tmpDAB>0)],na.rm=T))
  DABl <- LD_norm(para[8],conv=DABi)
  consv <- c(DAl,DBl,Deabl,DAbl,DaBl,DABl)
  return(consv)
}

LDeqfun <- function(par){
  pA <- par[1]
  pB <- par[2]
  pa <- 1-pA
  pb <- 1-pB
  
  DAi <- c(max(c(-pA^2,-pa^2)),pA*pa)
  z1_1 <- DA-DAi[1]
  z1_2 <- DA-DAi[2]
  DBi <- c(max(c(-pB^2,-pb^2)),pB*pb)
  Dabi <- c(max(c(-2*pA*pB,-2*pa*pb)),
             min(2*pA*pb,2*pa*pB))
  
}
library(Rsolnp)
EM_Y <- function(geno){
  
  nts <- rep(0,9)
  nt <- table(geno)
  nts[as.numeric(names(nt))] <- as.numeric(nt)
  nn <- sum(nts)
  nts<<- nts

 
  

  res <-  solnp(rep(1/9,9),fun = LL2,eqfun = autoP_DF10eqn,
                eqB = 1,LB=rep(0,9),UB = rep(1,9),control = list(trace=0))
  res4 <- res$pars
  pAA <- sum(res4[1:3])
  pAa <- sum(res4[4:6])
  paa <- sum(res4[7:9])
  
  pBB <- sum(res4[c(1,4,7)])
  pBb <- sum(res4[c(2,5,8)])
  pbb <- sum(res4[c(3,6,9)])
  
  pA <- pAA+pAa/2;pa <- paa+pAa/2;
  pB <- pBB+pBb/2;pb <- pbb+pBb/2;
  
  DA <- pAA-pA^2;DB <- pBB-pB^2;
  
  #Deab
  pAB <- (2*res4[1] + sum(res4[c(2,4)])+res4[5]/2)/2
  pAb <- (2*res4[3] + sum(res4[c(2,6)])+res4[5]/2)/2
  paB <- (2*res4[7] + sum(res4[c(4,8)])+res4[5]/2)/2
  pab <- (2*res4[9] + sum(res4[c(6,8)])+res4[5]/2)/2
  Deab <- 2*(pAB-pA*pB)
  
  #DAb
  DAb <- res4[1]+res4[2]/2-pA*Deab-pB*DA-pA^2*pB
  #-(res4[3]+res4[2]/2)-pA*Deab+pb*DA+pA^2*pb
  #-(res4[4]+res4[5]/2)/2-(pA-pa)/2*Deab-pB*DA+pA*pa*pB
  #(res4[6]+res4[5]/2)/2-(pA-pa)/2*Deab+pb*DA-pA*pa*pb
  #res4[7]+res4[8]/2+pa*Deab-pB*DA-pa^2*pB
  #-(res4[9]+res4[8]/2)+pa*Deab+pb*DA+pa^2*pb
  #DaB
  DaB <- res4[1]+res4[4]/2-pB*Deab-pA*DB-pA*pB^2
  #-(res4[7]+res4[4]/2)-pB*Deab+pa*DB+pa*pB^2
  #-(res4[2]+res4[5]/2)/2-(pB-pb)/2*Deab-pA*DB+pA*pB*pb
  #(res4[8]+res4[5]/2)/2-(pB-pb)/2*Deab+pa*DB-pa*pB*pb
  #(res4[3]+res4[6]/2)+pb*Deab-pA*DB-pA*pb^2
  #-(res4[9]+res4[6]/2)+pb*Deab+pa*DB+pa*pb^2
  
  #DAB
  DAB <- res4[1]-(pA^2*pB^2+pB^2*DA+pA^2*DB+2*pB*DAb+2*pA*DaB+2*pA*pB*Deab)-DA*DB-Deab^2
  #pA^2*pB*pb-pA^2*DB+pB*pb*DA+(pA*pb-pA*pB)*Deab+(pb-pB)*DAb-2*pA*DaB-res4[2]/2-DA*DB-Deab^2
  estp <- as.numeric(c(pA,pB,DA,DB,Deab,DAb,DaB,DAB))
  return(estp)
    
 
}





mix_est <- function(geno){
  
  nts <- rep(0,9)
  nt <- table(geno)
  nts[as.numeric(names(nt))] <- as.numeric(nt)

  res1 <- EM_Ym(geno=geno)
  ALLP1 <- autoP_DF10_m(res1)
  res2 <- EM_Y(geno=geno)

  ALLP2 <- autoP_DF10(res2)
  L1 <- nts*log(ALLP1)
  L2 <- nts*log(ALLP2)
  L1[which(is.nan(L1))] <- 0
  L2[which(is.nan(L2))] <- 0
  L <- -2*(sum(L1)-sum(L2))
  pv <- pchisq(L,5,lower.tail = F)
  
  ret <- c(LR=L,Pv=pv,m1_pA=res1[1],m1_pB=res1[2],m1_D=res1[3],m2_pA=res2[1],m2_pB=res2[2],
           m2_DA=res2[3],m2_DB=res2[4],m2_Deab=res2[5],m2_DAb=res2[6],m2_DaB=res2[7],m2_DAB=res2[8])
  return(ret)
}
