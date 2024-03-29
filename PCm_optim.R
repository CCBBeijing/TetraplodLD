mDF10 <- function(pall){
  
  pA <- pall[1]
  pa <- 1-pA
  pB <- pall[2]
  pb <- 1-pB
  D <- pall[3]
  
  
  PAABB <- (pA*pB+D)^2
  
  PAABb <- 2*(pA*pB+D)*(pA*pb-D)
  
  PAAbb <- (pA*pb-D)^2
  
  PAaBB <- 2*(pA*pB+D)*(pa*pB-D)
  
  PAaBb <- 2*(pA*pB+D)*(pa*pb+D)+2*(pa*pB-D)*(pA*pb-D)
  
  PAabb <- 2*(pA*pb-D)*(pa*pb+D)
  
  PaaBB <- (pa*pB-D)^2
  PaaBb <- 2*(pa*pB-D)*(pa*pb+D)
  Paabb <- (pa*pb+D)^2
  
  tmp <- c(PAABB,PAABb,PAAbb,PAaBB,PAaBb,PAabb,PaaBB,PaaBb,Paabb)
  #sum(tmp)
  return(tmp)
}

autoP_DF10_m <- function(pall){
  
  
  gp <- mDF10(pall=pall)
  
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

autoP_DF10_mnew <- function(pall){
  
  
  gp <- pall
  
  PAB <- gp[1]
  PAb <- gp[2]
  PaB <- gp[3] 
  Pab <- gp[4] 
  gp2 <- c(PAB,PAb,PaB,Pab)
  
  PAABB <- PAB^2
  PAABb <- 2*PAB*PAb
  PAAbb <- PAb^2 
  PAaBB <- 2*PAB*PaB 
  PAaBb <- 2*PAB*Pab+2*PAb*PaB 
  PAabb <- 2*PAb*Pab 
  PaaBB <- PaB^2 
  PaaBb <- 2*PaB*Pab 
  Paabb <- Pab^2

  
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

autoP_DF10_meqn <- function(pall){
  
  
  sum(pall)
}


sim_geno_autoP_m  <- function(allpar,n=1000){
  
  
  conp <- autoP_DF10_m(pall=allpar)
  
  id <- sample(1:9,n,replace = T,prob=conp)
  
  return(id)
}



LL3 <- function(pall){
  
  ALLP2 <- autoP_DF10_mnew(pall)
  L2 <- -sum(nts*log(ALLP2))
  return(L2)
}
EM_Ym <- function(geno){
  
  nts <- rep(0,9)
  nt <- table(geno)
  nts[as.numeric(names(nt))] <- as.numeric(nt)
  nn <- sum(nts)
  nts <<- nts
  res <- solnp(rep(1/4,4),fun = LL3,eqfun = autoP_DF10_meqn,
               eqB = 1,LB=rep(0,4),UB = rep(1,4),control = list(trace=0))
  #D
  res3 <- res$pars
  pA <- sum(res3[1:2])
  pB <- sum(res3[c(1,3)])
  D <- res3[1]-pA*pB
  
  estp <- as.numeric(c(pA,pB,D))
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
work_test1_mix <- function(M,mn){
  
  
  n <- dim(M)[2]
  nc <- combn(n,2)
  nci2 <- dim(nc)[2]
  LD <- matrix(NA,nrow=nci2,19)
  LDn <- rep(NA,nci2)
  for(i in 1:nci2){
    
    index <- paste(M[,nc[1,i]],M[,nc[2,i]],sep="")
    index[which(index=="22")] <- 1
    index[which(index=="21")] <- 2
    index[which(index=="20")] <- 3
    index[which(index=="12")] <- 4
    index[which(index=="11")] <- 5
    index[which(index=="10")] <- 6
    index[which(index=="02")] <- 7
    index[which(index=="01")] <- 8
    index[which(index=="00")] <- 9
    
    LDn[i] <- paste(mn[nc[1,i]],mn[nc[2,i]],sep="-")
    tmp <- try(mix_est(geno=as.numeric(index)),TRUE)
    
    if(class(tmp)=="try-error"){
      LD[i,] <- rep(NA,19)
    }else{
      LD[i,] <- c(tmp,cons1(tmp[-c(1:5)]))
    }
    
    if(i%%1000==0)
      cat("comb=",i,"\n")
  }
  colnames(LD) <- c("LR","Pv","m1_pA","m1_pB","m1_D","m2_pA","m2_pB",
                    "m2_DA","m2_DB","m2_Deab","m2_DAb","m2_DaB","m2_DAB",
                    "m2_DA_n","m2_DB_n","m2_Deab_n","m2_DAb_n","m2_DaB_n","m2_DAB_n")
  rownames(LD) <- LDn
  return(LD)
}



work_test1_1_mix <- function(M,nc,mn,interval=c(1,2)){
  
  
  n <- dim(M)[2]
  nci2 <- interval[2]-interval[1]+1
  LD <- matrix(NA,nrow=nci2,19)
  LDn <- rep(NA,nci2)
  k <- 1
  for(i in interval[1]:interval[2]){
    
    index <- paste(M[,nc[1,i]],M[,nc[2,i]],sep="")
    index[which(index=="22")] <- 1
    index[which(index=="21")] <- 2
    index[which(index=="20")] <- 3
    index[which(index=="12")] <- 4
    index[which(index=="11")] <- 5
    index[which(index=="10")] <- 6
    index[which(index=="02")] <- 7
    index[which(index=="01")] <- 8
    index[which(index=="00")] <- 9
    
    LDn[k] <- paste(mn[nc[1,i]],mn[nc[2,i]],sep="-")
    tmp <- try(mix_est(geno=as.numeric(index)),TRUE)
    
    if(class(tmp)=="try-error"){
      LD[k,] <- rep(NA,19)
    }else{
      LD[k,] <- c(tmp,cons1(tmp[-c(1:5)]))
    }
    if(k%%10000==0)
      cat("comb=",k,"\n")
    
    k <- k + 1
  }
  
  colnames(LD) <-  c("LR","Pv","m1_pA","m1_pB","m1_D","m2_pA","m2_pB",
                     "m2_DA","m2_DB","m2_Deab","m2_DAb","m2_DaB","m2_DAB",
                     "m2_DA_n","m2_DB_n","m2_Deab_n","m2_DAb_n","m2_DaB_n","m2_DAB_n")
  rownames(LD) <- LDn
  return(LD)
}

LD_norm <- function(Dp,conv){
  
  index <- match(sign(Dp),sign(conv))
  rr <- Dp/conv[index]
  list(rr=rr,cs=conv[index])
}





cons2 <- function(para){
  
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
  
  DA <- DAl$cs
  Deab <- Deabl$cs
  tmpDAb <- c(-pA*Deab-pB*DA-pA^2*pB,-pA*Deab+pb*DA+pA^2*pb,-(pA-pa)/2*Deab-pB*DA+pA*pa*pB,
              -(pA-pa)/2*Deab+pb*DA-pA*pa*pb,pa*Deab-pB*DA-pa^2*pB,pa*Deab+pb*DA+pa^2*pb)
  DAbi <- c(max(tmpDAb[which(tmpDAb<0)]),min(tmpDAb[which(tmpDAb>0)]))
  DAbl <- LD_norm(para[6],conv=DAbi)
  
  DB <- DBl$cs
  tmpDaB <- c(-pB*Deab-pA*DB-pA*pB^2,-pB*Deab+pa*DB+pa*pB^2,-(pB-pb)/2*Deab-pA*DB+pA*pB*pb,
              -(pB-pb)/2*Deab+pa*DB-pa*pB*pb,pb*Deab-pA*DB-pA*pb^2,pb*Deab+pa*DB+pa*pb^2)
  DaBi <- c(max(tmpDaB[which(tmpDaB<0)]),min(tmpDaB[which(tmpDaB>0)]))
  DaBl <- LD_norm(para[7],conv=DaBi)
  
  DAb <- DAbl$cs
  DaB <- DaBl$cs
  tmpDAB <- c(-(pA^2*pB^2+pA^2*DB+pB^2*DA+2*pA*pB*Deab+2*pB*DAb+2*pA*DaB+DA*DB+Deab^2),
              pA^2*pB*pb-pA^2*DB+pB*pb*DA+(pA*pb-pA*pB)*Deab+(pb-pB)*DAb-2*pA*DaB-DA*DB-Deab^2,
              -(pA^2*pb^2+pA^2*DB+pb^2*DA-2*pA*pb*Deab-2*pb*DAb+2*pA*DaB+DA*DB+Deab^2),
              pA*pa*pB^2+pA*pa*DB-pB^2*DA+(pa*pB-pA*pB)*Deab-2*pB*DAb+(pa-pA)*DaB-DA*DB-Deab^2,
              (-(2*(pA*pa*pB*pb-pA*pa*DB-pB*pb*DA+(pA*pB+pa*pb)*Deab-(pA*pb+pa*pB)*Deab+(pB-pb)*DAb+(pA-pa)*DaB)+
                   2*(pA*pa*pB*pb-pA*pa*DB-pB*pb*DA+(pB-pb)*DAb+(pA-pa)*DaB))-4*DA*DB-4*Deab^2)/4,
              pA*pa*pb^2+pA*pa*DB-pb^2*DA+(pA*pb-pa*pb)*Deab+2*pb*DAb+(pa-pA)*DaB-DA*DB-Deab^2,
              -(pa^2*pB^2+pa^2*DB+pB^2*DA-2*pa*pB*Deab+2*pB*DAb-2*pa*DaB +DA*DB+Deab^2),
              pa^2*pB*pb-pa^2*DB+pB*pb*DA+(pa*pB-pa*pb)*Deab+(pb-pB)*DAb+2*pa*DaB-DA*DB-Deab^2,
              -(pa^2*pb^2+pb^2*DA+pa^2*DB+2*pa*pb*Deab-2*pb*DAb-2*pa*DaB+DA*DB+Deab^2))
  DABi <- c(max(tmpDAB[which(tmpDAB<0)]),min(tmpDAB[which(tmpDAB>0)]))
  DABl <- LD_norm(para[8],conv=DABi)
  consv <- c(DAi=DAl$rr,DBi=DBl$rr,Deabi=Deabl$rr,DAbi=DAbl$rr,DaBi=DaBl$rr,DABi=DABl$rr)
  return(consv)
}





LD_norm <- function(Dp,conv){
  
  index <- match(sign(Dp),sign(conv))
  Dp/conv[index]
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



del_norm <- function(ptt,the){
  
  index1 <- which(ptt>0)
  p1 <- ptt[index1]
  index2 <- which(ptt<0)
  p2 <- ptt[index2]
  if(the>0){
    index_1 <- which(p1<the)
    if(length(index_1)>0){
      p1[index_1] <- NA
    }
    ptt[index1] <- p1
  }
  
  if(the<0){
    index_2 <- which(p2>the)
    if(length(index_2)>0){
      p2[index_2] <- NA
    }
    ptt[index2] <- p2
  }
  
  return(ptt)
  
}

r2c <- function(pA,pB,D){
  
  DC <- apply(D,2,function(x){
    (x)^2/sqrt(pA*(1-pA)*pB*(1-pB))
  })
  
  DC
  
}





Power_cal_mp <- function(res,geno){
  
  nts <- rep(0,9)
  nt <- table(geno)
  nts[as.numeric(names(nt))] <- as.numeric(nt)
  res1 <- res
  res1[3] <- 0
  ALLP1 <- autoP_DF10_m(res1)
  ALLP1[which(ALLP1<0)] <- 1e-5
  ALLP2 <- autoP_DF10_m(res)
  ALLP2[which(ALLP2<0)] <- 1e-5
  L1 <- nts*log(ALLP1)
  L2 <- nts*log(ALLP2)
  L1[which(is.nan(L1))] <- 0
  L2[which(is.nan(L2))] <- 0
  L <- -2*(sum(L1)-sum(L2))
  pv <- pchisq(L,1,lower.tail = F)
  
  ret <- c(Pv=pv)
  return(ret)
  
}






Power_cal_p <- function(res,geno){
  
  nts <- rep(0,9)
  nt <- table(geno)
  nts[as.numeric(names(nt))] <- as.numeric(nt)
  LL <- c()
  for(i in 3:8){
    res1 <- res
    res1[i] <- 0
    ALLP1 <- autoP_DF10(res1)
    ALLP1[which(ALLP1<0)] <- 1e-5
    L1 <- nts*log(ALLP1)
    L1[which(is.nan(L1))] <- 0
    LL <- c(LL,sum(L1))
  }
  ALLP2 <- autoP_DF10(res)
  ALLP2[which(ALLP2<0)] <- 1e-5
  L2 <- nts*log(ALLP2)
  L2[which(is.nan(L2))] <- 0
  L <- -2*(LL-sum(L2))
  pv <- pchisq(L,1,lower.tail = F)
  
  ret <- c(pv)
  return(ret)
}
