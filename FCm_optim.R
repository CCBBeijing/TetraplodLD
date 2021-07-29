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


auto_DF10_m <- function(pall){
  
  
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
  zy2 <- 2*PAABB*PAABb
  zy3 <- 2*PAABB*PAAbb+(PAABb)^2
  zy4 <- 2*PAABb*PAAbb
  zy5 <- (PAAbb)^2
  zy6 <- 2*PAABB*PAaBB
  zy7 <- 2*PAABB*PAaBb+2*PAaBB*PAABb
  zy8 <- 2*PAABb*PAaBb+2*PAaBB*PAAbb+2*PAabb*PAABB
  zy9 <- 2*PAAbb*PAaBb+2*PAabb*PAABb
  zy10 <- 2*PAabb*PAAbb
  zy11 <- (PAaBB)^2+2*PAABB*PaaBB
  zy12 <- 2*PAaBB*PAaBb+2*PAABB*PaaBb+2*PAABb*PaaBB
  zy13 <- (PAaBb)^2+2*PAABb*PaaBb+2*PAABB*Paabb+2*PAAbb*PaaBB+2*PAabb*PAaBB
  zy14 <- 2*PAabb*PAaBb+2*PAABb*Paabb+2*PAAbb*PaaBb
  zy15 <- (PAabb)^2+2*PAAbb*Paabb
  zy16 <- 2*PAaBB*PaaBB
  zy17 <- 2*PaaBB*PAaBb+2*PaaBb*PAaBB
  zy18 <- 2*PaaBb*PAaBb+2*Paabb*PAaBB+2*PaaBB*PAabb
  zy19 <- 2*Paabb*PAaBb+2*PaaBb*PAabb
  zy20 <- 2*Paabb*PAabb
  zy21 <- (PaaBB)^2
  zy22 <- 2*PaaBB*PaaBb
  zy23 <- (PaaBb)^2+2*PaaBB*Paabb
  zy24 <- 2*PaaBb*Paabb
  zy25 <- (Paabb)^2
  
  tmp <- c(zy1,zy2,zy3,zy4,zy5,zy6,zy7,zy8,zy9,zy10,
           zy11,zy12,zy13,zy14,zy15,zy16,zy17,zy18,zy19,
           zy20,zy21,zy22,zy23,zy24,zy25)
  #sum(tmp)
  return(tmp)
}
auto_DF10_mnew <- function(pall){
  
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
  zy2 <- 2*PAABB*PAABb
  zy3 <- 2*PAABB*PAAbb+(PAABb)^2
  zy4 <- 2*PAABb*PAAbb
  zy5 <- (PAAbb)^2
  zy6 <- 2*PAABB*PAaBB
  zy7 <- 2*PAABB*PAaBb+2*PAaBB*PAABb
  zy8 <- 2*PAABb*PAaBb+2*PAaBB*PAAbb+2*PAabb*PAABB
  zy9 <- 2*PAAbb*PAaBb+2*PAabb*PAABb
  zy10 <- 2*PAabb*PAAbb
  zy11 <- (PAaBB)^2+2*PAABB*PaaBB
  zy12 <- 2*PAaBB*PAaBb+2*PAABB*PaaBb+2*PAABb*PaaBB
  zy13 <- (PAaBb)^2+2*PAABb*PaaBb+2*PAABB*Paabb+2*PAAbb*PaaBB+2*PAabb*PAaBB
  zy14 <- 2*PAabb*PAaBb+2*PAABb*Paabb+2*PAAbb*PaaBb
  zy15 <- (PAabb)^2+2*PAAbb*Paabb
  zy16 <- 2*PAaBB*PaaBB
  zy17 <- 2*PaaBB*PAaBb+2*PaaBb*PAaBB
  zy18 <- 2*PaaBb*PAaBb+2*Paabb*PAaBB+2*PaaBB*PAabb
  zy19 <- 2*Paabb*PAaBb+2*PaaBb*PAabb
  zy20 <- 2*Paabb*PAabb
  zy21 <- (PaaBB)^2
  zy22 <- 2*PaaBB*PaaBb
  zy23 <- (PaaBb)^2+2*PaaBB*Paabb
  zy24 <- 2*PaaBb*Paabb
  zy25 <- (Paabb)^2
  
  tmp <- c(zy1,zy2,zy3,zy4,zy5,zy6,zy7,zy8,zy9,zy10,
           zy11,zy12,zy13,zy14,zy15,zy16,zy17,zy18,zy19,
           zy20,zy21,zy22,zy23,zy24,zy25)
  #sum(tmp)
  return(tmp)
}
auto_DF10_meqn <- function(pall){
  return(sum(pall))
}
sim_geno_auto_m  <- function(allpar,n=1000){
  
  
  conp <- auto_DF10_m(pall=allpar)
  
  id <- sample(1:25,n,replace = T,prob=conp)
  
  return(id)
}

LL3 <- function(pall){
  
  ALLP2 <- auto_DF10_mnew(pall)
  L2 <- -sum(nts*log(ALLP2))
  return(L2)
}
EM_Ym_FC <- function(geno){
  
  
  nts <- rep(0,25)
  nt <- table(geno)
  nts[as.numeric(names(nt))] <- as.numeric(nt)
  nn <- sum(nts)
  
  nts <<- nts
  res <- solnp(rep(1/4,4),fun = LL3,eqfun = auto_DF10_meqn,
               eqB = 1,LB=rep(0,4),UB = rep(1,4),control = list(trace=0))
  #D
  res3 <- res$pars
  pA <- sum(res3[1:2])
  pB <- sum(res3[c(1,3)])
  D <- res3[1]-pA*pB
  
  estp <- as.numeric(c(pA,pB,D))
  return(estp)
}
mix_est_f_Y <- function(geno){
  
  nts <- rep(0,25)
  nt <- table(geno)
  nts[as.numeric(names(nt))] <- as.numeric(nt)
  
  res1 <- EM_Ym_FC(geno=geno)
  ALLP1 <- auto_DF10_m(res1)
  res2 <- EM_Y_FC(geno=geno)
  ALLP2 <- auto_DF10(res2)
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



Power_cal_mf <- function(res,geno,ii=25){
  
  nts <- rep(0,ii)
  nt <- table(geno)
  nts[as.numeric(names(nt))] <- as.numeric(nt)
  res1 <- res
  res1[3] <- 0
  ALLP1 <- auto_DF10_m(res1)
  ALLP1[which(ALLP1<0)] <- 1e-5
  ALLP2 <- auto_DF10_m(res)
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




Power_cal_f <- function(res,geno,ii=25){
  
  nts <- rep(0,ii)
  nt <- table(geno)
  nts[as.numeric(names(nt))] <- as.numeric(nt)
  LL <- c()
  for(i in 3:8){
    res1 <- res
    res1[i] <- 0
    ALLP1 <- auto_DF10(res1)
    ALLP1[which(ALLP1<0)] <- 1e-5
    L1 <- nts*log(ALLP1)
    L1[which(is.nan(L1))] <- 0
    LL <- c(LL,sum(L1))
  }
  ALLP2 <- auto_DF10(res)
  ALLP2[which(ALLP2<0)] <- 1e-5
  L2 <- nts*log(ALLP2)
  L2[which(is.nan(L2))] <- 0
  L <- -2*(LL-sum(L2))
  pv <- pchisq(L,1,lower.tail = F)
  
  ret <- c(pv)
  return(ret)
}
