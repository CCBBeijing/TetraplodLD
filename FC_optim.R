
library(Rsolnp)
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

auto_DF10 <- function(pall){
  
  
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

auto_DF10new <- function(pall){
  
  
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

auto_DF10eqn <- function(pall){ sum(pall)}

sim_geno_auto  <- function(allpar,n=1000){
  
  
  conp <- auto_DF10(pall=allpar)
  
  id <- sample(1:25,n,replace = T,prob=conp)
  
  return(id)
}





LL2 <- function(pall){
  
  ALLP2 <- auto_DF10new(pall)
  L2 <- -sum(nts*log(ALLP2))
  return(L2)
}


EM_Y_FC <- function(geno){
  
  nts <- rep(0,25)
  nt <- table(geno)
  nts[as.numeric(names(nt))] <- as.numeric(nt)
  nn <- sum(nts)
  nts<<- nts

  
  
  
  res <-  solnp(rep(1/9,9),fun = LL2,eqfun = auto_DF10eqn,
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



