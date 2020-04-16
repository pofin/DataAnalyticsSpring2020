data <- read.csv("dataset_exploratoryFactorAnalysis.csv",header = TRUE)
data

library(psych)
corMat <- cor(data)
corMat

solution <- fa(r = corMat, nfactors = 2, rotate = "oblimin",fm = "pa")
solution

#lab2_fa
#1
v1 <- c(1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,4,5,6)
v2 <- c(1,2,1,1,1,1,2,1,2,1,3,4,3,3,3,4,6,5)
v3 <- c(3,3,3,3,3,1,1,1,1,1,1,1,1,1,1,5,4,6)
v4 <- c(3,3,4,3,3,1,1,2,1,1,1,1,2,1,1,5,6,4)
v5 <- c(1,1,1,1,1,3,3,3,3,3,1,1,1,1,1,6,4,5)
v6 <- c(1,1,1,2,1,3,3,3,4,3,1,1,1,2,1,6,5,4)
m1 <- cbind(v1,v2,v3,v4,v5,v6)
cor(m1)
factanal(m1, factors = 3) # varimax is the default
factanal(m1, factors = 3, rotation = "promax")
# The following shows the g factor as PC1
prcomp(m1) # signs may depend on platform

## formula interface
factanal(~v1+v2+v3+v4+v5+v6, factors = 3, scores = "Bartlett")$scores

#2
install.packages("Hmisc")
library(Hmisc)
AthleticsData <- spss.get("AthleticsData.sav")
attach(AthleticsData)
#
names(AthleticsData)

cor(AthleticsData)
prcomp(AthleticsData)

fit.2 <- factanal(AthleticsData,factors=2,rotation="varimax")
print(fit.2)

fit.3 <- factanal(AthleticsData,factors=3,rotation="varimax")
print(fit.3)
print(fit.3, digits = 2, cutoff = .2, sort = TRUE)

install.packages("GPArotation")
library(GPArotation)

fit <- principal(AthleticsData, nfactors=3, rotate="varimax")
fit # print results


# do not go past here unless you can find fa.promax.R


#found on github
fa.promax = function(fo,factors=1,digits=4,sort=F,m=3,...)
  
{
  
  # ML factor analysis with varimax and promax rotation including correlation
  
  # matrix of factors, optional sorting of loadings, and specification of
  
  # promax power (m, default = 3 as recommended by many statistical packages;
  
  # higher values simplify loadings at the cost of additional correlation
  
  # between factors). If "raw" data (not a correlation or covariance matrix)
  
  # and the additional argument score='regression' are used, the output will
  
  # contain matrices of factor scores of the varimax and promax rotated
  
  # solutions.
  
  
  
  sort.loadings = function(ld)
    
  {
    
    f = dim(ld)[2]
    
    loadmax=abs(ld[,1:f])==apply(abs(ld[,1:f]),1,max)
    
    FL = as.list(colnames(ld)[1:f])
    
    for (i in 1:f)
      
    {
      
      FL[[i]]=ld[loadmax[,i]==T,]
      
      if (length(dim(FL[[i]])) > 0)
        
      {
        
        FL[[i]]=FL[[i]][order(abs(FL[[i]][,i]),decreasing=T),]
        
      }
      
      if (i == 1)
        
      {
        
        erg=FL[[1]]
        
      }
      
      else
        
      {
        
        erg=rbind(erg,FL[[i]])
        
        if (i == 2)
          
        {
          
          if (length(dim(FL[[1]])) == 0)
            
          {
            
            rownames(erg)[1] = rownames(ld)[which(loadmax[,1]==T)]
            
          }
          
        }
        
        if (i > 1)
          
        {
          
          if (length(dim(FL[[i]])) == 0)
            
          {
            
            rownames(erg)[dim(erg)[1]] = rownames(ld)[which(loadmax[,i]==T)]
            
          }
          
        }
        
      }
      
    }
    
    erg
    
  }
  
  
  
  res = factanal(fo,factors=factors,rotation="none",...)
  
  if (factors > 1)
    
  {
    
    vm = varimax(loadings(res))
    
    ssvm = diag(t(vm$loadings[]) %*% vm$loadings[])
    
    ssvm = rbind(ssvm,ssvm/dim(vm$loadings[])[1])
    
    pm = promax(loadings(vm),m=m) # m=3 is default in many programs
    
    sspm = diag(t(pm$loadings[]) %*% pm$loadings[])
    
    sspm = rbind(sspm,sspm/dim(pm$loadings[])[1])
    
    A = vm$rotmat %*% pm$rotmat
    
    phi = solve(t(A) %*% A)
    
    pmst = pm$loadings %*% phi
    
    unld = res$loadings[]
    
    vmld = vm$loadings[]
    
    pmld = pm$loadings[]
    
    SeqF = order(colSums(vmld**2),decreasing=T)
    
    ssvm = ssvm[,SeqF]
    
    ssvm = rbind(ssvm,cumsum(ssvm[2,]))
    
    rownames(ssvm)=c('SS loadings','Proportion Var','Cumulative Var')
    
    sspm = sspm[,SeqF]
    
    sspm = rbind(sspm,cumsum(sspm[2,]))
    
    rownames(sspm)=c('SS loadings','Proportion Var','Cumulative Var')
    
    unld = unld[,SeqF]
    
    vmld = vmld[,SeqF]
    
    pmld = pmld[,SeqF]
    
    pmst = pmst[,SeqF]
    
    phi = phi[,SeqF]
    
    phi = phi[SeqF,]     
    
    colnames(unld) = paste(rep('Factor',factors),1:factors,sep='')
    
    colnames(ssvm) = colnames(unld)
    
    colnames(sspm) = colnames(unld)
    
    colnames(vmld) = colnames(unld)
    
    colnames(pmld) = colnames(unld)
    
    colnames(pmst) = colnames(unld)
    
    colnames(phi)=colnames(unld)
    
    rownames(phi)=colnames(unld)
    
    if (length(res$scores) > 0)
      
    {
      
      FS = sqrt(res$n.obs/(res$n.obs-1))*scale(res$scores)
      
      vm.fs = FS %*% vm$rotmat
      
      pm.fs = sqrt(res$n.obs/(res$n.obs-1))*scale(FS %*% pm$rotmat)[,]
      
      FS = FS[,SeqF]
      
      vm.fs = vm.fs[,SeqF]
      
      pm.fs = pm.fs[,SeqF]
      
      colnames(FS) = colnames(unld)
      
      colnames(vm.fs)=colnames(unld)
      
      colnames(pm.fs)=colnames(unld)
      
    }
    
    if (sort==T)
      
    {
      
      uniqueness = cbind(sort(res$uniqueness))
      
      vmld = sort.loadings(vmld)
      
      Dummy = NULL
      
      for (i in 1:nrow(unld))
        
      {
        
        Dummy = rbind(Dummy,unld[which(rownames(unld)==rownames(vmld)[i]),])
        
      }
      
      rownames(Dummy)=rownames(vmld)
      
      unld = Dummy
      
      pmld = sort.loadings(pmld)
      
      pmst = sort.loadings(pmst)
      
    }
    
    else
      
    {
      
      uniqueness = cbind(res$uniqueness)
      
    }
    
    colnames(uniqueness) = "residual variance"
    
    if (length(res$scores) > 0)
      
    {
      
      erg = list(uniqueness=round(uniqueness,digits),
                 
                 unrotated.loadings=round(unld,digits),
                 
                 unrotated.factorscores=round(FS[,],digits),
                 
                 varimax.SS = round(ssvm,digits),
                 
                 varimax.loadings=round(vmld,digits),
                 
                 varimax.factorscores = round(vm.fs,digits),
                 
                 promax.SS = round(sspm,digits),
                 
                 promax.loadings=round(pmld,digits),
                 
                 promax.structure=round(pmst,digits),
                 
                 corr.factors=round(phi,digits),
                 
                 promax.factorscores = round(pm.fs,digits),
                 
                 n=res$n.obs,chi=round(res$STATISTIC,3),df=res$dof,p=res$PVAL)
      
    }
    
    else
      
    {
      
      erg = list(uniqueness=round(uniqueness,digits),
                 
                 unrotated.loadings=round(unld,digits),
                 
                 varimax.SS = round(ssvm,digits),
                 
                 varimax.loadings=round(vmld,digits),
                 
                 promax.SS = round(sspm,digits),
                 
                 promax.loadings=round(pmld,digits),
                 
                 promax.structure=round(pmst,digits),
                 
                 corr.factors=round(phi,digits),
                 
                 n=res$n.obs,chi=round(res$STATISTIC,3),df=res$dof,p=res$PVAL)
      
    }
    
  }
  
  else
    
  {
    
    ss = diag(t(res$loadings[]) %*% res$loadings[])
    
    ss = rbind(ss,ss/dim(res$loadings[])[1])
    
    ss = rbind(ss,cumsum(ss[2,]))
    
    rownames(ss)=c('SS loadings','Proportion Var','Cumulative Var')
    
    if (sort==T)
      
    {
      
      uniqueness = cbind(sort(res$uniqueness))
      
      vmld=cbind(sign(res$loadings[order(abs(res$loadings),decreasing=T)])*
                   
                   sort(abs(res$loadings[1:dim(res$loadings)[1],]),dec=T))
      
      colnames(vmld)="Factor1"
      
    }
    
    else
      
    {
      
      uniqueness = cbind(res$uniqueness)
      
      vmld=res$loadings[]
      
    }
    
    colnames(uniqueness) = "residual variance"
    
    if (length(res$scores) > 0)
      
    {
      
      FS = cbind(sqrt(res$n.obs/(res$n.obs-1))*scale(res$scores))
      
      colnames(FS)='Factor1'
      
      erg = list(uniqueness=round(uniqueness,digits),
                 
                 SS = round(ss,digits),
                 
                 loadings=round(vmld,digits),
                 
                 factorscores = round(FS,digits),
                 
                 n=res$n.obs,chi=round(res$STATISTIC,3),df=res$dof,p=res$PVAL)
      
    }
    
    else
      
    {
      
      erg = list(uniqueness=round(uniqueness,digits),
                 
                 SS = round(ss,digits),
                 
                 loadings=round(vmld,digits),
                 
                 n=res$n.obs,chi=round(res$STATISTIC,3),df=res$dof,p=res$PVAL)
      
    }
    
  }
  
  erg
  
}



fit.3.promax <- update(fit.3,rotation="promax") 
colnames(fit.3.promax$loadings)<-c("Endurance","Strength","Hand-Eye") 
print(loadings(fit.3.promax), digits = 2, cutoff = .2, sort = TRUE)
AssignFactorNames <- function(fit.object,names)
{
  colnames(fit.object$promax.loadings)<-names
  colnames(fit.object$varimax.loadings)<-names
  rownames(fit.object$corr.factors)<-names
  colnames(fit.object$corr.factors)<-names
}
fit.3.Enzmann <- fa.promax(AthleticsData,factors=3, digits=2, sort=TRUE) 
AssignFactorNames(fit.3.Enzmann,)
fit.3.Enzmann


#4
#can't find a package that this data is in
#tried on the epi datasets in course repository but didn't work
data(epi)
epi.keys <- make.keys(epi,list(E = c(1, 3, -5, 8, 10, 13, -15, 17, -20, 22, 25, 27,
                                     -29, -32, -34, -37, 39, -41, 44, 46, 49, -51, 53, 56),
                               N=c(2, 4, 7, 9, 11, 14, 16, 19, 21, 23, 26, 28, 31, 33, 35, 38, 40,
                                   43, 45, 47, 50, 52, 55, 57),
                               L = c(6, -12, -18, 24, -30, 36, -42, -48, -54),
                               I =c(1, 3, -5, 8, 10, 13, 22, 39, -41), 
                               S = c(-11, -15, 17, -20, 25, 27, -29, -32, -37, 44, 46, -51, 53)))
scores <- scoreItems(epi.keys,epi)
N <- epi[abs(epi.keys[,"N"]) >0]
E <- epi[abs(epi.keys[,"E"]) >0]
fa.lookup(epi.keys[,1:3],epi.dictionary) #show the items and keying information



#5
set.seed(1.234)
N <- 200                             # number of observations
P <- 6                               # number of variables
Q <- 2                               # number of factors

# true P x Q loading matrix -> variable-factor correlations
Lambda <- matrix(c(0.7,-0.4, 0.8,0, -0.2,0.9, -0.3,0.4, 0.3,0.7, -0.8,0.1),
                 nrow=P, ncol=Q, byrow=TRUE)

library(mvtnorm)                      # for rmvnorm()
FF  <- rmvnorm(N, mean=c(5, 15), sigma=diag(Q))    # factor scores (uncorrelated factors)
E   <- rmvnorm(N, rep(0, P), diag(P)) # matrix with iid, mean 0, normal errors
X   <- FF %*% t(Lambda) + E           # matrix with variable values
Xdf <- data.frame(X)                  # data also as a data frame


library(psych) # for fa(), fa.poly(), factor.plot(), fa.diagram(), fa.parallel.poly, vss()
fa(X, nfactors=2, rotate="varimax")$loadings     # factor analysis continuous data

# dichotomize variables into a list of ordered factors
Xdi    <- lapply(Xdf, function(x) cut(x, breaks=c(-Inf, median(x), Inf), ordered=TRUE))
Xdidf  <- do.call("data.frame", Xdi) # combine list into a data frame
XdiNum <- data.matrix(Xdidf)         # dichotomized data as a numeric matrix

library(polycor)                     # for hetcor()
pc <- hetcor(Xdidf, ML=TRUE)         # polychoric corr matrix -> component correlations

#

faPC <- fa(r=pc$correlations, nfactors=2, n.obs=N, rotate="varimax")
faPC$loadings
#
faPCdirect <- fa.poly(XdiNum, nfactors=2, rotate="varimax")    # polychoric FA
faPCdirect$fa$loadings        # loadings are the same as above ...
#### NB: For factor scores, look at package ltm which has a factor.scores() function specifically for polytomous outcome data. An example is provided on this page -> "Factor Scores - Ability Estimates".

factor.plot(faPCdirect$fa, cut=0.5)
fa.diagram(faPCdirect)

fa.parallel.poly(XdiNum)      # parallel analysis for dichotomous data
vss(pc$correlations, n.obs=N, rotate="varimax")   # very simple structure
#
library(random.polychor.pa)    # for random.polychor.pa()
random.polychor.pa(data.matrix=XdiNum, nrep=5, q.eigen=0.99)















