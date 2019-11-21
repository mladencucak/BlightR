# ## Functions to calculate posterior distributions for the parameters of a model to 
# evaluate of changes in % leaf damage as a function of rainfall using Bayesian hierarchical model.
# 
# # These functions are adapted from (Rüger, N., A. Huth, S. P. Hubbell, and R. Condit. 2009.
# Response of recruitment to light availability across a tropical lowland rain forest community.
# Journal of Ecology 97:1360–1368). They use a hybrid of the Gibbs sampler and the
# Metropolis-Hastings algorithm. These functions should source in R2.13.1, and given 
# a table of data in the correct format, will return a list with the posterior distribution 
# of all parameters. Requires the package ‘coda’
# 
# # the input data file has three columns: sp = the species id for the individual 
# (the blocking factor), rain= rainfall for the site (indepentent variable), and dmg = 
#   the mean percent damage per seedling (dependent variable).
# 
# # Model parameters are the average herbivory in the drier site (a) and the effect of 
# rainfall on % damage (b). The four hyperparameters are: the mean (MuA) and SD (SdA) of 
# the damage at the dry site (a); and the mean (MuB) and SD (SdB) of the site effect (wet-dry)
# (b). The argument start.param sets their starting values.
# 
# # start.scale sets the initial step size for the proposal distribution of the 
# Metropolis-Hastings algorithm, the step size is adjusted scale.burnin steps (burn-in)
# into the run, so that acceptance rate is kept around 0.25. The number of cycles run by
# the Gibbs sampler is the argument steps, and the current state will be printed to the
# screen every showstep steps. Argument spp is a character vector listing the levels in
# the grouping factor, in this case the species’ acronyms.
# 
# # Output is a list with tables MuA, SdA, MuB, SdB, spa, spb, and scale    
# for each chain showing the dispersion of parameters in each step in each chain;
# and a table of convergence showing the test of convergence of the two chains.


metrop.dmg.Gibbs = function(data,
                            start.param = c(
                              MuA = 1,
                              SdA = 1,
                              MuB = 0,
                              SdB = 0,
                              spa = 1 ,
                              spb = 1
                            ),
                            start.scale = 0.01,
                            scale.burnin = 2000,
                            steps = 5000,
                            showstep = 500,
                            spp)
{
  
  MuA1=SdA1=MuB1=SdB1=MuA2=SdA2=MuB2=SdB2 = matrix(nrow=steps,ncol=3)
  MuA1[1,1]= MuA2[1,1] = start.param["MuA"]
  SdA1[1,1]= SdA2[1,1] = start.param["SdA"]
  MuB1[1,1] = MuB2[1,1]  = start.param["MuB"]
  SdB1[1,1] = SdB2[1,1]  = start.param["SdB"]
  MuA1[1,3]=SdA1[1,3]=MuB1[1,3]=SdB1[1,3]=
    MuA2[1,3]=SdA2[1,3]=MuB2[1,3]=SdB2[1,3]= start.scale
  
  nospp=length(spp)
  
  spa1 = spb1 = drawsa1 = drawsb1 = scalea1 = scaleb1 = 
    spa2 = spb2 = drawsa2 = drawsb2 = scalea2 = scaleb2 = matrix(ncol=steps,nrow=nospp)
  
  rownames(spa1)=rownames(spa2)=rownames(drawsa1)=rownames(drawsa2)=rownames(scalea1)=rownames(scalea2) =
    paste("a", spp, sep=".")
  rownames(spb2)=rownames(spb1)=rownames(drawsb1)=rownames(drawsb2)=rownames(scaleb1)=rownames(scaleb2) = paste("b", spp, sep=".")
  
  # chain 1 is initialized with average values
  spa1[,1]= mean(data$dmg)
  spb1[,1]= 0
  # chain 2 is initialized with more extreme values
  spa2[,1]= start.param["spa"]
  spb2[,1]= start.param["spb"]
  
  convergence = matrix(data=NA, ncol=2, nrow=nospp)
  #converged = matrix(data=F, ncol=2, nrow=nospp)=dimnames(converged)
  dimnames(convergence)= list(spp, c("a","b"))
  scalea1[,1] = scaleb1[,1] = scalea2[,1] = scaleb2[,1] = start.scale
  
  for(i in 2:steps)
  {
    adjust=ifelse((i <= scale.burnin), T, F)
    
    #Add if step if the parameter is not changing  for n(define the number of steps) steps
    MuA1[i,] = +(func=Mu.aGibbs, start.param=MuA1[i-1,1], scale.param=MuA1[i-1,3], SdA=SdA1[i-1,1], spa=spa1[,i-1], adjust.scale=adjust)
    
    SdA1[i,] = metrop1step(func=Sd.aGibbs, start.param=SdA1[i-1,1], scale.param=SdA1[i-1,3], MuA=MuA1[i,1], spa=spa1[,i-1], adjust.scale=adjust)
    
    MuB1[i,] = metrop1step(func=mu.bGibbs, start.param=MuB1[i-1,1], scale.param=MuB1[i-1,3], SdB=SdB1[i-1,1], spb=spb1[,i-1], adjust.scale=adjust)
    
    SdB1[i,] = metrop1step(func=sd.bGibbs, start.param=SdB1[i-1,1], scale.param=SdB1[i-1,3], MuB=MuB1[i,1], spb=spb1[,i-1], adjust.scale=adjust)
    
    MuA2[i,] = metrop1step(func=Mu.aGibbs, start.param=MuA2[i-1,1], scale.param=MuA2[i-1,3], SdA=SdA2[i-1,1], spa=spa2[,i-1], adjust.scale=adjust)
    
    SdA2[i,] = metrop1step(func=Sd.aGibbs, start.param=SdA2[i-1,1], scale.param=SdA2[i-1,3], MuA=MuA2[i,1], spa=spa2[,i-1], adjust.scale=adjust)
    
    MuB2[i,] = metrop1step(func=mu.bGibbs, start.param=MuB2[i-1,1], scale.param=MuB2[i-1,3], SdB=SdB2[i-1,1], spb=spb2[,i-1], adjust.scale=adjust)
    
    SdB2[i,] = metrop1step(func=sd.bGibbs, start.param=SdB2[i-1,1], scale.param=SdB2[i-1,3], MuB=MuB2[i,1], spb=spb2[,i-1], adjust.scale=adjust)
    
    for(j in 1:nospp)
    {
      nexta = metrop1step(func=spa.Gibbs, start.param=spa1[j,i-1], scale.param=scalea1[j,i-1], H=data[data$sp==spp[j],], spb=spb1[j,i-1], MuA=MuA1[i,1], SdA=SdA1[i,1], adjust.scale=adjust)
      spa1[j,i] = nexta[1]
      drawsa1[j,i] = nexta[2]
      scalea1[j,i] = nexta[3]
      
      nextb = metrop1step(func=spb.Gibbs, start.param=spb1[j,i-1], scale.param=scaleb1[j,i-1], H=data[data$sp==spp[j],], spa=spa1[j,i], MuB=MuB1[i,1], SdB=SdB1[i,1], adjust.scale=adjust)
      spb1[j,i] = nextb[1]
      drawsb1[j,i] = nextb[2]
      scaleb1[j,i] = nextb[3]
      
      nexta = metrop1step(func=spa.Gibbs, start.param=spa2[j,i-1], scale.param=scalea2[j,i-1], H=data[data$sp==spp[j],], spb=spb2[j,i-1], MuA=MuA2[i,1], SdA=SdA2[i,1], adjust.scale=adjust)
      spa2[j,i] = nexta[1]
      drawsa2[j,i] = nexta[2]
      scalea2[j,i] = nexta[3]
      
      nextb = metrop1step(func=spb.Gibbs, start.param=spb2[j,i-1], scale.param=scaleb2[j,i-1], H=data[data$sp==spp[j],], spa=spa2[j,i], MuB=MuB2[i,1], SdB=SdB2[i,1], adjust.scale=adjust)
      spb2[j,i] = nextb[1]
      drawsb2[j,i] = nextb[2]
      scaleb2[j,i] = nextb[3]
      
      # convergence test.   Needs the coda library
      if(i%%showstep==0 & i>=500)
      {
        g = gelman.diag(mcmc.list(mcmc(spa1[j,((i-500)+1):i]), mcmc(spa2[j,((i-500)+1):i])), confidence = 0.95,
                        transform=T, autoburnin=F)
        if (g[[1]][1] < 1.1 & is.na(convergence[j,1])) convergence[j,1] = i   
        
        g = gelman.diag(mcmc.list(mcmc(spb1[j,((i-500)+1):i]), mcmc(spb2[j,((i-500)+1):i])), confidence = 0.95,
                        transform=T, autoburnin=F)
        if (g[[1]][1] < 1.1 & is.na(convergence[j,2])) convergence[j,2] = i   
      } 
    } # end species loop
    
    if(i%%showstep==0 & i>=50) cat(i, "MuA ",round(MuA2[i,1],4), "SdA ", round(sqrt(SdA2[i,1]),2), "MuB ",round(MuB2[i,1],1), "SdB ", round(SdB2[i,1],2), "\n")
  } # end step loop
  
  return(list(SdA1=SdA1, SdA2=SdA2, MuA1=MuA1, MuA2=MuA2, MuB1=MuB1, MuB2=MuB2, SdB1=SdB1, SdB2=SdB2, spa1=spa1, spa2=spa2, spb1=spb1, spb2=spb2, scalea1=scalea1, scaleb1=scaleb1, convergence=convergence)	) 
}

# # likelihood of the mean of the gamma hyperdistribution for a
# Mu.aGibbs=function(MuA,spa,SdA)
# {
#   if(MuA<=0) return(-Inf)
#   if(SdA<=0) return(-Inf)
#   llike = dgamma(spa, scale=(SdA^2)/MuA, shape=(MuA/SdA)^2, log=T)
#   return(sum(llike) + dunif(MuA, min=0, max=1, log=T))
# }
# 
# # likelihood of the SD of the gamma hyperdistribution for a
# Sd.aGibbs=function(SdA,spa,MuA)
# {
#   if(SdA<=0) return(-Inf)
#   llike = dgamma(spa, scale=(SdA^2)/MuA, shape=(MuA/SdA)^2, log=T)
#   return(sum(llike) + dunif(SdA, min=0, max=100, log=T))
# }
# # likelihood of the mean of the normal hyperdistribution for b
# mu.bGibbs=function(MuB,spb,SdB)
# {
#   if(SdB<=0) return(-Inf)
#   llike = dnorm(spb, mean=MuB, sd=SdB, log=T)
#   return(sum(llike) + dunif(MuB, min=-100, max=100, log=T))
# }
# # likelihood of the standard deviation of the normal hyperdistribution for b
# sd.bGibbs=function(SdB,spb,MuB)
# {
#   if(SdB<=0) return(-Inf)
#   llike = dnorm(spb, mean=MuB, sd=SdB, log=T)
#   return(sum(llike) + dunif(SdB, min=0, max=100, log=T))
# }
# 
# 
# # likelihood of the constant of the linear relationship between log light and log
# # recruitment for one species
# spa.Gibbs=function(spa,H,spb,MuA,SdA)
# {
#   pred.herb = spa + spb*H$rain
#   if(any(pred.herb<=0)) return(-Inf)
#   lik.a = dexp(H$dmg, rate=1/pred.herb, log=T)
#   lik.A = dgamma(spa, scale=(SdA^2)/MuA, shape=(MuA/SdA)^2, log=T)
#   llike=sum(lik.a) + lik.A 
#   return(llike)
# }
# 
# ## !!! Define the model evaluation  function here with diag output
# # likelihood of the coefficient of light in the linear relationship between log
# # light and log recruitment
# spb.Gibbs=function(spb,H,spa,MuB,SdB)
# {
#   pred.herb = spa + spb*H$rain
#   if(any(pred.herb<=0)) return(-Inf)
#   lik.b = dexp(H$dmg, rate=1/pred.herb, log=T)
#   lik.B = dnorm(spb, mean=MuB, sd=SdB, log=TRUE)
#   llike=sum(lik.b) + lik.B
#   return(llike)
# }

# A generic routine for taking a single Metropolis step given any probability function, func, an initial parameter,
# start.param, and a scaling parameter for # the step size, scale.param. It returns the next parameter value as well as an
# acceptance indicator (0 if the value was accepted, 1 if rejected), and the new scale parameter. If adjust.scale=T, the
# scale parameter is adjusted to keep the acceptance rate at around 0.25, adjust.scale should be T only for
# steps<which.use (only during burnin period). Based on programs by Richard Condit and Helene Muller-Landau.
metrop1step=function(func,start.param,scale.param,adjust.scale=T,...)
{
  new.scale = scale.param
  #Remov ethe probability segment  because we want to go only up hil
  # probaccept = 0.25
  # multreject = 0.975
  # multaccept = multreject^(-(1-probaccept)/probaccept)
  
  #Example fro stack, defining par range, https://stats.stackexchange.com/questions/26858/how-to-generate-numbers-based-on-an-arbitrary-discrete-distribution
  sample(x=c(3:9), #define the parameter range
         size=1000, #number of iterations
         replace=TRUE, 
         prob=c(0.3,0.5, 0.7, 0.9, 0.8, 0.5, 0.3) #define the probablities
         )
  
  origlike = func(start.param,...) #previous parameter
  #to define temp range use runif() for Rfactor as well
  #To define other funs use rnorm()
  newval = rnorm(1,mean=start.param,sd=scale.param)
  #Change to new diagnostic and compare to previous one
  newlike = func(newval,...)
  if(!is.finite(newlike))
    accept = F
  else
  {
    if(newlike>=origlike) accept = T
    else
    {
      likeratio=exp(newlike-origlike)
      if(runif(1)<likeratio) accept = T
      else accept = F
    }
  }
  if(accept)
  {
    if(adjust.scale) new.scale=multaccept*scale.param
    return(c(newval,0,new.scale))
  }
  else
  {
    if(adjust.scale) new.scale=multreject*scale.param
    return(c(start.param,1,new.scale))
  }
}

