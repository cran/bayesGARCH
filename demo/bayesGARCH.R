'wait' <- function()
  {
    t <- readline("\nPlease 'q' to quit the demo or any other key to continue...\n")
    if (t == "q") stop ("end of the demo")
  }

## LOAD DATA
data(dem2gbp)
y <- dem2gbp[1:750]
plot(y, type='l')
wait()

## RUN THE SAMPLER (2 chains)
MCMC <- bayesGARCH(y, control = list(n.chain = 2, l.chain = 2000))
wait()

## MCMC ANALYSIS (using coda)
plot(MCMC)
autocorr.diag(MCMC)
gelman.diag(MCMC)
1-rejectionRate(MCMC)
wait()

## FORM THE POSTERIOR SAMPLE
smpl <- formSmpl(MCMC, l.bi = 50)

## POSTERIOR STATISTICS
summary(smpl)
smpl <- as.matrix(smpl)
pairs(smpl)
wait()

## GARCH(1,1) WITH NORMAL INNOVATIONS
MCMC <- bayesGARCH(y, lambda = 100, delta = 500,
                   control = list(n.chain = 2, l.chain = 2000))
wait()

## GARCH(1,1) WITH NORMAL INNOVATIONS AND
## WITH COVARIANCE STATIONARITY CONDITION
addPriorConditions <- function(psi)
{
    psi[2] + psi[3] < 1
}
MCMC <- bayesGARCH(y, lambda = 100, delta = 500,
                   control = list(n.chain = 2, l.chain = 2000, 
                   addPriorConditions = addPriorConditions))
wait()

## GARCH(1,1) WITH NORMAL INNOVATIONS AND
## WITH COVARIANCE STATIONARITY CONDITION
MCMC <- bayesGARCH(y, lambda = 100, delta = 500,
                   control = list(n.chain = 2, l.chain = 2000,
                   addPriorConditions = addPriorConditions))
wait()
                   
## MCMC ANALYSIS (using coda)
plot(MCMC)
autocorr.diag(MCMC)
gelman.diag(MCMC)
1-rejectionRate(MCMC)
wait()

## POSTERIOR STATISTICS
smpl <- formSmpl(MCMC, l.bi = 500)
summary(smpl)
smpl <- as.matrix(smpl)
hist(apply(smpl[,c("alpha1","beta")], 1, sum), nclass=100)

## END OF THE DEMO
