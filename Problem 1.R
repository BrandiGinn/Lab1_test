library(MASS)
library(lmerTest)
library(lme4)
# We vary sample size from 5 to 40 >> CHANGED B(1) FROM 5 TO 3 (WEAKER ASSOC.)
p.matrix = c()
for (N in seq(45,60,by=5)) {
  p.temp.vector = c()
  # We repeat simulation for 500 times for each N (Step 3)
  for (i in 1:100) {
    # Set seed
    set.seed(i)
    # Step 1: generate data
    # Generate id 
    dat = data.frame(id = paste0(615,1:N))
    # Long format: each subject was followed by ~10 days~>>CHANGED 10 TO 8 Nights
    dat = as.data.frame(dat[rep(1:N, each = 8), ])
    names(dat) = 'id'
    # Make Day variable ~>> CHANGED 10 NIGHTS TO 8
    dat$Days = rep(1:8, times = N)
    # Simulate random error >> CHANGE SD TO 30
    dat$err = rnorm(8*N,mean=0,sd=30)
    
    # Simulate (correlated) subject-level random effects for intercepts and slopes
    ## Covariance matrix >> CHANGED COR(B0, B1) TO 0
    S1 = diag(c(24.741, 5.922)) %*% matrix(c(1, 0, 0, 1), nrow=2) %*% diag(c(24.741, 5.922))
    ## Simulate realization of random intercept and slope
    U1 = as.data.frame(mvrnorm(N, mu=c(0,0), Sigma=S1))
    ## Add identifier (subject id)
    U1$id = paste0(615,1:N)
    ## Merge subject-level random effects back to data
    dat = merge(dat,U1,by='id')
    
    # Simulate the outcome: Reaction_ij
    dat$Reaction = (251.405 + dat$V1) + (3 + dat$V2)*dat$Days + dat$err
    
    # Step 2: test the null hypothesis
    mod = lmer(Reaction ~ Days + (Days | id), dat)
    p.value = summary(mod)$coef["Days","Pr(>|t|)"]
    # Save p value
    p.temp.vector = c(p.temp.vector,p.value)
  }
  # Save p value vector for each N
  p.matrix = cbind(p.matrix,p.temp.vector)
  print(N)
}
# Step 4: calculate power
power = apply(p.matrix,2,function(x) mean(x<0.05))


  
  
