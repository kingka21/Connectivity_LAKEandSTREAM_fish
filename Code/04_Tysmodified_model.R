######### Ty's CODE copied 

# rm(list=ls())
library(data.table)
# library(R2jags)
library(jagsUI)
library(car)
library(Metrics)

dat <- fread("Data/MI_lake_data.csv", header = TRUE)
dat[, .N]
head(dat)
summary(dat)

#need dummy variables for the Conny class 
dat$Conn2<-sapply(fish.data$LakeConnec, function(x) {
  if(x == 'DR_LakeStream') {1}
  else {
    if(x == 'Headwater') {3}
    else {
      if(x == 'DR_Stream') {2}
      else {
        if(x== 'Isolated') {4}
      }}}})

### HU4-level covariates
# Average by HU4 to get HU4 specicfic values
huc4_road <- as.numeric(by(dat$hu4_roaddensity_mperha, dat$HU4_ZoneID, mean)) 
huc4_ag <- as.numeric(by(dat$hu4_nlcd2006_agr, dat$HU4_ZoneID, mean)) 
huc4_wetland <- as.numeric(by(dat$hu4_nlcd2006_wet, dat$HU4_ZoneID, mean)) 
huc4_precip <- as.numeric(by(dat$prism_precip_mean, dat$HU4_ZoneID, mean)) 
huc4_temp <- as.numeric(by(dat$prism_temp_mean, dat$HU4_ZoneID, mean)) 


# Standardize HU4-level covariates
z_huc4_road <- as.numeric(scale(logit(huc4_road)))
z_huc4_ag <- as.numeric(scale(logit(huc4_ag)))
z_huc4_wetland <- as.numeric(scale(logit(huc4_wetland)))
z_huc4_precip <- as.numeric(scale(huc4_precip))
z_huc4_temp <- as.numeric(scale(huc4_temp))

# Number of sites
J <-dat[,length(unique(HU4_ZoneID))]

# Site indicator
#dat[, G:= as.numeric(as.factor(as.numeric(as.factor(HU4_ZoneID))))]
dat$G<-as.numeric(as.factor(as.numeric(as.factor(dat$HU4_ZoneID))))


# Load data
data <- list(richness = dat$richness, group = dat$G, J = J,
             lakearea = dat$lakearea_km2, elevation = dat$elevation_m, LakeConnec = dat$Conn2, lake_sdf = dat$lake_sdf,
             iwswet = dat$iws_nlcd2006_wet, iwsag = dat$iws_nlcd2006_agr, iwsroad = dat$iws_roaddensity_mperha,
             precip = z_huc4_precip , temp = z_huc4_temp , rwet = z_huc4_wetland, rag = z_huc4_ag, rroad = z_huc4_road)
             

#################################################################
########## JAGS CODE ############################################
#################################################################
# Define the model in the BUGS language and write a text file
sink("model.txt")
cat("
    model {
    # Likelihood: 
    for (i in 1:92){ 
    richness[i] ~ dnorm(mu[i], tau)               
    mu[i] <- alpha[group[i]] + b[1] * lakearea[i] + b[2] * elevation[i] + b[3] * LakeConnec[i] + b[4] * lake_sdf[i] + 
    b[5] * iwswet[i] + b[6] * iwsag[i] + b[7] * iwsroad[i]
    } 
    
    # Level-2 of the model
    for(j in 1:J){
    alpha[j] ~ dnorm(mu.alpha.hat[j],tau.alpha)
    mu.alpha.hat[j] <- mu.alpha + s[1] * precip[j] + s[2] * temp[j] + s[3] * rwet[j] + s[4] * rag[j] 
    + s[5] * rroad[j]
    }
    
    # Priors
    mu.alpha ~ dnorm(0, 0.0001)
    
    tau <- pow(sigma,-2) 
    tau.alpha <- pow(sigma.alpha,-2) 
    sigma ~ dunif(0, 100)
    sigma.alpha ~ dunif(0,100)
    
    # Priors for beta parameters  
    # Bayesian LASSO -  a Laplace (double exponential) prior
    # Level-1 predictors
    for(k in 1:7){
    b[k] ~ ddexp(0, lambda)
    }
    
    # Level-2 predictors
    for(k in 1:5){
    s[k] ~ ddexp(0, lambda2)
    }
    
    # Hyper-prior on lambda
    lambda ~ dgamma(0.1,0.1)
    lambda2 ~ dgamma(0.1,0.1)
    
    # Calculate marginal and conditional R2
    for (i in 1:92){
    predictY[i] <- mu[i]
    
    }
    
    Vfixed<-(sd(predictY))^2
    Vresidual <- 1/tau # get the variance of residuals
    Vrandom <- 1/tau.alpha  # get the variance of random intercept
    marginalR2 <- Vfixed/(Vfixed+Vrandom+Vresidual) # calculate marginalR2 (fixed effects only)
    conditionalR2 <- (Vrandom+Vfixed)/(Vfixed+Vrandom+Vresidual) # calculate conditional R2 (fixed + random)
    
    } # end model
    ",fill = TRUE)
sink()

# Initial values
inits <- function (){
  list (mu.alpha = rnorm(1), sigma=runif(1), sigma.alpha=runif(1) )
}


# Parameters monitored
parameters <- c("mu.alpha","sigma", "sigma.alpha","b","lambda",
                "predictY","marginalR2","conditionalR2","lambda2","s")


# MCMC settings
ni <- 90000
nt <- 2
nb <- 70000
nc <- 3


start.time = Sys.time()         # Set timer 
# Call JAGS from R 

out <- jags(data, inits, parameters, "model.txt", n.chains = nc, 
            n.thin = nt, n.iter = ni, n.burnin = nb, parallel=T)

end.time = Sys.time()
elapsed.time = round(difftime(end.time, start.time, units='mins'), dig = 2)
cat('Posterior computed in ', elapsed.time, ' minutes\n\n', sep='') 

###SAVE OUTPUT #####
#NOTE THIS NEEDS EDITING
saveRDS(out, file="output_class_Jul19.rds")

# Find which parameters, if any, have Rhat > 1.1
which(out$summary[, c("Rhat")] > 1.1)

# Summarize posteriors
print(out, dig = 3)
str(out)
#traceplot(out)
outExp <- out$summary
write.csv(outExp, "class_ModelSummary.csv", row.names = T)
#mcmcOut <- out$sims.list
# saveRDS(mcmcOut, file="CHL_mcmc_out.rds")

### Make a table of the local predictor (beta) estimates mean and quantiles 
betaEsts <- matrix(NA, nrow=3,ncol=7)
for(i in 1:7){
  betaEsts[1,i] <- mean(quantile(out$sims.list$b[,i],c(0.05,0.95)))
  betaEsts[2:3,i] <- quantile(out$sims.list$b[,i],c(0.05,0.95))
}
betaEsts

### Make a table of the regional predictor (s) estimates mean and quantiles
sEsts <- matrix(NA, nrow=3,ncol=5)
for(i in 1:5){
  sEsts[1,i] <- mean(quantile(out$sims.list$s[,i],c(0.05,0.95)))
  sEsts[2:3,i] <- quantile(out$sims.list$s[,i],c(0.05,0.95))
}
sEsts


predicted <- out$mean$predictY
observed <- dat$richness

res <- 6
name_figure <- "richness_observed_predicted.jpg"
jpeg(filename = name_figure, height = 500*res, width = 500*res, res=72*res)
def.par <- par(no.readonly = TRUE) 		# save default, for resetting...

nf <- layout(matrix(c(1:1),nrow=1,ncol=1,byrow=TRUE),  TRUE) 
layout.show(nf)
par(mar=c(0.5,0.5,0,0), oma=c(2,2,0,0) )

size.labels = 1
size.text = 1.0
y.label = expression(paste('Predicted (richness)' ))
x.label = expression(paste('Observed (richness)' ))

plot(predicted ~ observed, type='n', axes=F, ylim=c(min(predicted), max(predicted)),
     xlab='',ylab='', xlim=c(min(observed),max(observed)) )
axis(side=1,cex.axis=size.text, tck=-0.01, mgp=c(0,0.3,0) )
axis(side=2,cex.axis=size.text, las=1, mgp=c(0,0.3,0),tck=-0.01)

# Add data points
points(observed, predicted, pch=16, cex=1.0, col='black')

# Add axis labels
mtext(x.label, line = 0.7, side = 1, cex = size.text, outer=T)
mtext(y.label, line = 0.5, side = 2, cex = size.text, outer=T)
abline(0,1, lwd=2, col="blue")

box()
par(def.par)
dev.off()
### END PLOT

## Calculate RMSE
rmse(observed, predicted)   #4.242617

rmse_sim <- numeric()
for(i in 1:out$mcmc.info$n.samples){
  rmse_sim[i] <- rmse(observed, out$sims.list$predictY[i,])
  
}

mean(rmse_sim)  #4.53
quantile(rmse_sim, c(0.05, 0.95))
# 5%      95% 
#4.281092 4.821867 

#####################################################
########### PLOT ####################################
#####################################################
covariates <- c("area", "elevation", "connection", "shoreline", "local_wetland", "local_agriculture", "local_road",
                "Reg_precip", "Reg_temp", "Reg_wetland", "Reg_agriculture", "Reg_road")
                

res <- 6
name_figure <- "richness_effects.jpg"
jpeg(filename = name_figure, height = 500*res, width = 500*res, res=72*res)
def.par <- par(no.readonly = TRUE)     # save default, for resetting...

#nf <- layout(matrix(c(1:1),nrow=1,ncol=1,byrow=TRUE),  TRUE) 
# nf <- layout(matrix(c(1:2), nrow = 1, ncol=2,,byrow=TRUE), widths = c(0.7, 0.3))
nf <- layout(matrix(c(1:1), nrow = 1, ncol=1,byrow=TRUE))
layout.show(nf)
#par(mar=c(1,1,1,1), oma=c(2,2,1,1) )
par(mar = c(1, 4.9, 0, 0) + 0.1,oma=c(2,1.5,0,0))
def.par <- par(no.readonly = TRUE)     # save default, for resetting...

size.labels = 1
size.text = 1

x.label <- 'Estimated effect'
y.label <- 'Covariate'

# Posterior means and CIs for all parameters
Plot.data <- cbind(betaEsts,sEsts)

rows <- 1:dim(Plot.data)[2]

Plot.color <- as.numeric(Plot.data[2,] * Plot.data[3,] > 0 )
colorP <- rep("black", length(rows))
colorP[Plot.color > 0] <- "blue"


plotting.region <- range(Plot.data)

### axis label options
spc <- 0.2
lab <- 1:63
cex <- 0.5
adj <- 0
###
plot(c(plotting.region[1], plotting.region[2]), c(0.5,length(rows)+0.5), 
     axes=F, xlab='',ylab='',type='n')
axis(side=1,cex.axis=size.text, mgp=c(0,0.5,0),tck= -0.01) #, at=xlab1, labels=round(xlab2,2)
#axis(side=2,cex.axis=size.text,  mgp=c(0,0.5,0),tck= -0.01, las=1 ) # at=ylab1, labels=round(ylab2,2)
axis(side=2,at=c(1:length(rows)),labels=F,tck= -0.01)

text(par("usr")[1] - spc,1:length(rows),srt = 0, adj =adj,
     labels = covariates, xpd = TRUE,cex=cex)

# 95% CIs for censored analysis
segments(x0=Plot.data[2,], x1=Plot.data[3,],
         y0=1:length(rows), y1=1:length(rows), col=colorP,lwd=1)

# segments(x0=Plot.data[,4], x1=Plot.data[,6],
#          y0=1:length(rows), y1=1:length(rows), lwd=4, col="blue")
## Estiamtes from censored model
points(Plot.data[1,], 1:length(rows), col=colorP ,cex=1, pch=16)

abline(v=0, col="gray")
# abline(v=out2$mean$mu.ave, col="gray")

# Add x- and y-axis lables
mtext(x.label, line = 0.8, side = 1, cex = size.text, outer=T, adj=0.6)
mtext(y.label, line = 0.4, side = 2, cex = size.text, outer=T)

# abline(h=0)
box()

par(def.par)
dev.off()

##Print out R2 values 
margR2 <- out$mean$marginalR2
condR2 <- out$mean$conditionalR2

### next up= try to plot residuals! 
residuals<-observed-predicted
