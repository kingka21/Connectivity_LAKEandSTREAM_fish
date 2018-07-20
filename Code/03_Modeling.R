#### Written by Katelyn King modified from Ty Wagner
### Created: July 17, 2018 

install.packages('data.table')
install.packages('jagsUI')
install.packages('car')
install.packages('Metrics')

library(data.table)
library(jagsUI)
library(car)
library(Metrics)

 

# Define the model in the BUGS language and write a text file
sink("model.txt")
cat("
    model {
    # Likelihood: 
    for (i in 1:92){ 
    richness[i] ~ dpois(lambda[i])               
    log(lambda[i]) <- alpha[group[i]] + b[1] * lakearea[i] + b[2] * elevation[i] + b[3] * LakeConnec[i] + 
    b[4] * lake_sdf[i] + b[5] * iwswet[i] + b[6] * iwsag[i] + b[7] * iwsroad[i] + b[8] * iwslakecon[i] + 
    b[9] * iwswetcon[i] + b[10] * iwsstreamden[i]
    } 
    
    # Level-2 of the model
    for(j in 1:J){
    alpha[j] ~ dnorm(mu[j],tau.alpha)
    mu[j] <- mu.alpha + s[1] * precip[j] + s[2] * temp[j] + s[3] * rwet[j] + s[4] * rag[j] 
    + s[5] * rroad[j] + s[6] * rlakeconn[j] + s[7] * rwetcon[j] + s[8] * rstreamden[j] 
    }
    
    # Priors model 1 
    alpha~dnorm(0, 100000)
    # Level-1 predictors
    for(k in 1:10){
    b[k] ~ ddexp(0, lambda1)
    }

#Priors model 2
    mu.alpha ~ dnorm(0, 0.0001)    
     # Level-2 predictors
    for(k in 1:8){
    s[k] ~ ddexp(0, lambda2)
    }
    
    # Hyper-prior on lambda
    lambda1 ~ dgamma(0.1,0.1)
    lambda2 ~ dgamma(0.1,0.1)
    
    # Derived quantities
    tau.alpha <- pow(sigma.alpha,-2) 
    
    }
    
    Vrandom <- 1/tau.alpha  # get the variance of random intercept

    } # end model
    ",fill = TRUE)
sink()


# Number of regions
#J <-unique(fish.data$HU4_ZoneID)
J<-8
# Need to code a new column for group for HUC 4
fish.data$G<-as.numeric(as.factor(as.numeric(as.factor(fish.data$HU4_ZoneID))))

#need dummy variables for the Conny class 
fish.data$Conn2<-sapply(fish.data$LakeConnec, function(x) {
  if(x == 'DR_LakeStream') {1}
  else {
    if(x == 'Headwater') {3}
    else {
      if(x == 'DR_Stream') {2}
      else {
        if(x== 'Isolated') {4}
      }}}})

# Load data
data <- list(y = fish.data$richness, group = fish.data$G, J = J,
             lakearea = fish.data$lakearea_km2, elevation = fish.data$elevation_m, LakeConnec = fish.data$Conn2, lake_sdf = fish.data$lake_sdf,
             iwswet = fish.data$iws_nlcd2006_wet, iwsag = fish.data$iws_nlcd2006_agr, iwsroad = fish.data$iws_roaddensity_mperha, iwslakecon = fish.data$iws_upstream_lakes_4ha_area_ha,
             iwswetcon = fish.data$iws_wlconnections_area_ha, iwsstreamden = fish.data$iws_streamdensity_mperha, 
             precip = fish.data$prism_precip_mean, temp = fish.data$prism_temp_mean, rwet = fish.data$hu4_nlcd2006_wet, rag = fish.data$hu4_nlcd2006_agr, rroad = fish.data$hu4_roaddensity_mperha, 
             rlakeconn = fish.data$hu4_lakes_area_ha, rwetcon = fish.data$hu4_wetlands_area_ha, rstreamden = fish.data$hu4_streamdensity_mperha)

# Initial values
inits <- function (){
  list (mu.alpha = rnorm(1), sigma.alpha=runif(1) )
}


# Parameters monitored
parameters <- c("mu.alpha","sigma.alpha","b","lambda1",
                "Vrandom", "lambda2","s")


# MCMC settings
ni <- 90000
nt <- 2
nb <- 70000
nc <- 3

# Call JAGS from R 

out <- jags(data, inits, parameters, "model.txt", n.chains = nc, 
            n.thin = nt, n.iter = ni, n.burnin = nb, parallel=T)


##SAVE OUTPUT #####
#NOTE THIS NEEDS EDITING
saveRDS(out, file="something descriptive.rds")

# Calculate computation time

# Find which parameters, if any, have Rhat > 1.1
which(out$summary[, c("Rhat")] > 1.1)


# Summarize posteriors
print(out, dig = 3)
str(out)
traceplot(out)
outExp <- out$summary
write.csv(outExp, "CHL_ModelSummary.csv", row.names = T)
mcmcOut <- out$sims.list
saveRDS(mcmcOut, file="CHL_mcmc_out.rds")

betaEsts <- matrix(NA, nrow=3,ncol=22)
for(i in 1:22){
  betaEsts[1,i] <- mean(quantile(out$sims.list$b[,i],c(0.05,0.95)))
  betaEsts[2:3,i] <- quantile(out$sims.list$b[,i],c(0.05,0.95))
}
betaEsts


sEsts <- matrix(NA, nrow=3,ncol=10)
for(i in 1:10){
  sEsts[1,i] <- mean(quantile(out$sims.list$s[,i],c(0.05,0.95)))
  sEsts[2:3,i] <- quantile(out$sims.list$s[,i],c(0.05,0.95))
}
sEsts


predicted <- out$mean$predictY
observed <- tp_dat$log_tp

res <- 6
name_figure <- "CHL_observed_predicted.jpg"
jpeg(filename = name_figure, height = 500*res, width = 500*res, res=72*res)
def.par <- par(no.readonly = TRUE) 		# save default, for resetting...

nf <- layout(matrix(c(1:1),nrow=1,ncol=1,byrow=TRUE),  TRUE) 
layout.show(nf)
par(mar=c(0.5,0.5,0,0), oma=c(2,2,0,0) )

size.labels = 1
size.text = 1.0
y.label = expression(paste('Predicted ',log[e],'(CHL)' ))
x.label = expression(paste('Observed ',log[e],'(CHL)' ))

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
rmse(observed, predicted)

rmse_sim <- numeric()
for(i in 1:out$mcmc.info$n.samples){
  rmse_sim[i] <- rmse(observed, out$sims.list$predictY[i,])
  
}

mean(rmse_sim)
quantile(rmse_sim, c(0.05, 0.95))