model{
	
### the latent process

for(i in 1:(nspec)){
	for(k in 1:(nsite)){
	# have some covariates here
	logit(psi_in[i,k]) <- b0[i] 
	z[i,k,1] ~ dbern(psi_in[i,k])
		for(t in 2:(nyear)){
			# The colonization part
		logit(C[i,k,t-1]) <- gam0[i,t-1]  + 
					gam_d[i] * sum(exp(-(sum(alpha_open[i] * (open_dmat[sm[,k],k] * z[i,sm[,k],t-1])) + 
							  sum(alpha_dvlp_high[i] * (dvlp_high_dmat[sm[,k],k] * z[i,sm[,k],t-1])) +
							  sum(alpha_dvlp_mid_vec[i] * (dvlp_mid_dmat[sm[,k],k] * z[i,sm[,k],t-1])) +
							  sum(alpha_green_vec[,i] * (green_dmat[sm[,k],k] * z[i,sm[,k],t-1])) +
							  sum(alpha_water_vec[,i] * (water_dmat[sm[,k],k] * z[i,sm[,k],t-1])) + 
							  sum(alpha_ag_vec[,i] * (ag_dmat[sm[,k],k] * z[i,sm[,k], t-1])))))
			# the extinction part
		logit(E[i,k,t-1]) <- phi0[i,t-1] 

			# merge them together
		psi[i,k,t] <- ((1 - z[i,k,t-1]) * C[i,k,t-1]) + (z[i,k,t-1] *(1 - E[i,k,t-1]))

				# if rescue effect occurs
		# logit(psi[i,k,t]) <- ((1 - z[i,k,t-1]) * C[i,k,t-1]) + (z[i,k,t-1] * (1 - C[i,k,t-1]) *( 1 - E[i,k,t-1]))

			# likelihood
		z[i,k,t] ~ dbern(psi[i,k,t])
		}

	}
}

### observation model

for(i in 1:(nspec)){
	for(k in 1:(nsite)){
		for(t in 1:(nyear)){
			logit(p[i,k,t]) <- p0[i,t] 
			muZ[i,k,t] <- z[i,k,t] * p[i,k,t]
			y[i,k,t] ~ dbin(muZ[i,k,t], jmat[k,t])
		}
	}
}

### priors

for(i in 1:(nspec)){
	#first season
	b0[i] ~ dt(0, 2.5, 1) # psi intercept
	#b1[i] ~ dt(0, 2.5, 1) # covariate effect 1 (to be decided)
	#b2[i] ~ dt(0, 2.5, 1) # covariate effect 2 (to be decided)
	#colonization
	#gam1[i] ~ dt(0, 2.5, 1) # covariate effect 1 (to be decided)
	#gam2[i] ~ dt(0, 2.5, 1) # covariate effect 2 (to be decided)
	#gam3[i] ~ dt(0, 2.5, 1) # covariate effect 1 squared
	#gam4[i] ~ dt(0, 2.5, 1) # covariate effect 2 squared
	gam_d[i] ~ dt(0, 2.5, 1) # population-level effective dispersal rate
	# The alpha parameters all have to be positive to be biologically reasonable
	alpha_open[i] ~ dgamma(0.01, 0.01) # dispersal scale parameter for open space
	alpha_dvlp_high[i] ~ dgamma(0.01, 0.01) # dispersal scale parameter for high density urban
	alpha_dvlp_mid[i] ~ dgamma(0.01, 0.01) # dispersal scale parameter for mid and low density urban
	alpha_green[i] ~ dgamma(0.01, 0.01) # dispersal scale parameter for green space
	alpha_water[i] ~ dgamma(0.01, 0.01) # dispersal scale parameter for water
	alpha_ag[i] ~ dgamma(0.01, 0.01) # dispersal scale parameter for agriculture
	# fill in the vectors
	for(k in 1:(nsite - 1)){
		alpha_open_vec[k,i] <- alpha_open[i]
		alpha_dvlp_high_vec[k,i] <- alpha_dvlp_high[i]
		alpha_dvlp_mid_vec[k,i] <- alpha_dvlp_mid[i]
		alpha_green_vec[k,i] <- alpha_green[i]
		alpha_ag_vec[k,i] <- alpha_ag[i]
		alpha_water_vec[k,i] <- alpha_water[i]
	}
	#extinction
	#phi1[i] ~ dt(0, 2.5, 1) # covariate effect 1 (to be decided)
	#phi2[i] ~ dt(0, 2.5, 1) # covariate effect 2 (to be decided)
	#phi3[i] ~ dt(0, 2.5, 1) # covariate effect 1 squared
	#phi4[i] ~ dt(0, 2.5, 1) # covariate effect 2 squared
	# detection
	#p1[i] ~ dt(0, 2.5, 1) # covariate effect for detection
	for(t in 1:(nyear-1)){
		gam0[i,t] ~ dnorm(0, 0.01) # random intercept for colonization
		phi0[i,t] ~ dnorm(0, 0.01) # random intercept for extinction
	}
	for(v in 1:(nyear)){
		p0[i,v] ~ dnorm(0, 0.01) # random intercept for detection
	}

}


}