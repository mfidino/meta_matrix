model{
	
# the latent process

for(i in 1:(nspec)){
	for(k in 1:(nsite)){
	# have some covariates here
	logit(psi_in[i,k]) <- b0 + b1[i] * cov[k,1] + 
	b2[i] * cov[k,2]
	z[i,k,1] ~ dbern(psi_in[i,k])
		for(t in 2:(nyear)){
		logit(psi[i,k,t]) <- ((gam[i] + gam1[i] * cov[k,1] + gam2[i] * cov[k,2])   * 
		sum(z[i,-k,t-1] * exp(-(alpha_open[i] * open_dmat[-k,k] + 
				  alpha_dvlphigh[i] * dvlphigh_dmat[-k,k] + 
				  alpha_dvlp_mid[i] * dvlpmid_dmat[-k,k] + 
				  alpha_green[i] * green_dmat[-k,k] + 
				  alpha_water[i] * water_dmat[-k,k] + 
				  alpha_ag[i] ( ag_dmat[-k,k]))*area[-k]^source_scale[i]) +
				  ) + temporal_noise[i,t-1]) * 
				  (1 - z[i,k,t-1]) + 

		}

	}
}

}