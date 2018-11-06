########################
#### Simplenumbers #####
########################
simplenumbers <- NULL
simplenumbers_yfp <- data.frame(colMeans(YH_fp[,varlist2]))
simplenumbers_yfa <- data.frame(colMeans(YH_fa[,varlist2]))
simplenumbers_yap <- data.frame(colMeans(YH_ap[,varlist2]))
simplenumbers_yaa <- data.frame(colMeans(YH_aa[,varlist2]))

simplenumbers_rfp <- data.frame(colMeans(RBS_fp[,varlist2]))
simplenumbers_rfa <- data.frame(colMeans(RBS_fa[,varlist2]))
simplenumbers_rap <- data.frame(colMeans(RBS_ap[,varlist2]))
simplenumbers_raa <- data.frame(colMeans(RBS_aa[,varlist2]))

simplenumbers_yfp$sd <- colSds(as.matrix(YH_fp[,varlist2]))
simplenumbers_yfa$sd <- colSds(as.matrix(YH_fa[,varlist2]))
simplenumbers_yap$sd <- colSds(as.matrix(YH_ap[,varlist2]))
simplenumbers_yaa$sd <- colSds(as.matrix(YH_aa[,varlist2]))

simplenumbers_rfp$sd <- colSds(as.matrix(RBS_fp[,varlist2]))
simplenumbers_rfa$sd <- colSds(as.matrix(RBS_fa[,varlist2]))
simplenumbers_rap$sd <- colSds(as.matrix(RBS_ap[,varlist2]))
simplenumbers_raa$sd <- colSds(as.matrix(RBS_aa[,varlist2]))

count(RBS_ap[which(RBS_ap$trees=="yes"),])/length(RBS_ap$trees)
count(RBS_aa[which(RBS_aa$trees=="yes"),])/length(RBS_aa$trees)
count(RBS_fp[which(RBS_fp$trees=="yes"),])/length(RBS_fp$trees)
count(RBS_fa[which(RBS_fa$trees=="yes"),])/length(RBS_fa$trees)

count(YH_ap[which(YH_ap$trees=="yes"),])/length(YH_ap$trees)
count(YH_aa[which(YH_aa$trees=="yes"),])/length(YH_aa$trees)
count(YH_fp[which(YH_fp$trees=="yes"),])/length(YH_fp$trees)
count(YH_fa[which(YH_fa$trees=="yes"),])/length(YH_fa$trees)

count(RBS_ap[which(RBS_ap$stubs=="yes"),])/length(RBS_ap$stubs)
count(RBS_aa[which(RBS_aa$stubs=="yes"),])/length(RBS_aa$stubs)
count(RBS_fp[which(RBS_fp$stubs=="yes"),])/length(RBS_fp$stubs)
count(RBS_fa[which(RBS_fa$stubs=="yes"),])/length(RBS_fa$stubs)

count(YH_ap[which(YH_ap$stubs=="yes"),])/length(YH_ap$stubs)
count(YH_aa[which(YH_aa$stubs=="yes"),])/length(YH_aa$stubs)
count(YH_fp[which(YH_fp$stubs=="yes"),])/length(YH_fp$stubs)
count(YH_fa[which(YH_fa$stubs=="yes"),])/length(YH_fa$stubs)

for(i in 1:17){
  variable <- varlist[i]
  mean <- mean(yhobs_f[,variable]) 
  sd <- sd()
  simplenumbers <- rbind(simplenumbers, data.frame(variable, mean, sd))
}

rbsf_yr_occ <- as.data.frame(aggregate(as.numeric(rbsobs_f$rbs_occ), list(rbsobs_f$year), mean))
yhf_yr_occ <- aggregate(as.numeric(yhobs_f$yh_occ), list(yhobs_f$year), mean)
