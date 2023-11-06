library(ncdf4)
library(extRemes)
library(MASS)
library(geosphere)
library(tidyr)
library(dgof)
library(phonTools)
library(Metrics)


rm(list = ls())

#importing datasets
indices <-read.csv("/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/indices.csv")
narr <- nc_open('/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/narr.nc4')
e1 <- nc_open('/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/cropped_E1.nc4')
e2 <- nc_open('/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/cropped_E2.nc4')
e3 <- nc_open('/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/cropped_E3.nc4')
e4 <- nc_open('/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/cropped_E4.nc4')
e5 <- nc_open('/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/cropped_E5.nc4')
e6 <- nc_open('/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/cropped_E6.nc4')
e7 <- nc_open('/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/cropped_E7.nc4')
e8 <- nc_open('/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/cropped_E8.nc4')
e9 <- nc_open('/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/cropped_E9.nc4')

#extracting latitude and longtitude data from netcdf file
nlon <- ncvar_get(narr, "lon")
nlat <- ncvar_get(narr, "lat")
ntime <- ncvar_get(narr,"time")
elat <- ncvar_get(e1, "lat")
elon <- ncvar_get(e1, "lon") - 180
etime <- ncvar_get(e1,"time")

#extracting winter rainfall (acpcp) data
n_acpcp <- ncvar_get(narr, "acpcp")
e1_acpcp <- ncvar_get(e1, "PRECT")
e2_acpcp <- ncvar_get(e2, "PRECT")
e3_acpcp <- ncvar_get(e3, "PRECT")
e4_acpcp <- ncvar_get(e4, "PRECT")
e5_acpcp <- ncvar_get(e5, "PRECT")
e6_acpcp <- ncvar_get(e6, "PRECT")
e7_acpcp <- ncvar_get(e7, "PRECT")
e8_acpcp <- ncvar_get(e8, "PRECT")
e9_acpcp <- ncvar_get(e9, "PRECT")

n_enso_risk_factor <- matrix(0,nrow=dim(nlon)[1],ncol=dim(nlon)[2])
n_nao_risk_factor <- matrix(0,nrow=dim(nlon)[1],ncol=dim(nlon)[2])
n_pdo_risk_factor <- matrix(0,nrow=dim(nlon)[1],ncol=dim(nlon)[2])

e1_enso_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e1_nao_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e1_pdo_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))

e2_enso_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e2_nao_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e2_pdo_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))

e3_enso_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e3_nao_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e3_pdo_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))

e4_enso_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e4_nao_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e4_pdo_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))

e5_enso_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e5_nao_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e5_pdo_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))

e6_enso_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e6_nao_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e6_pdo_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))

e7_enso_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e7_nao_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e7_pdo_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))

e8_enso_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e8_nao_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e8_pdo_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))

e9_enso_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e9_nao_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e9_pdo_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))

for(i in 1:dim(nlon)[1]){
  for(j in 1:dim(nlon)[2]){
    
    data1<-n_acpcp[i,j,]
    
    if (sum(is.na(data1)) >= 50){
      
      n_enso_risk_factor[i,j] <- "NA"
      n_nao_risk_factor[i,j] <- "NA"
      n_pdo_risk_factor[i,j] <- "NA"
      
    }
    else{
      
      df <- data.frame(data1,indices$year,indices$month,indices$soi,indices$naoi,indices$pdoi)
      colnames(df) <- c("acpcp","year","month","soi","naoi","pdoi")
      
      df<- na.omit(df)

      enso_results <- extRemes::fevd(df$acpcp,data=df,location.fun=~soi,scale.fun=~soi)
      nao_results <- extRemes::fevd(df$acpcp,data=df,location.fun=~naoi,scale.fun=~naoi)
      pdo_results <- extRemes::fevd(df$acpcp,data=df,location.fun=~pdoi,scale.fun=~pdoi)
      
      enso_mu0 <- enso_results$results$par[1]
      enso_mu1 <- enso_results$results$par[2]
      enso_sigma0 <- enso_results$results$par[3]
      enso_sigma1 <- enso_results$results$par[4]
      enso_shape <- enso_results$results$par[5]
      
      nao_mu0 <- nao_results$results$par[1]
      nao_mu1 <- nao_results$results$par[2]
      nao_sigma0 <- nao_results$results$par[3]
      nao_sigma1 <- nao_results$results$par[4]
      nao_shape <- nao_results$results$par[5]
      
      pdo_mu0 <- pdo_results$results$par[1]
      pdo_mu1 <- pdo_results$results$par[2]
      pdo_sigma0 <- pdo_results$results$par[3]
      pdo_sigma1 <- pdo_results$results$par[4]
      pdo_shape <- pdo_results$results$par[5]
      
      cat(i,j,"\n")
      
      enso_rvalue <- extRemes::rlevd(df$acpcp,loc=enso_mu0 + enso_mu1 * (-1),scale=max(1e-100,enso_sigma0 + enso_sigma1 * (-1)),shape =enso_shape,period=20)
      enso_prob <- pevd(enso_rvalue, loc = enso_mu0 + enso_mu1 * (1), scale = enso_sigma0 + enso_sigma1 * (1), shape = enso_shape,lower.tail =FALSE)
      n_enso_risk_factor[i,j] <- enso_prob/0.05
      
      nao_rvalue <- extRemes::rlevd(df$acpcp,loc=nao_mu0 + nao_mu1 * (-1),scale=max(1e-100,nao_sigma0 + nao_sigma1 * (-1)),shape = nao_shape,period=20)
      nao_prob <- pevd(nao_rvalue, loc = nao_mu0 + nao_mu1 * (1), scale = max(1e-100,nao_sigma0 + nao_sigma1 * (1)), shape = nao_shape,lower.tail =FALSE)
      n_nao_risk_factor[i,j] <- nao_prob/0.05
      
      pdo_rvalue <- extRemes::rlevd(df$acpcp,loc=pdo_mu0 + pdo_mu1 * (-1),scale=max(1e-100,pdo_sigma0 + pdo_sigma1 * (-1)),shape = pdo_shape,period=20)
      pdo_prob <- pevd(nao_rvalue, loc = pdo_mu0 + pdo_mu1 * (1), scale = max(1e-100,pdo_sigma0 + pdo_sigma1 * (1)), shape = pdo_shape,lower.tail =FALSE)
      n_pdo_risk_factor[i,j] <- pdo_prob/0.05
    }
  }
}

n_enso_rf <- data.frame(n_enso_risk_factor)
n_nao_rf <- data.frame(n_nao_risk_factor)
n_pdo_rf <- data.frame(n_pdo_risk_factor)

write.csv(n_enso_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/n_enso_risk_factor.csv", row.names = FALSE)
write.csv(n_nao_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/n_nao_risk_factor.csv", row.names = FALSE)
write.csv(n_pdo_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/n_pdo_risk_factor.csv", row.names = FALSE)

for(i in 1:dim(elon)){
  for(j in 1:dim(elat)){
    
    data1<-e1_acpcp[i,j,]
    
    if (all(is.na(data1))){
      
      e1_enso_risk_factor[i,j] <- "NA"
      e1_nao_risk_factor[i,j] <- "NA"
      e1_pdo_risk_factor[i,j] <- "NA"
      
    }
    else{
      
      df <- data.frame(data1,indices$year,indices$month,indices$soi,indices$naoi,indices$pdoi)
      colnames(df) <- c("acpcp","year","month","soi","naoi","pdoi")
      
      df<- na.omit(df)
      
      enso_results <- extRemes::fevd(df$acpcp,data=df,location.fun=~soi,scale.fun=~soi)
      nao_results <- extRemes::fevd(df$acpcp,data=df,location.fun=~naoi,scale.fun=~naoi)
      pdo_results <- extRemes::fevd(df$acpcp,data=df,location.fun=~pdoi,scale.fun=~pdoi)
      
      enso_mu0 <- enso_results$results$par[1]
      enso_mu1 <- enso_results$results$par[2]
      enso_sigma0 <- enso_results$results$par[3]
      enso_sigma1 <- enso_results$results$par[4]
      enso_shape <- enso_results$results$par[5]
      
      nao_mu0 <- nao_results$results$par[1]
      nao_mu1 <- nao_results$results$par[2]
      nao_sigma0 <- nao_results$results$par[3]
      nao_sigma1 <- nao_results$results$par[4]
      nao_shape <- nao_results$results$par[5]
      
      pdo_mu0 <- pdo_results$results$par[1]
      pdo_mu1 <- pdo_results$results$par[2]
      pdo_sigma0 <- pdo_results$results$par[3]
      pdo_sigma1 <- pdo_results$results$par[4]
      pdo_shape <- pdo_results$results$par[5]
      
      cat(i,j,"\n")
      
      enso_rvalue <- extRemes::rlevd(df$acpcp,loc=enso_mu0 + enso_mu1 * (-1),scale=max(1e-100,enso_sigma0 + enso_sigma1 * (-1)),shape =enso_shape,period=20)
      enso_prob <- pevd(enso_rvalue, loc = enso_mu0 + enso_mu1 * (1), scale = max(1e-100,enso_sigma0 + enso_sigma1 * (1)), shape = enso_shape,lower.tail =FALSE)
      e1_enso_risk_factor[i,j] <- enso_prob/0.05
      
      nao_rvalue <- extRemes::rlevd(df$acpcp,loc=nao_mu0 + nao_mu1 * (-1),scale=max(1e-100,nao_sigma0 + nao_sigma1 * (-1)),shape = nao_shape,period=20)
      nao_prob <- pevd(nao_rvalue, loc = nao_mu0 + nao_mu1 * (1), scale = max(1e-100,nao_sigma0 + nao_sigma1 * (1)), shape = nao_shape,lower.tail =FALSE)
      e1_nao_risk_factor[i,j] <- nao_prob/0.05
      
      pdo_rvalue <- extRemes::rlevd(df$acpcp,loc=pdo_mu0 + pdo_mu1 * (-1),scale=max(1e-100,pdo_sigma0 + pdo_sigma1 * (-1)),shape = pdo_shape,period=20)
      pdo_prob <- pevd(nao_rvalue, loc = pdo_mu0 + pdo_mu1 * (1), scale = max(1e-100,pdo_sigma0 + pdo_sigma1 * (1)), shape = pdo_shape,lower.tail =FALSE)
      e1_pdo_risk_factor[i,j] <- pdo_prob/0.05
    }
  }
}

enso_rf <- data.frame(e1_enso_risk_factor)
nao_rf <- data.frame(e1_nao_risk_factor)
pdo_rf <- data.frame(e1_pdo_risk_factor)

write.csv(enso_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e1_enso_risk_factor.csv", row.names = FALSE)
write.csv(nao_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e1_nao_risk_factor.csv", row.names = FALSE)
write.csv(pdo_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e1_pdo_risk_factor.csv", row.names = FALSE)

for(i in 1:dim(elon)){
  for(j in 1:dim(elat)){
    
    data1<-e2_acpcp[i,j,]
    
    if (all(is.na(data1))){
      
      e2_enso_risk_factor[i,j] <- "NA"
      e2_nao_risk_factor[i,j] <- "NA"
      e2_pdo_risk_factor[i,j] <- "NA"
      
    }
    else{
      
      df <- data.frame(data1,indices$year,indices$month,indices$soi,indices$naoi,indices$pdoi)
      colnames(df) <- c("acpcp","year","month","soi","naoi","pdoi")
      
      df<- na.omit(df)
      
      enso_results <- extRemes::fevd(df$acpcp,data=df,location.fun=~soi,scale.fun=~soi)
      nao_results <- extRemes::fevd(df$acpcp,data=df,location.fun=~naoi,scale.fun=~naoi)
      pdo_results <- extRemes::fevd(df$acpcp,data=df,location.fun=~pdoi,scale.fun=~pdoi)
      
      enso_mu0 <- enso_results$results$par[1]
      enso_mu1 <- enso_results$results$par[2]
      enso_sigma0 <- enso_results$results$par[3]
      enso_sigma1 <- enso_results$results$par[4]
      enso_shape <- enso_results$results$par[5]
      
      nao_mu0 <- nao_results$results$par[1]
      nao_mu1 <- nao_results$results$par[2]
      nao_sigma0 <- nao_results$results$par[3]
      nao_sigma1 <- nao_results$results$par[4]
      nao_shape <- nao_results$results$par[5]
      
      pdo_mu0 <- pdo_results$results$par[1]
      pdo_mu1 <- pdo_results$results$par[2]
      pdo_sigma0 <- pdo_results$results$par[3]
      pdo_sigma1 <- pdo_results$results$par[4]
      pdo_shape <- pdo_results$results$par[5]
      
      cat(i,j,"\n")
      
      enso_rvalue <- extRemes::rlevd(df$acpcp,loc=enso_mu0 + enso_mu1 * (-1),scale=max(1e-100,enso_sigma0 + enso_sigma1 * (-1)),shape =enso_shape,period=20)
      enso_prob <- pevd(enso_rvalue, loc = enso_mu0 + enso_mu1 * (1), scale = max(1e-100,enso_sigma0 + enso_sigma1 * (1)), shape = enso_shape,lower.tail =FALSE)
      e2_enso_risk_factor[i,j] <- enso_prob/0.05
      
      nao_rvalue <- extRemes::rlevd(df$acpcp,loc=nao_mu0 + nao_mu1 * (-1),scale=max(1e-100,nao_sigma0 + nao_sigma1 * (-1)),shape = nao_shape,period=20)
      nao_prob <- pevd(nao_rvalue, loc = nao_mu0 + nao_mu1 * (1), scale = max(1e-100,nao_sigma0 + nao_sigma1 * (1)), shape = nao_shape,lower.tail =FALSE)
      e2_nao_risk_factor[i,j] <- nao_prob/0.05
      
      pdo_rvalue <- extRemes::rlevd(df$acpcp,loc=pdo_mu0 + pdo_mu1 * (-1),scale=max(1e-100,pdo_sigma0 + pdo_sigma1 * (-1)),shape = pdo_shape,period=20)
      pdo_prob <- pevd(nao_rvalue, loc = pdo_mu0 + pdo_mu1 * (1), scale = max(1e-100,pdo_sigma0 + pdo_sigma1 * (1)), shape = pdo_shape,lower.tail =FALSE)
      e2_pdo_risk_factor[i,j] <- pdo_prob/0.05
    }
  }
}

enso_rf <- data.frame(e2_enso_risk_factor)
nao_rf <- data.frame(e2_nao_risk_factor)
pdo_rf <- data.frame(e2_pdo_risk_factor)

write.csv(enso_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e2_enso_risk_factor.csv", row.names = FALSE)
write.csv(nao_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e2_nao_risk_factor.csv", row.names = FALSE)
write.csv(pdo_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e2_pdo_risk_factor.csv", row.names = FALSE)

for(i in 1:dim(elon)){
  for(j in 1:dim(elat)){
    
    data1<-e3_acpcp[i,j,]
    
    if (all(is.na(data1))){
      
      e3_enso_risk_factor[i,j] <- "NA"
      e3_nao_risk_factor[i,j] <- "NA"
      e3_pdo_risk_factor[i,j] <- "NA"
      
    }
    else{
      
      df <- data.frame(data1,indices$year,indices$month,indices$soi,indices$naoi,indices$pdoi)
      colnames(df) <- c("acpcp","year","month","soi","naoi","pdoi")
      
      df<- na.omit(df)
      
      enso_results <- extRemes::fevd(df$acpcp,data=df,location.fun=~soi,scale.fun=~soi)
      nao_results <- extRemes::fevd(df$acpcp,data=df,location.fun=~naoi,scale.fun=~naoi)
      pdo_results <- extRemes::fevd(df$acpcp,data=df,location.fun=~pdoi,scale.fun=~pdoi)
      
      enso_mu0 <- enso_results$results$par[1]
      enso_mu1 <- enso_results$results$par[2]
      enso_sigma0 <- enso_results$results$par[3]
      enso_sigma1 <- enso_results$results$par[4]
      enso_shape <- enso_results$results$par[5]
      
      nao_mu0 <- nao_results$results$par[1]
      nao_mu1 <- nao_results$results$par[2]
      nao_sigma0 <- nao_results$results$par[3]
      nao_sigma1 <- nao_results$results$par[4]
      nao_shape <- nao_results$results$par[5]
      
      pdo_mu0 <- pdo_results$results$par[1]
      pdo_mu1 <- pdo_results$results$par[2]
      pdo_sigma0 <- pdo_results$results$par[3]
      pdo_sigma1 <- pdo_results$results$par[4]
      pdo_shape <- pdo_results$results$par[5]
      
      cat(i,j,"\n")
      
      enso_rvalue <- extRemes::rlevd(df$acpcp,loc=enso_mu0 + enso_mu1 * (-1),scale=max(1e-100,enso_sigma0 + enso_sigma1 * (-1)),shape =enso_shape,period=20)
      enso_prob <- pevd(enso_rvalue, loc = enso_mu0 + enso_mu1 * (1), scale = max(1e-100,enso_sigma0 + enso_sigma1 * (1)), shape = enso_shape,lower.tail =FALSE)
      e3_enso_risk_factor[i,j] <- enso_prob/0.05
      
      nao_rvalue <- extRemes::rlevd(df$acpcp,loc=nao_mu0 + nao_mu1 * (-1),scale=max(1e-100,nao_sigma0 + nao_sigma1 * (-1)),shape = nao_shape,period=20)
      nao_prob <- pevd(nao_rvalue, loc = nao_mu0 + nao_mu1 * (1), scale = max(1e-100,nao_sigma0 + nao_sigma1 * (1)), shape = nao_shape,lower.tail =FALSE)
      e3_nao_risk_factor[i,j] <- nao_prob/0.05
      
      pdo_rvalue <- extRemes::rlevd(df$acpcp,loc=pdo_mu0 + pdo_mu1 * (-1),scale=max(1e-100,pdo_sigma0 + pdo_sigma1 * (-1)),shape = pdo_shape,period=20)
      pdo_prob <- pevd(nao_rvalue, loc = pdo_mu0 + pdo_mu1 * (1), scale = max(1e-100,pdo_sigma0 + pdo_sigma1 * (1)), shape = pdo_shape,lower.tail =FALSE)
      e3_pdo_risk_factor[i,j] <- pdo_prob/0.05
    }
  }
}

enso_rf <- data.frame(e3_enso_risk_factor)
nao_rf <- data.frame(e3_nao_risk_factor)
pdo_rf <- data.frame(e3_pdo_risk_factor)

write.csv(enso_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e3_enso_risk_factor.csv", row.names = FALSE)
write.csv(nao_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e3_nao_risk_factor.csv", row.names = FALSE)
write.csv(pdo_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e3_pdo_risk_factor.csv", row.names = FALSE)

for(i in 1:dim(elon)){
  for(j in 1:dim(elat)){
    
    data1<-e4_acpcp[i,j,]
    
    if (all(is.na(data1))){
      
      e4_enso_risk_factor[i,j] <- "NA"
      e4_nao_risk_factor[i,j] <- "NA"
      e4_pdo_risk_factor[i,j] <- "NA"
      
    }
    else{
      
      df <- data.frame(data1,indices$year,indices$month,indices$soi,indices$naoi,indices$pdoi)
      colnames(df) <- c("acpcp","year","month","soi","naoi","pdoi")
      
      df<- na.omit(df)
      
      enso_results <- extRemes::fevd(df$acpcp,data=df,location.fun=~soi,scale.fun=~soi)
      nao_results <- extRemes::fevd(df$acpcp,data=df,location.fun=~naoi,scale.fun=~naoi)
      pdo_results <- extRemes::fevd(df$acpcp,data=df,location.fun=~pdoi,scale.fun=~pdoi)
      
      enso_mu0 <- enso_results$results$par[1]
      enso_mu1 <- enso_results$results$par[2]
      enso_sigma0 <- enso_results$results$par[3]
      enso_sigma1 <- enso_results$results$par[4]
      enso_shape <- enso_results$results$par[5]
      
      nao_mu0 <- nao_results$results$par[1]
      nao_mu1 <- nao_results$results$par[2]
      nao_sigma0 <- nao_results$results$par[3]
      nao_sigma1 <- nao_results$results$par[4]
      nao_shape <- nao_results$results$par[5]
      
      pdo_mu0 <- pdo_results$results$par[1]
      pdo_mu1 <- pdo_results$results$par[2]
      pdo_sigma0 <- pdo_results$results$par[3]
      pdo_sigma1 <- pdo_results$results$par[4]
      pdo_shape <- pdo_results$results$par[5]
      
      cat(i,j,"\n")
      
      enso_rvalue <- extRemes::rlevd(df$acpcp,loc=enso_mu0 + enso_mu1 * (-1),scale=max(1e-100,enso_sigma0 + enso_sigma1 * (-1)),shape =enso_shape,period=20)
      enso_prob <- pevd(enso_rvalue, loc = enso_mu0 + enso_mu1 * (1), scale = max(1e-100,enso_sigma0 + enso_sigma1 * (1)), shape = enso_shape,lower.tail =FALSE)
      e4_enso_risk_factor[i,j] <- enso_prob/0.05
      
      nao_rvalue <- extRemes::rlevd(df$acpcp,loc=nao_mu0 + nao_mu1 * (-1),scale=max(1e-100,nao_sigma0 + nao_sigma1 * (-1)),shape = nao_shape,period=20)
      nao_prob <- pevd(nao_rvalue, loc = nao_mu0 + nao_mu1 * (1), scale = max(1e-100,nao_sigma0 + nao_sigma1 * (1)), shape = nao_shape,lower.tail =FALSE)
      e4_nao_risk_factor[i,j] <- nao_prob/0.05
      
      pdo_rvalue <- extRemes::rlevd(df$acpcp,loc=pdo_mu0 + pdo_mu1 * (-1),scale=max(1e-100,pdo_sigma0 + pdo_sigma1 * (-1)),shape = pdo_shape,period=20)
      pdo_prob <- pevd(nao_rvalue, loc = pdo_mu0 + pdo_mu1 * (1), scale = max(1e-100,pdo_sigma0 + pdo_sigma1 * (1)), shape = pdo_shape,lower.tail =FALSE)
      e4_pdo_risk_factor[i,j] <- pdo_prob/0.05
    }
  }
}

enso_rf <- data.frame(e4_enso_risk_factor)
nao_rf <- data.frame(e4_nao_risk_factor)
pdo_rf <- data.frame(e4_pdo_risk_factor)

write.csv(enso_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e4_enso_risk_factor.csv", row.names = FALSE)
write.csv(nao_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e4_nao_risk_factor.csv", row.names = FALSE)
write.csv(pdo_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e4_pdo_risk_factor.csv", row.names = FALSE)

for(i in 1:dim(elon)){
  for(j in 1:dim(elat)){
    
    data1<-e5_acpcp[i,j,]
    
    if (all(is.na(data1))){
      
      e5_enso_risk_factor[i,j] <- "NA"
      e5_nao_risk_factor[i,j] <- "NA"
      e5_pdo_risk_factor[i,j] <- "NA"
      
    }
    else{
      
      df <- data.frame(data1,indices$year,indices$month,indices$soi,indices$naoi,indices$pdoi)
      colnames(df) <- c("acpcp","year","month","soi","naoi","pdoi")
      
      df<- na.omit(df)
      
      enso_results <- extRemes::fevd(df$acpcp,data=df,location.fun=~soi,scale.fun=~soi)
      nao_results <- extRemes::fevd(df$acpcp,data=df,location.fun=~naoi,scale.fun=~naoi)
      pdo_results <- extRemes::fevd(df$acpcp,data=df,location.fun=~pdoi,scale.fun=~pdoi)
      
      enso_mu0 <- enso_results$results$par[1]
      enso_mu1 <- enso_results$results$par[2]
      enso_sigma0 <- enso_results$results$par[3]
      enso_sigma1 <- enso_results$results$par[4]
      enso_shape <- enso_results$results$par[5]
      
      nao_mu0 <- nao_results$results$par[1]
      nao_mu1 <- nao_results$results$par[2]
      nao_sigma0 <- nao_results$results$par[3]
      nao_sigma1 <- nao_results$results$par[4]
      nao_shape <- nao_results$results$par[5]
      
      pdo_mu0 <- pdo_results$results$par[1]
      pdo_mu1 <- pdo_results$results$par[2]
      pdo_sigma0 <- pdo_results$results$par[3]
      pdo_sigma1 <- pdo_results$results$par[4]
      pdo_shape <- pdo_results$results$par[5]
      
      cat(i,j,"\n")
      
      enso_rvalue <- extRemes::rlevd(df$acpcp,loc=enso_mu0 + enso_mu1 * (-1),scale=max(1e-100,enso_sigma0 + enso_sigma1 * (-1)),shape =enso_shape,period=20)
      enso_prob <- pevd(enso_rvalue, loc = enso_mu0 + enso_mu1 * (1), scale = max(1e-100,enso_sigma0 + enso_sigma1 * (1)), shape = enso_shape,lower.tail =FALSE)
      e5_enso_risk_factor[i,j] <- enso_prob/0.05
      
      nao_rvalue <- extRemes::rlevd(df$acpcp,loc=nao_mu0 + nao_mu1 * (-1),scale=max(1e-100,nao_sigma0 + nao_sigma1 * (-1)),shape = nao_shape,period=20)
      nao_prob <- pevd(nao_rvalue, loc = nao_mu0 + nao_mu1 * (1), scale = max(1e-100,nao_sigma0 + nao_sigma1 * (1)), shape = nao_shape,lower.tail =FALSE)
      e5_nao_risk_factor[i,j] <- nao_prob/0.05
      
      pdo_rvalue <- extRemes::rlevd(df$acpcp,loc=pdo_mu0 + pdo_mu1 * (-1),scale=max(1e-100,pdo_sigma0 + pdo_sigma1 * (-1)),shape = pdo_shape,period=20)
      pdo_prob <- pevd(nao_rvalue, loc = pdo_mu0 + pdo_mu1 * (1), scale = max(1e-100,pdo_sigma0 + pdo_sigma1 * (1)), shape = pdo_shape,lower.tail =FALSE)
      e5_pdo_risk_factor[i,j] <- pdo_prob/0.05
    }
  }
}

enso_rf <- data.frame(e5_enso_risk_factor)
nao_rf <- data.frame(e5_nao_risk_factor)
pdo_rf <- data.frame(e5_pdo_risk_factor)

write.csv(enso_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e5_enso_risk_factor.csv", row.names = FALSE)
write.csv(nao_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e5_nao_risk_factor.csv", row.names = FALSE)
write.csv(pdo_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e5_pdo_risk_factor.csv", row.names = FALSE)

for(i in 1:dim(elon)){
  for(j in 1:dim(elat)){
    
    data1<-e6_acpcp[i,j,]
    
    if (all(is.na(data1))){
      
      e6_enso_risk_factor[i,j] <- "NA"
      e6_nao_risk_factor[i,j] <- "NA"
      e6_pdo_risk_factor[i,j] <- "NA"
      
    }
    else{
      
      df <- data.frame(data1,indices$year,indices$month,indices$soi,indices$naoi,indices$pdoi)
      colnames(df) <- c("acpcp","year","month","soi","naoi","pdoi")
      
      df<- na.omit(df)
      
      enso_results <- extRemes::fevd(df$acpcp,data=df,location.fun=~soi,scale.fun=~soi)
      nao_results <- extRemes::fevd(df$acpcp,data=df,location.fun=~naoi,scale.fun=~naoi)
      pdo_results <- extRemes::fevd(df$acpcp,data=df,location.fun=~pdoi,scale.fun=~pdoi)
      
      enso_mu0 <- enso_results$results$par[1]
      enso_mu1 <- enso_results$results$par[2]
      enso_sigma0 <- enso_results$results$par[3]
      enso_sigma1 <- enso_results$results$par[4]
      enso_shape <- enso_results$results$par[5]
      
      nao_mu0 <- nao_results$results$par[1]
      nao_mu1 <- nao_results$results$par[2]
      nao_sigma0 <- nao_results$results$par[3]
      nao_sigma1 <- nao_results$results$par[4]
      nao_shape <- nao_results$results$par[5]
      
      pdo_mu0 <- pdo_results$results$par[1]
      pdo_mu1 <- pdo_results$results$par[2]
      pdo_sigma0 <- pdo_results$results$par[3]
      pdo_sigma1 <- pdo_results$results$par[4]
      pdo_shape <- pdo_results$results$par[5]
      
      cat(i,j,"\n")
      
      enso_rvalue <- extRemes::rlevd(df$acpcp,loc=enso_mu0 + enso_mu1 * (-1),scale=max(1e-100,enso_sigma0 + enso_sigma1 * (-1)),shape =enso_shape,period=20)
      enso_prob <- pevd(enso_rvalue, loc = enso_mu0 + enso_mu1 * (1), scale = max(1e-100,enso_sigma0 + enso_sigma1 * (1)), shape = enso_shape,lower.tail =FALSE)
      e6_enso_risk_factor[i,j] <- enso_prob/0.05
      
      nao_rvalue <- extRemes::rlevd(df$acpcp,loc=nao_mu0 + nao_mu1 * (-1),scale=max(1e-100,nao_sigma0 + nao_sigma1 * (-1)),shape = nao_shape,period=20)
      nao_prob <- pevd(nao_rvalue, loc = nao_mu0 + nao_mu1 * (1), scale = max(1e-100,nao_sigma0 + nao_sigma1 * (1)), shape = nao_shape,lower.tail =FALSE)
      e6_nao_risk_factor[i,j] <- nao_prob/0.05
      
      pdo_rvalue <- extRemes::rlevd(df$acpcp,loc=pdo_mu0 + pdo_mu1 * (-1),scale=max(1e-100,pdo_sigma0 + pdo_sigma1 * (-1)),shape = pdo_shape,period=20)
      pdo_prob <- pevd(nao_rvalue, loc = pdo_mu0 + pdo_mu1 * (1), scale = max(1e-100,pdo_sigma0 + pdo_sigma1 * (1)), shape = pdo_shape,lower.tail =FALSE)
      e6_pdo_risk_factor[i,j] <- pdo_prob/0.05
    }
  }
}

enso_rf <- data.frame(e6_enso_risk_factor)
nao_rf <- data.frame(e6_nao_risk_factor)
pdo_rf <- data.frame(e6_pdo_risk_factor)

write.csv(enso_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e6_enso_risk_factor.csv", row.names = FALSE)
write.csv(nao_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e6_nao_risk_factor.csv", row.names = FALSE)
write.csv(pdo_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e6_pdo_risk_factor.csv", row.names = FALSE)

for(i in 1:dim(elon)){
  for(j in 1:dim(elat)){
    
    data1<-e7_acpcp[i,j,]
    
    if (all(is.na(data1))){
      
      e7_enso_risk_factor[i,j] <- "NA"
      e7_nao_risk_factor[i,j] <- "NA"
      e7_pdo_risk_factor[i,j] <- "NA"
      
    }
    else{
      
      df <- data.frame(data1,indices$year,indices$month,indices$soi,indices$naoi,indices$pdoi)
      colnames(df) <- c("acpcp","year","month","soi","naoi","pdoi")
      
      df<- na.omit(df)
      
      enso_results <- extRemes::fevd(df$acpcp,data=df,location.fun=~soi,scale.fun=~soi)
      nao_results <- extRemes::fevd(df$acpcp,data=df,location.fun=~naoi,scale.fun=~naoi)
      pdo_results <- extRemes::fevd(df$acpcp,data=df,location.fun=~pdoi,scale.fun=~pdoi)
      
      enso_mu0 <- enso_results$results$par[1]
      enso_mu1 <- enso_results$results$par[2]
      enso_sigma0 <- enso_results$results$par[3]
      enso_sigma1 <- enso_results$results$par[4]
      enso_shape <- enso_results$results$par[5]
      
      nao_mu0 <- nao_results$results$par[1]
      nao_mu1 <- nao_results$results$par[2]
      nao_sigma0 <- nao_results$results$par[3]
      nao_sigma1 <- nao_results$results$par[4]
      nao_shape <- nao_results$results$par[5]
      
      pdo_mu0 <- pdo_results$results$par[1]
      pdo_mu1 <- pdo_results$results$par[2]
      pdo_sigma0 <- pdo_results$results$par[3]
      pdo_sigma1 <- pdo_results$results$par[4]
      pdo_shape <- pdo_results$results$par[5]
      
      cat(i,j,"\n")
      
      enso_rvalue <- extRemes::rlevd(df$acpcp,loc=enso_mu0 + enso_mu1 * (-1),scale=max(1e-100,enso_sigma0 + enso_sigma1 * (-1)),shape =enso_shape,period=20)
      enso_prob <- pevd(enso_rvalue, loc = enso_mu0 + enso_mu1 * (1), scale = max(1e-100,enso_sigma0 + enso_sigma1 * (1)), shape = enso_shape,lower.tail =FALSE)
      e7_enso_risk_factor[i,j] <- enso_prob/0.05
      
      nao_rvalue <- extRemes::rlevd(df$acpcp,loc=nao_mu0 + nao_mu1 * (-1),scale=max(1e-100,nao_sigma0 + nao_sigma1 * (-1)),shape = nao_shape,period=20)
      nao_prob <- pevd(nao_rvalue, loc = nao_mu0 + nao_mu1 * (1), scale = max(1e-100,nao_sigma0 + nao_sigma1 * (1)), shape = nao_shape,lower.tail =FALSE)
      e7_nao_risk_factor[i,j] <- nao_prob/0.05
      
      pdo_rvalue <- extRemes::rlevd(df$acpcp,loc=pdo_mu0 + pdo_mu1 * (-1),scale=max(1e-100,pdo_sigma0 + pdo_sigma1 * (-1)),shape = pdo_shape,period=20)
      pdo_prob <- pevd(nao_rvalue, loc = pdo_mu0 + pdo_mu1 * (1), scale = max(1e-100,pdo_sigma0 + pdo_sigma1 * (1)), shape = pdo_shape,lower.tail =FALSE)
      e7_pdo_risk_factor[i,j] <- pdo_prob/0.05
    }
  }
}

enso_rf <- data.frame(e7_enso_risk_factor)
nao_rf <- data.frame(e7_nao_risk_factor)
pdo_rf <- data.frame(e7_pdo_risk_factor)

write.csv(enso_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e7_enso_risk_factor.csv", row.names = FALSE)
write.csv(nao_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e7_nao_risk_factor.csv", row.names = FALSE)
write.csv(pdo_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e7_pdo_risk_factor.csv", row.names = FALSE)

for(i in 1:dim(elon)){
  for(j in 1:dim(elat)){
    
    data1<-e8_acpcp[i,j,]
    
    if (all(is.na(data1))){
      
      e8_enso_risk_factor[i,j] <- "NA"
      e8_nao_risk_factor[i,j] <- "NA"
      e8_pdo_risk_factor[i,j] <- "NA"
      
    }
    else{
      
      df <- data.frame(data1,indices$year,indices$month,indices$soi,indices$naoi,indices$pdoi)
      colnames(df) <- c("acpcp","year","month","soi","naoi","pdoi")
      
      df<- na.omit(df)
      
      enso_results <- extRemes::fevd(df$acpcp,data=df,location.fun=~soi,scale.fun=~soi)
      nao_results <- extRemes::fevd(df$acpcp,data=df,location.fun=~naoi,scale.fun=~naoi)
      pdo_results <- extRemes::fevd(df$acpcp,data=df,location.fun=~pdoi,scale.fun=~pdoi)
      
      enso_mu0 <- enso_results$results$par[1]
      enso_mu1 <- enso_results$results$par[2]
      enso_sigma0 <- enso_results$results$par[3]
      enso_sigma1 <- enso_results$results$par[4]
      enso_shape <- enso_results$results$par[5]
      
      nao_mu0 <- nao_results$results$par[1]
      nao_mu1 <- nao_results$results$par[2]
      nao_sigma0 <- nao_results$results$par[3]
      nao_sigma1 <- nao_results$results$par[4]
      nao_shape <- nao_results$results$par[5]
      
      pdo_mu0 <- pdo_results$results$par[1]
      pdo_mu1 <- pdo_results$results$par[2]
      pdo_sigma0 <- pdo_results$results$par[3]
      pdo_sigma1 <- pdo_results$results$par[4]
      pdo_shape <- pdo_results$results$par[5]
      
      cat(i,j,"\n")
      
      enso_rvalue <- extRemes::rlevd(df$acpcp,loc=enso_mu0 + enso_mu1 * (-1),scale=max(1e-100,enso_sigma0 + enso_sigma1 * (-1)),shape =enso_shape,period=20)
      enso_prob <- pevd(enso_rvalue, loc = enso_mu0 + enso_mu1 * (1), scale = max(1e-100,enso_sigma0 + enso_sigma1 * (1)), shape = enso_shape,lower.tail =FALSE)
      e8_enso_risk_factor[i,j] <- enso_prob/0.05
      
      nao_rvalue <- extRemes::rlevd(df$acpcp,loc=nao_mu0 + nao_mu1 * (-1),scale=max(1e-100,nao_sigma0 + nao_sigma1 * (-1)),shape = nao_shape,period=20)
      nao_prob <- pevd(nao_rvalue, loc = nao_mu0 + nao_mu1 * (1), scale = max(1e-100,nao_sigma0 + nao_sigma1 * (1)), shape = nao_shape,lower.tail =FALSE)
      e8_nao_risk_factor[i,j] <- nao_prob/0.05
      
      pdo_rvalue <- extRemes::rlevd(df$acpcp,loc=pdo_mu0 + pdo_mu1 * (-1),scale=max(1e-100,pdo_sigma0 + pdo_sigma1 * (-1)),shape = pdo_shape,period=20)
      pdo_prob <- pevd(nao_rvalue, loc = pdo_mu0 + pdo_mu1 * (1), scale = max(1e-100,pdo_sigma0 + pdo_sigma1 * (1)), shape = pdo_shape,lower.tail =FALSE)
      e8_pdo_risk_factor[i,j] <- pdo_prob/0.05
    }
  }
}

enso_rf <- data.frame(e8_enso_risk_factor)
nao_rf <- data.frame(e8_nao_risk_factor)
pdo_rf <- data.frame(e8_pdo_risk_factor)

write.csv(enso_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e8_enso_risk_factor.csv", row.names = FALSE)
write.csv(nao_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e8_nao_risk_factor.csv", row.names = FALSE)
write.csv(pdo_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e8_pdo_risk_factor.csv", row.names = FALSE)

for(i in 1:dim(elon)){
  for(j in 1:dim(elat)){
    
    data1<-e9_acpcp[i,j,]
    
    if (all(is.na(data1))){
      
      e9_enso_risk_factor[i,j] <- "NA"
      e9_nao_risk_factor[i,j] <- "NA"
      e9_pdo_risk_factor[i,j] <- "NA"
      
    }
    else{
      
      df <- data.frame(data1,indices$year,indices$month,indices$soi,indices$naoi,indices$pdoi)
      colnames(df) <- c("acpcp","year","month","soi","naoi","pdoi")
      
      df<- na.omit(df)
      
      enso_results <- extRemes::fevd(df$acpcp,data=df,location.fun=~soi,scale.fun=~soi)
      nao_results <- extRemes::fevd(df$acpcp,data=df,location.fun=~naoi,scale.fun=~naoi)
      pdo_results <- extRemes::fevd(df$acpcp,data=df,location.fun=~pdoi,scale.fun=~pdoi)
      
      enso_mu0 <- enso_results$results$par[1]
      enso_mu1 <- enso_results$results$par[2]
      enso_sigma0 <- enso_results$results$par[3]
      enso_sigma1 <- enso_results$results$par[4]
      enso_shape <- enso_results$results$par[5]
      
      nao_mu0 <- nao_results$results$par[1]
      nao_mu1 <- nao_results$results$par[2]
      nao_sigma0 <- nao_results$results$par[3]
      nao_sigma1 <- nao_results$results$par[4]
      nao_shape <- nao_results$results$par[5]
      
      pdo_mu0 <- pdo_results$results$par[1]
      pdo_mu1 <- pdo_results$results$par[2]
      pdo_sigma0 <- pdo_results$results$par[3]
      pdo_sigma1 <- pdo_results$results$par[4]
      pdo_shape <- pdo_results$results$par[5]
      
      cat(i,j,"\n")
      
      enso_rvalue <- extRemes::rlevd(df$acpcp,loc=enso_mu0 + enso_mu1 * (-1),scale=max(1e-100,enso_sigma0 + enso_sigma1 * (-1)),shape =enso_shape,period=20)
      enso_prob <- pevd(enso_rvalue, loc = enso_mu0 + enso_mu1 * (1), scale = max(1e-100,enso_sigma0 + enso_sigma1 * (1)), shape = enso_shape,lower.tail =FALSE)
      e9_enso_risk_factor[i,j] <- enso_prob/0.05
      
      nao_rvalue <- extRemes::rlevd(df$acpcp,loc=nao_mu0 + nao_mu1 * (-1),scale=max(1e-100,nao_sigma0 + nao_sigma1 * (-1)),shape = nao_shape,period=20)
      nao_prob <- pevd(nao_rvalue, loc = nao_mu0 + nao_mu1 * (1), scale = max(1e-100,nao_sigma0 + nao_sigma1 * (1)), shape = nao_shape,lower.tail =FALSE)
      e9_nao_risk_factor[i,j] <- nao_prob/0.05
      
      pdo_rvalue <- extRemes::rlevd(df$acpcp,loc=pdo_mu0 + pdo_mu1 * (-1),scale=max(1e-100,pdo_sigma0 + pdo_sigma1 * (-1)),shape = pdo_shape,period=20)
      pdo_prob <- pevd(nao_rvalue, loc = pdo_mu0 + pdo_mu1 * (1), scale = max(1e-100,pdo_sigma0 + pdo_sigma1 * (1)), shape = pdo_shape,lower.tail =FALSE)
      e9_pdo_risk_factor[i,j] <- pdo_prob/0.05
    }
  }
}

enso_rf <- data.frame(e9_enso_risk_factor)
nao_rf <- data.frame(e9_nao_risk_factor)
pdo_rf <- data.frame(e9_pdo_risk_factor)

write.csv(enso_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e9_enso_risk_factor.csv", row.names = FALSE)
write.csv(nao_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e9_nao_risk_factor.csv", row.names = FALSE)
write.csv(pdo_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e9_pdo_risk_factor.csv", row.names = FALSE)

n_enso_pos_nao_pos_risk_factor <- matrix(0,nrow=dim(nlon)[1],ncol=dim(nlon)[2])
n_enso_pos_nao_neg_risk_factor <- matrix(0,nrow=dim(nlon)[1],ncol=dim(nlon)[2])
n_enso_neg_nao_pos_risk_factor <- matrix(0,nrow=dim(nlon)[1],ncol=dim(nlon)[2])
n_enso_neg_nao_neg_risk_factor <- matrix(0,nrow=dim(nlon)[1],ncol=dim(nlon)[2])
n_enso_pos_pdo_pos_risk_factor <- matrix(0,nrow=dim(nlon)[1],ncol=dim(nlon)[2])
n_enso_pos_pdo_neg_risk_factor <- matrix(0,nrow=dim(nlon)[1],ncol=dim(nlon)[2])
n_enso_neg_pdo_pos_risk_factor <- matrix(0,nrow=dim(nlon)[1],ncol=dim(nlon)[2])
n_enso_neg_pdo_neg_risk_factor <- matrix(0,nrow=dim(nlon)[1],ncol=dim(nlon)[2])

e1_enso_pos_nao_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e1_enso_pos_nao_neg_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e1_enso_neg_nao_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e1_enso_pos_nao_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e1_enso_pos_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e1_enso_pos_pdo_neg_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e1_enso_neg_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e1_enso_pos_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))

e2_enso_pos_nao_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e2_enso_pos_nao_neg_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e2_enso_neg_nao_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e2_enso_pos_nao_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e2_enso_pos_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e2_enso_pos_pdo_neg_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e2_enso_neg_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e2_enso_pos_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))

e3_enso_pos_nao_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e3_enso_pos_nao_neg_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e3_enso_neg_nao_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e3_enso_pos_nao_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e3_enso_pos_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e3_enso_pos_pdo_neg_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e3_enso_neg_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e3_enso_pos_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))

e4_enso_pos_nao_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e4_enso_pos_nao_neg_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e4_enso_neg_nao_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e4_enso_pos_nao_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e4_enso_pos_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e4_enso_pos_pdo_neg_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e4_enso_neg_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e4_enso_pos_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))

e5_enso_pos_nao_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e5_enso_pos_nao_neg_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e5_enso_neg_nao_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e5_enso_pos_nao_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e5_enso_pos_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e5_enso_pos_pdo_neg_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e5_enso_neg_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e5_enso_pos_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))

e6_enso_pos_nao_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e6_enso_pos_nao_neg_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e6_enso_neg_nao_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e6_enso_pos_nao_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e6_enso_pos_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e6_enso_pos_pdo_neg_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e6_enso_neg_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e6_enso_pos_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))

e7_enso_pos_nao_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e7_enso_pos_nao_neg_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e7_enso_neg_nao_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e7_enso_pos_nao_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e7_enso_pos_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e7_enso_pos_pdo_neg_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e7_enso_neg_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e7_enso_pos_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))

e8_enso_pos_nao_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e8_enso_pos_nao_neg_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e8_enso_neg_nao_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e8_enso_pos_nao_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e8_enso_pos_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e8_enso_pos_pdo_neg_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e8_enso_neg_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e8_enso_pos_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))

e9_enso_pos_nao_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e9_enso_pos_nao_neg_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e9_enso_neg_nao_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e9_enso_pos_nao_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e9_enso_pos_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e9_enso_pos_pdo_neg_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e9_enso_neg_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e9_enso_pos_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))

for(i in 1:dim(nlon)[1]){
  
  for(j in 1:dim(nlon)[2]){

    cat(i,j,"\n")
    
    data1<-n_acpcp[i,j,]
    
    if (sum(is.na(data1)) >= 50){
      
      n_enso_pos_nao_neg_risk_factor[i,j] <- "NA"
      n_enso_neg_nao_pos_risk_factor[i,j] <- "NA"
      n_enso_neg_nao_neg_risk_factor[i,j] <- "NA"
      n_enso_pos_pdo_neg_risk_factor[i,j] <- "NA"
      n_enso_neg_pdo_pos_risk_factor[i,j] <- "NA"
      n_enso_neg_pdo_neg_risk_factor[i,j] <- "NA"
      
    }
    else{
      
      df <- data.frame(data1,indices$year,indices$month,indices$soi,indices$naoi,indices$pdoi)
      colnames(df) <- c("acpcp","year","month","soi","naoi","pdoi")
      
      df <- na.omit(df)
      enso_nao_results <- extRemes::fevd(df$acpcp,data=df,location.fun=~soi + naoi,scale.fun=~soi + naoi)
      enso_pdo_results <- extRemes::fevd(df$acpcp,data=df,location.fun=~soi + pdoi,scale.fun=~soi + pdoi)
      
      enso_nao_mu0 <- enso_nao_results$results$par[1]
      enso_nao_mu1 <- enso_nao_results$results$par[2]
      enso_nao_mu2 <- enso_nao_results$results$par[3]
      enso_nao_sigma0 <- enso_nao_results$results$par[4]
      enso_nao_sigma1 <- enso_nao_results$results$par[5]
      enso_nao_sigma2 <- enso_nao_results$results$par[6]
      enso_nao_shape <- enso_nao_results$results$par[7]
      
      enso_pdo_mu0 <- enso_pdo_results$results$par[1]
      enso_pdo_mu1 <- enso_pdo_results$results$par[2]
      enso_pdo_mu2 <- enso_pdo_results$results$par[3]
      enso_pdo_sigma0 <- enso_pdo_results$results$par[4]
      enso_pdo_sigma1 <- enso_pdo_results$results$par[5]
      enso_pdo_sigma2 <- enso_pdo_results$results$par[6]
      enso_pdo_shape <- enso_pdo_results$results$par[7]
      
      enso_neg_nao_neg_rvalue <- extRemes::rlevd(df$acpcp,loc=enso_nao_mu0 + enso_nao_mu1 * (-1) + enso_nao_mu2 * (-1),scale=max(1e-100, enso_nao_sigma0 + enso_nao_sigma1 * (-1) + enso_nao_sigma2 * (-1)),shape =enso_nao_shape,period=20)
      enso_pos_nao_neg_prob <- pevd(enso_neg_nao_neg_rvalue, loc = enso_nao_mu0 + enso_nao_mu1 * (1) + enso_nao_mu2 * (-1), scale = max(1e-100,enso_nao_sigma0 + enso_nao_sigma1 * (1) + enso_nao_sigma2 * (-1)), shape = enso_nao_shape,lower.tail =FALSE)
      enso_neg_nao_pos_prob <- pevd(enso_neg_nao_neg_rvalue, loc = enso_nao_mu0 + enso_nao_mu1 * (-1) + enso_nao_mu2 * (1), scale = max(1e-100,enso_nao_sigma0 + enso_nao_sigma1 * (-1) + enso_nao_sigma2 * (1)), shape = enso_nao_shape,lower.tail =FALSE)
      enso_pos_nao_pos_prob <- pevd(enso_neg_nao_neg_rvalue, loc = enso_nao_mu0 + enso_nao_mu1 * (1) + enso_nao_mu2 * (1), scale = max(1e-100,enso_nao_sigma0 + enso_nao_sigma1 * (1) + enso_nao_sigma2 * (1)), shape = enso_nao_shape,lower.tail =FALSE)
      n_enso_pos_nao_neg_risk_factor[i,j] <- enso_pos_nao_neg_prob/0.05
      n_enso_neg_nao_pos_risk_factor[i,j] <- enso_neg_nao_pos_prob/0.05
      n_enso_pos_nao_pos_risk_factor[i,j] <- enso_pos_nao_pos_prob/0.05
      
      enso_neg_pdo_neg_rvalue <- extRemes::rlevd(df$acpcp,loc=enso_pdo_mu0 + enso_pdo_mu1 * (-1) + enso_pdo_mu2 * (-1),scale=max(1e-100,enso_pdo_sigma0 + enso_pdo_sigma1 * (-1) + enso_pdo_sigma2 * (-1)),shape =enso_pdo_shape,period=20)
      enso_pos_pdo_neg_prob <- pevd(enso_neg_pdo_neg_rvalue, loc = enso_pdo_mu0 + enso_pdo_mu1 * (1) + enso_pdo_mu2 * (-1), scale = max(1e-100,enso_pdo_sigma0 + enso_pdo_sigma1 * (1) + enso_pdo_sigma2 * (-1)), shape = enso_pdo_shape,lower.tail =FALSE)
      enso_neg_pdo_pos_prob <- pevd(enso_neg_pdo_neg_rvalue, loc = enso_pdo_mu0 + enso_pdo_mu1 * (-1) + enso_pdo_mu2 * (1), scale = max(1e-100,enso_pdo_sigma0 + enso_pdo_sigma1 * (-1) + enso_pdo_sigma2 * (1)), shape = enso_pdo_shape,lower.tail =FALSE)
      enso_pos_pdo_pos_prob <- pevd(enso_neg_pdo_neg_rvalue, loc = enso_pdo_mu0 + enso_pdo_mu1 * (1) + enso_pdo_mu2 * (1), scale = max(1e-100,enso_pdo_sigma0 + enso_pdo_sigma1 * (1) + enso_pdo_sigma2 * (1)), shape = enso_pdo_shape,lower.tail =FALSE)
      n_enso_pos_pdo_neg_risk_factor[i,j] <- enso_pos_pdo_neg_prob/0.05
      n_enso_neg_pdo_pos_risk_factor[i,j] <- enso_neg_pdo_pos_prob/0.05
      n_enso_pos_pdo_pos_risk_factor[i,j] <- enso_pos_pdo_pos_prob/0.05

    }
  }
}

n_enso_pos_nao_neg_rf <- data.frame(n_enso_pos_nao_neg_risk_factor)
n_enso_neg_nao_pos_rf <- data.frame(n_enso_neg_nao_pos_risk_factor)
n_enso_pos_nao_pos_rf <- data.frame(n_enso_pos_nao_pos_risk_factor)
n_enso_pos_pdo_neg_rf <- data.frame(n_enso_pos_pdo_neg_risk_factor)
n_enso_neg_pdo_pos_rf <- data.frame(n_enso_neg_pdo_pos_risk_factor)
n_enso_pos_pdo_pos_rf <- data.frame(n_enso_pos_pdo_pos_risk_factor)


write.csv(n_enso_pos_nao_neg_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/n_enso_pos_nao_neg_risk_factor.csv", row.names = FALSE)
write.csv(n_enso_neg_nao_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/n_enso_neg_nao_pos_risk_factor.csv", row.names = FALSE)
write.csv(n_enso_pos_nao_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/n_enso_pos_nao_pos_risk_factor.csv", row.names = FALSE)
write.csv(n_enso_pos_pdo_neg_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/n_enso_pos_pdo_neg_risk_factor.csv", row.names = FALSE)
write.csv(n_enso_neg_pdo_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/n_enso_neg_pdo_pos_risk_factor.csv", row.names = FALSE)
write.csv(n_enso_pos_pdo_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/n_enso_pos_pdo_pos_risk_factor.csv", row.names = FALSE)

for(i in 1:length(elon)){
  
  for(j in 1:length(elat)){
    
    cat(i,j,"\n")
    
    data1<-e1_acpcp[i,j,]
    
    if (sum(is.na(data1)) >= 50){
      
      e1_enso_pos_nao_neg_risk_factor[i,j] <- "NA"
      e1_enso_neg_nao_pos_risk_factor[i,j] <- "NA"
      e1_enso_neg_nao_neg_risk_factor[i,j] <- "NA"
      e1_enso_pos_pdo_neg_risk_factor[i,j] <- "NA"
      e1_enso_neg_pdo_pos_risk_factor[i,j] <- "NA"
      e1_enso_neg_pdo_neg_risk_factor[i,j] <- "NA"
      
    }
    else{
      
      df <- data.frame(data1,indices$year,indices$month,indices$soi,indices$naoi,indices$pdoi)
      colnames(df) <- c("acpcp","year","month","soi","naoi","pdoi")
      
      df <- na.omit(df)
      enso_nao_results <- extRemes::fevd(df$acpcp,data=df,location.fun=~soi + naoi,scale.fun=~soi + naoi)
      enso_pdo_results <- extRemes::fevd(df$acpcp,data=df,location.fun=~soi + pdoi,scale.fun=~soi + pdoi)
      
      enso_nao_mu0 <- enso_nao_results$results$par[1]
      enso_nao_mu1 <- enso_nao_results$results$par[2]
      enso_nao_mu2 <- enso_nao_results$results$par[3]
      enso_nao_sigma0 <- enso_nao_results$results$par[4]
      enso_nao_sigma1 <- enso_nao_results$results$par[5]
      enso_nao_sigma2 <- enso_nao_results$results$par[6]
      enso_nao_shape <- enso_nao_results$results$par[7]
      
      enso_pdo_mu0 <- enso_pdo_results$results$par[1]
      enso_pdo_mu1 <- enso_pdo_results$results$par[2]
      enso_pdo_mu2 <- enso_pdo_results$results$par[3]
      enso_pdo_sigma0 <- enso_pdo_results$results$par[4]
      enso_pdo_sigma1 <- enso_pdo_results$results$par[5]
      enso_pdo_sigma2 <- enso_pdo_results$results$par[6]
      enso_pdo_shape <- enso_pdo_results$results$par[7]
      
      enso_neg_nao_neg_rvalue <- extRemes::rlevd(df$acpcp,loc=enso_nao_mu0 + enso_nao_mu1 * (-1) + enso_nao_mu2 * (-1),scale=max(1e-100, enso_nao_sigma0 + enso_nao_sigma1 * (-1) + enso_nao_sigma2 * (-1)),shape =enso_nao_shape,period=20)
      enso_pos_nao_neg_prob <- pevd(enso_neg_nao_neg_rvalue, loc = enso_nao_mu0 + enso_nao_mu1 * (1) + enso_nao_mu2 * (-1), scale = max(1e-100,enso_nao_sigma0 + enso_nao_sigma1 * (1) + enso_nao_sigma2 * (-1)), shape = enso_nao_shape,lower.tail =FALSE)
      enso_neg_nao_pos_prob <- pevd(enso_neg_nao_neg_rvalue, loc = enso_nao_mu0 + enso_nao_mu1 * (-1) + enso_nao_mu2 * (1), scale = max(1e-100,enso_nao_sigma0 + enso_nao_sigma1 * (-1) + enso_nao_sigma2 * (1)), shape = enso_nao_shape,lower.tail =FALSE)
      enso_pos_nao_pos_prob <- pevd(enso_neg_nao_neg_rvalue, loc = enso_nao_mu0 + enso_nao_mu1 * (1) + enso_nao_mu2 * (1), scale = max(1e-100,enso_nao_sigma0 + enso_nao_sigma1 * (1) + enso_nao_sigma2 * (1)), shape = enso_nao_shape,lower.tail =FALSE)
      e1_enso_pos_nao_neg_risk_factor[i,j] <- enso_pos_nao_neg_prob/0.05
      e1_enso_neg_nao_pos_risk_factor[i,j] <- enso_neg_nao_pos_prob/0.05
      e1_enso_pos_nao_pos_risk_factor[i,j] <- enso_pos_nao_pos_prob/0.05
      
      enso_neg_pdo_neg_rvalue <- extRemes::rlevd(df$acpcp,loc=enso_pdo_mu0 + enso_pdo_mu1 * (-1) + enso_pdo_mu2 * (-1),scale=max(1e-100,enso_pdo_sigma0 + enso_pdo_sigma1 * (-1) + enso_pdo_sigma2 * (-1)),shape =enso_pdo_shape,period=20)
      enso_pos_pdo_neg_prob <- pevd(enso_neg_pdo_neg_rvalue, loc = enso_pdo_mu0 + enso_pdo_mu1 * (1) + enso_pdo_mu2 * (-1), scale = max(1e-100,enso_pdo_sigma0 + enso_pdo_sigma1 * (1) + enso_pdo_sigma2 * (-1)), shape = enso_pdo_shape,lower.tail =FALSE)
      enso_neg_pdo_pos_prob <- pevd(enso_neg_pdo_neg_rvalue, loc = enso_pdo_mu0 + enso_pdo_mu1 * (-1) + enso_pdo_mu2 * (1), scale = max(1e-100,enso_pdo_sigma0 + enso_pdo_sigma1 * (-1) + enso_pdo_sigma2 * (1)), shape = enso_pdo_shape,lower.tail =FALSE)
      enso_pos_pdo_pos_prob <- pevd(enso_neg_pdo_neg_rvalue, loc = enso_pdo_mu0 + enso_pdo_mu1 * (1) + enso_pdo_mu2 * (1), scale = max(1e-100,enso_pdo_sigma0 + enso_pdo_sigma1 * (1) + enso_pdo_sigma2 * (1)), shape = enso_pdo_shape,lower.tail =FALSE)
      e1_enso_pos_pdo_neg_risk_factor[i,j] <- enso_pos_pdo_neg_prob/0.05
      e1_enso_neg_pdo_pos_risk_factor[i,j] <- enso_neg_pdo_pos_prob/0.05
      e1_enso_pos_pdo_pos_risk_factor[i,j] <- enso_pos_pdo_pos_prob/0.05
      
    }
  }
}

e1_enso_pos_nao_neg_rf <- data.frame(e1_enso_pos_nao_neg_risk_factor)
e1_enso_neg_nao_pos_rf <- data.frame(e1_enso_neg_nao_pos_risk_factor)
e1_enso_pos_nao_pos_rf <- data.frame(e1_enso_pos_nao_pos_risk_factor)
e1_enso_pos_pdo_neg_rf <- data.frame(e1_enso_pos_pdo_neg_risk_factor)
e1_enso_neg_pdo_pos_rf <- data.frame(e1_enso_neg_pdo_pos_risk_factor)
e1_enso_pos_pdo_pos_rf <- data.frame(e1_enso_pos_pdo_pos_risk_factor)


write.csv(e1_enso_pos_nao_neg_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e1_enso_pos_nao_neg_risk_factor.csv", row.names = FALSE)
write.csv(e1_enso_neg_nao_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e1_enso_neg_nao_pos_risk_factor.csv", row.names = FALSE)
write.csv(e1_enso_pos_nao_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e1_enso_pos_nao_pos_risk_factor.csv", row.names = FALSE)
write.csv(e1_enso_pos_pdo_neg_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e1_enso_pos_pdo_neg_risk_factor.csv", row.names = FALSE)
write.csv(e1_enso_neg_pdo_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e1_enso_neg_pdo_pos_risk_factor.csv", row.names = FALSE)
write.csv(e1_enso_pos_pdo_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e1_enso_pos_pdo_pos_risk_factor.csv", row.names = FALSE)

for(i in 1:length(elon)){
  
  for(j in 1:length(elat)){
    
    data1<-e2_acpcp[i,j,]
    
    if (sum(is.na(data1)) >= 50){
      
      e2_enso_pos_nao_neg_risk_factor[i,j] <- "NA"
      e2_enso_neg_nao_pos_risk_factor[i,j] <- "NA"
      e2_enso_neg_nao_neg_risk_factor[i,j] <- "NA"
      e2_enso_pos_pdo_neg_risk_factor[i,j] <- "NA"
      e2_enso_neg_pdo_pos_risk_factor[i,j] <- "NA"
      e2_enso_neg_pdo_neg_risk_factor[i,j] <- "NA"
      
    }
    else{
      
      df <- data.frame(data1,indices$year,indices$month,indices$soi,indices$naoi,indices$pdoi)
      colnames(df) <- c("acpcp","year","month","soi","naoi","pdoi")
      
      df <- na.omit(df)
      enso_nao_results <- extRemes::fevd(df$acpcp,data=df,location.fun=~soi + naoi,scale.fun=~soi + naoi)
      enso_pdo_results <- extRemes::fevd(df$acpcp,data=df,location.fun=~soi + pdoi,scale.fun=~soi + pdoi)
      
      enso_nao_mu0 <- enso_nao_results$results$par[1]
      enso_nao_mu1 <- enso_nao_results$results$par[2]
      enso_nao_mu2 <- enso_nao_results$results$par[3]
      enso_nao_sigma0 <- enso_nao_results$results$par[4]
      enso_nao_sigma1 <- enso_nao_results$results$par[5]
      enso_nao_sigma2 <- enso_nao_results$results$par[6]
      enso_nao_shape <- enso_nao_results$results$par[7]
      
      enso_pdo_mu0 <- enso_pdo_results$results$par[1]
      enso_pdo_mu1 <- enso_pdo_results$results$par[2]
      enso_pdo_mu2 <- enso_pdo_results$results$par[3]
      enso_pdo_sigma0 <- enso_pdo_results$results$par[4]
      enso_pdo_sigma1 <- enso_pdo_results$results$par[5]
      enso_pdo_sigma2 <- enso_pdo_results$results$par[6]
      enso_pdo_shape <- enso_pdo_results$results$par[7]
      
      enso_neg_nao_neg_rvalue <- extRemes::rlevd(df$acpcp,loc=enso_nao_mu0 + enso_nao_mu1 * (-1) + enso_nao_mu2 * (-1),scale=max(1e-100, enso_nao_sigma0 + enso_nao_sigma1 * (-1) + enso_nao_sigma2 * (-1)),shape =enso_nao_shape,period=20)
      enso_pos_nao_neg_prob <- pevd(enso_neg_nao_neg_rvalue, loc = enso_nao_mu0 + enso_nao_mu1 * (1) + enso_nao_mu2 * (-1), scale = max(1e-100,enso_nao_sigma0 + enso_nao_sigma1 * (1) + enso_nao_sigma2 * (-1)), shape = enso_nao_shape,lower.tail =FALSE)
      enso_neg_nao_pos_prob <- pevd(enso_neg_nao_neg_rvalue, loc = enso_nao_mu0 + enso_nao_mu1 * (-1) + enso_nao_mu2 * (1), scale = max(1e-100,enso_nao_sigma0 + enso_nao_sigma1 * (-1) + enso_nao_sigma2 * (1)), shape = enso_nao_shape,lower.tail =FALSE)
      enso_pos_nao_pos_prob <- pevd(enso_neg_nao_neg_rvalue, loc = enso_nao_mu0 + enso_nao_mu1 * (1) + enso_nao_mu2 * (1), scale = max(1e-100,enso_nao_sigma0 + enso_nao_sigma1 * (1) + enso_nao_sigma2 * (1)), shape = enso_nao_shape,lower.tail =FALSE)
      e2_enso_pos_nao_neg_risk_factor[i,j] <- enso_pos_nao_neg_prob/0.05
      e2_enso_neg_nao_pos_risk_factor[i,j] <- enso_neg_nao_pos_prob/0.05
      e2_enso_pos_nao_pos_risk_factor[i,j] <- enso_pos_nao_pos_prob/0.05
      
      enso_neg_pdo_neg_rvalue <- extRemes::rlevd(df$acpcp,loc=enso_pdo_mu0 + enso_pdo_mu1 * (-1) + enso_pdo_mu2 * (-1),scale=max(1e-100,enso_pdo_sigma0 + enso_pdo_sigma1 * (-1) + enso_pdo_sigma2 * (-1)),shape =enso_pdo_shape,period=20)
      enso_pos_pdo_neg_prob <- pevd(enso_neg_pdo_neg_rvalue, loc = enso_pdo_mu0 + enso_pdo_mu1 * (1) + enso_pdo_mu2 * (-1), scale = max(1e-100,enso_pdo_sigma0 + enso_pdo_sigma1 * (1) + enso_pdo_sigma2 * (-1)), shape = enso_pdo_shape,lower.tail =FALSE)
      enso_neg_pdo_pos_prob <- pevd(enso_neg_pdo_neg_rvalue, loc = enso_pdo_mu0 + enso_pdo_mu1 * (-1) + enso_pdo_mu2 * (1), scale = max(1e-100,enso_pdo_sigma0 + enso_pdo_sigma1 * (-1) + enso_pdo_sigma2 * (1)), shape = enso_pdo_shape,lower.tail =FALSE)
      enso_pos_pdo_pos_prob <- pevd(enso_neg_pdo_neg_rvalue, loc = enso_pdo_mu0 + enso_pdo_mu1 * (1) + enso_pdo_mu2 * (1), scale = max(1e-100,enso_pdo_sigma0 + enso_pdo_sigma1 * (1) + enso_pdo_sigma2 * (1)), shape = enso_pdo_shape,lower.tail =FALSE)
      e2_enso_pos_pdo_neg_risk_factor[i,j] <- enso_pos_pdo_neg_prob/0.05
      e2_enso_neg_pdo_pos_risk_factor[i,j] <- enso_neg_pdo_pos_prob/0.05
      e2_enso_pos_pdo_pos_risk_factor[i,j] <- enso_pos_pdo_pos_prob/0.05
      
    }
  }
}

e2_enso_pos_nao_neg_rf <- data.frame(e2_enso_pos_nao_neg_risk_factor)
e2_enso_neg_nao_pos_rf <- data.frame(e2_enso_neg_nao_pos_risk_factor)
e2_enso_pos_nao_pos_rf <- data.frame(e2_enso_pos_nao_pos_risk_factor)
e2_enso_pos_pdo_neg_rf <- data.frame(e2_enso_pos_pdo_neg_risk_factor)
e2_enso_neg_pdo_pos_rf <- data.frame(e2_enso_neg_pdo_pos_risk_factor)
e2_enso_pos_pdo_pos_rf <- data.frame(e2_enso_pos_pdo_pos_risk_factor)


write.csv(e2_enso_pos_nao_neg_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e2_enso_pos_nao_neg_risk_factor.csv", row.names = FALSE)
write.csv(e2_enso_neg_nao_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e2_enso_neg_nao_pos_risk_factor.csv", row.names = FALSE)
write.csv(e2_enso_pos_nao_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e2_enso_pos_nao_pos_risk_factor.csv", row.names = FALSE)
write.csv(e2_enso_pos_pdo_neg_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e2_enso_pos_pdo_neg_risk_factor.csv", row.names = FALSE)
write.csv(e2_enso_neg_pdo_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e2_enso_neg_pdo_pos_risk_factor.csv", row.names = FALSE)
write.csv(e2_enso_pos_pdo_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e2_enso_pos_pdo_pos_risk_factor.csv", row.names = FALSE)

for(i in 1:length(elon)){
  
  for(j in 1:length(elat)){
    
    data1<-e3_acpcp[i,j,]
    
    if (sum(is.na(data1)) >= 50){
      
      e3_enso_pos_nao_neg_risk_factor[i,j] <- "NA"
      e3_enso_neg_nao_pos_risk_factor[i,j] <- "NA"
      e3_enso_neg_nao_neg_risk_factor[i,j] <- "NA"
      e3_enso_pos_pdo_neg_risk_factor[i,j] <- "NA"
      e3_enso_neg_pdo_pos_risk_factor[i,j] <- "NA"
      e3_enso_neg_pdo_neg_risk_factor[i,j] <- "NA"
      
    }
    else{
      
      df <- data.frame(data1,indices$year,indices$month,indices$soi,indices$naoi,indices$pdoi)
      colnames(df) <- c("acpcp","year","month","soi","naoi","pdoi")
      
      df <- na.omit(df)
      enso_nao_results <- extRemes::fevd(df$acpcp,data=df,location.fun=~soi + naoi,scale.fun=~soi + naoi)
      enso_pdo_results <- extRemes::fevd(df$acpcp,data=df,location.fun=~soi + pdoi,scale.fun=~soi + pdoi)
      
      enso_nao_mu0 <- enso_nao_results$results$par[1]
      enso_nao_mu1 <- enso_nao_results$results$par[2]
      enso_nao_mu2 <- enso_nao_results$results$par[3]
      enso_nao_sigma0 <- enso_nao_results$results$par[4]
      enso_nao_sigma1 <- enso_nao_results$results$par[5]
      enso_nao_sigma2 <- enso_nao_results$results$par[6]
      enso_nao_shape <- enso_nao_results$results$par[7]
      
      enso_pdo_mu0 <- enso_pdo_results$results$par[1]
      enso_pdo_mu1 <- enso_pdo_results$results$par[2]
      enso_pdo_mu2 <- enso_pdo_results$results$par[3]
      enso_pdo_sigma0 <- enso_pdo_results$results$par[4]
      enso_pdo_sigma1 <- enso_pdo_results$results$par[5]
      enso_pdo_sigma2 <- enso_pdo_results$results$par[6]
      enso_pdo_shape <- enso_pdo_results$results$par[7]
      
      enso_neg_nao_neg_rvalue <- extRemes::rlevd(df$acpcp,loc=enso_nao_mu0 + enso_nao_mu1 * (-1) + enso_nao_mu2 * (-1),scale=max(1e-100, enso_nao_sigma0 + enso_nao_sigma1 * (-1) + enso_nao_sigma2 * (-1)),shape =enso_nao_shape,period=20)
      enso_pos_nao_neg_prob <- pevd(enso_neg_nao_neg_rvalue, loc = enso_nao_mu0 + enso_nao_mu1 * (1) + enso_nao_mu2 * (-1), scale = max(1e-100,enso_nao_sigma0 + enso_nao_sigma1 * (1) + enso_nao_sigma2 * (-1)), shape = enso_nao_shape,lower.tail =FALSE)
      enso_neg_nao_pos_prob <- pevd(enso_neg_nao_neg_rvalue, loc = enso_nao_mu0 + enso_nao_mu1 * (-1) + enso_nao_mu2 * (1), scale = max(1e-100,enso_nao_sigma0 + enso_nao_sigma1 * (-1) + enso_nao_sigma2 * (1)), shape = enso_nao_shape,lower.tail =FALSE)
      enso_pos_nao_pos_prob <- pevd(enso_neg_nao_neg_rvalue, loc = enso_nao_mu0 + enso_nao_mu1 * (1) + enso_nao_mu2 * (1), scale = max(1e-100,enso_nao_sigma0 + enso_nao_sigma1 * (1) + enso_nao_sigma2 * (1)), shape = enso_nao_shape,lower.tail =FALSE)
      e3_enso_pos_nao_neg_risk_factor[i,j] <- enso_pos_nao_neg_prob/0.05
      e3_enso_neg_nao_pos_risk_factor[i,j] <- enso_neg_nao_pos_prob/0.05
      e3_enso_pos_nao_pos_risk_factor[i,j] <- enso_pos_nao_pos_prob/0.05
      
      enso_neg_pdo_neg_rvalue <- extRemes::rlevd(df$acpcp,loc=enso_pdo_mu0 + enso_pdo_mu1 * (-1) + enso_pdo_mu2 * (-1),scale=max(1e-100,enso_pdo_sigma0 + enso_pdo_sigma1 * (-1) + enso_pdo_sigma2 * (-1)),shape =enso_pdo_shape,period=20)
      enso_pos_pdo_neg_prob <- pevd(enso_neg_pdo_neg_rvalue, loc = enso_pdo_mu0 + enso_pdo_mu1 * (1) + enso_pdo_mu2 * (-1), scale = max(1e-100,enso_pdo_sigma0 + enso_pdo_sigma1 * (1) + enso_pdo_sigma2 * (-1)), shape = enso_pdo_shape,lower.tail =FALSE)
      enso_neg_pdo_pos_prob <- pevd(enso_neg_pdo_neg_rvalue, loc = enso_pdo_mu0 + enso_pdo_mu1 * (-1) + enso_pdo_mu2 * (1), scale = max(1e-100,enso_pdo_sigma0 + enso_pdo_sigma1 * (-1) + enso_pdo_sigma2 * (1)), shape = enso_pdo_shape,lower.tail =FALSE)
      enso_pos_pdo_pos_prob <- pevd(enso_neg_pdo_neg_rvalue, loc = enso_pdo_mu0 + enso_pdo_mu1 * (1) + enso_pdo_mu2 * (1), scale = max(1e-100,enso_pdo_sigma0 + enso_pdo_sigma1 * (1) + enso_pdo_sigma2 * (1)), shape = enso_pdo_shape,lower.tail =FALSE)
      e3_enso_pos_pdo_neg_risk_factor[i,j] <- enso_pos_pdo_neg_prob/0.05
      e3_enso_neg_pdo_pos_risk_factor[i,j] <- enso_neg_pdo_pos_prob/0.05
      e3_enso_pos_pdo_pos_risk_factor[i,j] <- enso_pos_pdo_pos_prob/0.05
      
    }
  }
}

e3_enso_pos_nao_neg_rf <- data.frame(e3_enso_pos_nao_neg_risk_factor)
e3_enso_neg_nao_pos_rf <- data.frame(e3_enso_neg_nao_pos_risk_factor)
e3_enso_pos_nao_pos_rf <- data.frame(e3_enso_pos_nao_pos_risk_factor)
e3_enso_pos_pdo_neg_rf <- data.frame(e3_enso_pos_pdo_neg_risk_factor)
e3_enso_neg_pdo_pos_rf <- data.frame(e3_enso_neg_pdo_pos_risk_factor)
e3_enso_pos_pdo_pos_rf <- data.frame(e3_enso_pos_pdo_pos_risk_factor)


write.csv(e3_enso_pos_nao_neg_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e3_enso_pos_nao_neg_risk_factor.csv", row.names = FALSE)
write.csv(e3_enso_neg_nao_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e3_enso_neg_nao_pos_risk_factor.csv", row.names = FALSE)
write.csv(e3_enso_pos_nao_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e3_enso_pos_nao_pos_risk_factor.csv", row.names = FALSE)
write.csv(e3_enso_pos_pdo_neg_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e3_enso_pos_pdo_neg_risk_factor.csv", row.names = FALSE)
write.csv(e3_enso_neg_pdo_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e3_enso_neg_pdo_pos_risk_factor.csv", row.names = FALSE)
write.csv(e3_enso_pos_pdo_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e3_enso_pos_pdo_pos_risk_factor.csv", row.names = FALSE)

for(i in 1:length(elon)){
  
  for(j in 1:length(elat)){
    
    data1<-e4_acpcp[i,j,]
    
    if (sum(is.na(data1)) >= 50){
      
      e4_enso_pos_nao_neg_risk_factor[i,j] <- "NA"
      e4_enso_neg_nao_pos_risk_factor[i,j] <- "NA"
      e4_enso_neg_nao_neg_risk_factor[i,j] <- "NA"
      e4_enso_pos_pdo_neg_risk_factor[i,j] <- "NA"
      e4_enso_neg_pdo_pos_risk_factor[i,j] <- "NA"
      e4_enso_neg_pdo_neg_risk_factor[i,j] <- "NA"
      
    }
    else{
      
      df <- data.frame(data1,indices$year,indices$month,indices$soi,indices$naoi,indices$pdoi)
      colnames(df) <- c("acpcp","year","month","soi","naoi","pdoi")
      
      df <- na.omit(df)
      enso_nao_results <- extRemes::fevd(df$acpcp,data=df,location.fun=~soi + naoi,scale.fun=~soi + naoi)
      enso_pdo_results <- extRemes::fevd(df$acpcp,data=df,location.fun=~soi + pdoi,scale.fun=~soi + pdoi)
      
      enso_nao_mu0 <- enso_nao_results$results$par[1]
      enso_nao_mu1 <- enso_nao_results$results$par[2]
      enso_nao_mu2 <- enso_nao_results$results$par[3]
      enso_nao_sigma0 <- enso_nao_results$results$par[4]
      enso_nao_sigma1 <- enso_nao_results$results$par[5]
      enso_nao_sigma2 <- enso_nao_results$results$par[6]
      enso_nao_shape <- enso_nao_results$results$par[7]
      
      enso_pdo_mu0 <- enso_pdo_results$results$par[1]
      enso_pdo_mu1 <- enso_pdo_results$results$par[2]
      enso_pdo_mu2 <- enso_pdo_results$results$par[3]
      enso_pdo_sigma0 <- enso_pdo_results$results$par[4]
      enso_pdo_sigma1 <- enso_pdo_results$results$par[5]
      enso_pdo_sigma2 <- enso_pdo_results$results$par[6]
      enso_pdo_shape <- enso_pdo_results$results$par[7]
      
      enso_neg_nao_neg_rvalue <- extRemes::rlevd(df$acpcp,loc=enso_nao_mu0 + enso_nao_mu1 * (-1) + enso_nao_mu2 * (-1),scale=max(1e-100, enso_nao_sigma0 + enso_nao_sigma1 * (-1) + enso_nao_sigma2 * (-1)),shape =enso_nao_shape,period=20)
      enso_pos_nao_neg_prob <- pevd(enso_neg_nao_neg_rvalue, loc = enso_nao_mu0 + enso_nao_mu1 * (1) + enso_nao_mu2 * (-1), scale = max(1e-100,enso_nao_sigma0 + enso_nao_sigma1 * (1) + enso_nao_sigma2 * (-1)), shape = enso_nao_shape,lower.tail =FALSE)
      enso_neg_nao_pos_prob <- pevd(enso_neg_nao_neg_rvalue, loc = enso_nao_mu0 + enso_nao_mu1 * (-1) + enso_nao_mu2 * (1), scale = max(1e-100,enso_nao_sigma0 + enso_nao_sigma1 * (-1) + enso_nao_sigma2 * (1)), shape = enso_nao_shape,lower.tail =FALSE)
      enso_pos_nao_pos_prob <- pevd(enso_neg_nao_neg_rvalue, loc = enso_nao_mu0 + enso_nao_mu1 * (1) + enso_nao_mu2 * (1), scale = max(1e-100,enso_nao_sigma0 + enso_nao_sigma1 * (1) + enso_nao_sigma2 * (1)), shape = enso_nao_shape,lower.tail =FALSE)
      e4_enso_pos_nao_neg_risk_factor[i,j] <- enso_pos_nao_neg_prob/0.05
      e4_enso_neg_nao_pos_risk_factor[i,j] <- enso_neg_nao_pos_prob/0.05
      e4_enso_pos_nao_pos_risk_factor[i,j] <- enso_pos_nao_pos_prob/0.05
      
      enso_neg_pdo_neg_rvalue <- extRemes::rlevd(df$acpcp,loc=enso_pdo_mu0 + enso_pdo_mu1 * (-1) + enso_pdo_mu2 * (-1),scale=max(1e-100,enso_pdo_sigma0 + enso_pdo_sigma1 * (-1) + enso_pdo_sigma2 * (-1)),shape =enso_pdo_shape,period=20)
      enso_pos_pdo_neg_prob <- pevd(enso_neg_pdo_neg_rvalue, loc = enso_pdo_mu0 + enso_pdo_mu1 * (1) + enso_pdo_mu2 * (-1), scale = max(1e-100,enso_pdo_sigma0 + enso_pdo_sigma1 * (1) + enso_pdo_sigma2 * (-1)), shape = enso_pdo_shape,lower.tail =FALSE)
      enso_neg_pdo_pos_prob <- pevd(enso_neg_pdo_neg_rvalue, loc = enso_pdo_mu0 + enso_pdo_mu1 * (-1) + enso_pdo_mu2 * (1), scale = max(1e-100,enso_pdo_sigma0 + enso_pdo_sigma1 * (-1) + enso_pdo_sigma2 * (1)), shape = enso_pdo_shape,lower.tail =FALSE)
      enso_pos_pdo_pos_prob <- pevd(enso_neg_pdo_neg_rvalue, loc = enso_pdo_mu0 + enso_pdo_mu1 * (1) + enso_pdo_mu2 * (1), scale = max(1e-100,enso_pdo_sigma0 + enso_pdo_sigma1 * (1) + enso_pdo_sigma2 * (1)), shape = enso_pdo_shape,lower.tail =FALSE)
      e4_enso_pos_pdo_neg_risk_factor[i,j] <- enso_pos_pdo_neg_prob/0.05
      e4_enso_neg_pdo_pos_risk_factor[i,j] <- enso_neg_pdo_pos_prob/0.05
      e4_enso_pos_pdo_pos_risk_factor[i,j] <- enso_pos_pdo_pos_prob/0.05
      
    }
  }
}

e4_enso_pos_nao_neg_rf <- data.frame(e4_enso_pos_nao_neg_risk_factor)
e4_enso_neg_nao_pos_rf <- data.frame(e4_enso_neg_nao_pos_risk_factor)
e4_enso_pos_nao_pos_rf <- data.frame(e4_enso_pos_nao_pos_risk_factor)
e4_enso_pos_pdo_neg_rf <- data.frame(e4_enso_pos_pdo_neg_risk_factor)
e4_enso_neg_pdo_pos_rf <- data.frame(e4_enso_neg_pdo_pos_risk_factor)
e4_enso_pos_pdo_pos_rf <- data.frame(e4_enso_pos_pdo_pos_risk_factor)


write.csv(e4_enso_pos_nao_neg_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e4_enso_pos_nao_neg_risk_factor.csv", row.names = FALSE)
write.csv(e4_enso_neg_nao_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e4_enso_neg_nao_pos_risk_factor.csv", row.names = FALSE)
write.csv(e4_enso_pos_nao_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e4_enso_pos_nao_pos_risk_factor.csv", row.names = FALSE)
write.csv(e4_enso_pos_pdo_neg_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e4_enso_pos_pdo_neg_risk_factor.csv", row.names = FALSE)
write.csv(e4_enso_neg_pdo_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e4_enso_neg_pdo_pos_risk_factor.csv", row.names = FALSE)
write.csv(e4_enso_pos_pdo_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e4_enso_pos_pdo_pos_risk_factor.csv", row.names = FALSE)

for(i in 1:length(elon)){
  
  for(j in 1:length(elat)){
    
    data1<-e5_acpcp[i,j,]
    
    if (sum(is.na(data1)) >= 50){
      
      e5_enso_pos_nao_neg_risk_factor[i,j] <- "NA"
      e5_enso_neg_nao_pos_risk_factor[i,j] <- "NA"
      e5_enso_neg_nao_neg_risk_factor[i,j] <- "NA"
      e5_enso_pos_pdo_neg_risk_factor[i,j] <- "NA"
      e5_enso_neg_pdo_pos_risk_factor[i,j] <- "NA"
      e5_enso_neg_pdo_neg_risk_factor[i,j] <- "NA"
      
    }
    else{
      
      df <- data.frame(data1,indices$year,indices$month,indices$soi,indices$naoi,indices$pdoi)
      colnames(df) <- c("acpcp","year","month","soi","naoi","pdoi")
      
      df <- na.omit(df)
      enso_nao_results <- extRemes::fevd(df$acpcp,data=df,location.fun=~soi + naoi,scale.fun=~soi + naoi)
      enso_pdo_results <- extRemes::fevd(df$acpcp,data=df,location.fun=~soi + pdoi,scale.fun=~soi + pdoi)
      
      enso_nao_mu0 <- enso_nao_results$results$par[1]
      enso_nao_mu1 <- enso_nao_results$results$par[2]
      enso_nao_mu2 <- enso_nao_results$results$par[3]
      enso_nao_sigma0 <- enso_nao_results$results$par[4]
      enso_nao_sigma1 <- enso_nao_results$results$par[5]
      enso_nao_sigma2 <- enso_nao_results$results$par[6]
      enso_nao_shape <- enso_nao_results$results$par[7]
      
      enso_pdo_mu0 <- enso_pdo_results$results$par[1]
      enso_pdo_mu1 <- enso_pdo_results$results$par[2]
      enso_pdo_mu2 <- enso_pdo_results$results$par[3]
      enso_pdo_sigma0 <- enso_pdo_results$results$par[4]
      enso_pdo_sigma1 <- enso_pdo_results$results$par[5]
      enso_pdo_sigma2 <- enso_pdo_results$results$par[6]
      enso_pdo_shape <- enso_pdo_results$results$par[7]
      
      enso_neg_nao_neg_rvalue <- extRemes::rlevd(df$acpcp,loc=enso_nao_mu0 + enso_nao_mu1 * (-1) + enso_nao_mu2 * (-1),scale=max(1e-100, enso_nao_sigma0 + enso_nao_sigma1 * (-1) + enso_nao_sigma2 * (-1)),shape =enso_nao_shape,period=20)
      enso_pos_nao_neg_prob <- pevd(enso_neg_nao_neg_rvalue, loc = enso_nao_mu0 + enso_nao_mu1 * (1) + enso_nao_mu2 * (-1), scale = max(1e-100,enso_nao_sigma0 + enso_nao_sigma1 * (1) + enso_nao_sigma2 * (-1)), shape = enso_nao_shape,lower.tail =FALSE)
      enso_neg_nao_pos_prob <- pevd(enso_neg_nao_neg_rvalue, loc = enso_nao_mu0 + enso_nao_mu1 * (-1) + enso_nao_mu2 * (1), scale = max(1e-100,enso_nao_sigma0 + enso_nao_sigma1 * (-1) + enso_nao_sigma2 * (1)), shape = enso_nao_shape,lower.tail =FALSE)
      enso_pos_nao_pos_prob <- pevd(enso_neg_nao_neg_rvalue, loc = enso_nao_mu0 + enso_nao_mu1 * (1) + enso_nao_mu2 * (1), scale = max(1e-100,enso_nao_sigma0 + enso_nao_sigma1 * (1) + enso_nao_sigma2 * (1)), shape = enso_nao_shape,lower.tail =FALSE)
      e5_enso_pos_nao_neg_risk_factor[i,j] <- enso_pos_nao_neg_prob/0.05
      e5_enso_neg_nao_pos_risk_factor[i,j] <- enso_neg_nao_pos_prob/0.05
      e5_enso_pos_nao_pos_risk_factor[i,j] <- enso_pos_nao_pos_prob/0.05
      
      enso_neg_pdo_neg_rvalue <- extRemes::rlevd(df$acpcp,loc=enso_pdo_mu0 + enso_pdo_mu1 * (-1) + enso_pdo_mu2 * (-1),scale=max(1e-100,enso_pdo_sigma0 + enso_pdo_sigma1 * (-1) + enso_pdo_sigma2 * (-1)),shape =enso_pdo_shape,period=20)
      enso_pos_pdo_neg_prob <- pevd(enso_neg_pdo_neg_rvalue, loc = enso_pdo_mu0 + enso_pdo_mu1 * (1) + enso_pdo_mu2 * (-1), scale = max(1e-100,enso_pdo_sigma0 + enso_pdo_sigma1 * (1) + enso_pdo_sigma2 * (-1)), shape = enso_pdo_shape,lower.tail =FALSE)
      enso_neg_pdo_pos_prob <- pevd(enso_neg_pdo_neg_rvalue, loc = enso_pdo_mu0 + enso_pdo_mu1 * (-1) + enso_pdo_mu2 * (1), scale = max(1e-100,enso_pdo_sigma0 + enso_pdo_sigma1 * (-1) + enso_pdo_sigma2 * (1)), shape = enso_pdo_shape,lower.tail =FALSE)
      enso_pos_pdo_pos_prob <- pevd(enso_neg_pdo_neg_rvalue, loc = enso_pdo_mu0 + enso_pdo_mu1 * (1) + enso_pdo_mu2 * (1), scale = max(1e-100,enso_pdo_sigma0 + enso_pdo_sigma1 * (1) + enso_pdo_sigma2 * (1)), shape = enso_pdo_shape,lower.tail =FALSE)
      e5_enso_pos_pdo_neg_risk_factor[i,j] <- enso_pos_pdo_neg_prob/0.05
      e5_enso_neg_pdo_pos_risk_factor[i,j] <- enso_neg_pdo_pos_prob/0.05
      e5_enso_pos_pdo_pos_risk_factor[i,j] <- enso_pos_pdo_pos_prob/0.05
      
    }
  }
}

e5_enso_pos_nao_neg_rf <- data.frame(e5_enso_pos_nao_neg_risk_factor)
e5_enso_neg_nao_pos_rf <- data.frame(e5_enso_neg_nao_pos_risk_factor)
e5_enso_pos_nao_pos_rf <- data.frame(e5_enso_pos_nao_pos_risk_factor)
e5_enso_pos_pdo_neg_rf <- data.frame(e5_enso_pos_pdo_neg_risk_factor)
e5_enso_neg_pdo_pos_rf <- data.frame(e5_enso_neg_pdo_pos_risk_factor)
e5_enso_pos_pdo_pos_rf <- data.frame(e5_enso_pos_pdo_pos_risk_factor)


write.csv(e5_enso_pos_nao_neg_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e5_enso_pos_nao_neg_risk_factor.csv", row.names = FALSE)
write.csv(e5_enso_neg_nao_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e5_enso_neg_nao_pos_risk_factor.csv", row.names = FALSE)
write.csv(e5_enso_pos_nao_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e5_enso_pos_nao_pos_risk_factor.csv", row.names = FALSE)
write.csv(e5_enso_pos_pdo_neg_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e5_enso_pos_pdo_neg_risk_factor.csv", row.names = FALSE)
write.csv(e5_enso_neg_pdo_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e5_enso_neg_pdo_pos_risk_factor.csv", row.names = FALSE)
write.csv(e5_enso_pos_pdo_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e5_enso_pos_pdo_pos_risk_factor.csv", row.names = FALSE)

for(i in 1:length(elon)){
  
  for(j in 1:length(elat)){
    
    data1<-e6_acpcp[i,j,]
    
    if (sum(is.na(data1)) >= 50){
      
      e6_enso_pos_nao_neg_risk_factor[i,j] <- "NA"
      e6_enso_neg_nao_pos_risk_factor[i,j] <- "NA"
      e6_enso_neg_nao_neg_risk_factor[i,j] <- "NA"
      e6_enso_pos_pdo_neg_risk_factor[i,j] <- "NA"
      e6_enso_neg_pdo_pos_risk_factor[i,j] <- "NA"
      e6_enso_neg_pdo_neg_risk_factor[i,j] <- "NA"
      
    }
    else{
      
      df <- data.frame(data1,indices$year,indices$month,indices$soi,indices$naoi,indices$pdoi)
      colnames(df) <- c("acpcp","year","month","soi","naoi","pdoi")
      
      df <- na.omit(df)
      enso_nao_results <- extRemes::fevd(df$acpcp,data=df,location.fun=~soi + naoi,scale.fun=~soi + naoi)
      enso_pdo_results <- extRemes::fevd(df$acpcp,data=df,location.fun=~soi + pdoi,scale.fun=~soi + pdoi)
      
      enso_nao_mu0 <- enso_nao_results$results$par[1]
      enso_nao_mu1 <- enso_nao_results$results$par[2]
      enso_nao_mu2 <- enso_nao_results$results$par[3]
      enso_nao_sigma0 <- enso_nao_results$results$par[4]
      enso_nao_sigma1 <- enso_nao_results$results$par[5]
      enso_nao_sigma2 <- enso_nao_results$results$par[6]
      enso_nao_shape <- enso_nao_results$results$par[7]
      
      enso_pdo_mu0 <- enso_pdo_results$results$par[1]
      enso_pdo_mu1 <- enso_pdo_results$results$par[2]
      enso_pdo_mu2 <- enso_pdo_results$results$par[3]
      enso_pdo_sigma0 <- enso_pdo_results$results$par[4]
      enso_pdo_sigma1 <- enso_pdo_results$results$par[5]
      enso_pdo_sigma2 <- enso_pdo_results$results$par[6]
      enso_pdo_shape <- enso_pdo_results$results$par[7]
      
      enso_neg_nao_neg_rvalue <- extRemes::rlevd(df$acpcp,loc=enso_nao_mu0 + enso_nao_mu1 * (-1) + enso_nao_mu2 * (-1),scale=max(1e-100, enso_nao_sigma0 + enso_nao_sigma1 * (-1) + enso_nao_sigma2 * (-1)),shape =enso_nao_shape,period=20)
      enso_pos_nao_neg_prob <- pevd(enso_neg_nao_neg_rvalue, loc = enso_nao_mu0 + enso_nao_mu1 * (1) + enso_nao_mu2 * (-1), scale = max(1e-100,enso_nao_sigma0 + enso_nao_sigma1 * (1) + enso_nao_sigma2 * (-1)), shape = enso_nao_shape,lower.tail =FALSE)
      enso_neg_nao_pos_prob <- pevd(enso_neg_nao_neg_rvalue, loc = enso_nao_mu0 + enso_nao_mu1 * (-1) + enso_nao_mu2 * (1), scale = max(1e-100,enso_nao_sigma0 + enso_nao_sigma1 * (-1) + enso_nao_sigma2 * (1)), shape = enso_nao_shape,lower.tail =FALSE)
      enso_pos_nao_pos_prob <- pevd(enso_neg_nao_neg_rvalue, loc = enso_nao_mu0 + enso_nao_mu1 * (1) + enso_nao_mu2 * (1), scale = max(1e-100,enso_nao_sigma0 + enso_nao_sigma1 * (1) + enso_nao_sigma2 * (1)), shape = enso_nao_shape,lower.tail =FALSE)
      e6_enso_pos_nao_neg_risk_factor[i,j] <- enso_pos_nao_neg_prob/0.05
      e6_enso_neg_nao_pos_risk_factor[i,j] <- enso_neg_nao_pos_prob/0.05
      e6_enso_pos_nao_pos_risk_factor[i,j] <- enso_pos_nao_pos_prob/0.05
      
      enso_neg_pdo_neg_rvalue <- extRemes::rlevd(df$acpcp,loc=enso_pdo_mu0 + enso_pdo_mu1 * (-1) + enso_pdo_mu2 * (-1),scale=max(1e-100,enso_pdo_sigma0 + enso_pdo_sigma1 * (-1) + enso_pdo_sigma2 * (-1)),shape =enso_pdo_shape,period=20)
      enso_pos_pdo_neg_prob <- pevd(enso_neg_pdo_neg_rvalue, loc = enso_pdo_mu0 + enso_pdo_mu1 * (1) + enso_pdo_mu2 * (-1), scale = max(1e-100,enso_pdo_sigma0 + enso_pdo_sigma1 * (1) + enso_pdo_sigma2 * (-1)), shape = enso_pdo_shape,lower.tail =FALSE)
      enso_neg_pdo_pos_prob <- pevd(enso_neg_pdo_neg_rvalue, loc = enso_pdo_mu0 + enso_pdo_mu1 * (-1) + enso_pdo_mu2 * (1), scale = max(1e-100,enso_pdo_sigma0 + enso_pdo_sigma1 * (-1) + enso_pdo_sigma2 * (1)), shape = enso_pdo_shape,lower.tail =FALSE)
      enso_pos_pdo_pos_prob <- pevd(enso_neg_pdo_neg_rvalue, loc = enso_pdo_mu0 + enso_pdo_mu1 * (1) + enso_pdo_mu2 * (1), scale = max(1e-100,enso_pdo_sigma0 + enso_pdo_sigma1 * (1) + enso_pdo_sigma2 * (1)), shape = enso_pdo_shape,lower.tail =FALSE)
      e6_enso_pos_pdo_neg_risk_factor[i,j] <- enso_pos_pdo_neg_prob/0.05
      e6_enso_neg_pdo_pos_risk_factor[i,j] <- enso_neg_pdo_pos_prob/0.05
      e6_enso_pos_pdo_pos_risk_factor[i,j] <- enso_pos_pdo_pos_prob/0.05
      
    }
  }
}

e6_enso_pos_nao_neg_rf <- data.frame(e6_enso_pos_nao_neg_risk_factor)
e6_enso_neg_nao_pos_rf <- data.frame(e6_enso_neg_nao_pos_risk_factor)
e6_enso_pos_nao_pos_rf <- data.frame(e6_enso_pos_nao_pos_risk_factor)
e6_enso_pos_pdo_neg_rf <- data.frame(e6_enso_pos_pdo_neg_risk_factor)
e6_enso_neg_pdo_pos_rf <- data.frame(e6_enso_neg_pdo_pos_risk_factor)
e6_enso_pos_pdo_pos_rf <- data.frame(e6_enso_pos_pdo_pos_risk_factor)


write.csv(e6_enso_pos_nao_neg_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e6_enso_pos_nao_neg_risk_factor.csv", row.names = FALSE)
write.csv(e6_enso_neg_nao_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e6_enso_neg_nao_pos_risk_factor.csv", row.names = FALSE)
write.csv(e6_enso_pos_nao_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e6_enso_pos_nao_pos_risk_factor.csv", row.names = FALSE)
write.csv(e6_enso_pos_pdo_neg_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e6_enso_pos_pdo_neg_risk_factor.csv", row.names = FALSE)
write.csv(e6_enso_neg_pdo_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e6_enso_neg_pdo_pos_risk_factor.csv", row.names = FALSE)
write.csv(e6_enso_pos_pdo_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e6_enso_pos_pdo_pos_risk_factor.csv", row.names = FALSE)

for(i in 1:length(elon)){
  
  for(j in 1:length(elat)){
    
    data1<-e7_acpcp[i,j,]
    
    if (sum(is.na(data1)) >= 50){
      
      e7_enso_pos_nao_neg_risk_factor[i,j] <- "NA"
      e7_enso_neg_nao_pos_risk_factor[i,j] <- "NA"
      e7_enso_neg_nao_neg_risk_factor[i,j] <- "NA"
      e7_enso_pos_pdo_neg_risk_factor[i,j] <- "NA"
      e7_enso_neg_pdo_pos_risk_factor[i,j] <- "NA"
      e7_enso_neg_pdo_neg_risk_factor[i,j] <- "NA"
      
    }
    else{
      
      df <- data.frame(data1,indices$year,indices$month,indices$soi,indices$naoi,indices$pdoi)
      colnames(df) <- c("acpcp","year","month","soi","naoi","pdoi")
      
      df <- na.omit(df)
      enso_nao_results <- extRemes::fevd(df$acpcp,data=df,location.fun=~soi + naoi,scale.fun=~soi + naoi)
      enso_pdo_results <- extRemes::fevd(df$acpcp,data=df,location.fun=~soi + pdoi,scale.fun=~soi + pdoi)
      
      enso_nao_mu0 <- enso_nao_results$results$par[1]
      enso_nao_mu1 <- enso_nao_results$results$par[2]
      enso_nao_mu2 <- enso_nao_results$results$par[3]
      enso_nao_sigma0 <- enso_nao_results$results$par[4]
      enso_nao_sigma1 <- enso_nao_results$results$par[5]
      enso_nao_sigma2 <- enso_nao_results$results$par[6]
      enso_nao_shape <- enso_nao_results$results$par[7]
      
      enso_pdo_mu0 <- enso_pdo_results$results$par[1]
      enso_pdo_mu1 <- enso_pdo_results$results$par[2]
      enso_pdo_mu2 <- enso_pdo_results$results$par[3]
      enso_pdo_sigma0 <- enso_pdo_results$results$par[4]
      enso_pdo_sigma1 <- enso_pdo_results$results$par[5]
      enso_pdo_sigma2 <- enso_pdo_results$results$par[6]
      enso_pdo_shape <- enso_pdo_results$results$par[7]
      
      enso_neg_nao_neg_rvalue <- extRemes::rlevd(df$acpcp,loc=enso_nao_mu0 + enso_nao_mu1 * (-1) + enso_nao_mu2 * (-1),scale=max(1e-100, enso_nao_sigma0 + enso_nao_sigma1 * (-1) + enso_nao_sigma2 * (-1)),shape =enso_nao_shape,period=20)
      enso_pos_nao_neg_prob <- pevd(enso_neg_nao_neg_rvalue, loc = enso_nao_mu0 + enso_nao_mu1 * (1) + enso_nao_mu2 * (-1), scale = max(1e-100,enso_nao_sigma0 + enso_nao_sigma1 * (1) + enso_nao_sigma2 * (-1)), shape = enso_nao_shape,lower.tail =FALSE)
      enso_neg_nao_pos_prob <- pevd(enso_neg_nao_neg_rvalue, loc = enso_nao_mu0 + enso_nao_mu1 * (-1) + enso_nao_mu2 * (1), scale = max(1e-100,enso_nao_sigma0 + enso_nao_sigma1 * (-1) + enso_nao_sigma2 * (1)), shape = enso_nao_shape,lower.tail =FALSE)
      enso_pos_nao_pos_prob <- pevd(enso_neg_nao_neg_rvalue, loc = enso_nao_mu0 + enso_nao_mu1 * (1) + enso_nao_mu2 * (1), scale = max(1e-100,enso_nao_sigma0 + enso_nao_sigma1 * (1) + enso_nao_sigma2 * (1)), shape = enso_nao_shape,lower.tail =FALSE)
      e7_enso_pos_nao_neg_risk_factor[i,j] <- enso_pos_nao_neg_prob/0.05
      e7_enso_neg_nao_pos_risk_factor[i,j] <- enso_neg_nao_pos_prob/0.05
      e7_enso_pos_nao_pos_risk_factor[i,j] <- enso_pos_nao_pos_prob/0.05
      
      enso_neg_pdo_neg_rvalue <- extRemes::rlevd(df$acpcp,loc=enso_pdo_mu0 + enso_pdo_mu1 * (-1) + enso_pdo_mu2 * (-1),scale=max(1e-100,enso_pdo_sigma0 + enso_pdo_sigma1 * (-1) + enso_pdo_sigma2 * (-1)),shape =enso_pdo_shape,period=20)
      enso_pos_pdo_neg_prob <- pevd(enso_neg_pdo_neg_rvalue, loc = enso_pdo_mu0 + enso_pdo_mu1 * (1) + enso_pdo_mu2 * (-1), scale = max(1e-100,enso_pdo_sigma0 + enso_pdo_sigma1 * (1) + enso_pdo_sigma2 * (-1)), shape = enso_pdo_shape,lower.tail =FALSE)
      enso_neg_pdo_pos_prob <- pevd(enso_neg_pdo_neg_rvalue, loc = enso_pdo_mu0 + enso_pdo_mu1 * (-1) + enso_pdo_mu2 * (1), scale = max(1e-100,enso_pdo_sigma0 + enso_pdo_sigma1 * (-1) + enso_pdo_sigma2 * (1)), shape = enso_pdo_shape,lower.tail =FALSE)
      enso_pos_pdo_pos_prob <- pevd(enso_neg_pdo_neg_rvalue, loc = enso_pdo_mu0 + enso_pdo_mu1 * (1) + enso_pdo_mu2 * (1), scale = max(1e-100,enso_pdo_sigma0 + enso_pdo_sigma1 * (1) + enso_pdo_sigma2 * (1)), shape = enso_pdo_shape,lower.tail =FALSE)
      e7_enso_pos_pdo_neg_risk_factor[i,j] <- enso_pos_pdo_neg_prob/0.05
      e7_enso_neg_pdo_pos_risk_factor[i,j] <- enso_neg_pdo_pos_prob/0.05
      e7_enso_pos_pdo_pos_risk_factor[i,j] <- enso_pos_pdo_pos_prob/0.05
      
    }
  }
}

e7_enso_pos_nao_neg_rf <- data.frame(e7_enso_pos_nao_neg_risk_factor)
e7_enso_neg_nao_pos_rf <- data.frame(e7_enso_neg_nao_pos_risk_factor)
e7_enso_pos_nao_pos_rf <- data.frame(e7_enso_pos_nao_pos_risk_factor)
e7_enso_pos_pdo_neg_rf <- data.frame(e7_enso_pos_pdo_neg_risk_factor)
e7_enso_neg_pdo_pos_rf <- data.frame(e7_enso_neg_pdo_pos_risk_factor)
e7_enso_pos_pdo_pos_rf <- data.frame(e7_enso_pos_pdo_pos_risk_factor)


write.csv(e7_enso_pos_nao_neg_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e7_enso_pos_nao_neg_risk_factor.csv", row.names = FALSE)
write.csv(e7_enso_neg_nao_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e7_enso_neg_nao_pos_risk_factor.csv", row.names = FALSE)
write.csv(e7_enso_pos_nao_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e7_enso_pos_nao_pos_risk_factor.csv", row.names = FALSE)
write.csv(e7_enso_pos_pdo_neg_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e7_enso_pos_pdo_neg_risk_factor.csv", row.names = FALSE)
write.csv(e7_enso_neg_pdo_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e7_enso_neg_pdo_pos_risk_factor.csv", row.names = FALSE)
write.csv(e7_enso_pos_pdo_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e7_enso_pos_pdo_pos_risk_factor.csv", row.names = FALSE)

for(i in 1:length(elon)){
  
  for(j in 1:length(elat)){
    
    data1<-e8_acpcp[i,j,]
    
    if (sum(is.na(data1)) >= 50){
      
      e8_enso_pos_nao_neg_risk_factor[i,j] <- "NA"
      e8_enso_neg_nao_pos_risk_factor[i,j] <- "NA"
      e8_enso_neg_nao_neg_risk_factor[i,j] <- "NA"
      e8_enso_pos_pdo_neg_risk_factor[i,j] <- "NA"
      e8_enso_neg_pdo_pos_risk_factor[i,j] <- "NA"
      e8_enso_neg_pdo_neg_risk_factor[i,j] <- "NA"
      
    }
    else{
      
      df <- data.frame(data1,indices$year,indices$month,indices$soi,indices$naoi,indices$pdoi)
      colnames(df) <- c("acpcp","year","month","soi","naoi","pdoi")
      
      df <- na.omit(df)
      enso_nao_results <- extRemes::fevd(df$acpcp,data=df,location.fun=~soi + naoi,scale.fun=~soi + naoi)
      enso_pdo_results <- extRemes::fevd(df$acpcp,data=df,location.fun=~soi + pdoi,scale.fun=~soi + pdoi)
      
      enso_nao_mu0 <- enso_nao_results$results$par[1]
      enso_nao_mu1 <- enso_nao_results$results$par[2]
      enso_nao_mu2 <- enso_nao_results$results$par[3]
      enso_nao_sigma0 <- enso_nao_results$results$par[4]
      enso_nao_sigma1 <- enso_nao_results$results$par[5]
      enso_nao_sigma2 <- enso_nao_results$results$par[6]
      enso_nao_shape <- enso_nao_results$results$par[7]
      
      enso_pdo_mu0 <- enso_pdo_results$results$par[1]
      enso_pdo_mu1 <- enso_pdo_results$results$par[2]
      enso_pdo_mu2 <- enso_pdo_results$results$par[3]
      enso_pdo_sigma0 <- enso_pdo_results$results$par[4]
      enso_pdo_sigma1 <- enso_pdo_results$results$par[5]
      enso_pdo_sigma2 <- enso_pdo_results$results$par[6]
      enso_pdo_shape <- enso_pdo_results$results$par[7]
      
      enso_neg_nao_neg_rvalue <- extRemes::rlevd(df$acpcp,loc=enso_nao_mu0 + enso_nao_mu1 * (-1) + enso_nao_mu2 * (-1),scale=max(1e-100, enso_nao_sigma0 + enso_nao_sigma1 * (-1) + enso_nao_sigma2 * (-1)),shape =enso_nao_shape,period=20)
      enso_pos_nao_neg_prob <- pevd(enso_neg_nao_neg_rvalue, loc = enso_nao_mu0 + enso_nao_mu1 * (1) + enso_nao_mu2 * (-1), scale = max(1e-100,enso_nao_sigma0 + enso_nao_sigma1 * (1) + enso_nao_sigma2 * (-1)), shape = enso_nao_shape,lower.tail =FALSE)
      enso_neg_nao_pos_prob <- pevd(enso_neg_nao_neg_rvalue, loc = enso_nao_mu0 + enso_nao_mu1 * (-1) + enso_nao_mu2 * (1), scale = max(1e-100,enso_nao_sigma0 + enso_nao_sigma1 * (-1) + enso_nao_sigma2 * (1)), shape = enso_nao_shape,lower.tail =FALSE)
      enso_pos_nao_pos_prob <- pevd(enso_neg_nao_neg_rvalue, loc = enso_nao_mu0 + enso_nao_mu1 * (1) + enso_nao_mu2 * (1), scale = max(1e-100,enso_nao_sigma0 + enso_nao_sigma1 * (1) + enso_nao_sigma2 * (1)), shape = enso_nao_shape,lower.tail =FALSE)
      e8_enso_pos_nao_neg_risk_factor[i,j] <- enso_pos_nao_neg_prob/0.05
      e8_enso_neg_nao_pos_risk_factor[i,j] <- enso_neg_nao_pos_prob/0.05
      e8_enso_pos_nao_pos_risk_factor[i,j] <- enso_pos_nao_pos_prob/0.05
      
      enso_neg_pdo_neg_rvalue <- extRemes::rlevd(df$acpcp,loc=enso_pdo_mu0 + enso_pdo_mu1 * (-1) + enso_pdo_mu2 * (-1),scale=max(1e-100,enso_pdo_sigma0 + enso_pdo_sigma1 * (-1) + enso_pdo_sigma2 * (-1)),shape =enso_pdo_shape,period=20)
      enso_pos_pdo_neg_prob <- pevd(enso_neg_pdo_neg_rvalue, loc = enso_pdo_mu0 + enso_pdo_mu1 * (1) + enso_pdo_mu2 * (-1), scale = max(1e-100,enso_pdo_sigma0 + enso_pdo_sigma1 * (1) + enso_pdo_sigma2 * (-1)), shape = enso_pdo_shape,lower.tail =FALSE)
      enso_neg_pdo_pos_prob <- pevd(enso_neg_pdo_neg_rvalue, loc = enso_pdo_mu0 + enso_pdo_mu1 * (-1) + enso_pdo_mu2 * (1), scale = max(1e-100,enso_pdo_sigma0 + enso_pdo_sigma1 * (-1) + enso_pdo_sigma2 * (1)), shape = enso_pdo_shape,lower.tail =FALSE)
      enso_pos_pdo_pos_prob <- pevd(enso_neg_pdo_neg_rvalue, loc = enso_pdo_mu0 + enso_pdo_mu1 * (1) + enso_pdo_mu2 * (1), scale = max(1e-100,enso_pdo_sigma0 + enso_pdo_sigma1 * (1) + enso_pdo_sigma2 * (1)), shape = enso_pdo_shape,lower.tail =FALSE)
      e8_enso_pos_pdo_neg_risk_factor[i,j] <- enso_pos_pdo_neg_prob/0.05
      e8_enso_neg_pdo_pos_risk_factor[i,j] <- enso_neg_pdo_pos_prob/0.05
      e8_enso_pos_pdo_pos_risk_factor[i,j] <- enso_pos_pdo_pos_prob/0.05
      
    }
  }
}

e8_enso_pos_nao_neg_rf <- data.frame(e8_enso_pos_nao_neg_risk_factor)
e8_enso_neg_nao_pos_rf <- data.frame(e8_enso_neg_nao_pos_risk_factor)
e8_enso_pos_nao_pos_rf <- data.frame(e8_enso_pos_nao_pos_risk_factor)
e8_enso_pos_pdo_neg_rf <- data.frame(e8_enso_pos_pdo_neg_risk_factor)
e8_enso_neg_pdo_pos_rf <- data.frame(e8_enso_neg_pdo_pos_risk_factor)
e8_enso_pos_pdo_pos_rf <- data.frame(e8_enso_pos_pdo_pos_risk_factor)


write.csv(e8_enso_pos_nao_neg_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e8_enso_pos_nao_neg_risk_factor.csv", row.names = FALSE)
write.csv(e8_enso_neg_nao_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e8_enso_neg_nao_pos_risk_factor.csv", row.names = FALSE)
write.csv(e8_enso_pos_nao_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e8_enso_pos_nao_pos_risk_factor.csv", row.names = FALSE)
write.csv(e8_enso_pos_pdo_neg_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e8_enso_pos_pdo_neg_risk_factor.csv", row.names = FALSE)
write.csv(e8_enso_neg_pdo_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e8_enso_neg_pdo_pos_risk_factor.csv", row.names = FALSE)
write.csv(e8_enso_pos_pdo_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e8_enso_pos_pdo_pos_risk_factor.csv", row.names = FALSE)

for(i in 1:length(elon)){
  
  for(j in 1:length(elat)){
    
    data1<-e9_acpcp[i,j,]
    
    if (sum(is.na(data1)) >= 50){
      
      e9_enso_pos_nao_neg_risk_factor[i,j] <- "NA"
      e9_enso_neg_nao_pos_risk_factor[i,j] <- "NA"
      e9_enso_neg_nao_neg_risk_factor[i,j] <- "NA"
      e9_enso_pos_pdo_neg_risk_factor[i,j] <- "NA"
      e9_enso_neg_pdo_pos_risk_factor[i,j] <- "NA"
      e9_enso_neg_pdo_neg_risk_factor[i,j] <- "NA"
      
    }
    else{
      
      df <- data.frame(data1,indices$year,indices$month,indices$soi,indices$naoi,indices$pdoi)
      colnames(df) <- c("acpcp","year","month","soi","naoi","pdoi")
      
      df <- na.omit(df)
      enso_nao_results <- extRemes::fevd(df$acpcp,data=df,location.fun=~soi + naoi,scale.fun=~soi + naoi)
      enso_pdo_results <- extRemes::fevd(df$acpcp,data=df,location.fun=~soi + pdoi,scale.fun=~soi + pdoi)
      
      enso_nao_mu0 <- enso_nao_results$results$par[1]
      enso_nao_mu1 <- enso_nao_results$results$par[2]
      enso_nao_mu2 <- enso_nao_results$results$par[3]
      enso_nao_sigma0 <- enso_nao_results$results$par[4]
      enso_nao_sigma1 <- enso_nao_results$results$par[5]
      enso_nao_sigma2 <- enso_nao_results$results$par[6]
      enso_nao_shape <- enso_nao_results$results$par[7]
      
      enso_pdo_mu0 <- enso_pdo_results$results$par[1]
      enso_pdo_mu1 <- enso_pdo_results$results$par[2]
      enso_pdo_mu2 <- enso_pdo_results$results$par[3]
      enso_pdo_sigma0 <- enso_pdo_results$results$par[4]
      enso_pdo_sigma1 <- enso_pdo_results$results$par[5]
      enso_pdo_sigma2 <- enso_pdo_results$results$par[6]
      enso_pdo_shape <- enso_pdo_results$results$par[7]
      
      enso_neg_nao_neg_rvalue <- extRemes::rlevd(df$acpcp,loc=enso_nao_mu0 + enso_nao_mu1 * (-1) + enso_nao_mu2 * (-1),scale=max(1e-100, enso_nao_sigma0 + enso_nao_sigma1 * (-1) + enso_nao_sigma2 * (-1)),shape =enso_nao_shape,period=20)
      enso_pos_nao_neg_prob <- pevd(enso_neg_nao_neg_rvalue, loc = enso_nao_mu0 + enso_nao_mu1 * (1) + enso_nao_mu2 * (-1), scale = max(1e-100,enso_nao_sigma0 + enso_nao_sigma1 * (1) + enso_nao_sigma2 * (-1)), shape = enso_nao_shape,lower.tail =FALSE)
      enso_neg_nao_pos_prob <- pevd(enso_neg_nao_neg_rvalue, loc = enso_nao_mu0 + enso_nao_mu1 * (-1) + enso_nao_mu2 * (1), scale = max(1e-100,enso_nao_sigma0 + enso_nao_sigma1 * (-1) + enso_nao_sigma2 * (1)), shape = enso_nao_shape,lower.tail =FALSE)
      enso_pos_nao_pos_prob <- pevd(enso_neg_nao_neg_rvalue, loc = enso_nao_mu0 + enso_nao_mu1 * (1) + enso_nao_mu2 * (1), scale = max(1e-100,enso_nao_sigma0 + enso_nao_sigma1 * (1) + enso_nao_sigma2 * (1)), shape = enso_nao_shape,lower.tail =FALSE)
      e9_enso_pos_nao_neg_risk_factor[i,j] <- enso_pos_nao_neg_prob/0.05
      e9_enso_neg_nao_pos_risk_factor[i,j] <- enso_neg_nao_pos_prob/0.05
      e9_enso_pos_nao_pos_risk_factor[i,j] <- enso_pos_nao_pos_prob/0.05
      
      enso_neg_pdo_neg_rvalue <- extRemes::rlevd(df$acpcp,loc=enso_pdo_mu0 + enso_pdo_mu1 * (-1) + enso_pdo_mu2 * (-1),scale=max(1e-100,enso_pdo_sigma0 + enso_pdo_sigma1 * (-1) + enso_pdo_sigma2 * (-1)),shape =enso_pdo_shape,period=20)
      enso_pos_pdo_neg_prob <- pevd(enso_neg_pdo_neg_rvalue, loc = enso_pdo_mu0 + enso_pdo_mu1 * (1) + enso_pdo_mu2 * (-1), scale = max(1e-100,enso_pdo_sigma0 + enso_pdo_sigma1 * (1) + enso_pdo_sigma2 * (-1)), shape = enso_pdo_shape,lower.tail =FALSE)
      enso_neg_pdo_pos_prob <- pevd(enso_neg_pdo_neg_rvalue, loc = enso_pdo_mu0 + enso_pdo_mu1 * (-1) + enso_pdo_mu2 * (1), scale = max(1e-100,enso_pdo_sigma0 + enso_pdo_sigma1 * (-1) + enso_pdo_sigma2 * (1)), shape = enso_pdo_shape,lower.tail =FALSE)
      enso_pos_pdo_pos_prob <- pevd(enso_neg_pdo_neg_rvalue, loc = enso_pdo_mu0 + enso_pdo_mu1 * (1) + enso_pdo_mu2 * (1), scale = max(1e-100,enso_pdo_sigma0 + enso_pdo_sigma1 * (1) + enso_pdo_sigma2 * (1)), shape = enso_pdo_shape,lower.tail =FALSE)
      e9_enso_pos_pdo_neg_risk_factor[i,j] <- enso_pos_pdo_neg_prob/0.05
      e9_enso_neg_pdo_pos_risk_factor[i,j] <- enso_neg_pdo_pos_prob/0.05
      e9_enso_pos_pdo_pos_risk_factor[i,j] <- enso_pos_pdo_pos_prob/0.05
      
    }
  }
}

e9_enso_pos_nao_neg_rf <- data.frame(e9_enso_pos_nao_neg_risk_factor)
e9_enso_neg_nao_pos_rf <- data.frame(e9_enso_neg_nao_pos_risk_factor)
e9_enso_pos_nao_pos_rf <- data.frame(e9_enso_pos_nao_pos_risk_factor)
e9_enso_pos_pdo_neg_rf <- data.frame(e9_enso_pos_pdo_neg_risk_factor)
e9_enso_neg_pdo_pos_rf <- data.frame(e9_enso_neg_pdo_pos_risk_factor)
e9_enso_pos_pdo_pos_rf <- data.frame(e9_enso_pos_pdo_pos_risk_factor)


write.csv(e9_enso_pos_nao_neg_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e9_enso_pos_nao_neg_risk_factor.csv", row.names = FALSE)
write.csv(e9_enso_neg_nao_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e9_enso_neg_nao_pos_risk_factor.csv", row.names = FALSE)
write.csv(e9_enso_pos_nao_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e9_enso_pos_nao_pos_risk_factor.csv", row.names = FALSE)
write.csv(e9_enso_pos_pdo_neg_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e9_enso_pos_pdo_neg_risk_factor.csv", row.names = FALSE)
write.csv(e9_enso_neg_pdo_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e9_enso_neg_pdo_pos_risk_factor.csv", row.names = FALSE)
write.csv(e9_enso_pos_pdo_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e9_enso_pos_pdo_pos_risk_factor.csv", row.names = FALSE)

n_enso_pos_nao_pos_pdo_pos_risk_factor <- matrix(0,nrow=dim(nlon)[1],ncol=dim(nlon)[2])
n_enso_pos_nao_pos_pdo_neg_risk_factor <- matrix(0,nrow=dim(nlon)[1],ncol=dim(nlon)[2])
n_enso_pos_nao_neg_pdo_pos_risk_factor <- matrix(0,nrow=dim(nlon)[1],ncol=dim(nlon)[2])
n_enso_pos_nao_neg_pdo_neg_risk_factor <- matrix(0,nrow=dim(nlon)[1],ncol=dim(nlon)[2])
n_enso_neg_nao_pos_pdo_pos_risk_factor <- matrix(0,nrow=dim(nlon)[1],ncol=dim(nlon)[2])
n_enso_neg_nao_pos_pdo_neg_risk_factor <- matrix(0,nrow=dim(nlon)[1],ncol=dim(nlon)[2])
n_enso_neg_nao_neg_pdo_pos_risk_factor <- matrix(0,nrow=dim(nlon)[1],ncol=dim(nlon)[2])
n_enso_pos_nao_pos_pdo_pos_risk_factor <- matrix(0,nrow=dim(nlon)[1],ncol=dim(nlon)[2])

e1_enso_pos_nao_pos_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e1_enso_pos_nao_pos_pdo_neg_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e1_enso_pos_nao_neg_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e1_enso_pos_nao_neg_pdo_neg_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e1_enso_neg_nao_pos_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e1_enso_neg_nao_pos_pdo_neg_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e1_enso_neg_nao_neg_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e1_enso_pos_nao_pos_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))

e2_enso_pos_nao_pos_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e2_enso_pos_nao_pos_pdo_neg_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e2_enso_pos_nao_neg_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e2_enso_pos_nao_neg_pdo_neg_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e2_enso_neg_nao_pos_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e2_enso_neg_nao_pos_pdo_neg_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e2_enso_neg_nao_neg_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e2_enso_pos_nao_pos_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))

e3_enso_pos_nao_pos_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e3_enso_pos_nao_pos_pdo_neg_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e3_enso_pos_nao_neg_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e3_enso_pos_nao_neg_pdo_neg_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e3_enso_neg_nao_pos_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e3_enso_neg_nao_pos_pdo_neg_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e3_enso_neg_nao_neg_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e3_enso_pos_nao_pos_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))

e4_enso_pos_nao_pos_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e4_enso_pos_nao_pos_pdo_neg_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e4_enso_pos_nao_neg_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e4_enso_pos_nao_neg_pdo_neg_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e4_enso_neg_nao_pos_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e4_enso_neg_nao_pos_pdo_neg_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e4_enso_neg_nao_neg_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e4_enso_pos_nao_pos_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))

e5_enso_pos_nao_pos_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e5_enso_pos_nao_pos_pdo_neg_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e5_enso_pos_nao_neg_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e5_enso_pos_nao_neg_pdo_neg_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e5_enso_neg_nao_pos_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e5_enso_neg_nao_pos_pdo_neg_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e5_enso_neg_nao_neg_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e5_enso_pos_nao_pos_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))

e6_enso_pos_nao_pos_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e6_enso_pos_nao_pos_pdo_neg_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e6_enso_pos_nao_neg_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e6_enso_pos_nao_neg_pdo_neg_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e6_enso_neg_nao_pos_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e6_enso_neg_nao_pos_pdo_neg_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e6_enso_neg_nao_neg_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e6_enso_pos_nao_pos_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))

e7_enso_pos_nao_pos_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e7_enso_pos_nao_pos_pdo_neg_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e7_enso_pos_nao_neg_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e7_enso_pos_nao_neg_pdo_neg_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e7_enso_neg_nao_pos_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e7_enso_neg_nao_pos_pdo_neg_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e7_enso_neg_nao_neg_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e7_enso_pos_nao_pos_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))

e8_enso_pos_nao_pos_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e8_enso_pos_nao_pos_pdo_neg_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e8_enso_pos_nao_neg_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e8_enso_pos_nao_neg_pdo_neg_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e8_enso_neg_nao_pos_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e8_enso_neg_nao_pos_pdo_neg_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e8_enso_neg_nao_neg_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e8_enso_pos_nao_pos_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))

e9_enso_pos_nao_pos_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e9_enso_pos_nao_pos_pdo_neg_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e9_enso_pos_nao_neg_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e9_enso_pos_nao_neg_pdo_neg_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e9_enso_neg_nao_pos_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e9_enso_neg_nao_pos_pdo_neg_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e9_enso_neg_nao_neg_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))
e9_enso_pos_nao_pos_pdo_pos_risk_factor <- matrix(0,nrow=dim(elon),ncol=dim(elat))

for(i in 1:dim(elon)){
  
  for(j in 1:dim(elat)){
    
    cat(i,j,"\n")
    
    data1<-e1_acpcp[i,j,]
    
    if (sum(is.na(data1)) >= 40){
      
      e1_enso_pos_nao_pos_pdo_neg_risk_factor[i,j] <- "NA"
      e1_enso_pos_nao_neg_pdo_pos_risk_factor[i,j] <- "NA"
      e1_enso_pos_nao_neg_pdo_neg_risk_factor[i,j] <- "NA"
      e1_enso_neg_nao_pos_pdo_pos_risk_factor[i,j] <- "NA"
      e1_enso_neg_nao_pos_pdo_neg_risk_factor[i,j] <- "NA"
      e1_enso_neg_nao_neg_pdo_pos_risk_factor[i,j] <- "NA"
      e1_enso_pos_nao_pos_pdo_pos_risk_factor[i,j] <- "NA"
      
    }
    else{
      
      df <- data.frame(data1,indices$year,indices$month,indices$soi,indices$naoi,indices$pdoi)
      colnames(df) <- c("acpcp","year","month","soi","naoi","pdoi")
      
      df <- na.omit(df)
      
      enso_nao_pdo_results <- extRemes::fevd(df$acpcp,data=df,location.fun=~soi + naoi + pdoi,scale.fun=~soi + naoi + pdoi)
      
      enso_nao_pdo_mu0 <- enso_nao_pdo_results$results$par[1]
      enso_nao_pdo_mu1 <- enso_nao_pdo_results$results$par[2]
      enso_nao_pdo_mu2 <- enso_nao_pdo_results$results$par[3]
      enso_nao_pdo_mu3 <- enso_nao_pdo_results$results$par[4]
      enso_nao_pdo_sigma0 <- enso_nao_pdo_results$results$par[5]
      enso_nao_pdo_sigma1 <- enso_nao_pdo_results$results$par[6]
      enso_nao_pdo_sigma2 <- enso_nao_pdo_results$results$par[7]
      enso_nao_pdo_sigma3 <- enso_nao_pdo_results$results$par[8]
      enso_nao_pdo_shape <- enso_nao_pdo_results$results$par[9]
      
      enso_neg_nao_neg_pdo_neg_rvalue <- extRemes::rlevd(df$acpcp,loc=enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (-1) + enso_nao_pdo_mu2 * (-1) + enso_nao_pdo_mu3 * (-1),scale=max(1e-100, enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (-1) + enso_nao_pdo_sigma2 * (-1) + enso_nao_pdo_sigma3 * (-1)),shape =enso_nao_pdo_shape,period=20)
      enso_pos_nao_pos_pdo_neg_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (1) + enso_nao_pdo_mu2 * (1) + enso_nao_pdo_mu3 * (-1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (1) + enso_nao_pdo_sigma2 * (1) + enso_nao_pdo_sigma3 * (-1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      enso_pos_nao_neg_pdo_pos_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (1) + enso_nao_pdo_mu2 * (-1) + enso_nao_pdo_mu3 * (1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (1) + enso_nao_pdo_sigma2 * (-1) + enso_nao_pdo_sigma3 * (1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      enso_pos_nao_neg_pdo_neg_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (1) + enso_nao_pdo_mu2 * (-1) + enso_nao_pdo_mu3 * (-1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (1) + enso_nao_pdo_sigma2 * (-1) + enso_nao_pdo_sigma3 * (-1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      enso_neg_nao_pos_pdo_pos_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (-1) + enso_nao_pdo_mu2 * (1) + enso_nao_pdo_mu3 * (1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (-1) + enso_nao_pdo_sigma2 * (1) + enso_nao_pdo_sigma3 * (1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      enso_neg_nao_pos_pdo_neg_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (-1) + enso_nao_pdo_mu2 * (1) + enso_nao_pdo_mu3 * (-1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (-1) + enso_nao_pdo_sigma2 * (1) + enso_nao_pdo_sigma3 * (-1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      enso_neg_nao_neg_pdo_pos_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (-1) + enso_nao_pdo_mu2 * (-1) + enso_nao_pdo_mu3 * (1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (-1) + enso_nao_pdo_sigma2 * (-1) + enso_nao_pdo_sigma3 * (1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      enso_pos_nao_pos_pdo_pos_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (1) + enso_nao_pdo_mu2 * (1) + enso_nao_pdo_mu3 * (1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (1) + enso_nao_pdo_sigma2 * (1) + enso_nao_pdo_sigma3 * (1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
     
      e1_enso_pos_nao_pos_pdo_neg_risk_factor[i,j] <- enso_pos_nao_pos_pdo_neg_prob/0.05
      e1_enso_pos_nao_neg_pdo_pos_risk_factor[i,j] <- enso_pos_nao_neg_pdo_pos_prob/0.05
      e1_enso_pos_nao_neg_pdo_neg_risk_factor[i,j] <- enso_pos_nao_neg_pdo_neg_prob/0.05
      e1_enso_neg_nao_pos_pdo_pos_risk_factor[i,j] <- enso_neg_nao_pos_pdo_pos_prob/0.05
      e1_enso_neg_nao_pos_pdo_neg_risk_factor[i,j] <- enso_neg_nao_pos_pdo_neg_prob/0.05
      e1_enso_neg_nao_neg_pdo_pos_risk_factor[i,j] <- enso_neg_nao_neg_pdo_pos_prob/0.05
      e1_enso_pos_nao_pos_pdo_pos_risk_factor[i,j] <- enso_pos_nao_pos_pdo_pos_prob/0.05
      
    }
  }
}
indices


e1_enso_pos_nao_pos_pdo_neg_rf <- data.frame(e1_enso_pos_nao_pos_pdo_neg_risk_factor)
e1_enso_pos_nao_neg_pdo_pos_rf <- data.frame(e1_enso_pos_nao_neg_pdo_pos_risk_factor)
e1_enso_pos_nao_neg_pdo_neg_rf <- data.frame(e1_enso_pos_nao_neg_pdo_neg_risk_factor)
e1_enso_neg_nao_pos_pdo_pos_rf <- data.frame(e1_enso_neg_nao_pos_pdo_pos_risk_factor)
e1_enso_neg_nao_pos_pdo_neg_rf <- data.frame(e1_enso_neg_nao_pos_pdo_neg_risk_factor)
e1_enso_neg_nao_neg_pdo_pos_rf <- data.frame(e1_enso_neg_nao_neg_pdo_pos_risk_factor)
e1_enso_pos_nao_pos_pdo_pos_rf <- data.frame(e1_enso_pos_nao_pos_pdo_pos_risk_factor)

write.csv(e1_enso_pos_nao_pos_pdo_neg_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e1_enso_pos_nao_pos_pdo_neg_risk_factor.csv", row.names = FALSE)
write.csv(e1_enso_pos_nao_neg_pdo_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e1_enso_pos_nao_neg_pdo_pos_risk_factor.csv", row.names = FALSE)
write.csv(e1_enso_pos_nao_neg_pdo_neg_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e1_enso_pos_nao_neg_pdo_neg_risk_factor.csv", row.names = FALSE)
write.csv(e1_enso_neg_nao_pos_pdo_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e1_enso_neg_nao_pos_pdo_pos_risk_factor.csv", row.names = FALSE)
write.csv(e1_enso_neg_nao_pos_pdo_neg_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e1_enso_neg_nao_pos_pdo_neg_risk_factor.csv", row.names = FALSE)
write.csv(e1_enso_neg_nao_neg_pdo_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e1_enso_neg_nao_neg_pdo_pos_risk_factor.csv", row.names = FALSE)
write.csv(e1_enso_pos_nao_pos_pdo_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e1_enso_pos_nao_pos_pdo_pos_risk_factor.csv", row.names = FALSE)

for(i in 1:dim(elon)){
  
  for(j in 1:dim(elat)){
    
    data1<-e2_acpcp[i,j,]
    
    if (sum(is.na(data1)) >= 40){
      
      e2_enso_pos_nao_pos_pdo_neg_risk_factor[i,j] <- "NA"
      e2_enso_pos_nao_neg_pdo_pos_risk_factor[i,j] <- "NA"
      e2_enso_pos_nao_neg_pdo_neg_risk_factor[i,j] <- "NA"
      e2_enso_neg_nao_pos_pdo_pos_risk_factor[i,j] <- "NA"
      e2_enso_neg_nao_pos_pdo_neg_risk_factor[i,j] <- "NA"
      e2_enso_neg_nao_neg_pdo_pos_risk_factor[i,j] <- "NA"
      e2_enso_pos_nao_pos_pdo_pos_risk_factor[i,j] <- "NA"
      
    }
    else{
      
      df <- data.frame(data1,indices$year,indices$month,indices$soi,indices$naoi,indices$pdoi)
      colnames(df) <- c("acpcp","year","month","soi","naoi","pdoi")
      
      df <- na.omit(df)
      
      enso_nao_pdo_results <- extRemes::fevd(df$acpcp,data=df,location.fun=~soi + naoi + pdoi,scale.fun=~soi + naoi + pdoi)
      
      enso_nao_pdo_mu0 <- enso_nao_pdo_results$results$par[1]
      enso_nao_pdo_mu1 <- enso_nao_pdo_results$results$par[2]
      enso_nao_pdo_mu2 <- enso_nao_pdo_results$results$par[3]
      enso_nao_pdo_mu3 <- enso_nao_pdo_results$results$par[4]
      enso_nao_pdo_sigma0 <- enso_nao_pdo_results$results$par[5]
      enso_nao_pdo_sigma1 <- enso_nao_pdo_results$results$par[6]
      enso_nao_pdo_sigma2 <- enso_nao_pdo_results$results$par[7]
      enso_nao_pdo_sigma3 <- enso_nao_pdo_results$results$par[8]
      enso_nao_pdo_shape <- enso_nao_pdo_results$results$par[9]
      
      enso_neg_nao_neg_pdo_neg_rvalue <- extRemes::rlevd(df$acpcp,loc=enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (-1) + enso_nao_pdo_mu2 * (-1) + enso_nao_pdo_mu3 * (-1),scale=max(1e-100, enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (-1) + enso_nao_pdo_sigma2 * (-1) + enso_nao_pdo_sigma3 * (-1)),shape =enso_nao_pdo_shape,period=20)
      enso_pos_nao_pos_pdo_neg_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (1) + enso_nao_pdo_mu2 * (1) + enso_nao_pdo_mu3 * (-1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (1) + enso_nao_pdo_sigma2 * (1) + enso_nao_pdo_sigma3 * (-1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      enso_pos_nao_neg_pdo_pos_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (1) + enso_nao_pdo_mu2 * (-1) + enso_nao_pdo_mu3 * (1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (1) + enso_nao_pdo_sigma2 * (-1) + enso_nao_pdo_sigma3 * (1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      enso_pos_nao_neg_pdo_neg_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (1) + enso_nao_pdo_mu2 * (-1) + enso_nao_pdo_mu3 * (-1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (1) + enso_nao_pdo_sigma2 * (-1) + enso_nao_pdo_sigma3 * (-1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      enso_neg_nao_pos_pdo_pos_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (-1) + enso_nao_pdo_mu2 * (1) + enso_nao_pdo_mu3 * (1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (-1) + enso_nao_pdo_sigma2 * (1) + enso_nao_pdo_sigma3 * (1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      enso_neg_nao_pos_pdo_neg_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (-1) + enso_nao_pdo_mu2 * (1) + enso_nao_pdo_mu3 * (-1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (-1) + enso_nao_pdo_sigma2 * (1) + enso_nao_pdo_sigma3 * (-1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      enso_neg_nao_neg_pdo_pos_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (-1) + enso_nao_pdo_mu2 * (-1) + enso_nao_pdo_mu3 * (1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (-1) + enso_nao_pdo_sigma2 * (-1) + enso_nao_pdo_sigma3 * (1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      enso_pos_nao_pos_pdo_pos_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (1) + enso_nao_pdo_mu2 * (1) + enso_nao_pdo_mu3 * (1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (1) + enso_nao_pdo_sigma2 * (1) + enso_nao_pdo_sigma3 * (1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      
      e2_enso_pos_nao_pos_pdo_neg_risk_factor[i,j] <- enso_pos_nao_pos_pdo_neg_prob/0.05
      e2_enso_pos_nao_neg_pdo_pos_risk_factor[i,j] <- enso_pos_nao_neg_pdo_pos_prob/0.05
      e2_enso_pos_nao_neg_pdo_neg_risk_factor[i,j] <- enso_pos_nao_neg_pdo_neg_prob/0.05
      e2_enso_neg_nao_pos_pdo_pos_risk_factor[i,j] <- enso_neg_nao_pos_pdo_pos_prob/0.05
      e2_enso_neg_nao_pos_pdo_neg_risk_factor[i,j] <- enso_neg_nao_pos_pdo_neg_prob/0.05
      e2_enso_neg_nao_neg_pdo_pos_risk_factor[i,j] <- enso_neg_nao_neg_pdo_pos_prob/0.05
      e2_enso_pos_nao_pos_pdo_pos_risk_factor[i,j] <- enso_pos_nao_pos_pdo_pos_prob/0.05
      
    }
  }
}
indices


e2_enso_pos_nao_pos_pdo_neg_rf <- data.frame(e2_enso_pos_nao_pos_pdo_neg_risk_factor)
e2_enso_pos_nao_neg_pdo_pos_rf <- data.frame(e2_enso_pos_nao_neg_pdo_pos_risk_factor)
e2_enso_pos_nao_neg_pdo_neg_rf <- data.frame(e2_enso_pos_nao_neg_pdo_neg_risk_factor)
e2_enso_neg_nao_pos_pdo_pos_rf <- data.frame(e2_enso_neg_nao_pos_pdo_pos_risk_factor)
e2_enso_neg_nao_pos_pdo_neg_rf <- data.frame(e2_enso_neg_nao_pos_pdo_neg_risk_factor)
e2_enso_neg_nao_neg_pdo_pos_rf <- data.frame(e2_enso_neg_nao_neg_pdo_pos_risk_factor)
e2_enso_pos_nao_pos_pdo_pos_rf <- data.frame(e2_enso_pos_nao_pos_pdo_pos_risk_factor)

write.csv(e2_enso_pos_nao_pos_pdo_neg_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e2_enso_pos_nao_pos_pdo_neg_risk_factor.csv", row.names = FALSE)
write.csv(e2_enso_pos_nao_neg_pdo_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e2_enso_pos_nao_neg_pdo_pos_risk_factor.csv", row.names = FALSE)
write.csv(e2_enso_pos_nao_neg_pdo_neg_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e2_enso_pos_nao_neg_pdo_neg_risk_factor.csv", row.names = FALSE)
write.csv(e2_enso_neg_nao_pos_pdo_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e2_enso_neg_nao_pos_pdo_pos_risk_factor.csv", row.names = FALSE)
write.csv(e2_enso_neg_nao_pos_pdo_neg_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e2_enso_neg_nao_pos_pdo_neg_risk_factor.csv", row.names = FALSE)
write.csv(e2_enso_neg_nao_neg_pdo_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e2_enso_neg_nao_neg_pdo_pos_risk_factor.csv", row.names = FALSE)
write.csv(e2_enso_pos_nao_pos_pdo_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e2_enso_pos_nao_pos_pdo_pos_risk_factor.csv", row.names = FALSE)

for(i in 1:dim(elon)){
  
  for(j in 1:dim(elat)){
    
    data1<-e3_acpcp[i,j,]
    
    if (sum(is.na(data1)) >= 40){
      
      e3_enso_pos_nao_pos_pdo_neg_risk_factor[i,j] <- "NA"
      e3_enso_pos_nao_neg_pdo_pos_risk_factor[i,j] <- "NA"
      e3_enso_pos_nao_neg_pdo_neg_risk_factor[i,j] <- "NA"
      e3_enso_neg_nao_pos_pdo_pos_risk_factor[i,j] <- "NA"
      e3_enso_neg_nao_pos_pdo_neg_risk_factor[i,j] <- "NA"
      e3_enso_neg_nao_neg_pdo_pos_risk_factor[i,j] <- "NA"
      e3_enso_pos_nao_pos_pdo_pos_risk_factor[i,j] <- "NA"
      
    }
    else{
      
      df <- data.frame(data1,indices$year,indices$month,indices$soi,indices$naoi,indices$pdoi)
      colnames(df) <- c("acpcp","year","month","soi","naoi","pdoi")
      
      df <- na.omit(df)
      
      enso_nao_pdo_results <- extRemes::fevd(df$acpcp,data=df,location.fun=~soi + naoi + pdoi,scale.fun=~soi + naoi + pdoi)
      
      enso_nao_pdo_mu0 <- enso_nao_pdo_results$results$par[1]
      enso_nao_pdo_mu1 <- enso_nao_pdo_results$results$par[2]
      enso_nao_pdo_mu2 <- enso_nao_pdo_results$results$par[3]
      enso_nao_pdo_mu3 <- enso_nao_pdo_results$results$par[4]
      enso_nao_pdo_sigma0 <- enso_nao_pdo_results$results$par[5]
      enso_nao_pdo_sigma1 <- enso_nao_pdo_results$results$par[6]
      enso_nao_pdo_sigma2 <- enso_nao_pdo_results$results$par[7]
      enso_nao_pdo_sigma3 <- enso_nao_pdo_results$results$par[8]
      enso_nao_pdo_shape <- enso_nao_pdo_results$results$par[9]
      
      enso_neg_nao_neg_pdo_neg_rvalue <- extRemes::rlevd(df$acpcp,loc=enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (-1) + enso_nao_pdo_mu2 * (-1) + enso_nao_pdo_mu3 * (-1),scale=max(1e-100, enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (-1) + enso_nao_pdo_sigma2 * (-1) + enso_nao_pdo_sigma3 * (-1)),shape =enso_nao_pdo_shape,period=20)
      enso_pos_nao_pos_pdo_neg_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (1) + enso_nao_pdo_mu2 * (1) + enso_nao_pdo_mu3 * (-1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (1) + enso_nao_pdo_sigma2 * (1) + enso_nao_pdo_sigma3 * (-1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      enso_pos_nao_neg_pdo_pos_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (1) + enso_nao_pdo_mu2 * (-1) + enso_nao_pdo_mu3 * (1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (1) + enso_nao_pdo_sigma2 * (-1) + enso_nao_pdo_sigma3 * (1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      enso_pos_nao_neg_pdo_neg_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (1) + enso_nao_pdo_mu2 * (-1) + enso_nao_pdo_mu3 * (-1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (1) + enso_nao_pdo_sigma2 * (-1) + enso_nao_pdo_sigma3 * (-1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      enso_neg_nao_pos_pdo_pos_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (-1) + enso_nao_pdo_mu2 * (1) + enso_nao_pdo_mu3 * (1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (-1) + enso_nao_pdo_sigma2 * (1) + enso_nao_pdo_sigma3 * (1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      enso_neg_nao_pos_pdo_neg_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (-1) + enso_nao_pdo_mu2 * (1) + enso_nao_pdo_mu3 * (-1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (-1) + enso_nao_pdo_sigma2 * (1) + enso_nao_pdo_sigma3 * (-1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      enso_neg_nao_neg_pdo_pos_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (-1) + enso_nao_pdo_mu2 * (-1) + enso_nao_pdo_mu3 * (1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (-1) + enso_nao_pdo_sigma2 * (-1) + enso_nao_pdo_sigma3 * (1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      enso_pos_nao_pos_pdo_pos_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (1) + enso_nao_pdo_mu2 * (1) + enso_nao_pdo_mu3 * (1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (1) + enso_nao_pdo_sigma2 * (1) + enso_nao_pdo_sigma3 * (1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      
      e3_enso_pos_nao_pos_pdo_neg_risk_factor[i,j] <- enso_pos_nao_pos_pdo_neg_prob/0.05
      e3_enso_pos_nao_neg_pdo_pos_risk_factor[i,j] <- enso_pos_nao_neg_pdo_pos_prob/0.05
      e3_enso_pos_nao_neg_pdo_neg_risk_factor[i,j] <- enso_pos_nao_neg_pdo_neg_prob/0.05
      e3_enso_neg_nao_pos_pdo_pos_risk_factor[i,j] <- enso_neg_nao_pos_pdo_pos_prob/0.05
      e3_enso_neg_nao_pos_pdo_neg_risk_factor[i,j] <- enso_neg_nao_pos_pdo_neg_prob/0.05
      e3_enso_neg_nao_neg_pdo_pos_risk_factor[i,j] <- enso_neg_nao_neg_pdo_pos_prob/0.05
      e3_enso_pos_nao_pos_pdo_pos_risk_factor[i,j] <- enso_pos_nao_pos_pdo_pos_prob/0.05
      
    }
  }
}
indices


e3_enso_pos_nao_pos_pdo_neg_rf <- data.frame(e3_enso_pos_nao_pos_pdo_neg_risk_factor)
e3_enso_pos_nao_neg_pdo_pos_rf <- data.frame(e3_enso_pos_nao_neg_pdo_pos_risk_factor)
e3_enso_pos_nao_neg_pdo_neg_rf <- data.frame(e3_enso_pos_nao_neg_pdo_neg_risk_factor)
e3_enso_neg_nao_pos_pdo_pos_rf <- data.frame(e3_enso_neg_nao_pos_pdo_pos_risk_factor)
e3_enso_neg_nao_pos_pdo_neg_rf <- data.frame(e3_enso_neg_nao_pos_pdo_neg_risk_factor)
e3_enso_neg_nao_neg_pdo_pos_rf <- data.frame(e3_enso_neg_nao_neg_pdo_pos_risk_factor)
e3_enso_pos_nao_pos_pdo_pos_rf <- data.frame(e3_enso_pos_nao_pos_pdo_pos_risk_factor)

write.csv(e3_enso_pos_nao_pos_pdo_neg_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e3_enso_pos_nao_pos_pdo_neg_risk_factor.csv", row.names = FALSE)
write.csv(e3_enso_pos_nao_neg_pdo_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e3_enso_pos_nao_neg_pdo_pos_risk_factor.csv", row.names = FALSE)
write.csv(e3_enso_pos_nao_neg_pdo_neg_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e3_enso_pos_nao_neg_pdo_neg_risk_factor.csv", row.names = FALSE)
write.csv(e3_enso_neg_nao_pos_pdo_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e3_enso_neg_nao_pos_pdo_pos_risk_factor.csv", row.names = FALSE)
write.csv(e3_enso_neg_nao_pos_pdo_neg_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e3_enso_neg_nao_pos_pdo_neg_risk_factor.csv", row.names = FALSE)
write.csv(e3_enso_neg_nao_neg_pdo_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e3_enso_neg_nao_neg_pdo_pos_risk_factor.csv", row.names = FALSE)
write.csv(e3_enso_pos_nao_pos_pdo_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e3_enso_pos_nao_pos_pdo_pos_risk_factor.csv", row.names = FALSE)

for(i in 1:dim(elon)){
  
  for(j in 1:dim(elat)){
    
    data1<-e4_acpcp[i,j,]
    
    if (sum(is.na(data1)) >= 40){
      
      e4_enso_pos_nao_pos_pdo_neg_risk_factor[i,j] <- "NA"
      e4_enso_pos_nao_neg_pdo_pos_risk_factor[i,j] <- "NA"
      e4_enso_pos_nao_neg_pdo_neg_risk_factor[i,j] <- "NA"
      e4_enso_neg_nao_pos_pdo_pos_risk_factor[i,j] <- "NA"
      e4_enso_neg_nao_pos_pdo_neg_risk_factor[i,j] <- "NA"
      e4_enso_neg_nao_neg_pdo_pos_risk_factor[i,j] <- "NA"
      e4_enso_pos_nao_pos_pdo_pos_risk_factor[i,j] <- "NA"
      
    }
    else{
      
      df <- data.frame(data1,indices$year,indices$month,indices$soi,indices$naoi,indices$pdoi)
      colnames(df) <- c("acpcp","year","month","soi","naoi","pdoi")
      
      df <- na.omit(df)
      
      enso_nao_pdo_results <- extRemes::fevd(df$acpcp,data=df,location.fun=~soi + naoi + pdoi,scale.fun=~soi + naoi + pdoi)
      
      enso_nao_pdo_mu0 <- enso_nao_pdo_results$results$par[1]
      enso_nao_pdo_mu1 <- enso_nao_pdo_results$results$par[2]
      enso_nao_pdo_mu2 <- enso_nao_pdo_results$results$par[3]
      enso_nao_pdo_mu3 <- enso_nao_pdo_results$results$par[4]
      enso_nao_pdo_sigma0 <- enso_nao_pdo_results$results$par[5]
      enso_nao_pdo_sigma1 <- enso_nao_pdo_results$results$par[6]
      enso_nao_pdo_sigma2 <- enso_nao_pdo_results$results$par[7]
      enso_nao_pdo_sigma3 <- enso_nao_pdo_results$results$par[8]
      enso_nao_pdo_shape <- enso_nao_pdo_results$results$par[9]
      
      enso_neg_nao_neg_pdo_neg_rvalue <- extRemes::rlevd(df$acpcp,loc=enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (-1) + enso_nao_pdo_mu2 * (-1) + enso_nao_pdo_mu3 * (-1),scale=max(1e-100, enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (-1) + enso_nao_pdo_sigma2 * (-1) + enso_nao_pdo_sigma3 * (-1)),shape =enso_nao_pdo_shape,period=20)
      enso_pos_nao_pos_pdo_neg_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (1) + enso_nao_pdo_mu2 * (1) + enso_nao_pdo_mu3 * (-1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (1) + enso_nao_pdo_sigma2 * (1) + enso_nao_pdo_sigma3 * (-1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      enso_pos_nao_neg_pdo_pos_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (1) + enso_nao_pdo_mu2 * (-1) + enso_nao_pdo_mu3 * (1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (1) + enso_nao_pdo_sigma2 * (-1) + enso_nao_pdo_sigma3 * (1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      enso_pos_nao_neg_pdo_neg_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (1) + enso_nao_pdo_mu2 * (-1) + enso_nao_pdo_mu3 * (-1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (1) + enso_nao_pdo_sigma2 * (-1) + enso_nao_pdo_sigma3 * (-1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      enso_neg_nao_pos_pdo_pos_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (-1) + enso_nao_pdo_mu2 * (1) + enso_nao_pdo_mu3 * (1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (-1) + enso_nao_pdo_sigma2 * (1) + enso_nao_pdo_sigma3 * (1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      enso_neg_nao_pos_pdo_neg_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (-1) + enso_nao_pdo_mu2 * (1) + enso_nao_pdo_mu3 * (-1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (-1) + enso_nao_pdo_sigma2 * (1) + enso_nao_pdo_sigma3 * (-1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      enso_neg_nao_neg_pdo_pos_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (-1) + enso_nao_pdo_mu2 * (-1) + enso_nao_pdo_mu3 * (1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (-1) + enso_nao_pdo_sigma2 * (-1) + enso_nao_pdo_sigma3 * (1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      enso_pos_nao_pos_pdo_pos_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (1) + enso_nao_pdo_mu2 * (1) + enso_nao_pdo_mu3 * (1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (1) + enso_nao_pdo_sigma2 * (1) + enso_nao_pdo_sigma3 * (1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      
      e4_enso_pos_nao_pos_pdo_neg_risk_factor[i,j] <- enso_pos_nao_pos_pdo_neg_prob/0.05
      e4_enso_pos_nao_neg_pdo_pos_risk_factor[i,j] <- enso_pos_nao_neg_pdo_pos_prob/0.05
      e4_enso_pos_nao_neg_pdo_neg_risk_factor[i,j] <- enso_pos_nao_neg_pdo_neg_prob/0.05
      e4_enso_neg_nao_pos_pdo_pos_risk_factor[i,j] <- enso_neg_nao_pos_pdo_pos_prob/0.05
      e4_enso_neg_nao_pos_pdo_neg_risk_factor[i,j] <- enso_neg_nao_pos_pdo_neg_prob/0.05
      e4_enso_neg_nao_neg_pdo_pos_risk_factor[i,j] <- enso_neg_nao_neg_pdo_pos_prob/0.05
      e4_enso_pos_nao_pos_pdo_pos_risk_factor[i,j] <- enso_pos_nao_pos_pdo_pos_prob/0.05
      
    }
  }
}
indices


e4_enso_pos_nao_pos_pdo_neg_rf <- data.frame(e4_enso_pos_nao_pos_pdo_neg_risk_factor)
e4_enso_pos_nao_neg_pdo_pos_rf <- data.frame(e4_enso_pos_nao_neg_pdo_pos_risk_factor)
e4_enso_pos_nao_neg_pdo_neg_rf <- data.frame(e4_enso_pos_nao_neg_pdo_neg_risk_factor)
e4_enso_neg_nao_pos_pdo_pos_rf <- data.frame(e4_enso_neg_nao_pos_pdo_pos_risk_factor)
e4_enso_neg_nao_pos_pdo_neg_rf <- data.frame(e4_enso_neg_nao_pos_pdo_neg_risk_factor)
e4_enso_neg_nao_neg_pdo_pos_rf <- data.frame(e4_enso_neg_nao_neg_pdo_pos_risk_factor)
e4_enso_pos_nao_pos_pdo_pos_rf <- data.frame(e4_enso_pos_nao_pos_pdo_pos_risk_factor)

write.csv(e4_enso_pos_nao_pos_pdo_neg_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e4_enso_pos_nao_pos_pdo_neg_risk_factor.csv", row.names = FALSE)
write.csv(e4_enso_pos_nao_neg_pdo_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e4_enso_pos_nao_neg_pdo_pos_risk_factor.csv", row.names = FALSE)
write.csv(e4_enso_pos_nao_neg_pdo_neg_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e4_enso_pos_nao_neg_pdo_neg_risk_factor.csv", row.names = FALSE)
write.csv(e4_enso_neg_nao_pos_pdo_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e4_enso_neg_nao_pos_pdo_pos_risk_factor.csv", row.names = FALSE)
write.csv(e4_enso_neg_nao_pos_pdo_neg_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e4_enso_neg_nao_pos_pdo_neg_risk_factor.csv", row.names = FALSE)
write.csv(e4_enso_neg_nao_neg_pdo_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e4_enso_neg_nao_neg_pdo_pos_risk_factor.csv", row.names = FALSE)
write.csv(e4_enso_pos_nao_pos_pdo_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e4_enso_pos_nao_pos_pdo_pos_risk_factor.csv", row.names = FALSE)

for(i in 1:dim(elon)){
  
  for(j in 1:dim(elat)){
    
    data1<-e5_acpcp[i,j,]
    
    if (sum(is.na(data1)) >= 40){
      
      e5_enso_pos_nao_pos_pdo_neg_risk_factor[i,j] <- "NA"
      e5_enso_pos_nao_neg_pdo_pos_risk_factor[i,j] <- "NA"
      e5_enso_pos_nao_neg_pdo_neg_risk_factor[i,j] <- "NA"
      e5_enso_neg_nao_pos_pdo_pos_risk_factor[i,j] <- "NA"
      e5_enso_neg_nao_pos_pdo_neg_risk_factor[i,j] <- "NA"
      e5_enso_neg_nao_neg_pdo_pos_risk_factor[i,j] <- "NA"
      e5_enso_pos_nao_pos_pdo_pos_risk_factor[i,j] <- "NA"
      
    }
    else{
      
      df <- data.frame(data1,indices$year,indices$month,indices$soi,indices$naoi,indices$pdoi)
      colnames(df) <- c("acpcp","year","month","soi","naoi","pdoi")
      
      df <- na.omit(df)
      
      enso_nao_pdo_results <- extRemes::fevd(df$acpcp,data=df,location.fun=~soi + naoi + pdoi,scale.fun=~soi + naoi + pdoi)
      
      enso_nao_pdo_mu0 <- enso_nao_pdo_results$results$par[1]
      enso_nao_pdo_mu1 <- enso_nao_pdo_results$results$par[2]
      enso_nao_pdo_mu2 <- enso_nao_pdo_results$results$par[3]
      enso_nao_pdo_mu3 <- enso_nao_pdo_results$results$par[4]
      enso_nao_pdo_sigma0 <- enso_nao_pdo_results$results$par[5]
      enso_nao_pdo_sigma1 <- enso_nao_pdo_results$results$par[6]
      enso_nao_pdo_sigma2 <- enso_nao_pdo_results$results$par[7]
      enso_nao_pdo_sigma3 <- enso_nao_pdo_results$results$par[8]
      enso_nao_pdo_shape <- enso_nao_pdo_results$results$par[9]
      
      enso_neg_nao_neg_pdo_neg_rvalue <- extRemes::rlevd(df$acpcp,loc=enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (-1) + enso_nao_pdo_mu2 * (-1) + enso_nao_pdo_mu3 * (-1),scale=max(1e-100, enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (-1) + enso_nao_pdo_sigma2 * (-1) + enso_nao_pdo_sigma3 * (-1)),shape =enso_nao_pdo_shape,period=20)
      enso_pos_nao_pos_pdo_neg_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (1) + enso_nao_pdo_mu2 * (1) + enso_nao_pdo_mu3 * (-1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (1) + enso_nao_pdo_sigma2 * (1) + enso_nao_pdo_sigma3 * (-1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      enso_pos_nao_neg_pdo_pos_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (1) + enso_nao_pdo_mu2 * (-1) + enso_nao_pdo_mu3 * (1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (1) + enso_nao_pdo_sigma2 * (-1) + enso_nao_pdo_sigma3 * (1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      enso_pos_nao_neg_pdo_neg_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (1) + enso_nao_pdo_mu2 * (-1) + enso_nao_pdo_mu3 * (-1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (1) + enso_nao_pdo_sigma2 * (-1) + enso_nao_pdo_sigma3 * (-1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      enso_neg_nao_pos_pdo_pos_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (-1) + enso_nao_pdo_mu2 * (1) + enso_nao_pdo_mu3 * (1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (-1) + enso_nao_pdo_sigma2 * (1) + enso_nao_pdo_sigma3 * (1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      enso_neg_nao_pos_pdo_neg_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (-1) + enso_nao_pdo_mu2 * (1) + enso_nao_pdo_mu3 * (-1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (-1) + enso_nao_pdo_sigma2 * (1) + enso_nao_pdo_sigma3 * (-1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      enso_neg_nao_neg_pdo_pos_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (-1) + enso_nao_pdo_mu2 * (-1) + enso_nao_pdo_mu3 * (1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (-1) + enso_nao_pdo_sigma2 * (-1) + enso_nao_pdo_sigma3 * (1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      enso_pos_nao_pos_pdo_pos_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (1) + enso_nao_pdo_mu2 * (1) + enso_nao_pdo_mu3 * (1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (1) + enso_nao_pdo_sigma2 * (1) + enso_nao_pdo_sigma3 * (1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      
      e5_enso_pos_nao_pos_pdo_neg_risk_factor[i,j] <- enso_pos_nao_pos_pdo_neg_prob/0.05
      e5_enso_pos_nao_neg_pdo_pos_risk_factor[i,j] <- enso_pos_nao_neg_pdo_pos_prob/0.05
      e5_enso_pos_nao_neg_pdo_neg_risk_factor[i,j] <- enso_pos_nao_neg_pdo_neg_prob/0.05
      e5_enso_neg_nao_pos_pdo_pos_risk_factor[i,j] <- enso_neg_nao_pos_pdo_pos_prob/0.05
      e5_enso_neg_nao_pos_pdo_neg_risk_factor[i,j] <- enso_neg_nao_pos_pdo_neg_prob/0.05
      e5_enso_neg_nao_neg_pdo_pos_risk_factor[i,j] <- enso_neg_nao_neg_pdo_pos_prob/0.05
      e5_enso_pos_nao_pos_pdo_pos_risk_factor[i,j] <- enso_pos_nao_pos_pdo_pos_prob/0.05
      
    }
  }
}
indices


e5_enso_pos_nao_pos_pdo_neg_rf <- data.frame(e5_enso_pos_nao_pos_pdo_neg_risk_factor)
e5_enso_pos_nao_neg_pdo_pos_rf <- data.frame(e5_enso_pos_nao_neg_pdo_pos_risk_factor)
e5_enso_pos_nao_neg_pdo_neg_rf <- data.frame(e5_enso_pos_nao_neg_pdo_neg_risk_factor)
e5_enso_neg_nao_pos_pdo_pos_rf <- data.frame(e5_enso_neg_nao_pos_pdo_pos_risk_factor)
e5_enso_neg_nao_pos_pdo_neg_rf <- data.frame(e5_enso_neg_nao_pos_pdo_neg_risk_factor)
e5_enso_neg_nao_neg_pdo_pos_rf <- data.frame(e5_enso_neg_nao_neg_pdo_pos_risk_factor)
e5_enso_pos_nao_pos_pdo_pos_rf <- data.frame(e5_enso_pos_nao_pos_pdo_pos_risk_factor)

write.csv(e5_enso_pos_nao_pos_pdo_neg_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e5_enso_pos_nao_pos_pdo_neg_risk_factor.csv", row.names = FALSE)
write.csv(e5_enso_pos_nao_neg_pdo_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e5_enso_pos_nao_neg_pdo_pos_risk_factor.csv", row.names = FALSE)
write.csv(e5_enso_pos_nao_neg_pdo_neg_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e5_enso_pos_nao_neg_pdo_neg_risk_factor.csv", row.names = FALSE)
write.csv(e5_enso_neg_nao_pos_pdo_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e5_enso_neg_nao_pos_pdo_pos_risk_factor.csv", row.names = FALSE)
write.csv(e5_enso_neg_nao_pos_pdo_neg_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e5_enso_neg_nao_pos_pdo_neg_risk_factor.csv", row.names = FALSE)
write.csv(e5_enso_neg_nao_neg_pdo_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e5_enso_neg_nao_neg_pdo_pos_risk_factor.csv", row.names = FALSE)
write.csv(e5_enso_pos_nao_pos_pdo_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e5_enso_pos_nao_pos_pdo_pos_risk_factor.csv", row.names = FALSE)

for(i in 1:dim(elon)){
  
  for(j in 1:dim(elat)){
    
    data1<-e6_acpcp[i,j,]
    
    if (sum(is.na(data1)) >= 40){
      
      e6_enso_pos_nao_pos_pdo_neg_risk_factor[i,j] <- "NA"
      e6_enso_pos_nao_neg_pdo_pos_risk_factor[i,j] <- "NA"
      e6_enso_pos_nao_neg_pdo_neg_risk_factor[i,j] <- "NA"
      e6_enso_neg_nao_pos_pdo_pos_risk_factor[i,j] <- "NA"
      e6_enso_neg_nao_pos_pdo_neg_risk_factor[i,j] <- "NA"
      e6_enso_neg_nao_neg_pdo_pos_risk_factor[i,j] <- "NA"
      e6_enso_pos_nao_pos_pdo_pos_risk_factor[i,j] <- "NA"
      
    }
    else{
      
      df <- data.frame(data1,indices$year,indices$month,indices$soi,indices$naoi,indices$pdoi)
      colnames(df) <- c("acpcp","year","month","soi","naoi","pdoi")
      
      df <- na.omit(df)
      
      enso_nao_pdo_results <- extRemes::fevd(df$acpcp,data=df,location.fun=~soi + naoi + pdoi,scale.fun=~soi + naoi + pdoi)
      
      enso_nao_pdo_mu0 <- enso_nao_pdo_results$results$par[1]
      enso_nao_pdo_mu1 <- enso_nao_pdo_results$results$par[2]
      enso_nao_pdo_mu2 <- enso_nao_pdo_results$results$par[3]
      enso_nao_pdo_mu3 <- enso_nao_pdo_results$results$par[4]
      enso_nao_pdo_sigma0 <- enso_nao_pdo_results$results$par[5]
      enso_nao_pdo_sigma1 <- enso_nao_pdo_results$results$par[6]
      enso_nao_pdo_sigma2 <- enso_nao_pdo_results$results$par[7]
      enso_nao_pdo_sigma3 <- enso_nao_pdo_results$results$par[8]
      enso_nao_pdo_shape <- enso_nao_pdo_results$results$par[9]
      
      enso_neg_nao_neg_pdo_neg_rvalue <- extRemes::rlevd(df$acpcp,loc=enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (-1) + enso_nao_pdo_mu2 * (-1) + enso_nao_pdo_mu3 * (-1),scale=max(1e-100, enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (-1) + enso_nao_pdo_sigma2 * (-1) + enso_nao_pdo_sigma3 * (-1)),shape =enso_nao_pdo_shape,period=20)
      enso_pos_nao_pos_pdo_neg_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (1) + enso_nao_pdo_mu2 * (1) + enso_nao_pdo_mu3 * (-1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (1) + enso_nao_pdo_sigma2 * (1) + enso_nao_pdo_sigma3 * (-1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      enso_pos_nao_neg_pdo_pos_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (1) + enso_nao_pdo_mu2 * (-1) + enso_nao_pdo_mu3 * (1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (1) + enso_nao_pdo_sigma2 * (-1) + enso_nao_pdo_sigma3 * (1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      enso_pos_nao_neg_pdo_neg_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (1) + enso_nao_pdo_mu2 * (-1) + enso_nao_pdo_mu3 * (-1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (1) + enso_nao_pdo_sigma2 * (-1) + enso_nao_pdo_sigma3 * (-1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      enso_neg_nao_pos_pdo_pos_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (-1) + enso_nao_pdo_mu2 * (1) + enso_nao_pdo_mu3 * (1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (-1) + enso_nao_pdo_sigma2 * (1) + enso_nao_pdo_sigma3 * (1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      enso_neg_nao_pos_pdo_neg_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (-1) + enso_nao_pdo_mu2 * (1) + enso_nao_pdo_mu3 * (-1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (-1) + enso_nao_pdo_sigma2 * (1) + enso_nao_pdo_sigma3 * (-1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      enso_neg_nao_neg_pdo_pos_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (-1) + enso_nao_pdo_mu2 * (-1) + enso_nao_pdo_mu3 * (1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (-1) + enso_nao_pdo_sigma2 * (-1) + enso_nao_pdo_sigma3 * (1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      enso_pos_nao_pos_pdo_pos_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (1) + enso_nao_pdo_mu2 * (1) + enso_nao_pdo_mu3 * (1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (1) + enso_nao_pdo_sigma2 * (1) + enso_nao_pdo_sigma3 * (1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      
      e6_enso_pos_nao_pos_pdo_neg_risk_factor[i,j] <- enso_pos_nao_pos_pdo_neg_prob/0.05
      e6_enso_pos_nao_neg_pdo_pos_risk_factor[i,j] <- enso_pos_nao_neg_pdo_pos_prob/0.05
      e6_enso_pos_nao_neg_pdo_neg_risk_factor[i,j] <- enso_pos_nao_neg_pdo_neg_prob/0.05
      e6_enso_neg_nao_pos_pdo_pos_risk_factor[i,j] <- enso_neg_nao_pos_pdo_pos_prob/0.05
      e6_enso_neg_nao_pos_pdo_neg_risk_factor[i,j] <- enso_neg_nao_pos_pdo_neg_prob/0.05
      e6_enso_neg_nao_neg_pdo_pos_risk_factor[i,j] <- enso_neg_nao_neg_pdo_pos_prob/0.05
      e6_enso_pos_nao_pos_pdo_pos_risk_factor[i,j] <- enso_pos_nao_pos_pdo_pos_prob/0.05
      
    }
  }
}
indices


e6_enso_pos_nao_pos_pdo_neg_rf <- data.frame(e6_enso_pos_nao_pos_pdo_neg_risk_factor)
e6_enso_pos_nao_neg_pdo_pos_rf <- data.frame(e6_enso_pos_nao_neg_pdo_pos_risk_factor)
e6_enso_pos_nao_neg_pdo_neg_rf <- data.frame(e6_enso_pos_nao_neg_pdo_neg_risk_factor)
e6_enso_neg_nao_pos_pdo_pos_rf <- data.frame(e6_enso_neg_nao_pos_pdo_pos_risk_factor)
e6_enso_neg_nao_pos_pdo_neg_rf <- data.frame(e6_enso_neg_nao_pos_pdo_neg_risk_factor)
e6_enso_neg_nao_neg_pdo_pos_rf <- data.frame(e6_enso_neg_nao_neg_pdo_pos_risk_factor)
e6_enso_pos_nao_pos_pdo_pos_rf <- data.frame(e6_enso_pos_nao_pos_pdo_pos_risk_factor)

write.csv(e6_enso_pos_nao_pos_pdo_neg_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e6_enso_pos_nao_pos_pdo_neg_risk_factor.csv", row.names = FALSE)
write.csv(e6_enso_pos_nao_neg_pdo_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e6_enso_pos_nao_neg_pdo_pos_risk_factor.csv", row.names = FALSE)
write.csv(e6_enso_pos_nao_neg_pdo_neg_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e6_enso_pos_nao_neg_pdo_neg_risk_factor.csv", row.names = FALSE)
write.csv(e6_enso_neg_nao_pos_pdo_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e6_enso_neg_nao_pos_pdo_pos_risk_factor.csv", row.names = FALSE)
write.csv(e6_enso_neg_nao_pos_pdo_neg_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e6_enso_neg_nao_pos_pdo_neg_risk_factor.csv", row.names = FALSE)
write.csv(e6_enso_neg_nao_neg_pdo_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e6_enso_neg_nao_neg_pdo_pos_risk_factor.csv", row.names = FALSE)
write.csv(e6_enso_pos_nao_pos_pdo_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e6_enso_pos_nao_pos_pdo_pos_risk_factor.csv", row.names = FALSE)

for(i in 1:dim(elon)){
  
  for(j in 1:dim(elat)){
    
    data1<-e7_acpcp[i,j,]
    
    if (sum(is.na(data1)) >= 40){
      
      e7_enso_pos_nao_pos_pdo_neg_risk_factor[i,j] <- "NA"
      e7_enso_pos_nao_neg_pdo_pos_risk_factor[i,j] <- "NA"
      e7_enso_pos_nao_neg_pdo_neg_risk_factor[i,j] <- "NA"
      e7_enso_neg_nao_pos_pdo_pos_risk_factor[i,j] <- "NA"
      e7_enso_neg_nao_pos_pdo_neg_risk_factor[i,j] <- "NA"
      e7_enso_neg_nao_neg_pdo_pos_risk_factor[i,j] <- "NA"
      e7_enso_pos_nao_pos_pdo_pos_risk_factor[i,j] <- "NA"
      
    }
    else{
      
      df <- data.frame(data1,indices$year,indices$month,indices$soi,indices$naoi,indices$pdoi)
      colnames(df) <- c("acpcp","year","month","soi","naoi","pdoi")
      
      df <- na.omit(df)
      
      enso_nao_pdo_results <- extRemes::fevd(df$acpcp,data=df,location.fun=~soi + naoi + pdoi,scale.fun=~soi + naoi + pdoi)
      
      enso_nao_pdo_mu0 <- enso_nao_pdo_results$results$par[1]
      enso_nao_pdo_mu1 <- enso_nao_pdo_results$results$par[2]
      enso_nao_pdo_mu2 <- enso_nao_pdo_results$results$par[3]
      enso_nao_pdo_mu3 <- enso_nao_pdo_results$results$par[4]
      enso_nao_pdo_sigma0 <- enso_nao_pdo_results$results$par[5]
      enso_nao_pdo_sigma1 <- enso_nao_pdo_results$results$par[6]
      enso_nao_pdo_sigma2 <- enso_nao_pdo_results$results$par[7]
      enso_nao_pdo_sigma3 <- enso_nao_pdo_results$results$par[8]
      enso_nao_pdo_shape <- enso_nao_pdo_results$results$par[9]
      
      enso_neg_nao_neg_pdo_neg_rvalue <- extRemes::rlevd(df$acpcp,loc=enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (-1) + enso_nao_pdo_mu2 * (-1) + enso_nao_pdo_mu3 * (-1),scale=max(1e-100, enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (-1) + enso_nao_pdo_sigma2 * (-1) + enso_nao_pdo_sigma3 * (-1)),shape =enso_nao_pdo_shape,period=20)
      enso_pos_nao_pos_pdo_neg_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (1) + enso_nao_pdo_mu2 * (1) + enso_nao_pdo_mu3 * (-1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (1) + enso_nao_pdo_sigma2 * (1) + enso_nao_pdo_sigma3 * (-1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      enso_pos_nao_neg_pdo_pos_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (1) + enso_nao_pdo_mu2 * (-1) + enso_nao_pdo_mu3 * (1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (1) + enso_nao_pdo_sigma2 * (-1) + enso_nao_pdo_sigma3 * (1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      enso_pos_nao_neg_pdo_neg_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (1) + enso_nao_pdo_mu2 * (-1) + enso_nao_pdo_mu3 * (-1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (1) + enso_nao_pdo_sigma2 * (-1) + enso_nao_pdo_sigma3 * (-1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      enso_neg_nao_pos_pdo_pos_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (-1) + enso_nao_pdo_mu2 * (1) + enso_nao_pdo_mu3 * (1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (-1) + enso_nao_pdo_sigma2 * (1) + enso_nao_pdo_sigma3 * (1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      enso_neg_nao_pos_pdo_neg_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (-1) + enso_nao_pdo_mu2 * (1) + enso_nao_pdo_mu3 * (-1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (-1) + enso_nao_pdo_sigma2 * (1) + enso_nao_pdo_sigma3 * (-1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      enso_neg_nao_neg_pdo_pos_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (-1) + enso_nao_pdo_mu2 * (-1) + enso_nao_pdo_mu3 * (1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (-1) + enso_nao_pdo_sigma2 * (-1) + enso_nao_pdo_sigma3 * (1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      enso_pos_nao_pos_pdo_pos_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (1) + enso_nao_pdo_mu2 * (1) + enso_nao_pdo_mu3 * (1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (1) + enso_nao_pdo_sigma2 * (1) + enso_nao_pdo_sigma3 * (1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      
      e7_enso_pos_nao_pos_pdo_neg_risk_factor[i,j] <- enso_pos_nao_pos_pdo_neg_prob/0.05
      e7_enso_pos_nao_neg_pdo_pos_risk_factor[i,j] <- enso_pos_nao_neg_pdo_pos_prob/0.05
      e7_enso_pos_nao_neg_pdo_neg_risk_factor[i,j] <- enso_pos_nao_neg_pdo_neg_prob/0.05
      e7_enso_neg_nao_pos_pdo_pos_risk_factor[i,j] <- enso_neg_nao_pos_pdo_pos_prob/0.05
      e7_enso_neg_nao_pos_pdo_neg_risk_factor[i,j] <- enso_neg_nao_pos_pdo_neg_prob/0.05
      e7_enso_neg_nao_neg_pdo_pos_risk_factor[i,j] <- enso_neg_nao_neg_pdo_pos_prob/0.05
      e7_enso_pos_nao_pos_pdo_pos_risk_factor[i,j] <- enso_pos_nao_pos_pdo_pos_prob/0.05
      
    }
  }
}
indices


e7_enso_pos_nao_pos_pdo_neg_rf <- data.frame(e7_enso_pos_nao_pos_pdo_neg_risk_factor)
e7_enso_pos_nao_neg_pdo_pos_rf <- data.frame(e7_enso_pos_nao_neg_pdo_pos_risk_factor)
e7_enso_pos_nao_neg_pdo_neg_rf <- data.frame(e7_enso_pos_nao_neg_pdo_neg_risk_factor)
e7_enso_neg_nao_pos_pdo_pos_rf <- data.frame(e7_enso_neg_nao_pos_pdo_pos_risk_factor)
e7_enso_neg_nao_pos_pdo_neg_rf <- data.frame(e7_enso_neg_nao_pos_pdo_neg_risk_factor)
e7_enso_neg_nao_neg_pdo_pos_rf <- data.frame(e7_enso_neg_nao_neg_pdo_pos_risk_factor)
e7_enso_pos_nao_pos_pdo_pos_rf <- data.frame(e7_enso_pos_nao_pos_pdo_pos_risk_factor)

write.csv(e7_enso_pos_nao_pos_pdo_neg_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e7_enso_pos_nao_pos_pdo_neg_risk_factor.csv", row.names = FALSE)
write.csv(e7_enso_pos_nao_neg_pdo_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e7_enso_pos_nao_neg_pdo_pos_risk_factor.csv", row.names = FALSE)
write.csv(e7_enso_pos_nao_neg_pdo_neg_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e7_enso_pos_nao_neg_pdo_neg_risk_factor.csv", row.names = FALSE)
write.csv(e7_enso_neg_nao_pos_pdo_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e7_enso_neg_nao_pos_pdo_pos_risk_factor.csv", row.names = FALSE)
write.csv(e7_enso_neg_nao_pos_pdo_neg_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e7_enso_neg_nao_pos_pdo_neg_risk_factor.csv", row.names = FALSE)
write.csv(e7_enso_neg_nao_neg_pdo_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e7_enso_neg_nao_neg_pdo_pos_risk_factor.csv", row.names = FALSE)
write.csv(e7_enso_pos_nao_pos_pdo_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e7_enso_pos_nao_pos_pdo_pos_risk_factor.csv", row.names = FALSE)

for(i in 1:dim(elon)){
  
  for(j in 1:dim(elat)){
    
    data1<-e8_acpcp[i,j,]
    
    if (sum(is.na(data1)) >= 40){
      
      e8_enso_pos_nao_pos_pdo_neg_risk_factor[i,j] <- "NA"
      e8_enso_pos_nao_neg_pdo_pos_risk_factor[i,j] <- "NA"
      e8_enso_pos_nao_neg_pdo_neg_risk_factor[i,j] <- "NA"
      e8_enso_neg_nao_pos_pdo_pos_risk_factor[i,j] <- "NA"
      e8_enso_neg_nao_pos_pdo_neg_risk_factor[i,j] <- "NA"
      e8_enso_neg_nao_neg_pdo_pos_risk_factor[i,j] <- "NA"
      e8_enso_pos_nao_pos_pdo_pos_risk_factor[i,j] <- "NA"
      
    }
    else{
      
      df <- data.frame(data1,indices$year,indices$month,indices$soi,indices$naoi,indices$pdoi)
      colnames(df) <- c("acpcp","year","month","soi","naoi","pdoi")
      
      df <- na.omit(df)
      
      enso_nao_pdo_results <- extRemes::fevd(df$acpcp,data=df,location.fun=~soi + naoi + pdoi,scale.fun=~soi + naoi + pdoi)
      
      enso_nao_pdo_mu0 <- enso_nao_pdo_results$results$par[1]
      enso_nao_pdo_mu1 <- enso_nao_pdo_results$results$par[2]
      enso_nao_pdo_mu2 <- enso_nao_pdo_results$results$par[3]
      enso_nao_pdo_mu3 <- enso_nao_pdo_results$results$par[4]
      enso_nao_pdo_sigma0 <- enso_nao_pdo_results$results$par[5]
      enso_nao_pdo_sigma1 <- enso_nao_pdo_results$results$par[6]
      enso_nao_pdo_sigma2 <- enso_nao_pdo_results$results$par[7]
      enso_nao_pdo_sigma3 <- enso_nao_pdo_results$results$par[8]
      enso_nao_pdo_shape <- enso_nao_pdo_results$results$par[9]
      
      enso_neg_nao_neg_pdo_neg_rvalue <- extRemes::rlevd(df$acpcp,loc=enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (-1) + enso_nao_pdo_mu2 * (-1) + enso_nao_pdo_mu3 * (-1),scale=max(1e-100, enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (-1) + enso_nao_pdo_sigma2 * (-1) + enso_nao_pdo_sigma3 * (-1)),shape =enso_nao_pdo_shape,period=20)
      enso_pos_nao_pos_pdo_neg_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (1) + enso_nao_pdo_mu2 * (1) + enso_nao_pdo_mu3 * (-1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (1) + enso_nao_pdo_sigma2 * (1) + enso_nao_pdo_sigma3 * (-1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      enso_pos_nao_neg_pdo_pos_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (1) + enso_nao_pdo_mu2 * (-1) + enso_nao_pdo_mu3 * (1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (1) + enso_nao_pdo_sigma2 * (-1) + enso_nao_pdo_sigma3 * (1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      enso_pos_nao_neg_pdo_neg_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (1) + enso_nao_pdo_mu2 * (-1) + enso_nao_pdo_mu3 * (-1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (1) + enso_nao_pdo_sigma2 * (-1) + enso_nao_pdo_sigma3 * (-1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      enso_neg_nao_pos_pdo_pos_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (-1) + enso_nao_pdo_mu2 * (1) + enso_nao_pdo_mu3 * (1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (-1) + enso_nao_pdo_sigma2 * (1) + enso_nao_pdo_sigma3 * (1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      enso_neg_nao_pos_pdo_neg_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (-1) + enso_nao_pdo_mu2 * (1) + enso_nao_pdo_mu3 * (-1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (-1) + enso_nao_pdo_sigma2 * (1) + enso_nao_pdo_sigma3 * (-1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      enso_neg_nao_neg_pdo_pos_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (-1) + enso_nao_pdo_mu2 * (-1) + enso_nao_pdo_mu3 * (1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (-1) + enso_nao_pdo_sigma2 * (-1) + enso_nao_pdo_sigma3 * (1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      enso_pos_nao_pos_pdo_pos_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (1) + enso_nao_pdo_mu2 * (1) + enso_nao_pdo_mu3 * (1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (1) + enso_nao_pdo_sigma2 * (1) + enso_nao_pdo_sigma3 * (1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      
      e8_enso_pos_nao_pos_pdo_neg_risk_factor[i,j] <- enso_pos_nao_pos_pdo_neg_prob/0.05
      e8_enso_pos_nao_neg_pdo_pos_risk_factor[i,j] <- enso_pos_nao_neg_pdo_pos_prob/0.05
      e8_enso_pos_nao_neg_pdo_neg_risk_factor[i,j] <- enso_pos_nao_neg_pdo_neg_prob/0.05
      e8_enso_neg_nao_pos_pdo_pos_risk_factor[i,j] <- enso_neg_nao_pos_pdo_pos_prob/0.05
      e8_enso_neg_nao_pos_pdo_neg_risk_factor[i,j] <- enso_neg_nao_pos_pdo_neg_prob/0.05
      e8_enso_neg_nao_neg_pdo_pos_risk_factor[i,j] <- enso_neg_nao_neg_pdo_pos_prob/0.05
      e8_enso_pos_nao_pos_pdo_pos_risk_factor[i,j] <- enso_pos_nao_pos_pdo_pos_prob/0.05
      
    }
  }
}
indices


e8_enso_pos_nao_pos_pdo_neg_rf <- data.frame(e8_enso_pos_nao_pos_pdo_neg_risk_factor)
e8_enso_pos_nao_neg_pdo_pos_rf <- data.frame(e8_enso_pos_nao_neg_pdo_pos_risk_factor)
e8_enso_pos_nao_neg_pdo_neg_rf <- data.frame(e8_enso_pos_nao_neg_pdo_neg_risk_factor)
e8_enso_neg_nao_pos_pdo_pos_rf <- data.frame(e8_enso_neg_nao_pos_pdo_pos_risk_factor)
e8_enso_neg_nao_pos_pdo_neg_rf <- data.frame(e8_enso_neg_nao_pos_pdo_neg_risk_factor)
e8_enso_neg_nao_neg_pdo_pos_rf <- data.frame(e8_enso_neg_nao_neg_pdo_pos_risk_factor)
e8_enso_pos_nao_pos_pdo_pos_rf <- data.frame(e8_enso_pos_nao_pos_pdo_pos_risk_factor)

write.csv(e8_enso_pos_nao_pos_pdo_neg_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e8_enso_pos_nao_pos_pdo_neg_risk_factor.csv", row.names = FALSE)
write.csv(e8_enso_pos_nao_neg_pdo_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e8_enso_pos_nao_neg_pdo_pos_risk_factor.csv", row.names = FALSE)
write.csv(e8_enso_pos_nao_neg_pdo_neg_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e8_enso_pos_nao_neg_pdo_neg_risk_factor.csv", row.names = FALSE)
write.csv(e8_enso_neg_nao_pos_pdo_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e8_enso_neg_nao_pos_pdo_pos_risk_factor.csv", row.names = FALSE)
write.csv(e8_enso_neg_nao_pos_pdo_neg_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e8_enso_neg_nao_pos_pdo_neg_risk_factor.csv", row.names = FALSE)
write.csv(e8_enso_neg_nao_neg_pdo_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e8_enso_neg_nao_neg_pdo_pos_risk_factor.csv", row.names = FALSE)
write.csv(e8_enso_pos_nao_pos_pdo_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e8_enso_pos_nao_pos_pdo_pos_risk_factor.csv", row.names = FALSE)

for(i in 1:dim(elon)){
  
  for(j in 1:dim(elat)){
    
    data1<-e9_acpcp[i,j,]
    
    if (sum(is.na(data1)) >= 40){
      
      e9_enso_pos_nao_pos_pdo_neg_risk_factor[i,j] <- "NA"
      e9_enso_pos_nao_neg_pdo_pos_risk_factor[i,j] <- "NA"
      e9_enso_pos_nao_neg_pdo_neg_risk_factor[i,j] <- "NA"
      e9_enso_neg_nao_pos_pdo_pos_risk_factor[i,j] <- "NA"
      e9_enso_neg_nao_pos_pdo_neg_risk_factor[i,j] <- "NA"
      e9_enso_neg_nao_neg_pdo_pos_risk_factor[i,j] <- "NA"
      e9_enso_pos_nao_pos_pdo_pos_risk_factor[i,j] <- "NA"
      
    }
    else{
      
      df <- data.frame(data1,indices$year,indices$month,indices$soi,indices$naoi,indices$pdoi)
      colnames(df) <- c("acpcp","year","month","soi","naoi","pdoi")
      
      df <- na.omit(df)
      
      enso_nao_pdo_results <- extRemes::fevd(df$acpcp,data=df,location.fun=~soi + naoi + pdoi,scale.fun=~soi + naoi + pdoi)
      
      enso_nao_pdo_mu0 <- enso_nao_pdo_results$results$par[1]
      enso_nao_pdo_mu1 <- enso_nao_pdo_results$results$par[2]
      enso_nao_pdo_mu2 <- enso_nao_pdo_results$results$par[3]
      enso_nao_pdo_mu3 <- enso_nao_pdo_results$results$par[4]
      enso_nao_pdo_sigma0 <- enso_nao_pdo_results$results$par[5]
      enso_nao_pdo_sigma1 <- enso_nao_pdo_results$results$par[6]
      enso_nao_pdo_sigma2 <- enso_nao_pdo_results$results$par[7]
      enso_nao_pdo_sigma3 <- enso_nao_pdo_results$results$par[8]
      enso_nao_pdo_shape <- enso_nao_pdo_results$results$par[9]
      
      enso_neg_nao_neg_pdo_neg_rvalue <- extRemes::rlevd(df$acpcp,loc=enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (-1) + enso_nao_pdo_mu2 * (-1) + enso_nao_pdo_mu3 * (-1),scale=max(1e-100, enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (-1) + enso_nao_pdo_sigma2 * (-1) + enso_nao_pdo_sigma3 * (-1)),shape =enso_nao_pdo_shape,period=20)
      enso_pos_nao_pos_pdo_neg_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (1) + enso_nao_pdo_mu2 * (1) + enso_nao_pdo_mu3 * (-1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (1) + enso_nao_pdo_sigma2 * (1) + enso_nao_pdo_sigma3 * (-1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      enso_pos_nao_neg_pdo_pos_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (1) + enso_nao_pdo_mu2 * (-1) + enso_nao_pdo_mu3 * (1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (1) + enso_nao_pdo_sigma2 * (-1) + enso_nao_pdo_sigma3 * (1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      enso_pos_nao_neg_pdo_neg_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (1) + enso_nao_pdo_mu2 * (-1) + enso_nao_pdo_mu3 * (-1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (1) + enso_nao_pdo_sigma2 * (-1) + enso_nao_pdo_sigma3 * (-1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      enso_neg_nao_pos_pdo_pos_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (-1) + enso_nao_pdo_mu2 * (1) + enso_nao_pdo_mu3 * (1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (-1) + enso_nao_pdo_sigma2 * (1) + enso_nao_pdo_sigma3 * (1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      enso_neg_nao_pos_pdo_neg_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (-1) + enso_nao_pdo_mu2 * (1) + enso_nao_pdo_mu3 * (-1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (-1) + enso_nao_pdo_sigma2 * (1) + enso_nao_pdo_sigma3 * (-1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      enso_neg_nao_neg_pdo_pos_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (-1) + enso_nao_pdo_mu2 * (-1) + enso_nao_pdo_mu3 * (1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (-1) + enso_nao_pdo_sigma2 * (-1) + enso_nao_pdo_sigma3 * (1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      enso_pos_nao_pos_pdo_pos_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (1) + enso_nao_pdo_mu2 * (1) + enso_nao_pdo_mu3 * (1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (1) + enso_nao_pdo_sigma2 * (1) + enso_nao_pdo_sigma3 * (1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      
      e9_enso_pos_nao_pos_pdo_neg_risk_factor[i,j] <- enso_pos_nao_pos_pdo_neg_prob/0.05
      e9_enso_pos_nao_neg_pdo_pos_risk_factor[i,j] <- enso_pos_nao_neg_pdo_pos_prob/0.05
      e9_enso_pos_nao_neg_pdo_neg_risk_factor[i,j] <- enso_pos_nao_neg_pdo_neg_prob/0.05
      e9_enso_neg_nao_pos_pdo_pos_risk_factor[i,j] <- enso_neg_nao_pos_pdo_pos_prob/0.05
      e9_enso_neg_nao_pos_pdo_neg_risk_factor[i,j] <- enso_neg_nao_pos_pdo_neg_prob/0.05
      e9_enso_neg_nao_neg_pdo_pos_risk_factor[i,j] <- enso_neg_nao_neg_pdo_pos_prob/0.05
      e9_enso_pos_nao_pos_pdo_pos_risk_factor[i,j] <- enso_pos_nao_pos_pdo_pos_prob/0.05
      
    }
  }
}
indices


e9_enso_pos_nao_pos_pdo_neg_rf <- data.frame(e9_enso_pos_nao_pos_pdo_neg_risk_factor)
e9_enso_pos_nao_neg_pdo_pos_rf <- data.frame(e9_enso_pos_nao_neg_pdo_pos_risk_factor)
e9_enso_pos_nao_neg_pdo_neg_rf <- data.frame(e9_enso_pos_nao_neg_pdo_neg_risk_factor)
e9_enso_neg_nao_pos_pdo_pos_rf <- data.frame(e9_enso_neg_nao_pos_pdo_pos_risk_factor)
e9_enso_neg_nao_pos_pdo_neg_rf <- data.frame(e9_enso_neg_nao_pos_pdo_neg_risk_factor)
e9_enso_neg_nao_neg_pdo_pos_rf <- data.frame(e9_enso_neg_nao_neg_pdo_pos_risk_factor)
e9_enso_pos_nao_pos_pdo_pos_rf <- data.frame(e9_enso_pos_nao_pos_pdo_pos_risk_factor)

write.csv(e9_enso_pos_nao_pos_pdo_neg_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e9_enso_pos_nao_pos_pdo_neg_risk_factor.csv", row.names = FALSE)
write.csv(e9_enso_pos_nao_neg_pdo_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e9_enso_pos_nao_neg_pdo_pos_risk_factor.csv", row.names = FALSE)
write.csv(e9_enso_pos_nao_neg_pdo_neg_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e9_enso_pos_nao_neg_pdo_neg_risk_factor.csv", row.names = FALSE)
write.csv(e9_enso_neg_nao_pos_pdo_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e9_enso_neg_nao_pos_pdo_pos_risk_factor.csv", row.names = FALSE)
write.csv(e9_enso_neg_nao_pos_pdo_neg_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e9_enso_neg_nao_pos_pdo_neg_risk_factor.csv", row.names = FALSE)
write.csv(e9_enso_neg_nao_neg_pdo_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e9_enso_neg_nao_neg_pdo_pos_risk_factor.csv", row.names = FALSE)
write.csv(e9_enso_pos_nao_pos_pdo_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/e9_enso_pos_nao_pos_pdo_pos_risk_factor.csv", row.names = FALSE)

for(i in 1:dim(nlon)[1]){
  
  for(j in 1:dim(nlon)[2]){
    
    data1<-n_acpcp[i,j,]
    
    if (sum(is.na(data1)) >= 40){
      
      n_enso_pos_nao_pos_pdo_neg_risk_factor[i,j] <- "NA"
      n_enso_pos_nao_neg_pdo_pos_risk_factor[i,j] <- "NA"
      n_enso_pos_nao_neg_pdo_neg_risk_factor[i,j] <- "NA"
      n_enso_neg_nao_pos_pdo_pos_risk_factor[i,j] <- "NA"
      n_enso_neg_nao_pos_pdo_neg_risk_factor[i,j] <- "NA"
      n_enso_neg_nao_neg_pdo_pos_risk_factor[i,j] <- "NA"
      n_enso_pos_nao_pos_pdo_pos_risk_factor[i,j] <- "NA"
      
    }
    else{
      
      df <- data.frame(data1,indices$year,indices$month,indices$soi,indices$naoi,indices$pdoi)
      colnames(df) <- c("acpcp","year","month","soi","naoi","pdoi")
      
      df <- na.omit(df)
      
      enso_nao_pdo_results <- extRemes::fevd(df$acpcp,data=df,location.fun=~soi + naoi + pdoi,scale.fun=~soi + naoi + pdoi)
      
      enso_nao_pdo_mu0 <- enso_nao_pdo_results$results$par[1]
      enso_nao_pdo_mu1 <- enso_nao_pdo_results$results$par[2]
      enso_nao_pdo_mu2 <- enso_nao_pdo_results$results$par[3]
      enso_nao_pdo_mu3 <- enso_nao_pdo_results$results$par[4]
      enso_nao_pdo_sigma0 <- enso_nao_pdo_results$results$par[5]
      enso_nao_pdo_sigma1 <- enso_nao_pdo_results$results$par[6]
      enso_nao_pdo_sigma2 <- enso_nao_pdo_results$results$par[7]
      enso_nao_pdo_sigma3 <- enso_nao_pdo_results$results$par[8]
      enso_nao_pdo_shape <- enso_nao_pdo_results$results$par[9]
      
      enso_neg_nao_neg_pdo_neg_rvalue <- extRemes::rlevd(df$acpcp,loc=enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (-1) + enso_nao_pdo_mu2 * (-1) + enso_nao_pdo_mu3 * (-1),scale=max(1e-100, enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (-1) + enso_nao_pdo_sigma2 * (-1) + enso_nao_pdo_sigma3 * (-1)),shape =enso_nao_pdo_shape,period=20)
      enso_pos_nao_pos_pdo_neg_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (1) + enso_nao_pdo_mu2 * (1) + enso_nao_pdo_mu3 * (-1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (1) + enso_nao_pdo_sigma2 * (1) + enso_nao_pdo_sigma3 * (-1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      enso_pos_nao_neg_pdo_pos_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (1) + enso_nao_pdo_mu2 * (-1) + enso_nao_pdo_mu3 * (1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (1) + enso_nao_pdo_sigma2 * (-1) + enso_nao_pdo_sigma3 * (1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      enso_pos_nao_neg_pdo_neg_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (1) + enso_nao_pdo_mu2 * (-1) + enso_nao_pdo_mu3 * (-1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (1) + enso_nao_pdo_sigma2 * (-1) + enso_nao_pdo_sigma3 * (-1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      enso_neg_nao_pos_pdo_pos_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (-1) + enso_nao_pdo_mu2 * (1) + enso_nao_pdo_mu3 * (1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (-1) + enso_nao_pdo_sigma2 * (1) + enso_nao_pdo_sigma3 * (1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      enso_neg_nao_pos_pdo_neg_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (-1) + enso_nao_pdo_mu2 * (1) + enso_nao_pdo_mu3 * (-1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (-1) + enso_nao_pdo_sigma2 * (1) + enso_nao_pdo_sigma3 * (-1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      enso_neg_nao_neg_pdo_pos_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (-1) + enso_nao_pdo_mu2 * (-1) + enso_nao_pdo_mu3 * (1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (-1) + enso_nao_pdo_sigma2 * (-1) + enso_nao_pdo_sigma3 * (1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      enso_pos_nao_pos_pdo_pos_prob <- pevd(enso_neg_nao_neg_pdo_neg_rvalue, loc = enso_nao_pdo_mu0 + enso_nao_pdo_mu1 * (1) + enso_nao_pdo_mu2 * (1) + enso_nao_pdo_mu3 * (1), scale = max(1e-100,enso_nao_pdo_sigma0 + enso_nao_pdo_sigma1 * (1) + enso_nao_pdo_sigma2 * (1) + enso_nao_pdo_sigma3 * (1)), shape = enso_nao_pdo_shape,lower.tail =FALSE)
      
      n_enso_pos_nao_pos_pdo_neg_risk_factor[i,j] <- enso_pos_nao_pos_pdo_neg_prob/0.05
      n_enso_pos_nao_neg_pdo_pos_risk_factor[i,j] <- enso_pos_nao_neg_pdo_pos_prob/0.05
      n_enso_pos_nao_neg_pdo_neg_risk_factor[i,j] <- enso_pos_nao_neg_pdo_neg_prob/0.05
      n_enso_neg_nao_pos_pdo_pos_risk_factor[i,j] <- enso_neg_nao_pos_pdo_pos_prob/0.05
      n_enso_neg_nao_pos_pdo_neg_risk_factor[i,j] <- enso_neg_nao_pos_pdo_neg_prob/0.05
      n_enso_neg_nao_neg_pdo_pos_risk_factor[i,j] <- enso_neg_nao_neg_pdo_pos_prob/0.05
      n_enso_pos_nao_pos_pdo_pos_risk_factor[i,j] <- enso_pos_nao_pos_pdo_pos_prob/0.05
      
    }
  }
}
indices


n_enso_pos_nao_pos_pdo_neg_rf <- data.frame(n_enso_pos_nao_pos_pdo_neg_risk_factor)
n_enso_pos_nao_neg_pdo_pos_rf <- data.frame(n_enso_pos_nao_neg_pdo_pos_risk_factor)
n_enso_pos_nao_neg_pdo_neg_rf <- data.frame(n_enso_pos_nao_neg_pdo_neg_risk_factor)
n_enso_neg_nao_pos_pdo_pos_rf <- data.frame(n_enso_neg_nao_pos_pdo_pos_risk_factor)
n_enso_neg_nao_pos_pdo_neg_rf <- data.frame(n_enso_neg_nao_pos_pdo_neg_risk_factor)
n_enso_neg_nao_neg_pdo_pos_rf <- data.frame(n_enso_neg_nao_neg_pdo_pos_risk_factor)
n_enso_pos_nao_pos_pdo_pos_rf <- data.frame(n_enso_pos_nao_pos_pdo_pos_risk_factor)

write.csv(n_enso_pos_nao_pos_pdo_neg_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/n_enso_pos_nao_pos_pdo_neg_risk_factor.csv", row.names = FALSE)
write.csv(n_enso_pos_nao_neg_pdo_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/n_enso_pos_nao_neg_pdo_pos_risk_factor.csv", row.names = FALSE)
write.csv(n_enso_pos_nao_neg_pdo_neg_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/n_enso_pos_nao_neg_pdo_neg_risk_factor.csv", row.names = FALSE)
write.csv(n_enso_neg_nao_pos_pdo_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/n_enso_neg_nao_pos_pdo_pos_risk_factor.csv", row.names = FALSE)
write.csv(n_enso_neg_nao_pos_pdo_neg_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/n_enso_neg_nao_pos_pdo_neg_risk_factor.csv", row.names = FALSE)
write.csv(n_enso_neg_nao_neg_pdo_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/n_enso_neg_nao_neg_pdo_pos_risk_factor.csv", row.names = FALSE)
write.csv(n_enso_pos_nao_pos_pdo_pos_rf,"/Users/dessyb/Library/CloudStorage/Box-Box/Patrick_MS/data/climate_model_data/n_enso_pos_nao_pos_pdo_pos_risk_factor.csv", row.names = FALSE)
