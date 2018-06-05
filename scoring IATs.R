###########################   Converts variable types of multiple columns of a dataframe at once
convert.magic <- function(obj, type){
  FUN1 <- switch(type,
                 character = as.character,
                 numeric = as.numeric,
                 factor = as.factor)
  out <- lapply(obj, function(x) FUN1(as.character(x)))
  as.data.frame(out)
}

library(gdata)
library(plyr)
library(gdata)

  setwd("F:\\Part 4 IAT\\") 
  File <- list.files("F:\\Part 4 IAT\\") # import csv-files

  IATa <- mdply(File, read.csv, as.is=T)
  
  #cutting practice trials#
  IAT <-IATa[which(IATa$blocknum==4|IATa$blocknum==6|IATa$blocknum==8|IATa$blocknum==10), ]
  
  ##making variable to identify compatible and incompatible trials
  IAT$PA.pleas <-ifelse(IAT$blocknum ==4|IAT$blocknum==6, 1,0)
  IAT$PA.unpleas <-ifelse(IAT$blocknum ==8|IAT$blocknum==10, 1,0)
  
  ##making a dataset with one row per person
  Subj<-tapply(IAT$subject, IAT$subject,mean)
  Subja<-data.frame(Subj)
  BW<-data.frame(subject=Subja$Subj)
  
  ##cutting outliers as per Riichetin et al 2015 and Chevance et al 2016##
  
  #function to calculate mean and sd of RTs
  f <- function(x) c( iMean=mean(x, na.rm=TRUE), iSD=sd(x, na.rm=TRUE), iTen=quantile(x, prob = .10), iNinety=quantile(x, prob = .90) )
  iRT=do.call( "rbind", tapply( IAT$latency, IAT$subject, f ))
  iid=do.call( "rbind", tapply( IAT$subject, IAT$subject, f ))
  
  iRT<-data.frame(iRT)
  iid<-data.frame(iid)
  iRT$subject<-iid$iMean

  #adding i.RT values to big IAT file
  iIAT<-merge(IAT, iRT, by="subject")
  
  #cutting outliers per individual
  iIAT$RTa<-ifelse(iIAT$latency<iIAT$iTen.10., iIAT$iTen.10., iIAT$latency)
  iIAT$RT<-ifelse(iIAT$latency>iIAT$iNinety.90., iIAT$iNinety.90., iIAT$RTa)

  #making separate latency variables per compatible and incompatible trials
  iIAT$PApleas.RT<-ifelse(iIAT$PA.pleas==1,iIAT$RT,NA)
  iIAT$PAunpleas.RT<-ifelse(iIAT$PA.unpleas==1,iIAT$RT,NA)
  
 ##Calculating mean RTs by block, pooled SD, mean difference score, and D-score##
  Mean.PApleas<-tapply(iIAT$PApleas.RT, iIAT$subject, mean, na.rm=TRUE)
  Mean.PAunpleas<-tapply(iIAT$PAunpleas.RT, iIAT$subject, mean,na.rm = TRUE)
  SD<-tapply(iIAT$RT, iIAT$subject, sd, na.rm = TRUE)
  Mean.PApleas<-data.frame(Mean.PApleas)
  Mean.PApleas$subject<-BW$subject
  Mean.PAunpleas<-data.frame(Mean.PAunpleas)
  Mean.PAunpleas$subject<-BW$subject
  SD<-data.frame(SD)
  SD$subject<-BW$subject
  BWda<-merge(Mean.PApleas, Mean.PAunpleas, by="subject")
  BWd<-merge(BWda, SD, by="subject")
  BWd$Diff<-BWd$Mean.PAunpleas-BWd$Mean.PApleas
  BWd$d<-BWd$Diff/BWd$SD
  
  #Splitting data to calculate reliability and calculating two split-half D-scores#
  iIAT$rand<-runif(nrow(iIAT))
  iIAT$PApleas.RT1<-ifelse(iIAT$rand <= mean(iIAT$rand), iIAT$PApleas.RT, NA) 
  iIAT$PApleas.RT2<-ifelse(iIAT$rand > mean(iIAT$rand), iIAT$PApleas.RT, NA) 
  iIAT$PAunpleas.RT1<-ifelse(iIAT$rand <= mean(iIAT$rand), iIAT$PAunpleas.RT, NA) 
  iIAT$PAunpleas.RT2<-ifelse(iIAT$rand > mean(iIAT$rand), iIAT$PAunpleas.RT, NA)
  iIAT$RT1<-ifelse(iIAT$rand <= mean(iIAT$rand), iIAT$RT, NA) 
  iIAT$RT2<-ifelse(iIAT$rand > mean(iIAT$rand), iIAT$RT, NA) 

  Mean.PApleas.RT1<-tapply(iIAT$PApleas.RT1, iIAT$subject, mean, na.rm = TRUE)
  Mean.PApleas.RT2<-tapply(iIAT$PApleas.RT2, iIAT$subject, mean, na.rm = TRUE)
  Mean.PAunpleas.RT1<-tapply(iIAT$PAunpleas.RT1, iIAT$subject, mean, na.rm = TRUE)
  Mean.PAunpleas.RT2<-tapply(iIAT$PAunpleas.RT2, iIAT$subject, mean, na.rm = TRUE)
  SD.RT1<-tapply(iIAT$RT1, iIAT$subject, sd, na.rm = TRUE)
  SD.RT2<-tapply(iIAT$RT2, iIAT$subject, sd, na.rm = TRUE)

  BWreld<-data.frame(BWd$subject)
  BWreld$Mean.PApleas.RT1<-Mean.PApleas.RT1
  BWreld$Mean.PApleas.RT2<-Mean.PApleas.RT2
  BWreld$Mean.PAunpleas.RT1<-Mean.PAunpleas.RT1
  BWreld$Mean.PAunpleas.RT2<-Mean.PAunpleas.RT2
  BWreld$SD.RT1<-SD.RT1
  BWreld$SD.RT2<-SD.RT2
  BWreld$Diff.RT1<-BWreld$Mean.PAunpleas.RT1-BWreld$Mean.PApleas.RT1
  BWreld$Diff.RT2<-BWreld$Mean.PAunpleas.RT2-BWreld$Mean.PApleas.RT2
  BWreld$rel.D1<-BWreld$Diff.RT1/BWreld$SD.RT1
  BWreld$rel.D2<-BWreld$Diff.RT2/BWreld$SD.RT2
  
  
  ##merging and cleaning datasets##
  BWreldx<-data.frame(BWd$subject)
  BWreldx$relD1<-BWreld$rel.D1
  BWreldx$relD2<-BWreld$rel.D2
  BWreldx$subject<-BWd$subject
  BW<-merge(BWd, BWreldx, by="subject")
  BW<-remove.vars(BW, names="BWd.subject")
  
  #making values for EZ model##using same outlier cuts as in Klauer et al (2007)#
  iIAT$rtlow2<-ifelse (iIAT$RT > 100, iIAT$RT, NA)
  iIAT$RT.PApleas.EZ<-ifelse(iIAT$PA.pleas==1, iIAT$rtlow2, NA)
  iIAT$RT.PAunpleas.EZ<-ifelse(iIAT$PA.unpleas==1, iIAT$rtlow2, NA)
  iIAT$prop<-ifelse(iIAT$correct==0, 1, 0)
  iIAT$prop1<-ifelse(iIAT$PA.pleas==1, iIAT$prop, NA)
  iIAT$prop2<-ifelse(iIAT$PA.unpleas==1, iIAT$prop, NA)
  iIAT$realSmall<-ifelse(iIAT$RT < 100, 1, 0)
  tooTiny<-tapply(iIAT$realSmall,iIAT$subject, sum, na.rm=TRUE)
  tooTiny<-data.frame(tooTiny)
  tooTiny$subject<-BW$subject
  BWx<-merge(BW,tooTiny,by="subject")
  BW<-BWx
  
  IQR1<-tapply(iIAT$RT.PApleas.EZ, iIAT$subject, IQR, na.rm = TRUE)
  quartiles1<-tapply(iIAT$RT.PApleas.EZ, iIAT$subject, quantile, na.rm= TRUE)
  quartiles1x<-as.character(quartiles1)
  quart1xx<-unlist(strsplit(quartiles1x, ","))
  quartiles1x <- matrix(quart1xx, nrow = length(BWx[,1]), ncol=5, byrow=T)
  zoo<-data.frame(quartiles1x)
  zoo$subject<-BW$subject
  names(zoo)[2] <- "Q25"
  names(zoo)[4] <- "Q75"
  zoo$Q25<-trim(as.character(zoo$Q25)) ################## remove leading/trailing whitespace to avoid warnings
  zoo$Q75<-trim(as.character(zoo$Q75)) ################## remove leading/trailing whitespace to avoid warnings
  zoo[zoo == "NA"] <- NA ################## recode NAs to avoid warnings
  zoo$Q25<-as.numeric(zoo$Q25)
  zoo$Q75<-as.numeric(zoo$Q75)
  zoo$IQR1<-IQR1
  zoo$Q3and1<-(zoo$Q75+(1.5*zoo$IQR1))
  zoo$Q1and1<-(zoo$Q25-(1.5*zoo$IQR1))
  zoo2<-merge(zoo,iIAT, by="subject")
  zoo2$tooBig.PApleas<-ifelse(zoo2$RT.PApleas.EZ < zoo2$Q3and1, 0, 1)
  zoo2$tooLit.PApleas<-ifelse(zoo2$RT.PApleas.EZ > zoo2$Q1and1, 0, 1)
  tooLittle.PApleas<-tapply(zoo2$tooLit.PApleas,zoo2$subject,sum, na.rm=TRUE)
  tooBig.PApleas<-tapply(zoo2$tooBig.PApleas,zoo2$subject,sum, na.rm=TRUE)
  tooLittle.PApleas<-data.frame(tooLittle.PApleas)
  tooLittle.PApleas$tooBig.PApleas<-tooBig.PApleas
  tooLittle.PApleas$subject<-BW$subject
  BWx<-merge(BW, tooLittle.PApleas, by="subject")
  BW<-BWx
  
  zoo2$rtQ31<-ifelse (zoo2$RT.PApleas.EZ < zoo2$Q3and1, zoo2$RT.PApleas.EZ, NA)
  zoo2$RTz1<-ifelse (zoo2$rtQ31 > zoo2$Q1and1, zoo2$rtQ31, NA)
  zoo2$RTCor1<-ifelse (zoo2$correct ==1, zoo2$RTz1, NA)
  varCor1<-tapply(zoo2$RTCor1,zoo2$subject,var, na.rm=TRUE)
  prop1a<-tapply(zoo2$prop1, zoo2$subject, sum,na.rm = TRUE)
  MCor1<-tapply(zoo2$RTCor1,zoo2$subject,mean, na.rm=TRUE)
  prop1a<-data.frame(prop1a)
  prop1a$subject<-BW$subject
  prop1a$prop1b<-prop1a$prop1a/120
  prop1a$prop1<-ifelse(prop1a$prop1b>.01,prop1a$prop1b,(.5/120))
  prop1a$MCor1<-MCor1
  prop1a$varCor1<-varCor1
  BWx<-merge(BW,prop1a,by="subject")
  BW<-BWx
  BW<-remove.vars(BW,names=c("prop1b","prop1a" ))
  
  IQR2<-tapply(iIAT$RT.PAunpleas.EZ, iIAT$subject, IQR, na.rm = TRUE)
  quartiles2<-tapply(iIAT$RT.PAunpleas.EZ, iIAT$subject, quantile, na.rm= TRUE)
  quartiles2x<-as.character(quartiles2)
  quart2xx<-unlist(strsplit(quartiles2x, ","))
  quartiles2x <- matrix(quart2xx, nrow = length(BWx[,1]), ncol=5, byrow=T)
  zoox2<-data.frame(quartiles2x)
  zoox2$subject<-BW$subject
  names(zoox2)[2] <- "Q25"
  names(zoox2)[4] <- "Q75"
  zoox2$Q25<-as.character(zoox2$Q25)
  zoox2$Q75<-as.character(zoox2$Q75)
  zoox2$Q25<-as.numeric(zoox2$Q25)
  zoox2$Q75<-as.numeric(zoox2$Q75)
  zoox2$IQR2<-IQR2
  zoox2$Q3and1<-(zoox2$Q75+(1.5*zoox2$IQR2))
  zoox2$Q1and1<-(zoox2$Q25-(1.5*zoox2$IQR2))
  zoox222<-merge(zoox2,iIAT, by="subject")
  zoox222$tooBig.PAunpleas<-ifelse(zoox222$RT.PAunpleas.EZ < zoox222$Q3and1, 0, 1)
  zoox222$tooLit.PAunpleas<-ifelse(zoox222$RT.PAunpleas.EZ > zoox222$Q1and1, 0, 1)
  tooLittle.PAunpleas<-tapply(zoox222$tooLit.PAunpleas,zoox222$subject,sum, na.rm=TRUE)
  tooBig.PAunpleas<-tapply(zoox222$tooBig.PAunpleas,zoox222$subject,sum, na.rm=TRUE)
  tooLittle.PAunpleas<-data.frame(tooLittle.PAunpleas)
  tooLittle.PAunpleas$tooBig.PAunpleas<-tooBig.PAunpleas
  tooLittle.PAunpleas$subject<-BW$subject
  BWx<-merge(BW, tooLittle.PAunpleas, by="subject")
  BW<-BWx
  
  zoox222$rtQ31<-ifelse (zoox222$RT.PAunpleas.EZ < zoox222$Q3and1, c(zoox222$RT.PAunpleas.EZ), NA)
  zoox222$RTz2<-ifelse (zoox222$rtQ31 > zoox222$Q1and1, c(zoox222$rtQ31), NA)
  zoox222$RTCor2<-ifelse (zoox222$correct ==1, zoox222$RTz2, NA)
  varCor2<-tapply(zoox222$RTCor2,zoox222$subject,var, na.rm=TRUE)
  prop2a<-tapply(zoox222$prop2, zoox222$subject, sum,na.rm = TRUE)
  MCor2<-tapply(zoox222$RTCor2,zoox222$subject,mean, na.rm=TRUE)
  prop2a<-data.frame(prop2a)
  prop2a$subject<-BW$subject
  prop2a$prop2b<-prop2a$prop2a/120
  prop2a$prop2<-ifelse(prop2a$prop2b>.01,prop2a$prop2b,(.5/120))
  prop2a$MCor2<-MCor2
  prop2a$varCor2<-varCor2
  BWx<-merge(BW,prop2a,by="subject")
  BW<-BWx
  BW<-remove.vars(BW,names=c("prop2b","prop2a" ))
  BW$s2 = .01# The default value for the scaling parameter s equals .1# If Pc equals 0, .5, or 1, the method will not work, and# an edge-correction is required.
  BW$Pc1 = 1-BW$prop1
  BW$Pc2 = 1-BW$prop2
  BW$VRT1 =varCor1/1000000
  BW$VRT2 =varCor2/1000000
  BW$L1 = qlogis(BW$Pc1)
  BW$L2 = qlogis(BW$Pc2)
  # The function "qlogis" calculates the logit.
  BW$x1 = BW$L1*(BW$L1*BW$Pc1^2 - BW$L1*BW$Pc1 + BW$Pc1 - 0.5)/BW$VRT1
  BW$v1 = sign(BW$Pc1-0.5)*.1*BW$x1^(1/4)
  BW$x2 = BW$L2*(BW$L2*BW$Pc2^2 - BW$L2*BW$Pc2 + BW$Pc2 - 0.5)/BW$VRT2
  BW$v2 = sign(BW$Pc2-0.5)*.1*BW$x2^(1/4)
  # This gives drift rate.
  BW$a1 = BW$s2*qlogis(BW$Pc1)/BW$v1
  BW$a2 = BW$s2*qlogis(BW$Pc2)/BW$v2
  # This gives boundary separation.
  BW$y1 = -1*BW$v1*BW$a1/BW$s2
  BW$y2 = -1*BW$v2*BW$a2/BW$s2
  BW$MRT1 = MCor1/1000
  BW$MRT2 = MCor2/1000
  BW$MDT1 = (BW$a1/(2*BW$v1))*(1-exp(BW$y1))/(1+exp(BW$y1))
  BW$Ter1 = BW$MRT1-BW$MDT1
  BW$MDT2 = (BW$a2/(2*BW$v2))*(1-exp(BW$y2))/(1+exp(BW$y2))
  BW$Ter2 = BW$MRT2-BW$MDT2
  
  #################### FIX FROM HERE
  done<-data.frame("id"=BW$subject, "d"=BW$d, "reld1"=BW$relD1, "reld2"=BW$relD2,
                   "tooTiny"=(BW$tooTiny), "tooLittle"=(BW$tooLittle.PApleas+BW$tooLittle.PAunpleas),
                   "tooBig"=(BW$tooBig.PApleas+BW$tooBig.PAunpleas),
                   "IP"=(BW$v1-BW$v2))
  
  done <- convert.magic(done, "numeric")  ################## converts data to numeric
  
  row.names(done) <- NULL  ################## changes row numbers in data frame to be 1 to number of rows
  print(done)
  
  write.table(done, file = "F:\\IAT scores Part 4.csv", sep = ",", col.names = NA,
              qmethod = "double")
  
