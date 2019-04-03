## SETTING A WORKING DIRECTORY
setwd("your working directory here")

## LOADING THE NECESSARY PACKAGES
library(dplyr)
library(ggplot2)

## PREPARING AND CLEANING THE DATA
sch <-  read_sav("CY6_MS_CMB_SCH_QQQ.sav")

# since we're only interested in visualizing Indonesia, Brazil, Mexico, Thailand, and Vitnem, filtering needs to be applied
sch_r <- sch[sch$CNT == "IDN" | sch$CNT == "BRA" | sch$CNT == "MEX" | sch$CNT == "THA" | sch$CNT == "VNM",]

# the variables that we're interested in are "SC017Q05NA","SC017Q06NA","SC017Q07NA","SC017Q08NA"
# SC017Q05NA: Is your school’s capacity to provide instruction hindered by a lack of educational material (e.g. textbooks, IT equipment, library or laboratory material)?
# SC017Q06NA: Is your school’s capacity to provide instruction hindered by inadequate or poor quality educational material (e.g. textbooks, IT equipment, library or laboratory material). 
# SC017Q07NA: Is your school’s capacity to provide instruction hindered by a lack of physical infrastructure (e.g. building, grounds, heating/cooling, lighting and acoustic systems). 
# SC017Q08NA: Is your school’s capacity to provide instruction hindered by inadequate or poor quality physical infrastructure (e.g. building, grounds, heating/cooling, lighting and acoustic systems). 

sch_r <- sch_r[,c("CNT","CNTSCHID","SC017Q05NA","SC017Q06NA","SC017Q07NA","SC017Q08NA")]

# The objective of this exercise is to graph the comparison of school principals answering that a shortage of resources (educational material and physical infrastructure)  in terms of quality and quantity hinders a lot the school's capacity to provide instruction 
# the answer "a lot" is reflected in answer "4"
# 4 dummy variables (in which it will be coded as 1 if the respondent answers "4", 0 otherwise) will be created

sch_r$SC017Q05NA_d <- ifelse(sch_r$SC017Q05NA == 4,1,0)
sch_r$SC017Q06NA_d <- ifelse(sch_r$SC017Q06NA == 4,1,0)
sch_r$SC017Q07NA_d <- ifelse(sch_r$SC017Q07NA == 4,1,0)
sch_r$SC017Q08NA_d <- ifelse(sch_r$SC017Q08NA == 4,1,0)
sch_r[,c("SC017Q05NA","SC017Q06NA","SC017Q07NA","SC017Q08NA")] <- NULL

## CALCULATING THE SHARE OF SCHOOL PRINCIPALS ANSWERING "A LOT" ON THE SHORTAGE OF RESOURCES-RELATED QUESTIONS
SC017Q05NA_idn <- mean(sch_r$SC017Q05NA_d[sch_r$CNT=="IDN"],na.rm=T)
SC017Q05NA_bra <- mean(sch_r$SC017Q05NA_d[sch_r$CNT=="BRA"],na.rm=T)
SC017Q05NA_mex <- mean(sch_r$SC017Q05NA_d[sch_r$CNT=="MEX"],na.rm=T)
SC017Q05NA_tha <- mean(sch_r$SC017Q05NA_d[sch_r$CNT=="THA"],na.rm=T)
SC017Q05NA_vnm <- mean(sch_r$SC017Q05NA_d[sch_r$CNT=="VNM"],na.rm=T)
SC017Q06NA_idn <- mean(sch_r$SC017Q06NA_d[sch_r$CNT=="IDN"],na.rm=T)
SC017Q06NA_bra <- mean(sch_r$SC017Q06NA_d[sch_r$CNT=="BRA"],na.rm=T)
SC017Q06NA_mex <- mean(sch_r$SC017Q06NA_d[sch_r$CNT=="MEX"],na.rm=T)
SC017Q06NA_tha <- mean(sch_r$SC017Q06NA_d[sch_r$CNT=="THA"],na.rm=T)
SC017Q06NA_vnm <- mean(sch_r$SC017Q06NA_d[sch_r$CNT=="VNM"],na.rm=T)
SC017Q07NA_idn <- mean(sch_r$SC017Q07NA_d[sch_r$CNT=="IDN"],na.rm=T)
SC017Q07NA_bra <- mean(sch_r$SC017Q07NA_d[sch_r$CNT=="BRA"],na.rm=T)
SC017Q07NA_mex <- mean(sch_r$SC017Q07NA_d[sch_r$CNT=="MEX"],na.rm=T)
SC017Q07NA_tha <- mean(sch_r$SC017Q07NA_d[sch_r$CNT=="THA"],na.rm=T)
SC017Q07NA_vnm <- mean(sch_r$SC017Q07NA_d[sch_r$CNT=="VNM"],na.rm=T)
SC017Q08NA_idn <- mean(sch_r$SC017Q08NA_d[sch_r$CNT=="IDN"],na.rm=T)
SC017Q08NA_bra <- mean(sch_r$SC017Q08NA_d[sch_r$CNT=="BRA"],na.rm=T)
SC017Q08NA_mex <- mean(sch_r$SC017Q08NA_d[sch_r$CNT=="MEX"],na.rm=T
SC017Q08NA_tha <- mean(sch_r$SC017Q08NA_d[sch_r$CNT=="THA"],na.rm=T)
SC017Q08NA_vnm <- mean(sch_r$SC017Q08NA_d[sch_r$CNT=="VNM"],na.rm=T)


 
