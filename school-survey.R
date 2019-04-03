## SETTING A WORKING DIRECTORY
setwd("your working directory here")

## LOADING THE NECESSARY PACKAGES
library(dplyr)
library(reshape2)
library(ggplot2)
library(RColorBrewer)

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

sch_resource_idn <- data.frame(SC017Q05NA_idn, SC017Q06NA_idn, SC017Q07NA_idn, SC017Q08NA_idn)
sch_resource_bra <- data.frame(SC017Q05NA_bra, SC017Q06NA_bra, SC017Q07NA_bra, SC017Q08NA_bra)
sch_resource_mex <- data.frame(SC017Q05NA_mex, SC017Q06NA_mex, SC017Q07NA_mex, SC017Q08NA_mex)
sch_resource_tha <- data.frame(SC017Q05NA_tha, SC017Q06NA_tha, SC017Q07NA_tha, SC017Q08NA_tha)
sch_resource_vnm <- data.frame(SC017Q05NA_vnm, SC017Q06NA_vnm, SC017Q07NA_vnm, SC017Q08NA_vnm)   
sch_resource <- data.frame(sch_resource_idn, sch_resource_bra, sch_resource_mex, sch_resource_tha, sch_resource_vnm)       
sch_resource <- reshape(sch_resource,varying=c("SC017Q05NA_idn", "SC017Q06NA_idn", "SC017Q07NA_idn", "SC017Q08NA_idn","SC017Q05NA_bra", "SC017Q06NA_bra", "SC017Q07NA_bra", "SC017Q08NA_bra","SC017Q05NA_mex", "SC017Q06NA_mex", "SC017Q07NA_mex", "SC017Q08NA_mex","SC017Q05NA_tha", "SC017Q06NA_tha", "SC017Q07NA_tha", "SC017Q08NA_tha","SC017Q05NA_vnm", "SC017Q06NA_vnm", "SC017Q07NA_vnm", "SC017Q08NA_vnm"),direction="long",sep="_")                       
sch_resource$id <- NULL
colnames(sch_resource)[colnames(sch_resource) == "time"] <- "CNT"

# converting into percentage format                       
sch_resource$SC017Q05NA <- (sch_resource$SC017Q05NA*100)
sch_resource$SC017Q06NA <- (sch_resource$SC017Q06NA*100)
sch_resource$SC017Q07NA <- (sch_resource$SC017Q07NA*100)
sch_resource$SC017Q08NA <- (sch_resource$SC017Q08NA*100)                       

# restructuring the data                       
sch_resource <- melt(sch_resource,id.vars="CNT")
colnames(sch_resource)[colnames(sch_resource) == "variable"] <- "shortage"
colnames(sch_resource)[colnames(sch_resource) == "value"] <- "share_principal"
sch_resource$shortage[sch_resource$shortage == "SC017Q05NA"] <- "Lack of quantity in educational material"
sch_resource$shortage[sch_resource$shortage == "SC017Q06NA"] <- "Lack of quality in educational material"
sch_resource$shortage[sch_resource$shortage == "SC017Q07NA"] <- "Lack of quantity in infrastructure"                       
sch_resource$shortage[sch_resource$shortage == "SC017Q08NA"] <- "Lack of quality in infrastructure"

## DATA VISUALIZATION                       
sch_resource_plot <- ggplot(sch_resource, aes(fill=shortage, y=share_principal, x=CNT)) + geom_bar(position="dodge", stat="identity",width=0.65) + scale_fill_brewer(palette="BrBG") + xlab("Country") + ylab("Share of Principals")   

## PLOTTING SHORTAGE AND MEAN OF MATH SCORE
                       
df <- reshape(sch_resource, idvar="CNT",timevar="shortage",direction="wide")                       
df$shortage_mean <- rowMeans(df[,-1],na.rm=T) 
df <- df[,c("CNT","shortage_mean")]
df$CNT[df$CNT == "idn"] <- "IDN"
df$CNT[df$CNT == "bra"] <- "BRA"
df$CNT[df$CNT == "mex"] <- "MEX"
df$CNT[df$CNT == "tha"] <- "THA"
df$CNT[df$CNT == "vnm"] <- "VNM"                       
                       
math <- read.csv("PISA math mean score.csv")
math <- math[math$Country.Code == "IDN" | math$Country.Code == "BRA" | math$Country.Code == "MEX"| math$Country.Code == "THA" | math$Country.Code == "VNM",]
math <- math[,c("Country.Code","X2015..YR2015.")]
colnames(math)[colnames(math) == "X2015..YR2015."] <- "math_mean"
colnames(math)[colnames(math) == "Country.Code"] <- "CNT"                       

corr_math_shortage <- merge(math,df,by="CNT")                       
str(corr_math_shortage)
corr_math_shortage$math_mean <- as.numeric(paste(corr_math_shortage$math_mean))
corr <- cor.test(corr_math_shortage$math_mean, corr_math_shortage$shortage_mean)                       
ggplot(corr_math_shortage, aes(x=shortage_mean, y=math_mean)) + geom_point() + geom_smooth(method=lm, se=TRUE) +  geom_text(label=c("BRA","IDN","MEX","THA","VNM")) + labs(title="Scatterplot of Shortage vs Mean of Math Score", y="Mean of PISA Math Score", x="Resources Shortage")                       
