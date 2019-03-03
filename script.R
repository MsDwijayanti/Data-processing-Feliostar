#A simple data processing for plate reader assay by Ari

#install packages
install.packages(c("tidyverse","readr","ggplot2","readxl", "growthcurver" ))


#load library
library(tidyverse)
library(readr)
library(data.table)
library(devtools)
library(tibble)
library(ggplot2)
library(purrr)
library(tibbletime)
library(readxl)
library(dplyr)
library(growthcurver)

#NEED TO CHECK THIS SECTION
a<-100 #points, the number of time points measurement
d<-15 #min, the duration of each time points

#import platemap
platemap<-read_excel("platemap.xlsx")

Time= seq(0, (a-1)*d, d)

rawdata_length<-3+a

#Abs600 processing
Abs600_raw<-read_excel("Abs600.xlsx", col_names=F, skip=12)
Abs600_annotated<-bind_cols(Abs600_raw, platemap)
Abs600_subset<-select(Abs600_annotated, c(4:rawdata_length))
colnames(Abs600_subset)<-Time
Abs600_blank<-filter(Abs600_annotated, content=='medium')
Abs600_blank_mean<-select(Abs600_blank, c(4:rawdata_length))%>%
  summarise_all(.funs = (mean))
normalised_Abs600<-sweep(as.matrix(Abs600_subset),2,as.matrix(Abs600_blank_mean), "-")
Abs600_corrected<-bind_cols(platemap, as.data.frame(normalised_Abs600))
write.csv(Abs600_corrected, "Abs600_corrected.csv")

#growth parameterisation using logistic fitting by growthcurver, plot still inverted
transformed<-as.data.frame(t(normalised_Abs600))
tTime=as.data.frame(Time)
combined<-bind_cols(tTime, transformed)
colnames(combined)<-c("time", platemap$well)
parameter<-SummarizeGrowthByPlate(combined, plot_fit = T, plot_file = "fit.pdf")


#GFP processing
GFP_raw<-read_excel("GFP.xlsx", col_names=F, skip=12)
GFP_annotated<-bind_cols(GFP_raw, platemap)
GFP_subset<-select(GFP_annotated, c(4:rawdata_length))
colnames(GFP_subset)<-Time
GFP_autofluorescence<-filter(GFP_annotated, content=='negative')


#GFP autofluorescence simple correction 
GFP_blank_mean<-select(GFP_autofluorescence, c(4:rawdata_length))%>%
  summarise_all(.funs = (mean))
normalised_GFP<-sweep(as.matrix(GFP_subset),2,as.matrix(GFP_blank_mean), "-")
GFP_corrected<-bind_cols(platemap, as.data.frame(normalised_GFP))
write.csv(GFP_corrected, "GFP_corrected.csv")

#GFP/Abs600 corrected
FlperAbs600<-normalised_GFP/normalised_Abs600
colnames(FlperAbs600)<-Time
FlperAbs600<-bind_cols(platemap, as.data.frame(FlperAbs600))
write.csv(FlperAbs600, "FlperAbs600.csv")

