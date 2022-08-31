# Data from the EGM5, CO2-flux-measurements
#
# By Emil A.S. Andersen
# Modified from egm4 script
#
# Work in progress
# Mostly works, but need datafile ".txt" without each "start," "end," and "zero" as given in the EGM5
#
#
# Manual:
# The document is set up so that only a few changes should be necessary between data files.
# To better help finding these few places, the script has the following arrows pointing to code that should be adapted as necessary:
#
# ═══════════════════╗
#                    ▼
#
# Where this arrow is located it can be a good idea to check the settings are correct.
# But, before loading any files, the most important thing to keep in mind is the structure of the data file (".txt") given by the EGM5. The header specifying EGM model and software version should be removed, and the first ";" before "Plot" should also be removed.
# This means you will end up with a header as shown below, with no lines above it.
#
# [Example header, this should be the very first line of the ".dat" file from the EGM4, otherwise make sure to fit the data file to this format]
# Plot	RecNo	Day	Month	Hour	Min	CO2 Ref	mb Ref	mbR Temp	Input A	Input B	Input C	Input D	Input E	Input F	Input G	Input H	ATMP	Probe Type
# After that a few changes can be made to fit the data differences (by noticing the arrow as shown above). 
# First is the name of the file, followed by the end of records. For 2 min measurements there are 27 lines, but for 5 min there are 64. This should be calculated, but the lines have been given for 2, 5, 10, and 15min. Lastely is the name of the output file.
# 
# A few other things to keep in mind:
# This script does not use the build-in PAR sensor, nor temperature sensor. 
# For temperature it uses a separate TinyTag that has been logging every minute at the same time.
# For PAR sensor it uses a PAR Photon Flux Sensor connected to an EM50 logger.
# Both can be loaded and combined in this script to make one data file
#
#
#
# ### Libraries ###
#
library(tidyverse) # For the loading data in tibble and organizing it
library(readxl) # For reading excel files with the "read_excel" command
library(writexl) # For writing excel files
#
#
# ### Functions ###
#
# Function to extract p-value of model
# From https://gist.github.com/stephenturner/722049#file-pvalue-from-lm-object-r
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}
#
#
# ### Loading data ###
#
# From .dat
#
# ══════════════════════════════╗
#                               ▼
fluxdat<-read.delim("clean_data/EGM5_Test_clean_2min.dat") # Read as a tab delimited files
#
# ══════════════════════════════╗
#                               ▼
ID_info<-read.delim("clean_data/EGM5_IDs.txt") # The data with information on the plots and IDs
# ### Main Script: calculations ###
#
co2<-fluxdat$CO2 #When importing from excel use 'CO2 ref', when importing from .dat it converts to CO2.Ref
p<-fluxdat$ATMP/10 # Air pressure as measured by the EGM. Converted from mb to kPa
#Temp<-fluxdata$Tair # Temperature measured by CPY-5 chamber
Temp<-fluxdat$AirT # Temperature added from a TinyTag
PAR<-fluxdat$PAR # PAR added from PAR sensor/EM50 logger
#
# ═╗
#  ▼
A<-0.0167 # m2, CPY-5 without collar: 167cm^2 or 0.0167 m^2
#
# ═══╗
#    ▼
Vol<-0.002427 # m3, CPY-5 without collar: 2427mL or 0.002427 m^3
plot<-fluxdat$Plot
time_plot<-fluxdat$RecNo # Recording number, not currently used
time<-fluxdat$DT # DT (change in seconds) # Not currently used
recStart<-33 # Start of records, 33s cut
#
# ══════╗
#       ▼
recEnd<-121 # End of records(121s for 2min measurements, 301s for 5min, 601s for 10min, 901s for 15min, 181s for 3min)
#
#
# Change time at record start to T = 0
Time<-vector("double")
Time<-seq((-(recStart-1)),(recEnd-recStart)) # Make first 33s negative and end depending on last recording number
Dt<-rep(Time,(length(plot))/recEnd)
#
#
#
# quality test #### Make a bunch of graphs in a PDF with the CO2 increase over time measured
#
# ════════════════╗
#                 ▼
pdf(paste("output/EGM5_Test_2min","_QC-2",".pdf", sep = ""))
par(mar = c(4, 5, 2, 2), mfrow = c(2,2)) # mar-form: bottom, left, top, right # , mfrow = c(2,1)
for (q in 1:max(plot)) {
  #
  ind.1 <- plot[recStart+recEnd*(q-1)] == fluxdat$Plot # Creates a TRUE/FALSE binary for the whole dataset rows. Where TRUE is only the plots corresponding to q
  ind.dat <- which(fluxdat$Plot == plot[recStart+recEnd*(q-1)]) # locations in dataset. Where ind.1 is TRUE it finds the rows. Thus extracting the row numbers for plot q
  #
  c.co2.temp  <- fluxdat$CO2[ind.1] # the CO2 values that are associated with q
  #TS.dat.temp <- seq(1, length(c.co2.temp), 1) # the time length, in recNo, from one recording to next
  #
  plot(c.co2.temp[recStart:recEnd]~Dt[recStart:recEnd], ylab = "ppm CO2", xlab = "Time (s)") # plot the graph. Using CO2 values for y and time (Dt) for x, but removing the first number of measurements (recStart)
  abline(lm(c.co2.temp[recStart:recEnd]~Dt[recStart:recEnd])) # Create a linear line with lm
  #
  # mtext(text = paste("Plot nr", plot[recStart+recEnd*(q-1)], sep = " "), side = 3, line = -2) # Write plot number in plot
  mtext(text = paste("ID:", ID_info$ID[q], sep = " "), side = 3, line = -2) # Write ID in plot
  mtext(text = round(summary(lm(c.co2.temp[recStart:recEnd]~Dt[recStart:recEnd]))$r.squared, digits = 4), side = 1, line = -2) # calculate and write the R^2 value of the linear model 
}
dev.off()
#
#
#
# The process of getting all data as in matlab
# Define all vectors and matrices used
meanT<-vector("double")
meanP<-vector("double")
co2trim<-vector("list")
timetrim<-vector("list")
fluxmod<-vector("list")
sse<-vector("double")
C1_fit<-matrix(NA,nrow = max(plot),ncol = 2)
C1_fit_intcep<-vector("double") # Unused?
f1_lin_umol<-matrix(NA,nrow = max(plot),ncol = 1)
#
for (i in plot){
  meanT[i]<-mean(Temp[(recStart+recEnd*(i-1)):(recEnd+recEnd*(i-1))]) # averaging temperature over the recordings
  meanP[i]<-mean(p[(recStart+recEnd*(i-1)):(recEnd+recEnd*(i-1))]) # averaging pressure over the recordings
  meanPAR[i]<-mean(PAR[(recStart+recEnd*(i-1)):(recEnd+recEnd*(i-1))]) # averaging PAR over recordings
  co2trim[[i]]<-co2[(recStart+recEnd*(i-1)):(recEnd+recEnd*(i-1))] # CO2 for i=1:max(plot)
  timetrim[[i]]<-Dt[(recStart+recEnd*(i-1)):(recEnd+recEnd*(i-1))] # time for i=1:max(plot)
  fluxmod[[i]]<-summary(lm(co2trim[[i]]~timetrim[[i]])) # model summary of linear regression
  sse[[i]]<-deviance(lm(co2trim[[i]]~timetrim[[i]])) # sse
  C1_fit[i,]<-cbind(fluxmod[[i]]$coefficients[1,1],fluxmod[[i]]$coefficients[2,1]) # Array of intercept and slope (b and a in the regression y=a*x+b)
  linflux[[i]]<-lm(co2trim[[i]]~timetrim[[i]]) # The linear regression. To extract p-value of the model
}
#
# linear production of CO2
for (i in plot) {
  f1_lin_umol[i,]<-((C1_fit[i,2]*273.15*meanP[i]/(22.4*(meanT[i]+273.15)*101.325))*(Vol/A)*1000) # µmol CO2 m-2 s-1
  # The slope, C1_fit[i,2] is the change in CO2 concentration over time (µmol mol^-1 s^-1)
  # meanP[i] is the average pressure of the measurement.
  # The ideal gas at 273.15K and 101.325 kPa gives a molar volume of 22.4 dm^3 mol^-1
  # Volume is given at m^3 and area (A) at m^2 times 1000 dm^3 m^-3
  # meanT[i] is the average temperature in °C, converted to kelvin by adding 273.15
  #
  # This gives the equation with units:
  # (µmol mol^-1 s^-1 * 273.15K * kPa / (22.4 dm^3 mol^-1 * K * 101.325 kPa)) * (m^3 / m^2) * 1000 dm^3 m^-3
}
#
#
# Attach all data to an output file
output<-matrix(NA,nrow = max(plot),ncol = 16) # missing P-value and F statistic
output[,1]<-(1:max(plot)) # Plots
output[,2]<-(ID_info$ID) # ID of plots
for (i in plot) {
  output[i,3]<-meanT[i] # average temperature (°C)
  output[i,4]<-meanP[i] # average pressure (kPa)
  output[i,5]<-meanPAR[i] # average PAR (µmol m^-2 s^-1)
  output[i,6]<-sse[[i]] # sse
  output[i,7]<-fluxmod[[i]]$r.squared # R2
  output[i,8]<-fluxmod[[i]]$fstatistic[[3]] # Degrees of freedom, df
  output[i,9]<-fluxmod[[i]]$fstatistic[[2]] # Independent variables and categories
  output[i,10]<-fluxmod[[i]]$adj.r.squared # Adj. R2
  output[i,11]<-fluxmod[[i]]$sigma #RSME
  output[i,12]<-fluxmod[[i]]$fstatistic[[1]] # F statistic for model
  output[i,13]<-lmp(linflux[[i]]) # P-value of model
  # As this is a simple, one predictor, linear regression an alternative and easier value is the coefficient p-value, which will be the same.
  # output[i,12]<-fluxmod[[i]]$coefficients[,4][[2]]
}
output[,14]<-C1_fit[,2] # p1
output[,15]<-C1_fit[,1] # p2
output[,16]<-f1_lin_umol # Linear production of CO2; µmol CO2 m-2 s-1
colnames(output)<- c("Plot", "ID", "Tair_(°C)", "ATMP_(kPa)","sse","R2","df", "independent_var","R2_adj","rsme", "F-value_model", "P-value_model","p1","p2","f1_lin_umol") # For rewriting the column names
# Write excel file
output_xl<-as.data.frame(output)
#
# ═══════════════════════════════════╗
#                                    ▼
write_xlsx(output_xl, path = "output/EGM5_Output_Test_2min.xlsx", col_names = TRUE, format_headers = FALSE)
#
# The End