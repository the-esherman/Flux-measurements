# Data from the EGM4, CO2-flux-measurements
#
# By Emil A.S. Andersen
# Modified from scripts by Sylvain Monteux (R) and Josefine Walz (MATLAB)
# Additional help in graphing CO2 over time from Valentin Heinzelmann
#
# Work in progress
# Mostly works, but need datafile ".dat" without the header from egm4 and the last line stating length of measurement
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
# But, before loading any files, the most important thing to keep in mind is the structure of the data file (".dat") given by the EGM4. The header specifying EGM model and software version should be removed, and the first ";" before "Plot" should also be removed.
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
#
# ### Loading data ###
#
# From .dat
#
# ══════════════════════════════╗
#                               ▼
fluxdat<-read.delim("clean_data/Cleaned_test_2min.dat") # Read as a tab delimited files
#
# ══════════════════════════════╗
#                               ▼
ID_info<-read.delim("clean_data/EGM4_IDs.txt") # The data with information on the plots and IDs
#
# Gas constant
R_const<-8.31446261815324*1000/10^6 # m^3*Pa*K^-1*mol^-1 * kPa*Pa^-1 * µmol*mol^-1 =  m^3*kPa*K^-1*µmol^-1
#
# ### Main Script: calculations ###
#
co2<-fluxdat$CO2.Ref #When importing from excel use 'CO2 ref', when importing from .dat it converts to CO2.Ref
p<-fluxdat$ATMP/10 # Air pressure as measured by the EGM. Converted from mb to kPa
#Temp<-fluxdat$Input.C # soil temperature from EGM4 optional sensor
Temp<-fluxdat$AirT # Temperature added from a TinyTag
PAR<-fluxdat$PAR # PAR added from PAR sensor/EM50 logger
#
# ═╗
#  ▼
A<-0.0079 # m2
#
# ═══╗
#    ▼
Vol<-0.000550 # m3
plot<-fluxdat$Plot
time_plot<-fluxdat$RecNo # Recording number, should be org as 1-x (where x is 27 for 2min measurements, 64 for 5min, 127 for 10min, 189 for 15min). Not currently used
time<-fluxdat$Input.E # DT (change in seconds, note that it measures only whole seconds, but each measurement is pr 4.8s, this can be changed in the dat file) # Not currently used
recStart<-8 # Start of records, 8 ~ 33s cut
#
# ══════╗
#       ▼
recEnd<-27 # End of records(27 for 2min measurements, 64 for 5min, 127 for 10min, 189 for 15min, 39 for 3min)
#
#
# Change time at record start to T = 0
Time<-vector("double")
Time<-seq((-(recStart-1)*4.8),(4.8*(recEnd-recStart)), 4.8) # Make first 33.6s negative (recStart of 8 times 4.8s between measurements) and end depending on last recording number
Dt<-rep(Time,(length(plot))/recEnd)
#
#
#
# quality test #### Make a bunch of graphs in a PDF with the CO2 increase over time measured
#
# ════════════════╗
#                 ▼
pdf(paste("output/Test_2min","_QC-2",".pdf", sep = ""))
par(mar = c(4, 5, 2, 2), mfrow = c(2,2)) # mar-form: bottom, left, top, right # , mfrow = c(2,1)
for (q in 1:max(plot)) {
  #
  ind.1 <- plot[recStart+recEnd*(q-1)] == fluxdat$Plot # Creates a TRUE/FALSE binary for the whole dataset rows. Where TRUE is only the plots corresponding to q
  ind.dat <- which(fluxdat$Plot == plot[recStart+recEnd*(q-1)]) # locations in dataset. Where ind.1 is TRUE it finds the rows. Thus extracting the row numbers for plot q
  #
  c.co2.temp  <- fluxdat$CO2.Ref[ind.1] # the CO2 values that are associated with q
  #TS.dat.temp <- seq(1, length(c.co2.temp), 1) # the time length, in recNo, from one recording to next
  #
  plot(c.co2.temp[recStart:recEnd]~Dt[recStart:recEnd], ylab = "ppm CO2", xlab = "Time (s)") # plot the graph. Using CO2 values for y and time (Dt) for x, but removing the first number of measurements (recStart)
  abline(lm(c.co2.temp[recStart:recEnd]~Dt[recStart:recEnd])) # Create a linear line with lm
  #
  mtext(text = paste("Plot nr", plot[recStart+recEnd*(q-1)], sep = " "), side = 3, line = -2) # Write plot number in plot
  mtext(text = round(summary(lm(c.co2.temp[recStart:recEnd]~Dt[recStart:recEnd]))$r.squared, digits = 4), side = 1, line = -2) # calculate and write the R^2 value of the linear model 
}
dev.off()
#
#
# Bootstrap the samples to exclude x amount of samples. See XX for reference
# x = 20 here is a cutoff of 33.6 seconds just as with other cutoff
slopes <- data.frame(plot=NA,slope=NA)
for (plots in unique(fluxdat$Plot)){ # Run for each plot
  plotdat <- fluxdat[fluxdat$Plot == plots,]
  plotdat$number <- c(0:(nrow(plotdat)-1)*4.8)
  plotid <- rep(plots,15000)
  slope <- c()
  for (i in seq(1:15000)){ # Run 15000 repetitions
    sub <- plotdat[sample(nrow(plotdat),size=20),]
    slope[i] <- coef(lm(data=sub, CO2.Ref ~ number))[2] # Run through CO2 reference
  }
  slopes <- rbind(slopes,
                 data.frame(plot = plotid,
                            slope = slope))
}
slopes <- na.omit(slopes) # remove NA from top
#
# Plot 25 is 1 for all 15000 replicates
ggplot(slopes[slopes$plot != 25,],(aes(x=slope,fill=as.factor(plot)))) +
  geom_density(alpha = 0.5)
ggplot(slopes[slopes$plot == 25,],(aes(x=slope,fill=as.factor(plot)))) +
  geom_density(alpha = 0.5)
#

coef(lm(data=sub, CO2.Ref ~ number))[2]
summary(lm(co2trim[[30]]~timetrim[[30]]))$coefficients[2,1]

#
# Get most frequent value and confidence interval around
co2trim_boot_density<-vector(mode="list", length = length(unique(fluxdat$Plot)))
for (plots in unique(fluxdat$Plot)) {
  co2trim_boot_density[[plots]] <- with(slopes, density(slope[which(plot == plots)], n = 15000)) # kernel density estimate
  co2trim_boot[[plots,2]] <- with(co2trim_boot_density[[plots]], x[which.max(y)]) # based on which y value is max density, pick the x. This is the mode
}

#
#
# The process of getting all data as in matlab
# Define all vectors and matrices used
meanT<-vector("double") # Average temperature
meanP<-vector("double") # Average pressure
co2trim<-vector("list") # CO2 concentration trimmed to remove first few measures
timetrim<-vector("list") # Time, trimmed to remove the first few measurements (33,6s) as per variable "Time"
fluxmod<-vector("list") # a list to store model summary in
sse<-vector("double")
C1_fit<-matrix(NA,nrow = max(plot),ncol = 2) # for intercept and slope
C1_fit_intcep<-vector("double") # Unused?
linflux<-vector("list") # for storing information on linear regression.
f1_lin_umol<-matrix(NA,nrow = max(plot),ncol = 1) # linear production of CO2
#
for (i in plot){
  meanT[i]<-mean(Temp[(recStart+recEnd*(i-1)):(recEnd+recEnd*(i-1))]) # averaging temperature over the recordings
  meanP[i]<-mean(p[(recStart+recEnd*(i-1)):(recEnd+recEnd*(i-1))]) # averaging pressure over the recordings
  co2trim[[i]]<-co2[(recStart+recEnd*(i-1)):(recEnd+recEnd*(i-1))] # CO2 for i=1:max(plot)
  timetrim[[i]]<-Dt[(recStart+recEnd*(i-1)):(recEnd+recEnd*(i-1))] # time for i=1:max(plot)
  fluxmod[[i]]<-summary(lm(co2trim[[i]]~timetrim[[i]])) # model summary
  sse[[i]]<-deviance(lm(co2trim[[i]]~timetrim[[i]])) # sse
  C1_fit[i,]<-cbind(fluxmod[[i]]$coefficients[1,1],fluxmod[[i]]$coefficients[2,1]) # Array of intercept and slope (b and a in the regression y=a*x+b)
  linflux[[i]]<-lm(co2trim[[i]]~timetrim[[i]]) # The linear regression. To extract p-value of the model
}
#
# linear production of CO2
for (i in plot) {
  f1_lin_umol[i,]<-((C1_fit[i,2]*meanP[i]/((meanT[i]+273.15)*R_const))*(Vol/A)) # µmol CO2 m-2 s-1
  # This equation follows the ideal gas law (P*V = n*R*T)
  # d[CO2]*P / (T*R) * V/A
  #
  # The slope, C1_fit[i,2] is the change in CO2 concentration over time (d[CO2] = µmol mol^-1 s^-1)
  # meanP[i] is the average pressure of the measurement.
  # meanT[i] is the average temperature in °C, converted to kelvin by adding 273.15
  # Volume is given at m^3 and area (A) at m^2 times 1000 dm^3 m^-3
  #
  # This gives the equation with units:
  # (µmol mol^-1 s^-1 * kPa) / (K * m^3 * kPa * K^-1 * µmol^-1) * (m^3 / m^2)
}
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
# Attach all data to an output file
output<-matrix(NA,nrow = max(plot),ncol = 15) # missing P-value and F statistic
output[,1]<-(1:max(plot)) # Plots
output[,2]<-(ID_info[[1]]) # ID of plots
for (i in plot) {
  output[i,3]<-meanT[i] # average temperature (°C)
  output[i,4]<-meanP[i] # average pressure (kPa)
  output[i,5]<-sse[[i]] # sse
  output[i,6]<-fluxmod[[i]]$r.squared # R2
  output[i,7]<-fluxmod[[i]]$fstatistic[[3]] # Degrees of freedom, df
  output[i,8]<-fluxmod[[i]]$fstatistic[[2]] # Independent variables and categories
  output[i,9]<-fluxmod[[i]]$adj.r.squared # Adj. R2
  output[i,10]<-fluxmod[[i]]$sigma #RSME
  output[i,11]<-fluxmod[[i]]$fstatistic[[1]] # F statistic for model
  output[i,12]<-lmp(linflux[[i]]) # P-value of model
  # As this is a simple, one predictor, linear regression an alternative and easier value is the coefficient p-value, which will be the same.
  # output[i,12]<-fluxmod[[i]]$coefficients[,4][[2]]
}
output[,13]<-C1_fit[,2] # p1
output[,14]<-C1_fit[,1] # p2
output[,15]<-f1_lin_umol # Linear production of CO2; µmol CO2 m-2 s-1
colnames(output)<- c("Plot", "ID", "Tair_(°C)", "ATMP_(kPa)","sse","R2","df", "independent_var","R2_adj","rsme", "F-value_model", "P-value_model","p1","p2","f1_lin_umol") # For rewriting the column names
# Write excel file
output_xl<-as.data.frame(output)
#
# ═══════════════════════════════════╗
#                                    ▼
write_xlsx(output_xl, path = "output/Output_Test_2min.xlsx", col_names = TRUE, format_headers = FALSE)
#
# The End