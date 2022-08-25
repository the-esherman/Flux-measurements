# Data cleaning and combining
# For EGM4
# Using CPY-2 (Probe type 8), or forced probe type 8
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
#
#
# ### Libraries ###
#
library(tidyverse) # For the loading data in tibble and organizing it
library(readxl) # For reading excel files with the "read_excel" command
library(writexl) # For writing excel files
#
#
# # Combine several data files #
#
# Since the EGM4 has limited internal storage (can only save 1000 lines) several files might have to be merged to create one data set for one day (or several days if necessary)
# This does not assume similar length of each measurements, but later parts might!
#
# Before running the main script, it could be useful to not have to combine the egm4 file with the TinyTag data manually. Also if you are using a external PAR sensor logging on an EM50 logger
#
# Load different data sets from same day.
# OBS! the "Plot" needs to be continues between the files to work here. E.g. flux1 ends in Plot 36 and flux2 starts in Plot 37. Otherwise chaos ensures
#
# ═══════════════════╗
#                    ▼
flux1 <- read_delim("raw_data/raw_test_1.dat", skip = 2)
flux1 <- flux1 %>% # Here the last line is cut and all the columns are processed to match either as a numeric or character value
  slice(1:(n()-1)) %>%
  mutate(across(c(`;Plot`, `CO2 Ref`, `mb Ref`, `mbR Temp`, `Input A`, `Input B`, `Input C`, `Input D`, `Input E`, `Input F`, ATMP), as.numeric)) %>%
  mutate(across(c(RecNo, `Input G`, `Input H`, `Probe Type`), as.character))
flux1 <- flux1 %>%
  unite(Month, Day, col = "Date", sep = "-") %>%
  unite(Hour, Min, col = "Time", sep = ":") %>%
  unite(Date, Time, col = "Day_ID", sep = " ")
#
# ═══════════════════╗ # add more files as needed
#                    ▼
flux2 <- read_delim("raw_data/raw_test_2.dat", skip = 2)
flux2 <- flux2 %>% #Here the last line is cut and all the columns are processed to match either as a numeric or character value
  slice(1:(n()-1)) %>%
  mutate(across(c(`;Plot`, `CO2 Ref`, `mb Ref`, `mbR Temp`, `Input A`, `Input B`, `Input C`, `Input D`, `Input E`, `Input F`, ATMP), as.numeric)) %>%
  mutate(across(c(RecNo, `Input G`, `Input H`, `Probe Type`), as.character))
flux2 <- flux2 %>%
  unite(Month, Day, col = "Date", sep = "-") %>%
  unite(Hour, Min, col = "Time", sep = ":") %>%
  unite(Date, Time, col = "Day_ID", sep = " ")
#
flux_all <- bind_rows(flux1,flux2, .id = NULL) %>%
  rename("Plot" = ";Plot",
         "CO2.Ref" = `CO2 Ref`, # DC, concentration in ppm
         "mb.Ref" = `mb Ref`, # RH, relative humidity sensor, if attached
         "mbR.Temp" = `mbR Temp`, # Temperature of RH sensor
         "Input.A" = `Input A`, # PAR from PAR sensor
         "Input.B" = `Input B`, # RH, from chamber
         "Input.C" = `Input C`, # Temperature of soil
         "Input.D" = `Input D`, # DC, change in concentration in ppm
         "Input.E" = `Input E`, # DT, change in time in sec
         "Input.F" = `Input F`, # SR rate, g (CO2) m^2 Hour^-1
         "Input.G" = `Input G`, # Not used
         "Input.H" = `Input H`, # +/- SR rate, 00 if respiration (CO2 increase), 01 if CO2 decrease
         "Probe.Type" = `Probe Type`)
#
#
#
# ### Combine TinyTag temperatures ###
#
# Load TinyTag temperature measurements
#
# ════════════════════╗
#                     ▼
TinyTag1 <- read_csv("raw_data/raw_test_airT.csv", skip = 5, col_names = c("Record", "Date_time", "Max_Temp", "AirT", "Min_Temp"))
TinyTag1 <- TinyTag1 %>% # Split temperature from the unit and Date and time. Set temperature as numeric
  separate(AirT, sep = " ", into = c("AirT", "Unit")) %>%
  separate(Date_time, sep = " ", into = c("Date", "Time")) %>%
  separate(Date, sep = "-", into = c("Year", "Month", "Day")) %>%
  separate(Time, sep = ":", into = c("hour", "min", "sec")) %>%
  unite(Month, Day, col = "Date", sep = "-") %>%
  unite(hour, min, col = "Time", sep = ":") %>%
  unite(Date, Time, col = "Day_ID", sep = " ") %>%
  mutate(across(c(AirT), as.numeric))
#
# Create data frame with only Day_ID and the average temperature
TinyTag_mini <- tibble("Day_ID" = TinyTag1$Day_ID,"AirT" = TinyTag1$AirT)
#
# Combine and have 
flux_all_Temp <- left_join(flux_all, TinyTag_mini, by = "Day_ID")
#
#
# ### Combine PAR QED sensor from EM50 ###
#
# ═════════════════╗
#                  ▼
fPAR1 <- read_xls("raw_data/raw_test_em50_PAR.xls", col_names = c("Date_time", "PAR1", "PAR2", "PAR3", "PAR4", "PAR5"), skip = 3, col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric"))
fPAR <- fPAR1 %>%
  separate(Date_time, sep = " ", into = c("Date", "Time")) %>%
  separate(Date, sep = "-", into = c("Year", "Month", "Day")) %>%
  separate(Time, sep = ":", into = c("hour", "min", "sec")) %>%
  unite(Month, Day, col = "Date", sep = "-") %>%
  unite(hour, min, col = "Time", sep = ":") %>%
  unite(Date, Time, col = "Day_ID", sep = " ")
#
# Remove empty columns and keep the sensor where PAR was measured on
fDay_ID <- flux_all %>% select(Day_ID) %>% distinct(Day_ID)
#
fPAR_removal <- fDay_ID %>%
  left_join(fPAR, by = "Day_ID") %>%
  select(c("PAR1", "PAR2", "PAR3", "PAR4", "PAR5"))
PAR_colSums <- (colSums(fPAR_removal, na.rm=T) != 0) # T if colSum is not 0, F otherwise
Positive_PAR  <- fPAR_removal[, PAR_colSums] %>% # all the non-zero columns
  add_column(fDay_ID, .before = TRUE)
Positive_PAR <- Positive_PAR %>% rename("PAR" = colnames(Positive_PAR[2]))
#
# Combine, Temperature and PAR
flux_all_TempPAR <- full_join(flux_all_Temp, Positive_PAR, by = "Day_ID")
#
fluxdata <- flux_all_TempPAR
#
#
#
# Check length of each measurement so that they are all equal
#
flux_recLength<-matrix(NA,nrow = max(flux_all$Plot),ncol = 2, dimnames = list(c(),c("Plot", "Recordings")))
#matrix(NA,nrow = max(flux_all$Plot), ncol = 2)
for (w in flux_all$Plot){
  flux_recLength[w,2] <- length(which(flux_all$Plot==w))
  flux_recLength[w,1] <- w
}
flux_recLength <- as_tibble(flux_recLength)
#
#
# Move data according to whether they are 2min (27), 5min (64), 10min (127), or 15min (189)
# The EGM4 measures every 4.8s and therefor 2min (120s) becomes 27 lines (~124s)
#
twoMin <- 27
fiveMin <- 64
tenMin <- 127
fifteenMin <- 189
#
# 2min measurements
#
fluxdata_2min <- fluxdata %>%
  left_join(flux_recLength, by = "Plot") %>%
  filter(Recordings == twoMin)
if (nrow(fluxdata_2min)*ncol(fluxdata_2min)>0){
  for (v in max(fluxdata_2min$Plot)){
    fluxdata_2min$Plot[(1+twoMin*(v-1)):(twoMin+twoMin*(v-1))] = 0 + v
  }
#
# ═══════════════════════════╗
#                            ▼
  write_delim(fluxdata_2min,"clean_data/Test_clean_2min.dat", delim = "\t")
}
#
# 5min measurements
#
fluxdata_5min <- fluxdata %>%
  left_join(flux_recLength, by = "Plot") %>%
  filter(Recordings == fiveMin)
if (nrow(fluxdata_5min)*ncol(fluxdata_5min)>0){
  for (v in max(fluxdata_5min$Plot)){
    fluxdata_5min$Plot[(1+fiveMin*(v-1)):(fiveMin+fiveMin*(v-1))] = 0 + v
  }
#
# ═══════════════════════════╗
#                            ▼
  write_delim(fluxdata_5min,"clean_data/Test_clean_5min.dat", delim = "\t")
}
#
# 10min measurements
#
fluxdata_10min <- fluxdata %>%
  left_join(flux_recLength, by = "Plot") %>%
  filter(Recordings == tenMin)
if (nrow(fluxdata_10min)*ncol(fluxdata_10min)>0){
  for (v in max(fluxdata_10min$Plot)){
    fluxdata_10min$Plot[(1+tenMin*(v-1)):(tenMin+tenMin*(v-1))] = 0 + v
  }
#
# ════════════════════════════╗
#                             ▼
  write_delim(fluxdata_10min,"clean_data/Test_clean_10min.dat", delim = "\t")
}
#
# 15min measurements
#
fluxdata_15min <- fluxdata %>%
  left_join(flux_recLength, by = "Plot") %>%
  filter(Recordings == fifteenMin)
if (nrow(fluxdata_15min)*ncol(fluxdata_15min)>0){
  for (v in max(fluxdata_15min$Plot)){
    fluxdata_15min$Plot[(1+fifteenMin*(v-1)):(fifteenMin+fifteenMin*(v-1))] = 0 + v
  }
#
# ════════════════════════════╗
#                             ▼
  write_delim(fluxdata_15min,"clean_data/Test_clean_10min.dat", delim = "\t")
}
#
# leftover in need of manual checking
#
fluxdata_left <- fluxdata %>%
  left_join(flux_recLength, by = "Plot") %>%
  filter(Recordings != twoMin & Recordings != fiveMin & Recordings != tenMin & Recordings != fifteenMin)
if (nrow(fluxdata_left)*ncol(fluxdata_left)>0){
#
# ═══════════════════════════╗
#                            ▼
  write_delim(fluxdata_left,"clean_data/Test_clean_left.dat", delim = "\t")
}
#
# The End