# Data cleaning and combining
# For EGM5
# Using CPY mode (M3)
#
# By Emil A.S. Andersen
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
#library(readxl) # For reading excel files with the "read_excel" command
#
#
# # Combine several data files #
#
# The EGM5 has a large storage, but it might still be relevant to combine several files either for same day or several days.
# This does not assume similar length of each measurements, but later parts might!
#
# If an external TinyTag sensor is used for air temperature, it can be useful to not have to manually combine the egm5 file with the TinyTag data. Can also combine with an external PAR sensor logging on an EM50 logger
#
#
# ══════════════════════════════════╗
#                                   ▼
flux1 <- read_delim("raw_data/delim/xx.txt", delim="\t", col_types = list("c","?","?","n","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c"))
flux1 <- flux1 %>% # Here the "start," "end," and "zero" lines are cut and all the columns are processed to match either as a numeric or character value
  filter(!is.na(CO2)) %>%
  separate(Time, into = c("Hour", "Min", "Sec"), sep = ":") %>% # We will not match the other measurements to the second when combining!
  unite(Hour, Min, col = "Time", sep = ":") %>%
  unite(Date, Time, col = "Day_ID", sep = " ")
#
#
# ════════════════════════════════════╗ # add more files as needed
#                                     ▼
# flux2 <- read_delim("raw_data/delim/yy.txt", delim="\t", col_types = list("c","?","?","n","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c"))
# flux2 <- flux2 %>% # Here the "start," "end," and "zero" lines are cut and all the columns are processed to match either as a numeric or character value
#   filter(!is.na(CO2)) %>%
#   separate(Time, into = c("Hour", "Min", "Sec"), sep = ":") %>% # We will not match the other measurements to the second when combining!
#   unite(Hour, Min, col = "Time", sep = ":") %>%
#   unite(Date, Time, col = "Day_ID", sep = " ")
#
# If combining files
# flux_all <- bind_rows(flux1,flux2, .id = NULL) %>%
#   rename("Plot" = Plot_No,
#          "RecNo" = Rec_No,
#          "ATMP" = Pressure,
#          "Aux.V" = Aux_V,
#   #
#   #═══════╗
#   #       ▼
#          #"PAR.inner" = PAR, # If using internal PAR: Keep original name and comment this line
#          "SRL.Rate" = Flux, 
#          "SRQ.Rate" = QFlux)
#
# If using only one file
flux_all <- flux1 %>%
  rename("Plot" = Plot_No,
         "RecNo" = Rec_No,
         "ATMP" = Pressure,
         "Aux.V" = Aux_V,
  #
  #═══════╗
  #       ▼
         #"PAR.inner" = PAR, # If using internal PAR: Keep original name and comment this line
         "SRL.Rate" = Flux, 
         "SRQ.Rate" = QFlux)
#
fluxdata<-flux_all
#
#
# ### Combine TinyTag temperatures ###
#
# Load TinyTag temperature measurements
#
# ═════════════════════════════╗
#                              ▼
# TinyTag1 <- read_csv("raw_data/raw_test_airT.csv", skip = 5, col_names = c("Record", "Date_time", "Max_Temp", "AirT", "Min_Temp"))
# # Change accordingly, if only using average temperature and not max and min
# TinyTag1 <- TinyTag1 %>% # Split temperature from the unit and Date and time. Set temperature as numeric
#   separate(AirT, sep = " ", into = c("AirT", "Unit")) %>%
#   separate(Date_time, sep = " ", into = c("Date", "Time")) %>%
#   separate(Time, sep = ":", into = c("hour", "min", "sec")) %>%
#   unite(hour, min, col = "Time", sep = ":") %>%
#   unite(Date, Time, col = "Day_ID", sep = " ") %>%
#   mutate(across(c(AirT), as.numeric))
# #
# # Create data frame with only Day_ID and the average temperature
# TinyTag_mini <- tibble("Day_ID" = TinyTag1$Day_ID,"AirT" = TinyTag1$AirT)
# #
# # Combine and have
# flux_all_Temp <- left_join(flux_all, TinyTag_mini, by = "Day_ID")
# #
# #
# # ### Combine PAR QED sensor from EM50 ###
# #
# # ══════════════════════════╗
# #                           ▼
# fPAR1 <- read_xls("raw_data/raw_test_em50_PAR.xls", col_names = c("Date_time", "PAR1", "PAR2", "PAR3", "PAR4", "PAR5"), skip = 3, col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric"))
# fPAR <- fPAR1 %>%
#   separate(Date_time, sep = " ", into = c("Date", "Time")) %>%
#   separate(Time, sep = ":", into = c("hour", "min", "sec")) %>%
#   unite(hour, min, col = "Time", sep = ":") %>%
#   unite(Date, Time, col = "Day_ID", sep = " ")
# #
# # Remove empty columns and keep the sensor where PAR was measured on
# fDay_ID <- flux_all %>% select(Day_ID) %>% distinct(Day_ID)
# #
# fPAR_removal <- fDay_ID %>%
#   left_join(fPAR, by = "Day_ID") %>%
#   select(c("PAR1", "PAR2", "PAR3", "PAR4", "PAR5"))
# PAR_colSums <- (colSums(fPAR_removal, na.rm=T) != 0) # T if colSum is not 0, F otherwise
# Positive_PAR  <- fPAR_removal[, PAR_colSums] %>% # all the non-zero columns
#   add_column(fDay_ID, .before = TRUE)
# Positive_PAR <- Positive_PAR %>% rename("PAR" = colnames(Positive_PAR[2]))
# #
# # Combine, Temperature and PAR
# flux_all_TempPAR <- full_join(flux_all_Temp, Positive_PAR, by = "Day_ID")
# #
# fluxdata <- flux_all_TempPAR
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
# Move data according to whether they are 2min (121), 5min (301), 10min (601), or 15min (901)
# For the EGM5 there is a measurement per second and then a last one. So 2min (120s) becomes 121 measurements
#
twoMin <- 121
fiveMin <- 301
tenMin <- 601
fifteenMin <- 901
#
# 2min measurements
#
fluxdata_2min <- fluxdata %>%
  left_join(flux_recLength, by = "Plot") %>%
  filter(Recordings == twoMin) %>%
  select(-c("Recordings"))
if (nrow(fluxdata_2min)*ncol(fluxdata_2min)>0){
  for (v in max(fluxdata_2min$Plot)){
    fluxdata_2min$Plot[(1+twoMin*(v-1)):(twoMin+twoMin*(v-1))] = 0 + v
  }
  #
  # ════════════════════════════════════╗
  #                                     ▼
  write_delim(fluxdata_2min,"clean_data/ArcEco_zz_2min.dat", delim = "\t")
}
#
# 5min measurements
#
fluxdata_5min <- fluxdata %>%
  left_join(flux_recLength, by = "Plot") %>%
  filter(Recordings == fiveMin) %>%
  select(-c("Recordings"))
if (nrow(fluxdata_5min)*ncol(fluxdata_5min)>0){
  for (v in max(fluxdata_5min$Plot)){
    fluxdata_5min$Plot[(1+fiveMin*(v-1)):(fiveMin+fiveMin*(v-1))] = 0 + v
  }
  #
  # ════════════════════════════════════╗
  #                                     ▼
  write_delim(fluxdata_5min,"clean_data/ArcEco_zz_5min.dat", delim = "\t")
}
#
# 10min measurements
#
fluxdata_10min <- fluxdata %>%
  left_join(flux_recLength, by = "Plot") %>%
  filter(Recordings == tenMin) %>%
  select(-c("Recordings"))
if (nrow(fluxdata_10min)*ncol(fluxdata_10min)>0){
  for (v in max(fluxdata_10min$Plot)){
    fluxdata_10min$Plot[(1+tenMin*(v-1)):(tenMin+tenMin*(v-1))] = 0 + v
  }
  #
  # ═════════════════════════════════════╗
  #                                      ▼
  write_delim(fluxdata_10min,"clean_data/ArcEco_zz_10min.dat", delim = "\t")
}
#
# 15min measurements
#
fluxdata_15min <- fluxdata %>%
  left_join(flux_recLength, by = "Plot") %>%
  filter(Recordings == fifteenMin) %>%
  select(-c("Recordings"))
if (nrow(fluxdata_15min)*ncol(fluxdata_15min)>0){
  for (v in max(fluxdata_15min$Plot)){
    fluxdata_15min$Plot[(1+fifteenMin*(v-1)):(fifteenMin+fifteenMin*(v-1))] = 0 + v
  }
  #
  # ═════════════════════════════════════╗
  #                                      ▼
  write_delim(fluxdata_15min,"clean_data/ArcEco_zz_10min.dat", delim = "\t")
}
#
# leftover in need of manual checking
#
fluxdata_left <- fluxdata %>%
  left_join(flux_recLength, by = "Plot") %>%
  filter(Recordings != twoMin & Recordings != fiveMin & Recordings != tenMin & Recordings != fifteenMin)
if (nrow(fluxdata_left)*ncol(fluxdata_left)>0){
  #
  # ════════════════════════════════════╗
  #                                     ▼
  write_delim(fluxdata_left,"clean_data/ArcEco_zz_left.dat", delim = "\t")
}
#
# The End