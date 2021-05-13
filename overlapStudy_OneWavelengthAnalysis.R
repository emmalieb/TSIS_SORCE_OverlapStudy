##Name: overlapStudy_OneWavelengthAnalysis.R
#Purpose: build data frame of interpolated TSIS irradiance onto common SORCE wavelengths, then perform statistical analysis on that data frame following the procedure in Weatherhead et 1998
#Author: Emma Lieb
#Last updated: 05/12/2021
#Contact: emma.lieb@lasp.colorado.edu
#Directory: emli8337/SIST_PROJECT/DATA_AND_CODE/

library(tidyverse)
library(ghibli)
library(wesanderson)
#read in all the files-
#read sorce-sim irradiance (not tav) for comparison later
sorce_irrad <-  read.table("sorce_irradiance.txt", header = FALSE, sep = "", fill = TRUE, quote="", skip = 8)
sorce_irrad <- gather(sorce_irrad)
sorce_irrad <- sorce_irrad[2]
colnames(sorce_irrad) <- c("irradiance")
#read in TSIS irradiance v05
tsis_irrad <-  read.table("tsis_irrad.txt", header = FALSE, sep = "", fill = TRUE, quote="", skip = 8)
tsis_irrad <- gather(tsis_irrad)
tsis_irrad <- tsis_irrad[2]
colnames(tsis_irrad) <- c("irradiance")
#read in SORCE-SIM TAV irradiance v01 (new TAV on April 12th 2021)
tav_irrad <- read.table("sorce_tav_irrad.txt", header = FALSE, sep = "", fill = TRUE, quote="", skip = 8)
tav_irrad <- gather(tav_irrad)
tav_irrad <- tav_irrad[2]
colnames(tav_irrad) <- c("irradiance")
#read in common dates and wavelengths
dates_jdn <- read.table("jdcommon.txt", header = FALSE, sep = "", fill = TRUE, quote="")
#dates_iso <- read.table("isotime_common.txt", header = FALSE, sep = "", fill = TRUE, quote="")
waves <- read.table("wvlcommon.txt", header = FALSE, sep = "", fill = TRUE, quote="")

#tweak the days and wavelengths dataframes to match the format of the data
dates_jdn <- data.frame(A=dates_jdn, ntimes=1234)
dates_jdn <- as.data.frame(lapply(dates_jdn, rep, dates_jdn$ntimes))
dates_jdn <- dates_jdn[1]
waves <- do.call("rbind", replicate(n, waves, simplify = FALSE))

library("data.table")
library("fastmatch")

#read in new common dates after some were removed due to a lack of wavelengths on those days
new_jdn <- read.table("jdcommon_20210412.txt", header = FALSE, sep = "", fill = TRUE, quote="")
new_jdn <- data.frame(A=new_jdn, ntimes=1234)
new_jdn <- as.data.frame(lapply(new_jdn, rep, new_jdn$ntimes))
new_jdn <- new_jdn[1]
tsis_data <- cbind(dates_jdn, waves,tsis_irrad)
#get days that were removed from common dates
newdata <- setdiff(dates_jdn, new_jdn)
#remove the entries from the tsis data so that it matches the new tav data 
tsis_data <- tsis_data[!(tsis_data$V1 %in% c(2458246, 2458375,2458408,2458492,2458572,2458592,2458609,2458634,2458648,2458668,2458698,2458760,2458894)),]

#combine all the data into one data frame and set the column names
data <- cbind(tsis_data, tav_irrad)
colnames(data)<- c("dates_jdn", "wavelength", "tsis_irradiance", "tav_irradiance")
#need all data to be numbers, for some reason the irradiances read in as "chr" objects so this command just makes them all the same object type
data$tav_irradiance <- as.numeric(data$tav_irradiance)
data$tsis_irradiance <- as.numeric(data$tsis_irradiance)

#remove unused variables for clarity - not necessary just cleans up the "Data" view to the right
rm(dates_jdn,new_jdn,newdata,tav_irrad,tsis_data,tsis_irrad)
#subset one day to check spectra
data_oneday <- subset(data, data$dates_jdn==2458201)
#plotting to check spectrum
ggplot(data_oneday)+geom_point( aes(x = wavelength, y = tav_irradiance, colour = "TAV"))+
  geom_point( aes(x = wavelength, y = tsis_irradiance, colour = "TSIS"))+
  xlab("Wavelength (nm)")+ylab("Irradiance (W/m^2/nm)")+ggtitle("Spectra of TAV and TSIS Interpolated")+
  labs(colour ="Data Product")+scale_colour_manual(values = wes_palette("Darjeeling1"))

#subset to one wave to check time series
desired_wave <-  477.44000244 #<------------------------------------- change this variable to look at different wavelengths
data_onewave <- subset(data, data$wavelength == desired_wave)
#remove NaNs (makes math easier later)
data_onewave<-NaRV.omit(data_onewave)
#get the days of overlap (a list from 1 to 541)
days <- seq_along(data_onewave$dates_jdn)
#create data frame for the one-wavelength subsetted data to plot and use later
data_onewave <- cbind(days,data_onewave$dates_jdn, data_onewave$tsis_irradiance, data_onewave$tav_irradiance)
data_onewave <- as.data.frame(data_onewave)
colnames(data_onewave) <- c("days","dates_jdn","tsis_irradiance","tav_irradiance")
#plotting to check time series
ggplot(data_onewave)+geom_line( aes(x = days, y = tav_irradiance, colour = "TAV"), size = 1)+
  geom_line( aes(x = days, y = tsis_irradiance, colour = "TSIS"), size = 1)+ylim(2.094, 2.097)+
  xlab("Overlap Days")+ylab("Irradiance (W/m^2/nm)")+#+ggtitle(paste("Time Series of TAV and TSIS at wave",desired_wave))+
  labs(colour ="Instrument")+scale_colour_manual(values =wes_palette("BottleRocket2"))#+theme_minimal()#scale_colour_ghibli_d("YesterdayMedium", direction = -1)
#save plot in high resolution, defaults to save in current working directory
ggsave("timeseries_477_overlap_last.png",plot = last_plot(), dpi=300)

#get autocorrelation of irradiances at one wavelength - this function produces a plot and a list of autocorrelations as a function of lag
acf_tsis <- acf(data_onewave$tsis_irradiance)
acf_tav <- acf(data_onewave$tav_irradiance)
#create variable for the number of days of data (may be needed later)
n <- 540
#bin the data according to Jerry and Steve P's description, 15 day bins = 47 total bins, take the mean in each bin
library("rbin")
library("mltools")
#these are the days that should "cut" up the time series every 15 days - but this variable isnt used in binning 
cut_points <- c(2458201.0,2458216.0,2458231.0,2458246.0,2458261.0,458276.0,2458291.0,2458306.0,2458321.0,2458336.0,2458351.0,2458366.0,2458381.0,2458396.0,2458411.0,2458426.0,2458441.0,2458456.0,2458471.0,2458486.0,2458501.0,2458516.0,2458531.0,2458546.0,2458561.0,2458576.0,2458591.0,2458606.0,2458621.0,2458636.0,2458651.0,2458666.0,2458681.0,2458696.0,2458711.0,2458726.0,2458741.0,2458756.0,2458771.0,2458786.0,2458801.0,2458816.0,2458831.0,2458846.0,2458861.0,2458876.0,2458891.0)
#from here to line 109 is the binning/averaging
step = 15
tav_mean_table <- data_onewave %>% 
  mutate(window = data_onewave$dates_jdn %/% step) %>% 
  group_by(window) %>% summarise(tav_irradiance = mean(tav_irradiance)) %>%
  mutate(start = window*step, stop=(window+1)*step) %>%
  select(-window)
tsis_mean_table <- data_onewave %>% 
  mutate(window = data_onewave$dates_jdn %/% step) %>% 
  group_by(window) %>% summarise(tsis_irradiance = mean(tsis_irradiance)) %>%
  mutate(start = window*step, stop=(window+1)*step) %>%
  select(-window)
#make data frame of binned mean values
binned_data <- as.data.frame(cbind(seq_along(tav_mean_table$start), tav_mean_table$tav_irradiance, tsis_mean_table$tsis_irradiance))
colnames(binned_data) <- c("binned_days", "tav_mean", "tsis_mean")
#plotting binned mean irradiance for each instrument over time
ggplot(binned_data)+geom_line(aes(binned_days, tav_mean, colour = "TAV"),size = 2)+geom_line(aes(binned_data$binned_days,binned_data$tsis_mean, colour = "TSIS"),size = 2)+
  xlab("Binned Days")+ylab("Mean Irradiance")+labs(colour ="Instrument")+scale_colour_manual(values =wes_palette("BottleRocket2"))#+ggtitle(paste("Mean Irradiance in 15-day bins for TSIS and TAV at ",desired_wave))#+scale_colour_manual(values =c("#DD517F", "#E68E36"))
#save the plot in high-res, defaults to current working directory
ggsave("binned_avg_overlap_477_last.png",plot = last_plot(), dpi=300)

#remove unnecessary variables for clarity in the "Data" window
rm(tav_mean_table,tsis_mean_table)

#get autocorrelations of binned data sets
tav_binned_autocorr <- acf(binned_data$tav_mean)
tav_binned_autocorr <- 0.265 #<------------------- lag of 1
tsis_binned_autocorr <- acf(binned_data$tsis_mean)
tsis_binned_autocorr<-  0.653 #<------------------ lag of 1

#gather some necessary parameters for noise simulation:
#get mean of each binned data set
tav_tot_mean <- mean(binned_data$tav_mean)
tsis_tot_mean <- mean(binned_data$tsis_mean)
#get standard deviation of each binned data set
tav_std <- sd(binned_data$tav_mean)
tsis_std <- sd(binned_data$tsis_mean)
#get the overlap days from 0 to 541
TT <- seq_along(binned_data$binned_days)
#get difference of binned data sets
binned_diff <- binned_data$tav_mean - binned_data$tsis_mean
#get autocorrelation of binned difference
acf_bin_diff <- acf(binned_diff)
autocorr_bin_diff <- 0.273
#recreate data frame of binned data (now with difference column) and set column names
binned_data <- as.data.frame(cbind(TT, binned_data$tav_mean, binned_data$tsis_mean, binned_diff))
colnames(binned_data) <- c("days", "tav_mean", "tsis_mean","binned_diff")

#plot difference of binned data
ggplot(binned_data)+geom_line(aes(days,binned_diff),color = "#E68E36", size = 2)+
  xlab("Binned Days")+ylab("Binned Difference")#+ggtitle(paste("Difference in binned data, TAV-TSIS, at ",desired_wave))
ggsave("binned_diff_overlap_477_last.png",plot = last_plot(), dpi=300)

#simulating noise for binned TAV from formula for N_t in Weatherhead et al 1998
Nt_tav <- rnorm(TT, mean = tav_tot_mean, sd = tav_std) #<--- initialize variable for noise term
epsilon_tav <- rnorm(TT,0,tav_std) #<-- white noise term
#loop through the number of days and add the noise term for each day -> Nt = phi*Nt_1 + episilon 
for (t in 2:TT) {
  Nt_tav[t] <- tav_binned_autocorr*Nt_tav[t - 1] + epsilon_tav[t]
}
#calculating noise for binned TSIS from formula for N_t in Weatherhead et al 1998
Nt_tsis <- rnorm(TT, mean = tsis_tot_mean, sd = tsis_std)#<--- initialize variable for noise term
epsilon_tsis <- rnorm(TT,0,tsis_std) #<-- white noise term
#loop through the number of days and add the noise term for each day -> Nt = phi*Nt_1 + episilon 
for (t in 2:TT) {
  Nt_tsis[t] <- tsis_binned_autocorr*Nt_tsis[t - 1] + epsilon_tsis[t]
}

#make data frame of noise
noise_df <- data.frame(TT,Nt_tav, Nt_tsis)
#remove major outlier from noise - messes with the trend otherwise
noise_df<- noise_df[-c(2), ]
#get trend in TAV with linear least squares fit 
fit_tav <- lm(noise_df$Nt_tav~noise_df$TT)
intercept_tav <- fit_tav$coefficients[[1]]
slope_tav <- fit_tav$coefficients[[2]]
#get trend in TSIS with linear least squares fit 
fit_tsis <- lm(noise_df$Nt_tsis~noise_df$TT)
intercept_tsis <- fit_tsis$coefficients[[1]]
slope_tsis <- fit_tsis$coefficients[[2]]
#plot the noise in each instrument separately with trend lines
ggplot(noise_df)+geom_line(aes(TT,Nt_tav, colour = "TAV"), size = 2)+geom_line(aes(TT,Nt_tsis, colour = "TSIS"), size = 2)+
  ylab("N_t")+xlab("Binned Days")+labs(colour ="Instrument")+#ylim(2.095,2.0975)++ggtitle(paste("Noise in binned TSIS and TAV at", desired_wave))
  scale_colour_manual(values = c("#6D5B87","#EC745C"))+geom_line(aes(TT,slope_tav*TT+intercept_tav),color = "#6D5B87", linetype = "dashed", size = 1)+geom_line(aes(TT,slope_tsis*TT+intercept_tsis),color = "#EC745C", linetype = "dashed", size = 1)
ggsave("binned_noise_trend_overlap_477_last.png",plot = last_plot(), dpi=300) 

#take difference of binned noises
noise_diff <- noise_df$Nt_tav - noise_df$Nt_tsis
noise_df <- data.frame(cbind(noise_df$TT, noise_df$Nt_tav, noise_df$Nt_tsis, noise_diff))
colnames(noise_df)<-c("TT","Nt_tav","Nt_tsis","noise_diff")
#get trend for difference in noise
fit <- lm(noise_df$noise_diff~noise_df$TT)
intercept <- fit$coefficients[[1]]
slope <- fit$coefficients[[2]]
#plot difference in noise
ggplot(noise_df)+geom_line(aes(TT, noise_diff), color = "#5D7F61",size = 2)+xlab("Binned Overlap Days")+
  ylab("Difference in Noise")+geom_line(aes(TT,slope*TT+intercept), linetype = "dashed", size = 1)#+ggtitle(paste("Difference in Noise of TAV and TSIS with Trend at ",desired_wave))
ggsave("noise_diff_trend_overlap_477_last.png",plot = last_plot(), dpi=300) 
#get standard deviation of noises
std_noise_tav <- sd(noise_df$Nt_tav)
std_noise_tsis <- sd(noise_df$Nt_tsis)
#get standard deviation of the difference in the noise
std_noise_diff <- sd(noise_df$noise_diff)
#check the "decision" rule from Weatherhead et al 1998
fit_line <- slope*TT+intercept
#this is formula (2)from weatehrhead et al 1998 section 2.2 page 17150
std_fit <- (std_noise_diff/(TT^(3/2)))*((1+autocorr_bin_diff)/(1-autocorr_bin_diff))
mean_fit_line <- mean(fit_line)
decision_rule <- abs(fit_line / std_fit)
#take the mean of the array produced by the decision rule variable - if the mean is greater than two then the trend is statistically significant and the decision rule is satisfied
mean_decision_rule <- mean(decision_rule)
#get mean of the binned difference
binned_diff_mean <- mean(binned_diff)

#formula (3) from Weatherhead et al 1998 -- the final answer is to calculate the number of years of overlap necessary to detect a trend
auto_corr <-  0.273 #<------ this is the autocorrelation of the difference in the binned datasets at lag = 1
omega_0 <- abs(.005*binned_diff_mean)#<---- (omega_0 = 0.05% per year of the mean of the difference, this variable can be changed to check different trends)
n <- (((3.3*std_noise_diff)/(abs(omega_0)))*(sqrt((1+auto_corr)/(1-auto_corr))))^(2/3) 
n <- n/365
