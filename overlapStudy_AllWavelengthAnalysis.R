##Name: overlapStudy_AllWavelengthAnalysis.R
#Purpose: build data frame of integrated spectra in 3 wavelength ranges of TSIS and SORCE, then perform statistical analysis on that data frame following the procedure in Weatherhead et 1998
#Author: Emma Lieb
#Last updated: 05/13/2021
#Contact: emma.lieb@lasp.colorado.edu
#Directory: emli8337/SIST_PROJECT/DATA_AND_CODE/

#compare integrated wavelength ranges --- everything from here on is in repetitive for each range but it wouldve taken too long to autmoate with a function
#read data from files
tsis_vis <- read.table("tsis_irrad_vis_20210412.txt", header = FALSE, sep = "", fill = TRUE, quote="")
tsis_ir <- read.table("tsis_irrad_ir_20210412.txt", header = FALSE, sep = "", fill = TRUE, quote="")
tsis_uv <- read.table("tsis_irrad_uv_20210412.txt", header = FALSE, sep = "", fill = TRUE, quote="")
tav_vis <- read.table("sorce_tav_irrad_vis_20210412.txt", header = FALSE, sep = "", fill = TRUE, quote="")
tav_ir <- read.table("sorce_tav_irrad_ir_20210412.txt", header = FALSE, sep = "", fill = TRUE, quote="")
tav_uv <- read.table("sorce_tav_irrad_uv_20210412.txt", header = FALSE, sep = "", fill = TRUE, quote="")
#get days column 
days <- seq_along(tsis_vis$V1)
days <- as.numeric(days)
#create data frame of all the data
integrated_df <- cbind(days, tsis_uv, tsis_vis, tsis_ir, tav_uv, tav_vis, tav_ir)
#set column names
colnames(integrated_df) <- c("days", "tsis_uv", "tsis_vis", "tsis_ir","tav_uv", "tav_vis", "tav_ir")

#plot TSIS UV alone
ggplot(integrated_df)+geom_line(aes(days,tsis_uv),color = "#64CFF7")+xlab("Days")+ylab("Integrated Irradiance")+ggtitle("TSIS UV")
#plot TSIS IR alone
ggplot(integrated_df)+geom_line(aes(days,tsis_ir),color = "#FF68A8")+xlab("Days")+ylab("Integrated Irradiance")+ggtitle("TSIS IR")
#plot TSIS VIS alone
ggplot(integrated_df)+geom_line(aes(days,tsis_vis),color = "#43AA47")+xlab("Days")+ylab("Integrated Irradiance")+ggtitle("TSIS VIS")

#plot TAV UV alone
ggplot(integrated_df)+geom_line(aes(days,tav_uv),color = "#64CFF7")+xlab("Days")+ylab("Integrated Irradiance")+ggtitle("TAV UV")
#plot TAV IR alone
ggplot(integrated_df)+geom_line(aes(days,tav_ir),color = "#FF68A8")+xlab("Days")+ylab("Integrated Irradiance")+ggtitle("TAV IR")
#plot TAV VIS alone
ggplot(integrated_df)+geom_line(aes(days,tav_vis),color = "#43AA47")+xlab("Days")+ylab("Integrated Irradiance")+ggtitle("TAV VIS")

#plot tav and tsis together for each wavelength range - UV
ggplot(integrated_df)+geom_line(aes(days, tsis_uv, colour = "TSIS UV"), size = 1)+
  geom_line(aes(days, tav_uv,colour = "TAV UV"), size = 1)+scale_colour_manual(values = c("#4A88C5","#E8C539"))+labs(colour ="Instrument")+
  xlab("Overlap Days")+ylab("Integrated Irradiance")+ylim(15.68,15.73)#+ggtitle("TAV and TSIS Integrated UV Irradiance Over Time")
#save the plot in higher resolution- defaults to current working directory
ggsave("UV_zoom_last.png",plot = last_plot(), dpi=300) 
#plot tav and tsis together for each wavelength range - VIS
ggplot(integrated_df)+geom_line(aes(days, tsis_vis, colour = "TSIS VIS"), size = 1)+
  geom_line(aes(days, tav_vis,colour = "TAV VIS"), size = 1)+scale_colour_manual(values = c("#A653F5","#F96CFF"))+labs(colour ="Instrument")+
  xlab("Overlap Days")+ylab("Integrated Irradiance")+ylim(892.88,893.4)#+ggtitle("TAV and TSIS Integrated VIS Irradiance Over Time")
#save the plot in higher resolution- defaults to current working directory
ggsave("VIS_zoom_last.png",plot = last_plot(), dpi=300) 
#plot tav and tsis together for each wavelength range - IR
ggplot(integrated_df)+geom_line(aes(days, tsis_ir, colour = "TSIS IR"),size = 1)+
  geom_line(aes(days, tav_ir,colour = "TAV IR"),size = 1)+scale_colour_manual(values = c("#43AA47","#6D5B87"))+labs(colour ="Instrument")+
  xlab("Overlap Days")+ylab("Integrated Irradiance")+ylim(293.55,293.8)#+ggtitle("TAV and TSIS Integrated IR Irradiance Over Time")
#save the plot in higher resolution- defaults to current working directory
ggsave("IR_zoom.png",plot = last_plot(), dpi=300) 
#get autocorrelations of wavelength ranges - this function produces a plot and array of the autocorrelations as a function of lag
acf_tsis_uv <- acf(tsis_uv)
acf_tsis_vis <- acf(tsis_vis)
acf_tsis_ir <- acf(tsis_ir)
acf_tav_uv <- acf(tav_uv)
acf_tav_vis <- acf(tav_vis)
acf_tav_ir <- acf(tav_ir)
#get autocorrelation for each range with lag = 15 (15 because they arent binned yet)
autocorr_tsis_uv <- 0.624
autocorr_tsis_vis <- 0.250
autocorr_tsis_ir <- 0.528
autocorr_tav_uv <- 0.178
autocorr_tav_vis <- -0.006
autocorr_tav_ir <- 0.287
#get mean of all wavelength ranges for both instruments
mean_tsis_uv <- mean(tsis_uv$V1) #<-- the V1 here is because I did not set column names
mean_tsis_vis <- mean(tsis_vis$V1)
mean_tsis_ir<- mean(tsis_ir$V1)
mean_tav_uv <- mean(tav_uv$V1)
mean_tav_vis <- mean(tav_vis$V1)
mean_tav_ir<- mean(tav_ir$V1)
#get standard deviations of all wavelength ranges for both instruments
std_tsis_uv <- sd(tsis_uv$V1)
std_tsis_vis <- sd(tsis_vis$V1)
std_tsis_ir<- sd(tsis_ir$V1)
std_tav_uv <- sd(tav_uv$V1)
std_tav_vis <- sd(tav_vis$V1)
std_tav_ir<- sd(tav_ir$V1)

#simulate the noise in each instrument in each wavelength range
#simulate noise in tsis - UV
Nt_tsis_uv <- rnorm(days, mean = mean_tsis_uv, sd = std_tsis_uv)#<-- initialize noise term to be populated in loop
epsilon_tsis_uv <- rnorm(days,0,std_tsis_uv) #<-- white noise term
#loop through the number of days and add the noise term for each day -> Nt = phi*Nt_1 + episilon 
for (t in 2:days) {
  Nt_tsis_uv[t] <- autocorr_tsis_uv*Nt_tsis_uv[t - 1] + epsilon_tsis_uv[t]
}
#simulate noise in tsis - VIS
Nt_tsis_vis <- rnorm(days, mean = mean_tsis_vis, sd = std_tsis_vis)#<-- initialize noise term to be populated in loop
epsilon_tsis_vis <- rnorm(days,0,std_tsis_vis) #<-- white noise term
#loop through the number of days and add the noise term for each day -> Nt = phi*Nt_1 + episilon 
for (t in 2:days) {
  Nt_tsis_vis[t] <- autocorr_tsis_vis*Nt_tsis_vis[t - 1] + epsilon_tsis_vis[t]
}
#simulate noise in tsis - IR
Nt_tsis_ir <- rnorm(days, mean = mean_tsis_ir, sd = std_tsis_ir) #<-- initialize noise term to be populated in loop
epsilon_tsis_ir <- rnorm(days,0,std_tsis_ir) #<-- white noise term
#loop through the number of days and add the noise term for each day -> Nt = phi*Nt_1 + episilon 
for (t in 2:days) {
  Nt_tsis_ir[t] <- autocorr_tsis_ir*Nt_tsis_ir[t - 1] + epsilon_tsis_ir[t]
}
#simulate noise in tav - UV
Nt_tav_uv <- rnorm(days, mean = mean_tav_uv, sd = std_tav_uv)#<-- initialize noise term to be populated in loop
epsilon_tav_uv <- rnorm(days,0,std_tav_uv) #<-- white noise term
#loop through the number of days and add the noise term for each day -> Nt = phi*Nt_1 + episilon 
for (t in 2:days) {
  Nt_tav_uv[t] <- autocorr_tav_uv*Nt_tav_uv[t - 1] + epsilon_tav_uv[t]
}
#simulate noise in tav - VIS
Nt_tav_vis <- rnorm(days, mean = mean_tav_vis, sd = std_tav_vis)#<-- initialize noise term to be populated in loop
epsilon_tav_vis <- rnorm(days,0,std_tav_vis) #<-- white noise term
#loop through the number of days and add the noise term for each day -> Nt = phi*Nt_1 + episilon 
for (t in 2:days) {
  Nt_tav_vis[t] <- autocorr_tav_vis*Nt_tav_vis[t - 1] + epsilon_tav_vis[t]
}
#simulate noise in tav - IR
Nt_tav_ir <- rnorm(days, mean = mean_tav_ir, sd = std_tav_ir)#<-- initilize noise term to be populated in loop
epsilon_tav_ir <- rnorm(days,0,std_tav_ir) #<-- white noise term
#loop through the number of days and add the noise term for each day -> Nt = phi*Nt_1 + episilon 
for (t in 2:days) {
  Nt_tav_ir[t] <- autocorr_tav_ir*Nt_tav_ir[t - 1] + epsilon_tav_ir[t]
}
#create data frame of all the noise data with the days column for plotting and math later
integrated_noise_df <- as_data_frame(cbind(days,Nt_tsis_uv,Nt_tav_uv,Nt_tsis_vis,Nt_tav_vis,Nt_tsis_ir,Nt_tav_ir))
#set column names of noise data frame
colnames(integrated_noise_df)<-c("days","Nt_tsis_uv","Nt_tav_uv","Nt_tsis_vis","Nt_tav_vis","Nt_tsis_ir","Nt_tav_ir")

#plot tav and tsis noise together for each wavelength range - UV
ggplot(integrated_noise_df)+geom_line(aes(days, Nt_tsis_uv, colour = "TSIS UV"), size = 1)+labs(colour ="Instrument")+
  geom_line(aes(days, Nt_tav_uv,colour = "TAV UV"), size =1)+scale_colour_manual(values = c("#4A88C5","#E8C539"))+
  xlab("Overlap Days")+ylab("Integrated Irradiance")+ylim(15.68,15.73)#+ggtitle("Noise in TAV and TSIS Integrated UV Irradiance Over Time")
ggsave("UV_noise.png",plot = last_plot(), dpi=300) 
#plot tav and tsis noise together for each wavelength range - VIS
ggplot(integrated_noise_df)+geom_line(aes(days, Nt_tsis_vis, colour = "TSIS VIS"))+labs(colour ="Instrument")+
  geom_line(aes(days, Nt_tav_vis,colour = "TAV VIS"))+scale_colour_manual(values = c("#A653F5","#F96CFF"))+
  xlab("Overlap Days")+ylab("Integrated Irradiance")+ylim(850,930)#+ggtitle("Noise in TAV and TSIS Integrated VIS Irradiance Over Time")
ggsave("VIS_noise.png",plot = last_plot(), dpi=300) 
#plot tav and tsis noise together for each wavelength range - IR
ggplot(integrated_noise_df)+geom_line(aes(days, Nt_tsis_ir, colour = "TSIS IR"))+
  geom_line(aes(days, Nt_tav_ir,colour = "TAV IR"))+scale_colour_manual(values = c("#43AA47","#6D5B87"))+
  xlab("Overlap Days")+ylab("Noise")+ylim(293.55,293.8)#+ggtitle("Noise in TAV and TSIS Integrated IR Irradiance Over Time")
ggsave("IR_noise.png",plot = last_plot(), dpi=300) 

#get differences in noises for each wavelength range
Nt_uv_diff <- Nt_tav_uv - Nt_tsis_uv
Nt_vis_diff <- Nt_tav_vis - Nt_tsis_vis
Nt_ir_diff<- Nt_tav_ir - Nt_tsis_ir
#get trend lines in noise differences for each wavelength range
fit_uv <- lm(integrated_noise_df$Nt_uv_diff~integrated_noise_df$days)
intercept_uv <- fit_uv$coefficients[[1]]
slope_uv <- fit_uv$coefficients[[2]]

fit_vis <- lm(integrated_noise_df$Nt_vis_diff~integrated_noise_df$days)
intercept_vis <- fit_vis$coefficients[[1]]
slope_vis <- fit_vis$coefficients[[2]]

fit_ir <- lm(integrated_noise_df$Nt_ir_diff~integrated_noise_df$days)
intercept_ir <- fit_ir$coefficients[[1]]
slope_ir <- fit_ir$coefficients[[2]]

#add differences to the noise data frame so it can be plotted
integrated_noise_df <- as_data_frame(cbind(days,Nt_tsis_uv,Nt_tav_uv,Nt_tsis_vis,Nt_tav_vis,Nt_tsis_ir,Nt_tav_ir,Nt_uv_diff,Nt_vis_diff,Nt_ir_diff))
colnames(integrated_noise_df)<-c("days","Nt_tsis_uv","Nt_tav_uv","Nt_tsis_vis","Nt_tav_vis","Nt_tsis_ir","Nt_tav_ir","Nt_uv_diff","Nt_vis_diff","Nt_ir_diff")
#plot difference in UV noise
ggplot(integrated_noise_df)+geom_line(aes(days, Nt_uv_diff), colour = "#4A88C5")+
  xlab("Overlap Days")+ylab("Difference in Noise")+ggtitle("Difference in Noise in TAV and TSIS Integrated UV Irradiance")+
  geom_line(aes(days,slope_uv*days+intercept_uv),color = "#6D5B87", linetype = "dashed")+
  ylim(mean(Nt_uv_diff)-.051,mean(Nt_uv_diff)+.055)
ggsave("UV_noise_diff.png",plot = last_plot(), dpi=300) 
#plot difference in VIS noise
ggplot(integrated_noise_df)+geom_line(aes(days, Nt_vis_diff), colour = "#43AA47")+
  xlab("Overlap Days")+ylab("Difference in Noise")+ggtitle("Difference in Noise in TAV and TSIS Integrated VIS Irradiance")+
  geom_line(aes(days,slope_vis*days+intercept_vis),color = "#6D5B87", linetype = "dashed")+
  ylim(-40,40)
ggsave("VIS_noise_diff.png",plot = last_plot(), dpi=300) 
#plot difference in IR noise
ggplot(integrated_noise_df)+geom_line(aes(days, Nt_ir_diff), colour = "#7C1715")+
  xlab("Overlap Days")+ylab("Difference in Noise")+ggtitle("Difference in Noise in TAV and TSIS Integrated IR Irradiance")+
  geom_line(aes(days,slope_ir*days+intercept_ir),color = "#6D5B87", linetype = "dashed")+
  ylim(-.13,.13)
ggsave("IR_noise_diff.png",plot = last_plot(), dpi=300) 
#get differences in wavelength ranges
uv_diff <- tav_uv - tsis_uv
vis_diff <- tav_vis-tsis_vis
ir_diff <- tav_ir- tsis_ir
#get mean of differences for each wavelength range
mean_uv_diff <- mean(uv_diff$V1)
mean_vis_diff <- mean(vis_diff$V1)
mean_ir_diff<- mean(ir_diff$V1)
#get standard deviation of differences for each wavelength range
std_uv_diff <- sd(uv_diff$V1)
std_vis_diff <- sd(vis_diff$V1)
std_ir_diff<- sd(ir_diff$V1)
#get standard deviation of noise differences for each wavelength range
std_nt_uv_diff <- sd(Nt_uv_diff)
std_nt_vis_diff <- sd(Nt_vis_diff)
std_nt_ir_diff <- sd(Nt_ir_diff)

#get autocorrelation of difference in each wavelength rangee
acf_uv_diff<-acf(uv_diff$V1)
acf_vis_diff<-acf(vis_diff$V1)
acf_ir_diff<-acf(ir_diff$V1)
autocorr_uv <- 0.060 #<------ lag = 15 because this data isnt binned 
autocorr_vis <- -0.006 #<------ lag = 15 because this data isnt binned 
autocorr_ir <- 0.337 #<------ lag = 15 because this data isnt binned 

#formula (3) from Weatherhead et al 1998 <--UV
omega_0_uv <- abs(.05*mean_uv_diff)#<---- (omega_0 = 0.5% per year of the mean of the difference, can be changed to check different trends)
n_uv <- (((3.3*std_nt_uv_diff)/(abs(omega_0_uv)))*(sqrt((1+autocorr_uv)/(1-autocorr_uv))))^(2/3) 
n_uv<- n_uv/365
#formula (3) from Weatherhead et al 1998 <--- VIS
omega_0_vis <- abs(.05*mean_vis_diff)#<---- (omega_0 = 0.5% per year of the mean of the difference, can be changed to check different trends)
n_vis <- (((3.3*std_nt_vis_diff)/(abs(omega_0_vis)))*(sqrt((1+autocorr_vis)/(1-autocorr_vis))))^(2/3) 
n_vis<- n_vis/365 
#formula (3) from Weatherhead et al 1998 <-- IR
omega_0_ir <- abs(.05*mean_ir_diff)#<---- (omega_0 = 0.5% per year of the mean of the difference, can be changed to check different trends)
n_ir <- (((3.3*std_nt_ir_diff)/(abs(omega_0_ir)))*(sqrt((1+autocorr_ir)/(1-autocorr_ir))))^(2/3) 
n_ir<- n_ir/365

#Bin all the wavelength range data and repeat everything with binned data
step = 15
tav_uv_mean_table <- integrated_df %>% 
  mutate(window = integrated_df$days %/% step) %>% 
  group_by(window) %>% summarise(tav_uv = mean(tav_uv)) %>%
  mutate(start = window*step, stop=(window+1)*step) %>%
  select(-window)
tav_vis_mean_table <- integrated_df %>% 
  mutate(window = integrated_df$days %/% step) %>% 
  group_by(window) %>% summarise(tav_vis = mean(tav_vis)) %>%
  mutate(start = window*step, stop=(window+1)*step) %>%
  select(-window)
tav_ir_mean_table <- integrated_df %>% 
  mutate(window = integrated_df$days %/% step) %>% 
  group_by(window) %>% summarise(tav_ir = mean(tav_ir)) %>%
  mutate(start = window*step, stop=(window+1)*step) %>%
  select(-window)
tsis_uv_mean_table <- integrated_df %>% 
  mutate(window = integrated_df$days %/% step) %>% 
  group_by(window) %>% summarise(tsis_uv = mean(tsis_uv)) %>%
  mutate(start = window*step, stop=(window+1)*step) %>%
  select(-window)
tsis_vis_mean_table <- integrated_df %>% 
  mutate(window = integrated_df$days %/% step) %>% 
  group_by(window) %>% summarise(tsis_vis = mean(tsis_vis)) %>%
  mutate(start = window*step, stop=(window+1)*step) %>%
  select(-window)
tsis_ir_mean_table <- integrated_df %>% 
  mutate(window = integrated_df$days %/% step) %>% 
  group_by(window) %>% summarise(tsis_ir = mean(tsis_ir)) %>%
  mutate(start = window*step, stop=(window+1)*step) %>%
  select(-window)
#create data frame of binned data and set column names
binned_integrated_df <- cbind(tsis_uv_mean_table$start,tsis_uv_mean_table$tsis_uv,tsis_vis_mean_table$tsis_vis,tsis_ir_mean_table$tsis_ir,tav_uv_mean_table$tav_uv,tav_vis_mean_table$tav_vis,tav_ir_mean_table$tav_ir)
binned_integrated_df <- as.data.frame(binned_integrated_df)
colnames(binned_integrated_df) <- c("days","tsis_uv","tsis_vis","tsis_ir","tav_uv","tav_vis","tav_ir")

#plot binned tav and tsis together for each wavelength range - UV
ggplot(binned_integrated_df)+geom_line(aes(days, tsis_uv, colour = "TSIS UV"),size = 1)+
  geom_line(aes(days, tav_uv,colour = "TAV UV"),size = 1)+scale_colour_manual(values = c("#4A88C5","#E8C539"))+labs(colour ="Instrument")+ylim(15.68,15.73)+
  xlab("Overlap Days")+ylab("Integrated Irradiance")#+ggtitle("TAV and TSIS Integrated UV Irradiance Over Time")
ggsave("UV_binned_zoom_last.png",plot = last_plot(), dpi=300) 
#plot binned tav and tsis together for each wavelength range - VIS
ggplot(binned_integrated_df)+geom_line(aes(days, tsis_vis, colour = "TSIS VIS"), size = 1)+
  geom_line(aes(days, tav_vis,colour = "TAV VIS"), size = 1)+scale_colour_manual(values = c("#A653F5","#F96CFF"))+labs(colour ="Instrument")+ylim(892.88,893.4)+
  xlab("Overlap Days")+ylab("Integrated Irradiance")#+ggtitle("TAV and TSIS Integrated VIS Irradiance Over Time")#
ggsave("VIS_binned_last.png",plot = last_plot(), dpi=300) 
#plot binned tav and tsis together for each wavelength range - IR
ggplot(binned_integrated_df)+geom_line(aes(days, tsis_ir, colour = "TSIS IR"), size =1)+
  geom_line(aes(days, tav_ir,colour = "TAV IR"), size = 1)+scale_colour_manual(values = c("#43AA47","#6D5B87"))+labs(colour ="Instrument")+
  xlab("Overlap Days")+ylab("Integrated Irradiance")+ylim(293.55,293.8)#+ggtitle("TAV and TSIS Integrated IR Irradiance Over Time")
ggsave("IR_binned_zoom.png",plot = last_plot(), dpi=300) 

#gather parameters for simulating noise in binned data
#get autocorrelation of binned wavelength ranges
acf_binned_tsis_uv <- acf(binned_integrated_df$tsis_uv)
acf_binned_tsis_vis <- acf(binned_integrated_df$tsis_vis)
acf_binned_tsis_ir <- acf(binned_integrated_df$tsis_ir)
acf_binned_tav_uv <- acf(binned_integrated_df$tav_uv)
acf_binned_tav_vis <- acf(binned_integrated_df$tav_vis)
acf_binned_tav_ir <- acf(binned_integrated_df$tav_ir)
#get autocorrelation for each binned wavelength range with lag = 1
autocorr_binned_tsis_uv <- 0.829
autocorr_binned_tsis_vis <- 0.560
autocorr_binned_tsis_ir <- 0.714
autocorr_binned_tav_uv <- 0.647
autocorr_binned_tav_vis <- -0.039
autocorr_binned_tav_ir <- 0.606
#get mean of each binned wavelength range
mean_binned_tsis_uv <- mean(binned_integrated_df$tsis_uv)
mean_binned_tsis_vis <- mean(binned_integrated_df$tsis_vis)
mean_binned_tsis_ir<- mean(binned_integrated_df$tsis_ir)
mean_binned_tav_uv <- mean(binned_integrated_df$tav_uv)
mean_binned_tav_vis <- mean(binned_integrated_df$tav_vis)
mean_binned_tav_ir<- mean(binned_integrated_df$tav_ir)
#get standard deviations of binned wavelength ranges
std_binned_tsis_uv <- sd(binned_integrated_df$tsis_uv)
std_binned_tsis_vis <- sd(binned_integrated_df$tsis_vis)
std_binned_tsis_ir<- sd(binned_integrated_df$tsis_ir)
std_binned_tav_uv <- sd(binned_integrated_df$tav_uv)
std_binned_tav_vis <- sd(binned_integrated_df$tav_vis)
std_binned_tav_ir<- sd(binned_integrated_df$tav_ir)

#simulate the noise in each binned wavelength range
#get list of days 
TT <- seq_along(binned_integrated_df$tsis_ir)
#simulate noise in tsis - UV
Nt_binned_tsis_uv <- rnorm(TT, mean = mean_binned_tsis_uv, sd = std_binned_tsis_uv)#<-- initialize noise term to be populated in loop
epsilon_binned_tsis_uv <- rnorm(TT,0,std_binned_tsis_uv) #<-- white noise term
#loop through the number of days and add the noise term for each day -> Nt = phi*Nt_1 + episilon 
for (t in 2:TT) {
  Nt_binned_tsis_uv[t] <- autocorr_binned_tsis_uv*Nt_binned_tsis_uv[t - 1] + epsilon_binned_tsis_uv[t]
}
#sim noise in tsis vis
Nt_binned_tsis_vis <- rnorm(TT, mean = mean_binned_tsis_vis, sd = std_binned_tsis_vis)#<-- initialize noise term to be populated in loop
epsilon_binned_tsis_vis <- rnorm(TT,0,std_binned_tsis_vis) #<-- white noise term
#loop through the number of days and add the noise term for each day -> Nt = phi*Nt_1 + episilon 
for (t in 2:TT) {
  Nt_binned_tsis_vis[t] <- autocorr_binned_tsis_vis*Nt_binned_tsis_vis[t - 1] + epsilon_binned_tsis_vis[t]
}
#sim noise in tsis ir
Nt_binned_tsis_ir <- rnorm(TT, mean = mean_binned_tsis_ir, sd = std_binned_tsis_ir)#<-- initialize noise term to be populated in loop
epsilon_binned_tsis_ir <- rnorm(TT,0,std_binned_tsis_ir) #<-- white noise term
#loop through the number of days and add the noise term for each day -> Nt = phi*Nt_1 + episilon 
for (t in 2:TT) {
  Nt_binned_tsis_ir[t] <- autocorr_binned_tsis_ir*Nt_binned_tsis_ir[t - 1] + epsilon_binned_tsis_ir[t]
}
#sim noise in tav uv
Nt_binned_tav_uv <- rnorm(TT, mean = mean_binned_tav_uv, sd = std_binned_tav_uv)#<-- initialize noise term to be populated in loop
epsilon_binned_tav_uv <- rnorm(TT,0,std_binned_tav_uv) #<-- white noise term
#loop through the number of days and add the noise term for each day -> Nt = phi*Nt_1 + episilon 
for (t in 2:TT) {
  Nt_binned_tav_uv[t] <- autocorr_binned_tav_uv*Nt_binned_tav_uv[t - 1] + epsilon_binned_tav_uv[t]
}
#sim noise in tav vis
Nt_binned_tav_vis <- rnorm(TT, mean = mean_binned_tav_vis, sd = std_tav_vis)#<-- initialize noise term to be populated in loop
epsilon_binned_tav_vis <- rnorm(TT,0,std_binned_tav_vis) #<-- white noise term
#loop through the number of days and add the noise term for each day -> Nt = phi*Nt_1 + episilon 
for (t in 2:TT) {
  Nt_binned_tav_vis[t] <- autocorr_binned_tav_vis*Nt_binned_tav_vis[t - 1] + epsilon_binned_tav_vis[t]
}
#sim noise in tav ir
Nt_binned_tav_ir <- rnorm(TT, mean = mean_binned_tav_ir, sd = std_binned_tav_ir)#<-- initialize noise term to be populated in loop
epsilon_binned_tav_ir <- rnorm(TT,0,std_binned_tav_ir) #<-- white noise term
#loop through the number of days and add the noise term for each day -> Nt = phi*Nt_1 + episilon 
for (t in 2:TT) {
  Nt_binned_tav_ir[t] <- autocorr_binned_tav_ir*Nt_binned_tav_ir[t - 1] + epsilon_binned_tav_ir[t]
}
#create data frame of binned noise and set column names
binned_integrated_noise_df <- as_data_frame(cbind(binned_integrated_df$days,Nt_binned_tsis_uv,Nt_binned_tav_uv,Nt_binned_tsis_vis,Nt_binned_tav_vis,Nt_binned_tsis_ir,Nt_binned_tav_ir))
colnames(binned_integrated_noise_df)<-c("days","Nt_binned_tsis_uv","Nt_binned_tav_uv","Nt_binned_tsis_vis","Nt_binned_tav_vis","Nt_binned_tsis_ir","Nt_binned_tav_ir")

#plot binned tav and tsis noise together for each wavelength range - UV
ggplot(binned_integrated_noise_df)+geom_line(aes(days, Nt_binned_tsis_uv, colour = "TSIS UV"), size = 1)+labs(colour ="Instrument")+
  geom_line(aes(days, Nt_binned_tav_uv,colour = "TAV UV"),size = 1)+scale_colour_manual(values = c("#4A88C5","#E8C539"))+
  xlab("Binned Overlap Days")+ylab("Noise")+ylim(15.68,15.73)#+ggtitle("Noise in Binned TAV and TSIS Integrated UV Irradiance Over Time")
ggsave("UV_binned_noise_last.png",plot = last_plot(), dpi=300) 
#plot binned tav and tsis noise together for each wavelength range - VIS
ggplot(binned_integrated_noise_df)+geom_line(aes(days, Nt_binned_tsis_vis, colour = "TSIS VIS"),size = 1)+labs(colour ="Instrument")+
  geom_line(aes(days, Nt_binned_tav_vis,colour = "TAV VIS"),size = 1)+scale_colour_manual(values = c("#A653F5","#F96CFF"))+
  xlab("Binned Overlap Days")+ylab("Noise")+ylim(860,920)#+ggtitle("Noise in Binned TAV and TSIS Integrated VIS Irradiance Over Time")
ggsave("VIS_binned_noise_last.png",plot = last_plot(), dpi=300) 
#plot binned tav and tsis noise together for each wavelength range - IR
ggplot(binned_integrated_noise_df)+geom_line(aes(days, Nt_binned_tsis_ir, colour = "TSIS IR"), size =1)+labs(colour ="Instrument")+
  geom_line(aes(days, Nt_binned_tav_ir,colour = "TAV IR"),size = 1)+scale_colour_manual(values = c("#43AA47","#6D5B87"))+
  xlab("Binned Overlap Days")+ylab("Noise")+ylim(293.6,293.73)#+ggtitle("Noise in Binned TAV and TSIS Integrated IR Irradiance Over Time")
ggsave("IR_binned_noise_last.png",plot = last_plot(), dpi=300) 

#get differences in binned noises
TT <- as.numeric(TT)
Nt_binned_uv_diff <- Nt_binned_tav_uv - Nt_binned_tsis_uv
Nt_binned_vis_diff <- Nt_binned_tav_vis - Nt_binned_tsis_vis
Nt_binned_ir_diff<- Nt_binned_tav_ir - Nt_binned_tsis_ir
#remove outliar (it messes up the trend line)
Nt_binned_uv_diff<- Nt_binned_uv_diff[-2]
Nt_binned_vis_diff<- Nt_binned_vis_diff[-2]
Nt_binned_ir_diff<- Nt_binned_ir_diff[-2]
binned_integrated_noise_df <- binned_integrated_noise_df[-2,]

#get trend lines in binned noise differences
binned_fit_uv <- lm(Nt_binned_uv_diff~binned_integrated_noise_df$days)
binned_intercept_uv <- binned_fit_uv$coefficients[[1]]
binned_slope_uv <- binned_fit_uv$coefficients[[2]]

binned_fit_vis <- lm(Nt_binned_vis_diff~binned_integrated_noise_df$days)
binned_intercept_vis <- binned_fit_vis$coefficients[[1]]
binned_slope_vis <- binned_fit_vis$coefficients[[2]]

binned_fit_ir <- lm(Nt_binned_ir_diff~binned_integrated_noise_df$days)
binned_intercept_ir <- binned_fit_ir$coefficients[[1]]
binned_slope_ir <- binned_fit_ir$coefficients[[2]]

#add differences to data frame so it can be plotted
binned_integrated_noise_df <- as_data_frame(cbind(binned_integrated_df$days,Nt_binned_tsis_uv,Nt_binned_tav_uv,Nt_binned_tsis_vis,Nt_binned_tav_vis,Nt_binned_tsis_ir,Nt_binned_tav_ir,Nt_binned_uv_diff,Nt_binned_vis_diff,Nt_binned_ir_diff))
colnames(binned_integrated_noise_df)<-c("days","Nt_binned_tsis_uv","Nt_binned_tav_uv","Nt_binned_tsis_vis","Nt_binned_tav_vis","Nt_binned_tsis_ir","Nt_binned_tav_ir","Nt_binned_uv_diff","Nt_binned_vis_diff","Nt_binned_ir_diff")

#plot difference in binned UV noise
ggplot(binned_integrated_noise_df)+geom_line(aes(days, Nt_binned_uv_diff), colour = "#8F8CF2", size = 1)+
  xlab("Overlap Days")+ylab("Difference in Noise")+#ggtitle("Difference in Noise in binned TAV and TSIS Integrated VIS Irradiance")+
  geom_line(aes(binned_integrated_df$days,binned_slope_uv*binned_integrated_df$days+binned_intercept_uv),color = "black", linetype = "dashed", size = 1)#+
#ylim(-40,40)
ggsave("UV_binned_noise_diff_last.png",plot = last_plot(), dpi=300) 
#plot difference in binned VIS noise
ggplot(binned_integrated_noise_df)+geom_line(aes(days, Nt_binned_vis_diff), colour = "#43AA47", size = 1)+
  xlab("Overlap Days")+ylab("Difference in Noise")+#ggtitle("Difference in Noise in binned TAV and TSIS Integrated VIS Irradiance")+
  geom_line(aes(binned_integrated_df$days,binned_slope_vis*binned_integrated_df$days+binned_intercept_vis),color = "black", linetype = "dashed", size = 1)#+
#ylim(-40,40)
ggsave("VIS_binned_noise_diff_last.png",plot = last_plot(), dpi=300) 
#plot difference in binned IR noise
ggplot(binned_integrated_noise_df)+geom_line(aes(days, Nt_binned_ir_diff), colour = "#AB2838", size = 1)+
  xlab("Overlap Days")+ylab("Difference in Noise")+#ggtitle("Difference in Noise in binned TAV and TSIS Integrated IR Irradiance")+
  geom_line(aes(binned_integrated_df$days,binned_slope_ir*binned_integrated_df$days+binned_intercept_ir),color = "black", linetype = "dashed",size = 1)#+
#ylim(-.13,.13)
ggsave("IR_binned_noise_diff.png",plot = last_plot(), dpi=300) 

#get standard deviation of noise differences
std_Nt_binned_uv_diff <- sd(Nt_binned_uv_diff)
std_Nt_binned_vis_diff <- sd(Nt_binned_vis_diff)
std_Nt_binned_ir_diff <- sd(Nt_binned_ir_diff)
#get mean of noise differences
mean_Nt_binned_uv_diff <- mean(Nt_binned_uv_diff)
mean_Nt_binned_vis_diff <- mean(Nt_binned_vis_diff)
mean_Nt_binned_ir_diff <- mean(Nt_binned_ir_diff)
#check decision rule of trends 
binned_uv_fit_line <- binned_slope_uv*binned_integrated_df$days+binned_intercept_uv
#this is formula (2) from weatherhead et al 1998 section 2.2 page 17150
binned_uv_std_fit <- (std_Nt_binned_uv_diff/(TT^(3/2)))*((1+autocorr_binned_uv)/(1-autocorr_binned_uv))
binned_uv_decision_rule <- abs(binned_uv_fit_line / binned_uv_std_fit)
#take the mean of the array produced by the decision rule variable - if the mean is greater than two then the trend is statistically significant and the decision rule is satisfied
mean_binned_uv_decision_rule <- mean(binned_uv_decision_rule)

binned_vis_fit_line <- binned_slope_vis*binned_integrated_df$days+binned_intercept_vis
#this is formula (2) from weatherhead et al 1998 section 2.2 page 17150
binned_vis_std_fit <- (std_Nt_binned_vis_diff/(TT^(3/2)))*((1+autocorr_binned_vis)/(1-autocorr_binned_vis))
binned_vis_decision_rule <- abs(binned_vis_fit_line / binned_vis_std_fit)
#take the mean of the array produced by the decision rule variable - if the mean is greater than two then the trend is statistically significant and the decision rule is satisfied
mean_binned_vis_decision_rule <- mean(binned_vis_decision_rule)

binned_ir_fit_line <- binned_slope_ir*binned_integrated_df$days+binned_intercept_ir
#this is formula (2) from weatherhead et al 1998 section 2.2 page 17150
binned_ir_std_fit <- (std_Nt_binned_ir_diff/(TT^(3/2)))*((1+autocorr_binned_ir)/(1-autocorr_binned_ir))
binned_ir_decision_rule <- abs(binned_ir_fit_line / binned_ir_std_fit)
#take the mean of the array produced by the decision rule variable - if the mean is greater than two then the trend is statistically significant and the decision rule is satisfied
mean_binned_ir_decision_rule <- mean(binned_ir_decision_rule)

#gather parameters for table of n values
#get differences of binned wavelength ranges
binned_uv_diff <- binned_integrated_df$tav_uv-binned_integrated_df$tsis_uv
binned_vis_diff <- binned_integrated_df$tav_vis-binned_integrated_df$tsis_vis
binned_ir_diff <- binned_integrated_df$tav_ir-binned_integrated_df$tsis_ir
#get mean of binned differences 
mean_binned_uv_diff <- mean(binned_uv_diff)
mean_binned_vis_diff <- mean(binned_vis_diff)
mean_binned_ir_diff <- mean(binned_ir_diff)
#get autocorrelation of difference in each wavelength
acf_binned_uv_diff<-acf(binned_uv_diff)
acf_binned_vis_diff<-acf(binned_vis_diff)
acf_binned_ir_diff<-acf(binned_ir_diff)
autocorr_binned_uv <- 0.341 #<------ lag = 1 
autocorr_binned_vis <- -0.04 #<------ lag = 1 
autocorr_binned_ir <- 0.597 #<------ lag = 1

#formula (3) from Weatherhead et al 1998 <--UV
omega_0_binned_uv <- abs(.05*mean_binned_uv_diff)#<---- (omega_0 = 0.5% per year of the mean of the difference, can be changed to check different trends)
n_binned_uv <- (((3.3*std_Nt_binned_uv_diff)/(abs(omega_0_binned_uv)))*(sqrt((1+autocorr_binned_uv)/(1-autocorr_binned_uv))))^(2/3) 
n_binned_uv<- n_binned_uv/365
#formula (3) from Weatherhead et al 1998 <--UV
omega_0_binned_vis <- abs(.05*mean_binned_vis_diff)#<---- (omega_0 = 0.5% per year of the mean of the difference, can be changed to check different trends)
n_binned_vis <- (((3.3*std_Nt_binned_vis_diff)/(abs(omega_0_binned_vis)))*(sqrt((1+autocorr_binned_vis)/(1-autocorr_binned_vis))))^(2/3) 
n_binned_vis<- n_binned_vis/365
#formula (3) from Weatherhead et al 1998 <--UV
omega_0_binned_ir <- abs(.05*mean_binned_ir_diff)#<---- (omega_0 = 0.5% per year of the mean of the difference, can be changed to check different trends)
n_binned_ir <- (((3.3*std_Nt_binned_ir_diff)/(abs(omega_0_binned_ir)))*(sqrt((1+autocorr_binned_ir)/(1-autocorr_binned_ir))))^(2/3) 
n_binned_ir<- n_binned_ir/365

