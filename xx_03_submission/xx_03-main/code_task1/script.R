
#This didn't turn out well. RStudio crashed, and then I couldn't access fhidata::norway_locations_b2020 anymore,
#and the code became non-functional. With access to this dataset however, it does run. 


data <- readRDS("C://Users//peter//xx_03_submission//xx_03-main//data_raw//individual_level_data.RDS")



raw_dates <- seq(as.Date("2010-01-01"), as.Date("2020-12-31"), by="days")
dates <- data.frame(raw_dates)
colnames(dates) <- c("date")

nr_dates <- length(dates)
communes <- unique(data$location_code)
nr_communes <- length(communes)

#per day cases without 0 days
summ_data <- group_by(data, location_code, date) %>% summarise(n=n())




daily_commune <- list()
#add days with 0 cases
for(i in 1:nr_communes) {
  commune_nr <- communes[i]
  comm <- filter(summ_data, location_code == commune_nr)
  comm <- full_join(dates, comm, by="date")
  comm$n[is.na(comm$n)] <- 0
  comm$location_code[is.na(comm$location_code)] <- commune_nr
  daily_commune[[i]] <- comm
  
}

#aggregate iso data
isos <- isoWeekYear(raw_dates)
isos <- data.frame(isos)
weekly_commune <- list()
yearly_commune <- list()

for(i in 1:nr_communes) {
  iso_commune <- cbind(isos, daily_commune[[i]])
  weekly_commune[[i]] <- group_by(iso_commune, ISOYear, ISOWeek, location_code) %>% summarise(n=sum(n))
  yearly_commune[[i]] <- group_by(iso_commune, ISOYear, location_code) %>% summarise(n=sum(n))
  
}


#create fylke data
norway_data <- data.frame(norway_locations_b2020$municip_code, norway_locations_current$county_name)
colnames(norway_data) <- c("municip_code", "county_name")
nr_fylker <- length(unique(norway_data$county_name))
weekly_fylke <- list()
yearly_fylke <- list()


nr_weeks <- length(weekly_commune[1])
nr_years <- length(yearly_commune[1])

for(i in 1:nr_communes) { #add fylke name to the commune data sets
  
  kom_code <- weekly_commune[[i]]$location_code[1]  #get muniip code fom each commune
  fylke_name <- filter(norway_data, municip_code == kom_code)$county_name
  weekly_commune[[i]] <- cbind(rep(fylke_name, nr_weeks), weekly_commune[[i]])
  yearly_commune[[i]] <- cbind(rep(fylke_name, nr_years), yearly_commune[[i]])
}


#combine all the commune data frames into one for weekly and annual data
tot_weekly_commune <-  data.frame()
tot_yearly_commune <- data.frame()
for(i in 1:nr_communes) {
  
  tot_weekly_commune <- rbind(tot_weekly_commune, weekly_commune[[i]])
  tot_yearly_commune <- rbind(tot_yearly_commune, yearly_commune[[i]])
}
colnames(tot_weekly_commune)[1] <- "fylke_name"
colnames(tot_yearly_commune)[1] <- "fylke_name"

#all fylke weekly and annual counts in data frames.
fylke_weekly <- group_by(tot_weekly_commune, fylke_name, ISOYear, ISOWeek) %>% summarise(n=sum(n))
fylke_yearly <- group_by(tot_yearly_commune, fylke_name, ISOYear) %>% summarise(n=sum(n))
norway_weekly <-group_by(fylke_weekly, ISOYear, ISOWeek) %>% summarise(n=sum(n))
norway_yearly <- group_by(fylke_yearly, fylke_name, ISOYear) %>% summarise(n=sum(n))

#Running out of time so will only preform regression on national weekly data

time <- c(1:length(norway_weekly$ISOWeek)) #number of weeks from start date
sin_week <- sin(2*pi*norway_weekly$ISOWeek/52)  #sin and cos of week number to account for seasonality
cos_week <- cos(2*pi*norway_weekly$ISOWeek/52)
norway_weekly <- cbind(time, norway_weekly, sin_week, cos_week)
colnames(norway_weekly) <- c("time", "ISOYear", "ISOWeek", "n", "sin_week", "cos_week")

#split training and testing data
training_norge_data <- filter(norway_weekly, ISOYear < 2020)
predict_norge_data <- filter(norway_weekly, ISOYear == 2020)


#Poisson glm model using #weeks (time), and sin and cos fo ISOS week number
model1 <- glm(n ~ time + sin_week + cos_week, data = training_norge_data,family = 'poisson')

#"Predicting" training values with a 2std prediction interval
init_training_predict <- predict(model1, training_norge_data, type="response")
pred_interval <- predict(model1, training_norge_data, interval="predict")

#remove training values for outbreaks
new_training_norge_data <- data.frame()
for(i in 1:length(training_norge_data$ISOWeek)) {
  #if value is within prediction interval
  if((init_training_predict[i] + pred_interval[i] > training_norge_data$n[i])) {
    new_training_norge_data <- rbind(new_training_norge_data, training_norge_data[i,])
  }
}


#new training
model1 <- glm(n ~ time + sin_week + cos_week, data = new_training_norge_data,family = 'poisson')
future_predict <- predict(model1, predict_norge_data, type="response")
future_pred_interval <- predict(model1, predict_norge_data, interval="predict")

#outbreak identification with new prediction intervals
outbreaks <- data.frame()
for(i in 1:length(predict_norge_data$ISOWeek)) {
  #if value is within prediction interval
  if((future_training_predict[i] + future_pred_interval[i] > predict_norge_data$n[i])) {
    outbreaks <- rbind(outbreaks, predict_norge_data[i,])
  }
}

write.xlsx(outbreaks,"outbreaks_national.xlsx")

plot(predict_norge_data$time, predict_norge_data$n)
lines(predict_norge_data$time, future:predict)
