# divide the files into folders based on the data represented by them ex: daily, hourly, minutes

# DAILY

library(readr)
library(dplyr)
library(ggplot2)
library(hms)
library(plotly)

# 5 files in daily, lets import all of them and check them one by one

dailyActivity_merged <- read_csv("archive/Fitabase Data 4.12.16-5.12.16/daily/dailyActivity_merged.csv")
dailyCalories_merged <- read_csv("archive/Fitabase Data 4.12.16-5.12.16/daily/dailyCalories_merged.csv")
dailyIntensities_merged <- read_csv("archive/Fitabase Data 4.12.16-5.12.16/daily/dailyIntensities_merged.csv")
dailySteps_merged <- read_csv("archive/Fitabase Data 4.12.16-5.12.16/daily/dailySteps_merged.csv")
sleepDay_merged <- read_csv("archive/Fitabase Data 4.12.16-5.12.16/daily/sleepDay_merged.csv")

#checking if datatypes of columns matches with the data using head function

head(dailyActivity_merged)
head(dailyCalories_merged)
head(dailyIntensities_merged)
head(dailySteps_merged)
head(sleepDay_merged)

#no discrepancy in data formats

#checking for duplicate entries

count_duplicates <- function(dataframe){
  n <- dataframe %>% nrow() - dataframe %>% unique() %>% nrow() #number of duplicate rows
  return(n)
  rm(n)
}

count_duplicates(dailyActivity_merged)
count_duplicates(dailyCalories_merged)
count_duplicates(dailyIntensities_merged)
count_duplicates(dailySteps_merged)
count_duplicates(sleepDay_merged)

#there are 3 duplicate rows in sleepDay_merged, lets take care of it

sleepDay_merged <- unique(sleepDay_merged)

# now checking for null values
dailyActivity_merged %>% is.na() %>% which()
dailyCalories_merged %>% is.na() %>% which()
dailyIntensities_merged %>% is.na() %>% which()
dailySteps_merged %>% is.na() %>% which()
sleepDay_merged %>% is.na() %>% which()

#as they all returned zero there are no NA values in our files

#lets try to find relations in between the columns of the tables using colnames() function

#upon inspecting the columns of all the files we find that 
#1.they all have Id and ActivityDate in common
#2.dailyActivity_merged file has columns of all the tables except the sleepDay_merged, we cannot join the two tables as there are only 413 rows in sleepDay_merged

#we can now get rid of the files that arent required

rm(dailyCalories_merged)
rm(dailyIntensities_merged)
rm(dailySteps_merged)

#on inspecting all the files we find that except sleepDay_merged all files have 940 rows

#lets perform some data redundancy on dailyActivity_merged tables

daily_activity <- dailyActivity_merged %>% 
                  transform(active_minutes = VeryActiveMinutes+FairlyActiveMinutes+LightlyActiveMinutes,  ActivityDate = as.Date(ActivityDate, format = "%m/%d/%Y") ,weekday = weekdays(as.Date(ActivityDate, format = "%m/%d/%Y"))) %>%
                  rename(inactive_minutes = SedentaryMinutes, date = ActivityDate, total_steps = TotalSteps, total_distance = TotalDistance) %>%
                  select(Id, date, weekday, total_steps, total_distance, active_minutes, inactive_minutes, Calories)

colnames(daily_activity) <- tolower(colnames(daily_activity))

rm(dailyActivity_merged)

#1.here we calculated the weekday at the ActivityDate
#2.calculated total activity minutes by summing up VeryActiveMinutes, FairlyActiveMinutes and LightlyActiveMinutes
#3.inactive minutes as SedentaryMinutes
#4. selected only, id, date,weekday, total_steps, total_distance, active_minutes, inactive_minutes, Calories columns
#5. renamed all the columns to lowercase

#lets try to find the relationship between total_steps and total_distance, we assume it to be linear and directly propotional

ggplot(data = daily_activity, aes(x = total_steps, y = total_distance)) + geom_point(size = 0.4, color = "Blue")

#as the data also confirms our hypothesis we can ignore the total_distance column

#now lets plot total_steps, active_minutes and inactive_minutes against calories, our hypothesis is that 
#1. total_steps directly proportional to calories  
#2. active_minutes directly proportional to calories
#3. inactive_minutes inversely proportional to calories

steps_cal <- ggplot(data = daily_activity, aes(x = total_steps, y = calories)) + geom_point(size = 0.4, color = "Blue")
activeMin_cal <- ggplot(data = daily_activity, aes(x = active_minutes, y = calories)) + geom_point(size = 0.4, color = "Blue")
inactiveMin_cal <- ggplot(data = daily_activity, aes(x = inactive_minutes, y = calories)) + geom_point(size = 0.4, color = "Blue")

#subplot(steps_cal, activeMin_cal, inactiveMin_cal,  nrows = 2) 

# lets apply linear regression to the datasets and check the slope of the line

steps_cal <- steps_cal + geom_smooth(method = "lm", color = "Pink", size = 0.5)
activeMin_cal <- activeMin_cal + geom_smooth(method = "lm", color = "Pink", size = 0.5)
inactiveMin_cal <- inactiveMin_cal + geom_smooth(method = "lm", color = "Pink", size = 0.5)

#subplot(steps_cal, activeMin_cal, inactiveMin_cal, nrows = 2)

rm(steps_cal)
rm(activeMin_cal)
rm(inactiveMin_cal)

#its clear that hypothesis stands correct

#lets also calculate the calories Vs TotalSleepMinutes

sleep_minutes <-  sleepDay_merged %>% 
                  rename(date = SleepDay, sleep_time = TotalMinutesAsleep) %>% 
                  transform(date = as.Date(date, "%m/%d/%Y %I:%M:%S %p")) %>%
                  select(Id, date, sleep_time)

colnames(sleep_minutes) <- tolower(colnames(sleep_minutes))

rm(sleepDay_merged)

sleep_activity <- merge(daily_activity, sleep_minutes, .by = id, .by = date ,all.y = TRUE) 

ggplot(data = sleep_activity, aes(x = sleep_time, y = calories)) + geom_point(size = 0.4, color = "Blue")


# it is clear from the graph that there arent any interesting trends in the data.

#lets now calculate the average sleep time per weekday

order_days <- function(data, x){
  data$weekday <- factor(data$weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  return(data)
}

median_sleep_weekday <- sleep_activity %>% group_by(weekday) %>% summarize(median_time = median(sleep_time))
median_sleep_weekday <- order_days(median_sleep_weekday)
ggplot(data = median_sleep_weekday, aes(x = weekday, y = median_time)) + geom_line(color = "Blue", group = 1) + geom_point() + ylim(350, 500)

rm(median_sleep_weekday)
rm(sleep_minutes)
rm(sleep_activity)

#it is clear from the above plot that users like to sleep the most on weekends and Wednesday

#lets find median of all the attributes in daily_activity grouped by weekdays

median_daily <- daily_activity %>% group_by(weekday) %>% summarize(median_active = median(active_minutes), median_inactive = median(inactive_minutes), median_steps = median(total_steps), median_calories = median(calories))
median_daily <- order_days(median_daily)

#as active_minutes and inactive_minutes are contrapositive to each other, we will plot their ratio to get the whole picture
act_inact_weekday <- ggplot(data = median_daily %>% transform(act_inact = median_active/median_inactive), aes(x = weekday, y = act_inact)) + geom_line(color = "Blue", group = 1) + geom_point() + ylim(0.175,0.275)

steps_weekday <- ggplot(data = median_daily, aes(x = weekday, y = median_steps)) + geom_line(color = "Blue", group = 1) + geom_point() + ylim(5500, 9000)
calories_weekday <- ggplot(data = median_daily, aes(x = weekday, y = median_calories)) + geom_line(color = "Blue", group = 1) + geom_point() + ylim(1900, 2300)

#subplot(steps_weekday, calories_weekday, act_inact_weekday, nrows = 2)

# so in the overall picture, people appear to be most functional and energetic on Tuesday and Thursday(except least calories are burned on Thursday) and lazy on weekends.

rm(act_inact_weekday)
rm(calories_weekday)
rm(median_daily)
rm(steps_weekday)

#now lets analyze hourly folder
# lets import the hourlyCalories_merged, hourlyIntensities_merged and hourlySteps_merged

hourlyCalories_merged <- read_csv("archive/Fitabase Data 4.12.16-5.12.16/hourly/hourlyCalories_merged.csv")
hourlyIntensities_merged <- read_csv("archive/Fitabase Data 4.12.16-5.12.16/hourly/hourlyIntensities_merged.csv")
hourlySteps_merged <- read_csv("archive/Fitabase Data 4.12.16-5.12.16/hourly/hourlySteps_merged.csv")

#lets check for duplicates before diving into analyzing

count_duplicates(hourlyCalories_merged)
count_duplicates(hourlyIntensities_merged)
count_duplicates(hourlySteps_merged)

#there are no duplicates in these files
#lets check for na values

hourlyCalories_merged %>% is.na() %>% which()
hourlyIntensities_merged %>% is.na() %>% which()
hourlySteps_merged %>% is.na() %>% which()

#they all returned zero that means no null values
#lets verify the format and respective data values

head(hourlyCalories_merged)
head(hourlyIntensities_merged)
head(hourlySteps_merged)

#as they all have same rows lets merge them

hourlyCalories_merged$ActivityHour <- as.POSIXct(hourlyCalories_merged$ActivityHour, format = "%m/%d/%Y %I:%M:%S %p")
hourlyIntensities_merged$ActivityHour <- as.POSIXct(hourlyIntensities_merged$ActivityHour, format = "%m/%d/%Y %I:%M:%S %p")
hourlySteps_merged$ActivityHour <- as.POSIXct(hourlySteps_merged$ActivityHour, format = "%m/%d/%Y %I:%M:%S %p")

one <- merge(hourlyCalories_merged, hourlySteps_merged, .by = Id, .by = ActivityHour)
hourly_merged <- merge(one, hourlyIntensities_merged, .by = Id, .by = ActivityHour)

rm(hourlyCalories_merged)
rm(hourlyIntensities_merged)
rm(hourlySteps_merged)
rm(one)

extract_time <- function(data){
  data$ActivityHour <- as.POSIXct(data$ActivityHour, format = "%m/%d/%Y %I:%M:%S %p")
  data$hour <- format(data$ActivityHour, "%H:%M:%S")
  return(data)
}
#now we have data in time
hourly_merged = extract_time(hourly_merged)

#lets rename the rows and eliminate the average intensity row

hourly_merged <-  hourly_merged %>% 
                  rename(id = Id, time = hour, steps = StepTotal, intensity = TotalIntensity, calories = Calories, date_time = ActivityHour) %>% 
                  select(id, time, steps, intensity, calories, date_time)

# lets plot

intensity_calories <- ggplot(data = hourly_merged, aes(x = intensity, y = calories)) + geom_point(size = 0.5) 
steps_calories <- ggplot(data = hourly_merged, aes(x = steps, y = calories)) + geom_point(size = 0.5) 
steps_intensity <- ggplot(data = hourly_merged, aes(x = steps, y = intensity)) + geom_point(size = 0.5) 

#subplot(intensity_calories, steps_calories, steps_intensity, nrows = 2)

#its clearly visible that intensity and steps are linearly proportional to calories so lets fit a linear model to them

intensity_calories <- intensity_calories + geom_smooth(method = "lm", color = "Orange")
steps_calories <- steps_calories + geom_smooth(method = "lm", color = "Orange")
steps_intensity <- steps_intensity + geom_smooth(method = "lm", color = "Orange")

#subplot(intensity_calories, steps_calories, steps_intensity, nrows = 2)

#lets find median calories, intensities and steps through different times throughout the day

hourly_mean <- hourly_merged %>% group_by(time) %>% summarize(mean_steps = mean(steps), mean_intensity = mean(intensity), mean_calories = mean(calories))

plot_cal <- ggplot(data = hourly_mean, aes(as_hms(time), mean_calories)) + geom_line(color = "Orange") + geom_point(size = 0.5) + scale_x_time()
plot_intensity <- ggplot(data = hourly_mean, aes(as_hms(time), mean_intensity)) + geom_line(color = "Orange") + geom_point(size = 0.5) + scale_x_time()
plot_steps <- ggplot(data = hourly_mean, aes(as_hms(time), mean_steps)) + geom_line(color = "Orange") + geom_point(size = 0.5) + scale_x_time()

#subplot(plot_cal, plot_intensity, plot_steps, nrows = 2)

rm(plot_cal)
rm(plot_intensity)
rm(plot_steps)

#all the graphs above are similar to and we can conclude that people are most active during 6 PM and 7 PM


#as the data is highly linear we can apply a linear regression model to fit the data and predict calories burned

model <- lm(calories~steps+intensity, hourly_merged)

summary(model)

#as you can see there is a lot of variance in the data and we are getting R-Squared: 0.8046 which indicates a good but not strong corelation between the data

prediction_data <- hourly_merged %>% select(steps, intensity, calories)

prediction_data <- prediction_data %>% mutate(pred_cal = predict(model)) %>% select(calories, pred_cal)

head(prediction_data, 10)

#here you can see few actual nd predicted calories burnt.

heartrate_seconds_merged <- read_csv("archive/Fitabase Data 4.12.16-5.12.16/heartrate_seconds_merged.csv")

#lets analyze heart rate now
heartrate_seconds_merged$hour <- as.POSIXct(heartrate_seconds_merged$Time, format = "%m/%d/%Y %I:%M:%S %p")
heartrate_seconds_merged$hour <- format(heartrate_seconds_merged$hour, "%H")
heartrate_mean <- heartrate_seconds_merged %>% group_by(hour) %>% summarize(mean_value = mean(Value))

bpm_time <- ggplot(heartrate_mean, aes(hour, mean_value)) + geom_line (color= "Red", group = 1, linetype="dashed") + geom_point(color = "Black")
#ggplotly(bpm_time)

heartrate_seconds_merged$Time <- as.POSIXct(heartrate_seconds_merged$Time, format = "%m/%d/%Y %I:%M:%S %p")
heartrate_seconds_merged$Time <- format(heartrate_seconds_merged$Time, "%m/%d/%Y %I:00:00 %p")
heartrate_seconds_merged_mean <- heartrate_seconds_merged %>% group_by(Time, Id) %>% summarize(heartrate = mean(Value)) 

rm(heartrate_seconds_merged)
rm(heartrate_mean)

heartrate_seconds_merged_mean <-  heartrate_seconds_merged_mean %>% 
                                  rename(date_time = Time, id = Id) %>% 
                                  select(id, date_time, heartrate)

heartrate_seconds_merged_mean$date_time <-  as.POSIXct(heartrate_seconds_merged_mean$date_time, format = "%m/%d/%Y %I:%M:%S %p")

heartrate_hourly_merged <- merge(heartrate_seconds_merged_mean,hourly_merged, .by = id, .by = date_time)

rm(heartrate_seconds_merged)
rm(hourly_mean)
rm(daily_activity)
rm(prediction_data)
rm(hourly_merged)
rm(heartrate_seconds_merged_mean)

intensity_heartrate <- ggplot(data = heartrate_hourly_merged, aes(intensity, heartrate)) + geom_point(size = 0.7, color = "Blue") + geom_smooth(method = "lm", color = "Orange")
steps_heartrate <- ggplot(data = heartrate_hourly_merged, aes(steps, heartrate)) + geom_point(size = 0.7, color = "Blue") + geom_smooth(method = "lm", color = "Orange")
calories_heartrate <- ggplot(data = heartrate_hourly_merged, aes(calories, heartrate)) + geom_point(size = 0.7, color = "Blue") + geom_smooth(method = "lm", color = "Orange")

#subplot(intensity_heartrate, steps_heartrate, calories_heartrate, nrows = 2)

gc()
