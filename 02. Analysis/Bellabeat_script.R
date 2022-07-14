# Download packages

library(tidyverse)
library(dplyr)
library(tidyr)
library(data.table)
library(lubridate)
library(scales)
library(ggplot2)

# Combine daily data frames in one

d_activity <- read.csv("C:/Bellebeat_Capstone_Project/dailyActivity_merged.csv")
d_calories <- read.csv("C:/Bellebeat_Capstone_Project/dailyCalories_merged.csv")
d_intensities <- read.csv("C:/Bellebeat_Capstone_Project/dailyIntensities_merged.csv")
d_steps <- read.csv("C:/Bellebeat_Capstone_Project/dailySteps_merged.csv")
d_sleep <- read.csv("C:/Bellebeat_Capstone_Project/sleepDay_merged.csv")

d_data2 <- merge(d_calories, d_activity, by = c("Id", "Calories"))
d_data3 <- merge(d_intensities, d_data2, by = c("Id","ActivityDay","SedentaryMinutes", "LightlyActiveMinutes","FairlyActiveMinutes","VeryActiveMinutes", "SedentaryActiveDistance", "LightActiveDistance", "ModeratelyActiveDistance", "VeryActiveDistance"))

d_data4 <- merge(d_data2, d_data3, by = c("Id","ActivityDay","SedentaryMinutes", "LightlyActiveMinutes","FairlyActiveMinutes","VeryActiveMinutes", "SedentaryActiveDistance", "LightActiveDistance", "ModeratelyActiveDistance", "VeryActiveDistance", "Calories", "ActivityDate",  "TotalSteps", "TotalDistance", "TrackerDistance", "LoggedActivitiesDistance"))%>%
  select(-ActivityDay)

# Rename column with dates

d_data4 %>% 
  data.table::setnames(
    old = "ActivityDate",
    new = "Date")

d_data <- merge(d_data4, d_sleep, by = c("Id"), all = TRUE) %>% 
  drop_na() %>% 
  select(-SleepDay, -TrackerDistance)

# Let's have a look at our data
## We can see that average sedentary time is high 938 mins(15h), Very and Fairly Active time is around 15mins and Daily number of Calories is above the norm 2220 ccal.

summary(d_data)

Id            SedentaryMinutes LightlyActiveMinutes FairlyActiveMinutes
Min.   :1.504e+09   Min.   :   0.0   Min.   :  0.0        Min.   :  0.00     
1st Qu.:4.020e+09   1st Qu.: 687.0   1st Qu.:  0.0        1st Qu.:  0.00     
Median :4.703e+09   Median : 781.0   Median :171.0        Median :  3.00     
Mean   :5.117e+09   Mean   : 938.6   Mean   :156.4        Mean   : 13.58     
3rd Qu.:6.962e+09   3rd Qu.:1440.0   3rd Qu.:240.0        3rd Qu.: 19.00     
Max.   :8.792e+09   Max.   :1440.0   Max.   :518.0        Max.   :143.00     
VeryActiveMinutes SedentaryActiveDistance LightActiveDistance
Min.   :  0.00    Min.   :0.0000000       Min.   : 0.000     
1st Qu.:  0.00    1st Qu.:0.0000000       1st Qu.: 0.000     
Median :  0.00    Median :0.0000000       Median : 2.860     
Mean   : 18.76    Mean   :0.0005276       Mean   : 2.771     
3rd Qu.: 28.00    3rd Qu.:0.0000000       3rd Qu.: 4.480     
Max.   :210.00    Max.   :0.1100000       Max.   :10.300     
ModeratelyActiveDistance VeryActiveDistance    Calories        Date          
Min.   :0.0000           Min.   : 0.000     Min.   :   0   Length:15901      
1st Qu.:0.0000           1st Qu.: 0.000     1st Qu.:1693   Class :character  
Median :0.1100           Median : 0.000     Median :2013   Mode  :character  
Mean   :0.5729           Mean   : 1.094     Mean   :2220                     
3rd Qu.:0.7900           3rd Qu.: 1.740     3rd Qu.:2643                     
Max.   :6.4800           Max.   :13.400     Max.   :4900                     
TotalSteps    TotalDistance    LoggedActivitiesDistance TotalSleepRecords
Min.   :    0   Min.   : 0.000   Min.   :0.00000          Min.   :1.000    
1st Qu.:    0   1st Qu.: 0.000   1st Qu.:0.00000          1st Qu.:1.000    
Median : 6393   Median : 4.480   Median :0.00000          Median :1.000    
Mean   : 6351   Mean   : 4.487   Mean   :0.09649          Mean   :1.116    
3rd Qu.:10460   3rd Qu.: 7.390   3rd Qu.:0.00000          3rd Qu.:1.000    
Max.   :22988   Max.   :17.950   Max.   :4.94214          Max.   :3.000    
TotalMinutesAsleep TotalTimeInBed 
Min.   : 58.0      Min.   : 61.0  
1st Qu.:360.0      1st Qu.:402.0  
Median :427.0      Median :459.0  
Mean   :417.3      Mean   :456.1  
3rd Qu.:490.0      3rd Qu.:530.0  
Max.   :796.0      Max.   :961.0  

# Check for duplicates

sum(duplicated(d_data))

[1] 3553

# Remove duplicates

d_data <- d_data %>%
  distinct()

# Let's verify duplicated has been removed

sum(duplicated(d_data))

[1] 0

# Convert Date/Time into Time/Date stamp

d_data$Date <- lubridate::mdy(d_data$Date)

# Calculate average steps per day user

daily_average <- d_data %>%
  group_by(Id) %>%
  summarise(avg_daily_steps = mean(TotalSteps), avg_daily_calories = mean(Calories), avg_daily_sleep = mean(TotalMinutesAsleep))
head(daily_average)

# Classify user group

daily_average <- daily_average %>%
  mutate(user_type = case_when(avg_daily_steps < 5000 ~ "sedentary", 
                               avg_daily_steps >=5000 & avg_daily_steps < 7499 ~ "lightly_active", 
                               avg_daily_steps >= 7500 & avg_daily_steps < 9999 ~ "fairly_active", 
                               avg_daily_steps >= 10000 ~ "very_active"))

# Create a data frame with percentage of each user type
daily_average_percentage <- daily_average %>%
  group_by(user_type) %>%
  summarise(total = n()) %>%
  mutate(totals = sum(total)) %>%
  group_by(user_type) %>%
  summarise(total_percent = total / totals) %>%
  mutate(labels = scales::percent(total_percent)) 

daily_average_percentage$user_type <-ordered(daily_average_percentage$user_type, levels=c("very_active", "fairly_active", "lightly_active", "sedentary"))


# Let's visualize our data (User's Activity and Calories burnt)

## Plot shows that second big group of user have Sedentary lifestyle, while Fairly Active user are more than 30%
daily_average_percentage %>%
  ggplot(aes(x="", y=total_percent, fill=user_type)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  theme_minimal() +
  theme(axis.title.x= element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(), 
        panel.grid = element_blank(), 
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5, size=14, face = "bold")) +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#ec917d", "#F7EA8B", "#B1EAEA", "#F8A7E8")) +
  labs(title=NULL)

  
## We can see that the more active users are, the more calories they burn.

daily_average$user_type <- ordered(daily_average$user_type, levels=c("very_active", "fairly_active", "lightly_active", "sedentary"))

daily_average %>%
ggplot(aes(user_type, avg_daily_calories, fill=user_type)) +
  geom_boxplot() +
  labs(title="Calories burnt by User type", x=NULL) +
  scale_fill_manual(values = c("#ec917d", "#F7EA8B", "#B1EAEA", "#F8A7E8")) +
  theme(legend.position="none", text = element_text(size = 15),plot.title = element_text(hjust = 0,5))

# Calculate Sleeping time by User type
summary(daily_average)
daily_sleep <- daily_average %>%
  group_by(user_type) %>%
  summarise_at(vars(avg_daily_sleep), list(avg_sleep = mean))

daily_sleep$user_type <- ordered(daily_sleep$user_type, levels=c("very_active", "fairly_active", "lightly_active", "sedentary"))
daily_sleep$avg_sleep <- round(daily_sleep$avg_sleep, digits = 1)

daily_sleep %>%
  ggplot(aes(user_type, avg_sleep, fill = user_type)) +
  geom_col() +
  geom_hline(yintercept = 480) +
  geom_text(aes(label=avg_sleep)) +
  labs(title = NULL, x = NULL, y = "min") +
  theme(legend.position="none", text = element_text(size = 15),plot.title = element_text(hjust = 0,5)) +
scale_fill_manual(values = c("#ec917d", "#F7EA8B", "#B1EAEA", "#F8A7E8")) 
  
# Calculate asleep time and Number of step by Weekday

weekday_steps_sleep <- d_data %>%
  mutate(weekday = weekdays(Date))

weekday_steps_sleep$weekday <- ordered(weekday_steps_sleep$weekday, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

weekday_steps_sleep <- weekday_steps_sleep %>%
  group_by(weekday) %>%
  summarize(daily_steps = mean(TotalSteps), daily_sleep = mean(TotalMinutesAsleep))  

weekday_steps_sleep$daily_sleep <- round(weekday_steps_sleep$daily_sleep, digits = 1)

weekday_steps_sleep %>%
  ggplot(aes(weekday, daily_steps)) +
  geom_col(fill = "#ec917d") +
  geom_hline(yintercept = 7500) +
  labs(title = "Steps by Weekday", x = NULL, y = NULL) +
  theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1))

weekday_steps_sleep %>% 
  ggplot(aes(weekday, daily_sleep)) +
  geom_col(fill = "#B1EAEA") +
  geom_hline(yintercept = 480) +
  geom_text(aes(label=daily_sleep)) +
  labs(title = NULL, x = NULL, y = "min") +
  theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1)) 

  
