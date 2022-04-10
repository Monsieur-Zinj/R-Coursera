library(statsr)
library(dplyr)
library(ggplot2)


data(nycflights)



#Q1
# Create a new data frame that includes flights headed to SFO in February, 
# and save this data frame assfo_feb_flights. 
# How many flights meet these criteria? 


sfo_feb_flights<- nycflights %>% filter(month==2 & dest=="SFO")

#Q2 

# Make a histogram and calculate appropriate summary statistics for
# arrival delays of sfo_feb_flights. Which of the following is false? 

ggplot(data = sfo_feb_flights, aes(x = arr_delay)) + geom_histogram()

# Q3
# Calculate the median and interquartile range for arr_delays of flights 
# in the sfo_feb_flights data frame, grouped by carrier. 
# Which carrier has the highest IQR of arrival delays? 

sfo_feb_flights %>%
  group_by(carrier) %>%
  summarise(IQR = IQR(arr_delay))

#Q4
# Considering the data from all the NYC airports, 
# which month has the highest average departure delay?

nycflights %>% 
  group_by(month) %>% 
  summarise(mean=mean(dep_delay)) %>%
  arrange(desc(mean))


#Q5
# Which month has the highest median departure delay from an NYC airport?
  
nycflights %>% 
  group_by(month) %>% 
  summarise(med=median(dep_delay)) %>%
  arrange(desc(med))


#Q6

# Is the mean or the median a more reliable measure for deciding which month(s)
# to avoid flying if you really dislike delayed flights, and why?
  
  
ggplot(nycflights, aes(x = factor(month), y = dep_delay)) +
  geom_boxplot()


#Q7
# If you were selecting an airport simply based on on time departure percentage,
# which NYC airport would you choose to fly out of?

  
nycflights <- nycflights %>%
  mutate(dep_type = ifelse(dep_delay < 5, "on time", "delayed"))


nycflights %>%
  group_by(origin) %>%
  summarise(ot_dep_rate = sum(dep_type == "on time") / n()) %>%
  arrange(desc(ot_dep_rate))


#Q8
# Mutate the data frame so that it includes a new variable that contains the
# average speed, avg_speed traveled by the plane for each journey (in mph). 
# What is the tail number of the plane with the fastest avg_speed? 

nycflights<- nycflights %>%   mutate(speed_avg = distance/air_time)

nycflights %>% arrange(desc(speed_avg)) %>% select(speed_avg, tailnum)

#Q9
# Make a scatterplot of avg_speed vs. distance. Which of the following 
# is true about the relationship between average speed and distance.

ggplot(nycflights,aes(y=speed_avg, x=distance)) + geom_point()


#Q10

# Suppose you define a flight to be “on time” if it gets to the destination
# on time or earlier than expected, regardless of any departure delays. 
# Mutate the data frame to create a new variable called arr_type 
# with levels "on time" and "delayed" based on this definition. 
# Then, determine the on time arrival percentage based on whether the 
# flight departed on time or not. What proportion of flights 
# that were "delayed" departing arrive "on time"?


nycflights<- nycflights %>%   mutate(arr_type = ifelse(arr_delay<=0,"on_time","delayed"))

nycflights %>% filter(dep_type=="delayed") %>% summarise(dep_del_arr_ontime_rate = sum(arr_type == "on_time")/ n())
                                                         