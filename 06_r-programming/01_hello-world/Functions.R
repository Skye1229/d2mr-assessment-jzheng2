# Function to categorize the time into morning, afternoon, evening, and night based on the time_of_day
check_time <- function(time_of_day){
  
  # time_of_day: one of the arguments in the main function; the time in string format "xx:xx"
  
  # Transform the string to POSIXlt date-time format 
  time <- strptime(time_of_day, "%H:%M")
  
  # Check if time_of_day is in the correct format, if not, return an error message
  if (is.na(time)) {
    stop("Invalid time format. Please input time as 'xx:xx'in 24-hour format.")
  }
  
  # Extract the hour only  
  hour <- as.integer(format(time, "%H"))
  
  # Return the period of day based on the actual time (hour)
  time_period <- case_when(
    hour >= 5 & hour < 12  ~ "morning",
    hour >= 12 & hour < 17 ~ "afternoon",
    hour >= 17 & hour < 20 ~ "evening",
    TRUE                   ~ "night"  
  )
  
}

# Function to return other greeting words based on the time of the day besides Good morning/afternoon/evening/night
# Greeting sentences will be randomly chosen for each time period
# Greeting sentences used:
# Morning: 
#    1. Hope you have a great day!
#    2. How are you?
#    3. Miss me?
# Afternoon:
#    1. Hope you have a great afternoon!
#    2. How is your day going so far?
#    3. Any plans for the evening?
# Evening:
#    1. What's your plan for dinner?
#    2. How was your day?
#    3. Any exciting news to share?
# Night:
#    1. Good night! 
#    2. Have a good night!
#    3. Bed time!
morning_words = c("Hope you have a great day!","How are you?","Miss me?")
afternoon_words = c("Hope you have a great afternoon!","How is your day going so far?","Any plans for the evening?")
evening_words = c("What's your plan for dinner?","How was your day?","Any exciting news to share?")
night_words = c("Good night!","Have a good night!","Bed time!")

# Function to randomly return a greeting sentence based on the time period
# ?sample
greeting_words <- function(time_period){
  
  # time_period: the output of check_time() function
  
  if (time_period == "morning"){
    sample(morning_words, 1)
  } else if (time_period == "afternoon") {
    sample(afternoon_words, 1)
  } else if (time_period == "evening"){
    sample(evening_words, 1)
  } else{
    sample(night_words, 1)
  }
  
}

# Function to return other greeting words based on the time of day besides Good morning/afternoon/evening/night
# Greeting sentences will be randomly chosen for each time period
# All possible greeting sentences:
# Morning: 
#    1. Hope you have a great day!
#    2. How are you?
#    3. Miss me?
# Afternoon:
#    1. Hope you have a great afternoon!
#    2. How is your day going so far?
#    3. Any plans for the evening?
# Evening:
#    1. What's your plan for dinner?
#    2. How was your day?
#    3. Any exciting news to share?
# Night:
#    1. Good night! 
#    2. Have a good night!
#    3. Bed time!
morning_words = c("Hope you have a great day!","How are you?","Miss me?")
afternoon_words = c("Hope you have a great afternoon!","How is your day going so far?","Any plans for the evening?")
evening_words = c("What's your plan for dinner?","How was your day?","Any exciting news to share?")
night_words = c("Good night!","Have a good night!","Bed time!")

# Function to randomly return a greeting sentence based on the time period
# ?sample
greeting_words <- function(time_period){
  if (time_period == "morning"){
    sample(morning_words, 1)
  } else if (time_period == "afternoon") {
    sample(afternoon_words, 1)
  } else if (time_period == "evening"){
    sample(evening_words, 1)
  } else{
    sample(night_words, 1)
  }
  
}

# Function to tell people how much time is left until bedtime
time_left_for_bed <- function(time, bed_time){
  
  # time: the time of day in the format of "xx:xx"
  # bed_time: the customized bedtime in the format of "xx:xx"
  
  # Transform the time and bed_time into standard time format
  time <- strptime(time, "%H:%M") 
  bed_time <- strptime(bed_time, "%H:%M")
  
  # Calculate the time difference between current time and bedtime
  time_left = bed_time - time
  # ?abs
  time_left_num <- abs(as.numeric(time_left)) # transform time_left to a single positive digit
  
  # Calculate the time left in hours and minutes separately
  hours_left <- as.integer((floor(time_left_num)))
  minutes_left = as.integer((time_left_num - hours_left)*60)     
  
  # Return the time left in hours and minutes
  return(c(hours_left, minutes_left))
}

