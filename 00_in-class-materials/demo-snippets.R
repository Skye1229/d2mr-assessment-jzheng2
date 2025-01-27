library(tidyverse)

##############################################
############# WEEK 2 CLASS 1 #################
##############################################

# Defining a simple function
add.two <- function(x) {return(x + 2)}

# The names don't matter
# Assign it to any name and call the arguments whatever you want
sdfkjsdf <- function(kgguw) {return(kgguw + 2)}

# Slightly more complicated, including other functions in a function
add.two2 <- function(x) {return(paste(as.character(x), "two"))}

# What counts as "returning"?

fnc_paste_return <- function(argument1, argument2) {
    return(paste(argument1, argument2))
}

fnc_paste <- function(argument1, argument2) {
    paste(argument1, argument2)
}

fnc_paste_store <- function(argument1, argument2) {
    myvalue <- paste(argument1, argument2)
}

fnc_paste_return("hello", "world")
fnc_paste("hello", "world")
fnc_paste_store("hello", "world")


#### See groceries.qmd for all the grocer's partner conditional/functions shenanigans! ####


for (i in c(2,4,8,16)) {print(i)}

for (i in c(1,2,3,4)) {print(2^i)}

timestwo <- function(number) {x <- number*2
    print(x) 
}


i <- 1
while(i <= 4) {
    print(2^i)
    i <- i + 1}

# Hello world demo

print("Hello world!")

print(paste("Hello", "world!")) # Default separator is a space

print(paste("Hello", "world!", sep = "-")) # Make it anything

print(paste0("Hello", "world!")) # Or nothing

print(paste0("Hello", " ", "world", "!")) # Break it up with more than two arguments



##############################################
############# WEEK 3 CLASS 1/2 ###############
##############################################

### readr (and related packages): READ AND WRITE DATA ###


## REFERENCE: Key functions from readr, readxl, googlesheets4 #
## functions listed for reference (won't run without filepath string arguments)
# 
# # library(readr) # Note: readr included in library(tidyverse) or can be loaded alone
# read_delim()
# read_csv()
# read_csv2()
# 
# # library(readxl)
# read_excel_csv()
# read_excel_csv2()
# read_tsv()
# 
# # library(googlesheets4)
# read_sheet()
# 
# # readr can also export tibbles to tabular files  # with similar syntax to how it imports
# 
# # library(readr) # Note: readr included in library(tidyverse) or can be loaded alone
# write_delim()
# write_csv()
# write_csv2()
# 
# # library(readxl)
# write_excel_csv()
# write_excel_csv2()
# write_tsv()
# 
# # library(googlesheets4)
# write_sheet()


### IN-CLASS READR EXERCISE ##

# 1. Pull the most recent commit of the d2mr-assessment repo (seems like you've done that if you're here!)
# 2. Find the excel spreadsheet: `in-class-materials/data/MM data.xlsx`
# 3. Load the readxl library


# 4. Use the read_excel() function to assign the data to a tibble called mmdata


# 5. Examine your data in the console to see if it looks like the image
# Running the line below will load the csv that I made for you
# This will(/should) produce the same tibble as what you see in the slides
mmdata <- read_csv("00_in-class-materials/data/mmdata-ND.csv")

# 6. Open `MM data.xlsx` in Excel (or similar) and examine it with your human eyeballs
    # What about this file creates problems when you read-in?
    # How would you fix the issue here in the Excel document? Don't do it in code yet, but write out your ideas in comments:
    # ...
    # ...

# 7. Review the ?read_excel documentation 
# What arguments can you pass to read_excel() to fix the issue on read-in?

# 8. Use the read_excel() function to assign the data to a tibble called mmdata that looks like the image

# 9. Use write_csv() to save mmdata to an intermediate dataset: `in-class-materials/data/mmdata.csv`




### magrittr: THE PIPE ###
## (BACK TO THE SLIDES) ##

# This assumes you have the mmdata tbl in your environment already
# Make sure you run whatever code is necessary to create/load that tbl first!

# Assign everything to follows to mmdata.long
# Using the mmdata tbl ...

mmdata.long <- mmdata %>%	
    # ...select three columns... 	
    select(Red, Green, Blue) %>% 	
    # ...then made that 3 col tbl long... 	
    pivot_longer(cols = c("Red", "Green","Blue"),              
                 names_to = "Color",              
                 values_to = "Number") %>% 	
    # ...then sort that tbl by new Number column...	
    arrange(Number) 
# Pipeline is over, so everything assigned to mmdata.long


### dplyr: TIDY TRANSFORMATION ###

## REFERENCE: Key functions from dplyr #
## functions listed for reference (won't run without arguments)

# library(dplyr) # Note: dplyr included in library(tidyverse) or can be loaded alone
# filter()
# select()
# arrange()
# mutate()
# summarize()
# group_by()
# ungroup() # This "resets" grouping, good practice to include at the end of pipelines that use group_by() but not usually necessary



### IN-CLASS (time permitting) dplyr DEMO ##

## dplyr functions

# filter()
filter(mmdata, Weight >= 50)
filter(mmdata, Green == 7, Blue == 7)

# select()
select(mmdata, Bag, Weight)
select(mmdata, Bag, Yellow, Red)
select(mmdata, Red, Yellow, Bag)
select(mmdata, -Bag)
select(mmdata, starts_with("B"))
select(mmdata, bag_id = Bag, Red, Blue, wght = Weight)
select(mmdata, red = Red, bag_id = Bag)
rename(mmdata, red = Red, bag_id = Bag)

# arrange() 
arrange(mmdata, Weight)
mmdata %>% 
    arrange(Blue, -Red)

# put it together: filter+select+rename+arrange
mmdata %>% 
    filter(Weight >= 50) %>% 
    select(-Bag) %>% 
    rename(Tangerine = Orange, Lemon = Yellow) %>% 
    arrange(Tangerine, -Lemon)

# mutate()
mutate(mmdata, Candy = "M&M") #very useful
mutate(mmdata, Christmas = Red + Green)
mutate(mmdata,
       Primary = Red + Blue + Yellow,
       Avg_Primary_Weight = Weight/Primary)
mutate(mmdata,
       Weight = Weight*.035, #convert grams to ounces, replacing values in Weight column
       Weight_Gr = Weight/.035, # convert back...just to be confusing I guess
       Total = Red + Green + Blue + Orange + Yellow + Brown,
       Avg_Weight_Oz = Weight/Total)

#####

# messing with the data a bit to use in next examples
mmdata2 <- mmdata %>%  
    mutate(Whose = case_when(Bag < 11 ~ "Natalie",
                             Bag < 21 ~ "Yuchen",
                             TRUE ~ "Mian"),
           Big_Green = ifelse(Green > 7, TRUE, FALSE))
#####

# group_by() & summarize()
summarize(mmdata2, mean_wgt = mean(Weight))
group_by(mmdata2, Whose) %>% summarize(whose_mean_wgt = mean(Weight))
mmdata2 %>% 
    group_by(Whose, Big_Green) %>% 
    summarize(count_big_green_bags = n()) # the n() function counts rows without taking any arguments
mmdata2 %>% 
    group_by(Whose, Big_Green) %>% 
    summarize(Sum_BG_Green = sum(Green), Sum_BG_Red = sum(Red), Sum_BG_Blue = sum(Blue)) %>% 
    # We can regroup by just one condition to then summarize on summary values
    # although why we would want the mean per person of the sum of each color Big Green and not Big Green is beyond me
    group_by(Whose) %>% 
    summarize(why_green = mean(Sum_BG_Green), why_red = mean(Sum_BG_Red), why_blue = (mean(Sum_BG_Blue)))

# ONE GIANT PIPE!
# Who has the Citrus-y-est, Primary-est, and Christmas-y collections of M&Ms?
# This is an absurd pipe for example purposes only
# Please never actually construct something that does something so confusing and pointless

mmdata %>%  
    # Divvy up the bags evenly
    mutate(Whose = case_when(Bag < 11 ~ "Natalie",
                             Bag < 21 ~ "Yuchen",
                             TRUE ~ "Mian")) %>% 
    # Natalie benevolently removes herself from the competition
    filter(Whose != "Natalie") %>% 
    # Who cares about bag number now that we've divided them up
    select(-Bag) %>% 
    # This rowwise() function makes it easier to sum up many columns in the next line, but you could skip it if you wrote out each column individually: mutate(Total = Red + Yellow + Green + ...)
    rowwise() %>% 
    # c_across() is a helper function that makes it easier to sum up many columns
    mutate(Total = sum(c_across(Red:Brown))) %>% 
    # Fruity flavors please
    rename(Tangerine = Orange, Lemon = Yellow, Grapefruit = Red, Lime = Green) %>% 
    # Classic m&m color schemes, doesn't everyone associate grapefruit and lime with Christmas?
    mutate(Christmas = Grapefruit + Lime,
           Primary = Grapefruit + Blue + Lemon,
           Citrus = Tangerine + Lemon + Grapefruit + Lime) %>% 
    # Group by Whose to summarize Yuchen's and Mian's collections separately
    group_by(Whose) %>% 
    # Sum up the totals for each collection
    summarize(Total = sum(Total), sum_Christmas = sum(Christmas), sum_Primary = sum(Primary), sum_Citrus = sum(Citrus)) %>% 
    # Calculate the percentage of each collection that is each flavor, which we need to know for important reasons
    # Take a second to figure out exactly what these percentages mean, so we know what the winner has actually managed to achieve
    mutate(pct_Christmas = sum_Christmas/Total, pct_Primary = sum_Primary/Total, pct_Citrus = sum_Citrus/Total) %>%
    # Select only the columns we care about
    select(1:2, Christmas = pct_Christmas, Primary = pct_Primary, Citrus = pct_Citrus) %>% 
    # Sort rows to find our citrus champion
    arrange(Citrus)

# Congratulations Mian! You have the most citrus-y collection of M&Ms!


##############################################
############# WEEK 4 CLASS 1 #################
##############################################


### COMBINING DATA ###

# Bind rows with base::rbind and dplyr::bind_rows

# Create two tibbles, 3 columns: col1 col2 col3; each with 5 rows

tibble1 <- tibble(col1 = c(1, 2, 3, 4, 5),
                  col2 = c(6, 7, 8, 9, 10),
                  col3 = c(11, 12, 13, 14, 15))

tibble2 <- tibble(col1 = c(16, 17, 18, 19, 20),
                  col2 = c(21, 22, 23, 24, 25),
                  col3 = c(26, 27, 28, 29, 30))

# a third with an extra column
tibble3 <- tibble(col1 = c(16, 17, 18, 19, 20),
                  col2 = c(21, 22, 23, 24, 25),
                  col3 = c(26, 27, 28, 29, 30),
                  col4 = c(31, 32, 33, 34, 35))

# a fourth with different column names
tibble4 <- tibble(col_1 = c(16, 17, 18, 19, 20),
                  col_2 = c(21, 22, 23, 24, 25),
                  col_3 = c(26, 27, 28, 29, 30))

# a fifth with a different number of rows
tibble5 <- tibble(col1 = c(16, 17, 18, 19, 20, 21),
                  col2 = c(21, 22, 23, 24, 25, 26),
                  col3 = c(26, 27, 28, 29, 30, 31))

tibble1
tibble2
tibble3
tibble4
tibble5

rbind(tibble1, tibble2) # works fine
# rbind(tibble1, tibble3) ## NOT RUN -- gives an error because the tibbles have a different number of columns
# rbind(tibble1, tibble4) ## NOT RUN -- gives an error because the tibbles have different column names

bind_rows(tibble1, tibble2) # works fine, identical to rbind
bind_rows(tibble1, tibble3) # works fine, adds the extra column with NAs for missing the tibble1 values
bind_rows(tibble1, tibble4) # runs without error but treats the columns as different, so it adds the tibble4 columns to the end of the tibble1 columns

# Bind columns with base::cbind and dplyr::bind_cols

cbind(tibble1, tibble2) # works fine -- adds the columns of tibble2 to the right of tibble1, though it's not great that the columns have the same names
cbind(tibble1, tibble3) # works fine -- same number of rows, it's works like above
# cbind(tibble1, tibble5) ## NOT RUN -- gives an error because the tibbles have a different number of rows

bind_cols(tibble1, tibble2) # works fine, identical to cbind *except* it renames the columns to avoid conflicts, which is preferable
bind_cols(tibble1, tibble4) # column names were already different, so it doesn't need to rename
# bind_cols(tibble1, tibble5) ## NOT RUN -- gives an error because the tibbles have a different number of rows (same as with cbind)

# Note the difference between bind_rows and bind_cols in how they handle different numbers of rows/columns
# bind_rows will add NAs for missing values, bind_cols will give an error

## JOINS ##

# a family of functions that combine data from two data frames based on common variables
# anything you can do with bind_rows/bind_cols you can do with joins (not the reverse)

# Two data frames to join
# Both have a "name" column, but the values in the "name" columns are not the same
# Mick is only in _members, Keith is only in _instruments
# John and Paul are in both

band_members
band_instruments

## Four most common kinds of joins

# Inner join: only rows with matching values in both data frames are returned
# result: 2 rows, one for John and one for Paul
inner_join(band_members, band_instruments)

# Full join: all rows from both data frames are returned, with NAs for missing values
# result: 4 rows, one for each band member; NAs for Mick's instrument and Keith's band
full_join(band_members, band_instruments)


# Left join: all rows from the first data frame are returned, with NAs for missing values in the second data frame
# result: 3 rows, one for each band member in band_members; no Keith
left_join(band_members, band_instruments)


# Right join: all rows from the second data frame are returned, with NAs for missing values in the first data frame
# result: 3 rows, one for each band member in band_instruments; no Mick
right_join(band_members, band_instruments)

# For left and right joins, order matters! The first data frame is the "left" and the second is the "right"
# Which means a left join is the same as a right join with the data frames reversed (though row order will differ)
left_join(band_members, band_instruments)
right_join(band_instruments, band_members)


# The "by" argument specifies the column(s) to join on
# These are all identical
left_join(band_members, band_instruments)
left_join(band_members, band_instruments,
          by = join_by(name))
left_join(band_members, band_instruments,
          by = "name")
right_join(band_instruments, band_members)
band_instruments %>%
    right_join(band_members,
               by = "name")

# If the columns have different names, you can specify the columns to join on in each data frame
rename.band_members <- band_members %>%
    rename(member = name)

# the order of the names should match the order of the dfs
left_join(rename.band_members, band_instruments,
          by = c("member" = "name")) 



### TIDYR ###


# Read in the mmdata from above if it's not loaded!
# mmdata <- read_csv("00_in-class-materials/data/mmdata-ND.csv")


## Pivot wide and long

# pivot mmdata (originally wide) to long
mmdata.long <- mmdata %>%
    pivot_longer(cols = c("Red", "Green", "Blue", "Orange", "Yellow", "Brown"),
                 names_to = "Color",
                 values_to = "Number")


# pivot long mmdata to wide; back to where it started
mmdata.wide <- mmdata.long %>% 
    pivot_wider(names_from = "Color", values_from = "Number") %>% 
    # Relocate works like select by reordering columns, but doesn't drop anything
    # Kind of like how rename works like select by renaming but not dropping
    relocate(Weight,.after = last_col())

# They are the same
mmdata.wide == mmdata
# Or are they??
all.equal(mmdata.wide, mmdata)
# Basically yes. The differences are in the df attributes, not the data
# You can view attributes of any object in the environment tab by clicking
# the little blue arrow next to the object name
# or with

attributes(mmdata.wide)
attributes(mmdata)


# Create example tbl for tidyr functions

# ?Q?: Is this wide or long?
glasses <- tibble(
    condition = c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2),
    participant = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6),
    score = runif(12),
    notes = c("glasses_none", "glasses_none", "none_none", "none_none",
              "none_incomplete", "none_incomplete", "none_none", "none_none",
              "glasses_none", "glasses_none", "glasses_late", "glasses_late")
)

# Separate "notes" column into two columns by recognizing "_" separator
glasses.sep <- glasses %>%
    separate(notes, c("vision_correction", "other_notes"), sep = "_")

# separate() works, but it has been superseded by 
# separate_wider_position() & separate_wider_position()

# *_position() will split based on integer values for each new column and for the separator
glasses.sep.pos <- glasses %>% 
    separate_wider_position(notes, c(vision_correction = 4, 1, other_notes = 4), too_many = "drop") # not super helpful here

# *_delim() will split based on a given separator, like separate()
# but has slightly different syntax
glasses.sep.delim <- glasses %>% 
    separate_wider_delim(notes, delim = "_", names = c("vision_correction", "other_notes"))

## Take a minute to view glasses.sep, glasses.sep.pos, and glasses.sep.delim to see the differences

# Unite the 2 columns back to 1 with a new separator (use ; not _)
glasses.unite <- glasses.sep %>%
    unite("semicolon_notes", "vision_correction":"other_notes", sep = ";")


## Missing data

# ?Q? What's actually missing in this missingdata tibble?
missingdata <- tibble(
    condition = c(1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2),
    participant = c(1, NA, NA, NA, 2, NA, NA, NA, 3, NA, NA),
    trial = c(1, 2, 3, 4, 1, 2, 3, 4, 1, 3, 4),
    score = c(0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1),
)

fixeddata <- missingdata %>%
    # participant id is only indicated for the first observation for each participant, fill it in for the rest
    fill(participant) %>% 
    # p3 did not complete trial 2, but we should still see evidence of the (incomplete) observation
    complete(condition, trial) %>%
    replace_na(list(participant = 3)) %>% # a HACK (will not generalize/will break)
    arrange(condition, participant, trial) # just to make it pretty

## Alternatively...

# use crossing() to create a tibble of all possible (i.e. expected) participant x trial observations 
# note that crossing() is a wrapper for expand_grid(), which is a helper for expand(), meaning you can replicate this just with expand() but this is more convenient
# Remember this isn't the data itself, it's a kind of template for how the data could be structured in an ideal world with nothing missing
all.ptcp.trial.combos <- crossing(
    participant = unique(missingdata$participant),
    trial = c(1:4)) %>%
    filter(!is.na(participant)) 

## now fix the missing data by...
fixeddata2 <- missingdata %>%
    # using the same fill() fnct to fix participant col.
    fill(participant) %>%
    # join with the expanded tibble of expected combos
    full_join(all.ptcp.trial.combos) %>%
    arrange(participant, trial) %>% # superstitious (it's already sorted this way) but harmless
    # the expected observation for p3, trial2 has been created and is missing values for score (appropriately) and condition, but we know what the condition should be and use fill() as we did above 
    fill(condition)



## STRINGR ##

# Load the stringr library
# library(stringr) # Note: stringr included in library(tidyverse) or can be loaded alone

# Create a character vector with some strings
my_strings <- c("Hello, world!", "Goodbye, world!", "Hello, universe!", "Goodbye, universe!", "Hello hello hello!", "Goodbye cruel, cruel world!")

# str_c() concatenates strings
# similar to but not quite the same as paste()
# default separator is empty string "", like paste0()
paste("Hello", "world") # "Hello world"
paste0("Hello", "world") # "Helloworld"
str_c("Hello", "world") # "Helloworld"
str_c("Hello", "world", sep = ", ") # "Hello, world"

# but it handles missing values differently than paste() and paste0()
str_c("Hello", NA, "world") # NA
paste0("Hello", NA, "world") # "HelloNAworld"

# str_glue() is a version of paste() that uses {} to insert variables
name <- "world"
str_glue("Hello, {name}!")
str_c("Hello, ", name, "!") # equivalent to the above
# main the difference between str_c() and str_glue() is that str_glue() that it can insert variables directly into the string rather than concatenating them at the end, which can get annoying
str_glue("Hello, {name}! The time is {Sys.time()}") # you can insert any R expression into the {}
str_c("Hello, ", name, "! The time is ", Sys.time()) # equivalent to the above
# the other difference is in the output type, str_glue has its own class (+ character)
class(str_glue("Hello, {name}! The time is {Sys.time()}")) 
class(str_c("Hello, ", name, "! The time is ", Sys.time())) 


# str_length() returns the number of characters in a string
str_length("hello")
str_length(my_strings) # returns a vector of the number of characters in each string in the vector
str_length(my_strings[1]) # index just the first element of the vector

# str_sub() extracts a substring based on starting and ending positions
str_sub("hello", 1, 3)
str_sub("hello", 1, 1)
# if you only provide one position or the end position is greater than the length of the string it will return from that(first) position to the end of the string
str_sub("hello", 2) # "ello"
str_sub("hello", 2, 8) # "ello"
# use negative positions to count back from the end of the string
str_sub("hello", -2)
str_sub("hello", -2, -1)
str_sub("hello", -2, 5)

# str_detect() returns TRUE if a pattern is found in a string
str_detect("Hello there!", "Hello")
str_detect(my_strings, "Hello")
str_detect(my_strings, "hello") # case sensitive
str_detect(my_strings, "Hello", negate = TRUE) # negate = TRUE returns TRUE if the pattern is NOT found
# Similar to the %like% helper from data.table
# library(data.table)
# "hello" %like% "ello"
# "ello" %like% "hello"

# return the index of matching values with str_which()
str_which(my_strings, "Hello")

# to return the actual values that match the pattern, use str_subset()
str_subset(my_strings, "Hello")

# str_detect() and related functions are basically more user-friendly 
# versions of the base R grep functions
grepl("Hello", "Hello there!") # <- ~ str_detect, returns T/F
grep("Hello", "Hello there!") # <- ~ str_which, returns index (in a vector that is just a single thing, and so not very useful)
grep("Hello", my_strings) # <- ~ str_which, returns index (in a real vector, so more useful)
# the grep equivalent of str_subset() is grep() with value = TRUE
grep("Hello", my_strings, value = TRUE) # <- ~ str_subset, returns the actual values that match the pattern


# str_replace() replaces the first instance of a pattern in a string
str_replace("Hello, world!", "world", "universe")
str_replace(my_strings, "hello", "hi") # only replaces the first instance of "hello" in each string, case sensitive
str_replace(my_strings, "cruel", "kind") # only replaces the first instance of "cruel" in each string, case sensitive

# str_replace_all() replaces all instances of a pattern in a string
str_replace_all(my_strings, "hello", "hi") # replaces all instances of "hello" in each string, case sensitive
str_replace_all(my_strings, "cruel", "kind") # replaces all instances of "cruel" in each string, case sensitive

# str_replace() and str_replace_all() are basically more user-friendly 
# versions of the base R sub/gsub functions
sub("hello", "hi", my_strings) # <- ~ str_replace, only replaces the first instance of "hello" in each string, case sensitive  
gsub("hello", "hi", my_strings) # <- ~ str_replace_all

# since stringr functions are more or less versions of grep functions,
# they can take regular expressions

# for example, using str_replace_all():
str_replace_all(my_strings, "Hello|Goodbye", "Greetings") # replaces "Hello" or "Goodbye" with "Greetings"
str_replace_all(my_strings, "[Hh]ello", "Greetings") # replaces "Hello" or "hello" with "Greetings"
str_replace_all(my_strings, "[Hh]ello|[Gh]oodbye", "Greetings") # replaces "Hello" "Goodbye" with "Greetings", functinally case insensitive
# you can also make it case insensitive by explicitly saying the pattern to match is a regex (wrap in regex()) and setting ignore_case = TRUE **within the regex() function** not the str_replace_all() function
str_replace_all(my_strings, regex("hello", ignore_case=TRUE), "Greetings") # replaces "Hello" or "hello" with "Greetings"

# delete patterns from strings by replacing with ""
str_replace_all(my_strings, "Hello|Goodbye", "") # deletes "Hello" or "Goodbye"
# or with str_remove() & str_remove_all()
str_remove(my_strings, "Hello|Goodbye") # deletes "Hello" or "Goodbye"
str_remove_all(my_strings, "Hello|Goodbye") # deletes "Hello" or "Goodbye"

# read more about the magic of regex here: https://stringr.tidyverse.org/articles/regular-expressions.html

# MORE STRINGR FUNCTIONS #

# str_to_*()  convert strings to lowercase and uppercase
str_to_lower("Hello, world!")
str_to_upper("Hello, world!")
str_to_title("Hello, world!") # capitalizes the first letter of each word

# str_trim() removes leading and trailing whitespace
str_trim("  Hello, world!  ")

# str_pad() pads strings to a specified width
str_pad("Hello", width = 10) # pads with spaces and places in front by default
str_pad("Hello", width = 10, side = "right") # pads with spaces and places at the end
str_pad("Hello", width = 10, pad = "-") # pads with a specified character
# pad both sides
str_pad("Hello", width = 10, side = "both")

# str_trunc() truncates strings to a specified (max) width
# 3 of those characters will be an ellipsis ("...") by default
str_trunc("Hello, world!", width = 8) # truncates to 8 characters; by default the truncation / ellipsis is on the right side
# but you can also put it on the left or in the center
str_trunc("Hello, world!", width = 8, side = "left")
str_trunc("Hello, world!", width = 8, side = "center")
# and you can change the ellipsis to any character(s)
str_trunc("Hello, world!", width = 8, ellipsis = "!")
str_trunc("Hello, world!", width = 3, ellipsis = "!!!")
str_trunc("Hello, world!", width = 8, ellipsis = "")

# str_split() splits strings into a list of substrings
str_split("Hello, world!", ",")
str_split(my_strings, " ") # splits each string in the vector by spaces

# str_flatten() flattens a list of strings into a single string
str_flatten(my_strings) # collapses the strings with no separator
str_flatten(my_strings, collapse = " ") # collapses the strings with a space between them


### FORCATS ###

# Load the forcats library
# library(forcats) # Note: forcats included in library(tidyverse) or can be loaded alone

# Create a factor with levels in a specific order
my_factor <- factor(c("a", "b", "c", "a", "b", "c", "a", "b", "c"), levels = c("c", "b", "a"))
# This ^ is an unordered factor, but the order that the levels are listed in matters for working with the factor

# use levels() to see the levels of a factor
levels(my_factor)

my_ordered_factor <- factor(c("a", "b", "c", "a", "b", "c", "a", "b", "c"), levels = c("c", "b", "a"), ordered = TRUE)

# use levels() to see the levels of an ordered factor
levels(my_ordered_factor)

# you can see the difference in order if you look at each
my_factor
my_ordered_factor # the levels are represented least to greatest

# or to change the levels of a factor
# replacing all instances of "a" with "z", "b" with "y", and "c" with "x"
my_factor2 <- my_factor
levels(my_factor2) <- c("z", "y", "x") # order matters, should match existing level order 
# alternatively, specify which new levels correspond to the old levels
my_factor3 <- my_factor
levels(my_factor3) <- c("z" = "a", "y" = "b", "x" = "c") # order doesn't matter

my_factor2
my_factor3


# fct_count() counts the number of occurrences of each level
fct_count(my_factor)


## Advanced factor manipulation

# Start off with more basics

# Create a basic factor
fruits <- factor(c("apple", "banana", "apple", "cherry", "banana", "banana"))
print(fruits)

# Check and modify levels
levels(fruits)  # View current levels
levels(fruits) <- c("APPLE", "BANANA", "CHERRY")  # Change level names, matched on order
levels(fruits)  # Verify changes
levels(fruits) <- c("apple", "banana", "cherry")  # Change them back

# Create factor with specified ordered levels
fruits_ordered <- factor(
    c("apple", "banana", "apple", "cherry", "banana"),
    levels = c("cherry", "banana", "apple"),
    ordered = TRUE
)
print(fruits_ordered) # check levels, notice the new ordering "<"

# Even for unordered factors, you need to pay attention to the order of the levels in order to manipulate them correctly! They just won't be treated as ordered categorical variables

# Move to more advanced

# Using fct_relevel
fruits2 <- fct_relevel(fruits, "cherry", "apple")  # Move specific levels
# Same with base R
fruits2_base <- factor(fruits, levels = c("cherry", "apple", "banana"))

# Using fct_reorder to reorder by another variable
bags_sold <- c(5, 2, 7, 1, 3, 6)
# fct_reorder will match each element of fruits with the corresponding element of bags_sold
# The .fun argument specifies the function to use for reordering
# Resulting in the levels of fruits being reordered based on the mean of bags_sold, least to greatest (cherries = 1, bananas = 2.5, apples = 6)
fruits3 <- fct_reorder(fruits, bags_sold, .fun = mean)  # Order by mean of values
fruits3
# Base R equivalent requires more steps
means <- tapply(bags_sold, fruits, mean)
fruits3_base <- factor(fruits, levels = names(sort(means)))

# Order by frequency
fruits4 <- fct_infreq(fruits)  # Most frequent first
# Base R equivalent
freq_table <- sort(table(fruits), decreasing = TRUE)
fruits4_base <- factor(fruits, levels = names(freq_table))

# Reverse level order
fruits5 <- fct_rev(fruits)
# Reverses the actual functional order of ordered levels as well
print(fruits_ordered)
fruits_ordered_rev <- fct_rev(fruits_ordered)
print(fruits_ordered_rev) # check levels, notice the new ordering
# Base R equivalent
fruits5_base <- factor(fruits, levels = rev(levels(fruits)))



