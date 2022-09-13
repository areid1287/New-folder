# library(tidyverse)
# library(dplyr)
# 
# # renaming time column to specify it is in PAL-times
# records2 <- rename(records, "time PAL" = "time")
# 
# # Adding time column that is in NTSC-times
# records2 <- records2 %>% 
#   mutate("time NTSC" = `time PAL` * 0.96)
# 
# # Moving NTSC-times col next to PAL-times col
# records2 <- records2 %>% 
#   relocate("time NTSC", .before = record_duration)

library(tidyverse)
library(dplyr)
library(stringr)
library(readr)

# records <- read_csv("records.csv")

# Plot 1
records %>% 
  # Filter data to include only the three lap, no shortcut times
  filter(type == "Three Lap", shortcut == "No") %>% 
  # Create line graph of date against WR time
  ggplot(aes(x = date, y = time)) +
  geom_line() + 
  # Rotating x-axis labels so they are vertical
  guides(x =  guide_axis(angle = 90)) +
  # Split plot to create time series chart for each specific track 
  facet_wrap(facets = vars(track), scales = "free") +
  # Create chart title and subtitle
  ggtitle("How the three lap, with no shortcut world record develop over time")

# Plot 2
records %>% 
  # Filter data to include only the WR times for Rainbow Road
  filter(track == "Rainbow Road") %>% 
  # Creating a new column called Race containing string specifying the type 
  # of race and if it had a shortcut or not  
  mutate(Race = str_c(type, " with ", 
                      ifelse(shortcut == "No", tolower(shortcut) , ""), 
                      ifelse(shortcut == "No", " shortcut", "shortcut"))
  ) %>% 
  # Creating point and line graph of date against WR time, grouping by Race
  ggplot(aes(x = date, y = time, colour = Race)) +
  geom_point() +
  geom_line() +
  # Create chart title and subtitle
  ggtitle("How the WR for Rainbow Road develop over time", 
          subtitle = "With shortcuts, it is quicker to finish a 3 lap race than completing a single lap!")


# pisa_2018 <- read_csv("pisa_2018.csv")

# Plot 1
pisa_2018 %>%
  # Selecting only country, read and math cols
  select(-c(gender, science)) %>% 
  group_by(country) %>%
  # Construct tibble containing mean of read and math for each country
  summarise(mean_math = mean(math), mean_read = mean(read)) %>% 
  # Create scatter plot of mean_math against mean_read
  ggplot(aes(x = mean_math, y = mean_read)) +
  geom_point() +
  # Adding rectangular data labels for scatter plot showing between country comparison 
  ggrepel::geom_label_repel(aes(label = country)) +
  # Create chart title and subtitle
  ggtitle("Average reading against average maths for each country", 
          subtitle = "Australia has room for improvement!")

# Plot 2
# Data wrangling to obtain new data table with mean of read, math and science for each country
data <- pisa_2018 %>%
  group_by(country, gender) %>%
  summarise(math = mean(math), read = mean(read), sci = mean(science)) %>% 
  # Finding the difference (female - male mean) for each country
  mutate(mean_math = math[1] - math[2], mean_read = read[1] - read[2], mean_sci = sci[1] - sci[2]) %>% 
  select(country, mean_math, mean_read, mean_sci)

# Filtering out the repeated rows that resulted from grouping gender
distinct(data) %>%
  # Change table structure so the type of mean for each country (math, reading or science) is a new variable 
  # and diff col include the differences for each country
  pivot_longer(!country, names_to = "type", values_to = "diff") %>% 
  # Add new col. of logical, specifying whether or not the differences (in col. diff) satisfy the condition diff > 0
  mutate(lgcl = diff > 0) %>% 
  # Create scatter plot of diff against country, showing between country comparison of differences
  ggplot(aes(x = diff, y = country, colour = lgcl)) +
  geom_point() +
  # Add center line at x = 0 (where mean for girls would be equal to  mean for boys for each country)
  geom_vline(xintercept = 0) +
  xlim(-90, 90) +
  guides(colour = guide_legend(title = "diff > 0")) +
  # Add annotations to each plot specifying which side of the graph represents where 
  # boys have higher mean vs where girls have higher mean for each country 
  annotate("text", x = -75, y = "AUS", label = "Boys", color = "black") +
  annotate("text", x = 75, y = "AUS", label = "Girls", color = "black") +
  # Split plot to create comparison chart for different countries for each specific subject/type of mean 
  facet_grid(~ type) +
  # Create chart title and subtitle
  ggtitle("Average gender difference (diff = female − male) per Country", 
          subtitle = "Gender gap in reading is universal, but the math gap is not.")







