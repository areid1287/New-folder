# Include tidyverse package (and install if it doesn't exist)
if (!require("tidyverse")) {
  install.packages("tidyverse");
  library("tidyverse")
}

# Read csv file into a dataframe
X <- read.csv("A1_Ex1.csv")

# Print structure of X
str(X)

glue::glue("sum of expenditure: {sum(X$expenditure)}")

# Select and display March total expenditure
X %>%
 filter(month == "Mar") %>%
 summarize(March_expenditure = sum(expenditure))

# Select and display total expenditure on cloudy March days
X %>%
  filter(month == "Mar", weather == "cloudy") %>%
  summarize(March_cloudy_expenditure = sum(expenditure))

# Display individual month with highest expenditure
X %>% 
  select(month, expenditure) %>% 
  filter(expenditure == max(expenditure))
