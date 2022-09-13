babynames %>% 
  filter(name == "Sebastian", sex == "M") %>% 
  select(prop, year) %>% 
  ggplot(mapping = aes(x = year,
                       y = prop,
                       colour = year)) +
  geom_line() +
  ggtitle("Popularity of Sebastian as a boys name", 
          subtitle = "From years 1880 - 2017") +
  scale_colour_gradient(low = "purple", high = "red")

babynames %>% 
  filter(name == "Sebastian" & sex == "M" | name == "Alana" & sex == "F") %>% 
  select(prop, year, name) %>% 
  ggplot(mapping = aes(x = year,
                       y = prop,
                       colour = name)) +
  geom_line() +
  ggtitle("Popularity of Sebastian as a boys name", 
          subtitle = "From years 1880 - 2017") +
  scale_colour_gradient(low = "purple", high = "red")


babynames %>% 
  filter(str_detect(name, "^Al+an+ah*"), sex == "F") %>% 
  select(prop, year, name) %>% 
  ggplot(mapping = aes(x = year,
                       y = prop,
                       colour = name)) +
  geom_line() +
  ggtitle("Populatiry of variations of the name Alana",
          subtitle = "From years 1880 - 2017")
  
# scale_colour_manual(values = c("red", 
#                                  "purple", 
#                                  "black", 
#                                  "green", 
#                                  "blue", 
#                                  "orange", 
#                                  "black", 
#                                  "pink"))

## + scale_colour_gradient2(low = "grey", 
##                          high = "black")
##                          mid = "red", 
##                          midpoint = 1960,


# Exercise 3
frog_signal$StandardTotal = rowSums(frog_signal[, c("Standard1", "Standard2", "Standard3")])
# Calculates Mean and SD of call duration for all combinations of speaker position 
#                   and which call was first presented
frog_signal %>% 
  group_by(SpeakerPosition, FirstPresented) %>% 
  summarise(Mean = mean(AlternativeCD), 
            Standard_Dev = sd(AlternativeCD)) %>% 
  arrange(Mean)

frog_signal %>% 
  filter(is.na(SpeakerPosition)) %>% 
  select(TwoChoice, FirstPresented)

frog_signal %>% 
  select(num_range("Standard", 1:3), num_range("Alternative", 1:3))


# Exercise 4
crampton.pig %>% 
  ggplot(mapping = aes(x = weight1, y = weight2, size = feed, colour = treatment)) +
  geom_point() +
  scale_colour_manual(values = c("#E5B3FA", "#F741AC", "#DC1B67", "#B14FDC", "#CD1656"))

crampton.pig %>% 
  mutate("weight2 - weight1" = weight2 - weight1) %>% 
  ggplot(aes(x = weight2 - weight1, fill = treatment)) +
  geom_density()

crampton.pig %>% 
  ggplot(aes(x = treatment, y = feed, fill = rep)) +
  geom_bar(stat = "identity", position = position_dodge())







