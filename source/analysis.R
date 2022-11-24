library(tidyverse)
library(usmap)

# The functions might be useful for A4
source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num = 6) {
  v <- seq(1:num)
  return(v)
}

incarceration <- read.csv("../incarceration_trends.csv")
## Section 2  ----
# What is the average value of my variable across all the counties (in a given year)?
avg_totalpop_2013 <- incarceration %>%
  drop_na() %>%
  select(total_pop, year) %>%
  filter(year == 2013)

avg_totalpop_2013 <- avg_totalpop_2013 %>%
  mutate(avg_totalpop_2013, avg = mean(total_pop)) %>%
  select(avg)

avg_totalpop_2013 <- unique.data.frame(avg_totalpop_2013)

avg_totalpop_2013 <- avg_totalpop_2013 %>%
  mutate(avg_totalpop_2013, avg = round(avg, digits = 0)) %>%
  pull(avg)


# Where is my variable the highest or lowest?
max_totalpop_2013 <- incarceration %>%
  drop_na() %>%
  select(total_pop, year, county_name) %>%
  filter(year == 2013) %>%
  filter(total_pop == max(total_pop)) %>%
  pull(county_name)

min_totalpop_2013 <- incarceration %>%
  drop_na() %>%
  select(total_pop, year, county_name) %>%
  filter(year == 2013) %>%
  filter(total_pop == min(total_pop)) %>%
  pull(county_name)


# How much has my variable change over the last N years?
change_totalpop_2013 <- incarceration %>%
  drop_na() %>%
  select(total_pop, year, county_name) %>%
  filter(year == 2000 | year == 2013) %>%
  filter(county_name == "King County")

change_totalpop_2013 <- change_totalpop_2013 %>%
  mutate(change_totalpop_2013, difference = diff(total_pop)) %>%
  select(difference)

change_totalpop_2013 <- unique.data.frame(change_totalpop_2013)

change_totalpop_2013 <- change_totalpop_2013 %>%
  pull(difference)

#----------------------------------------------------------------------------#

## Section 3  ----
# Growth of the U.S. Prison Population
# Use DPLYR and ggplot2 to replicate Figure 1
# That is, produce a bar chart that shows the growth of the U.S. prison population from 1970 to 2018.

# This data wrangling function should return a data frame that is suitable for visualization.
get_year_jail_pop <- function() {
  year_total_pop <- incarceration %>%
    select(total_jail_pop, year) %>%
    group_by(year) %>%
    drop_na() %>%
    summarise(Total = sum(total_jail_pop))
  return(year_total_pop)
}

# This plotting function should return the chart. This function: (1) Takes no parameters; and (2) Should call the data wrangling function.
plot_jail_pop_for_us <- function() {
  plot <- ggplot(get_year_jail_pop(), aes(x = year, y = Total)) +
    geom_bar(stat = "identity") +
    ggtitle("Jail Population in the US") +
    ylab("Population") +
    xlab("Year")
  return(plot)
}

## Section 4  ----
# Growth of Prison Population by State
# produce a line chart that shows the growth of the U.S. prison population from 1970 to 2018 by one or more states.
states <- read.csv("../source/state_names_and_codes.csv")
states <- states %>%
  select(Code) %>%
  pull(Code)

get_jail_pop_by_states <- function(states) {
  pop_by_state <- incarceration %>%
    select(state, total_jail_pop, year) %>%
    filter(state %in% states) %>%
    group_by(state, year) %>%
    drop_na() %>%
    summarise(pop = sum(total_jail_pop), .groups = "drop")
  return(pop_by_state)
}


plot_jail_pop_by_states <- function(states) {
  jail_pop_by_states <- get_jail_pop_by_states(states) %>%
    ggplot(aes(x = year, y = pop, group = state, color = state)) +
    geom_line() +
    ggtitle("Jail Population by States") +
    ylab("Total Jail Population") +
    xlab("Year")
  return(jail_pop_by_states)
}

#----------------------------------------------------------------------------#
## Section 5  ----
get_states_gender_jail_pop <- function(states) {
  gender_jail_pop <- incarceration %>%
    select(year, female_jail_pop,male_jail_pop,state) %>%
    filter(state %in% states) %>%
    group_by(year,state) %>%
    drop_na() %>%
    summarise(female_pop = sum(female_jail_pop), male_pop = sum(male_jail_pop), .groups = 'drop')
  gender_jail_pop <- gender_jail_pop %>%
    mutate(gender_jail_pop, total_male_female = male_pop + female_pop)
return(gender_jail_pop)
}

plot_female_jail_pop <- function(states) {
  female_plot <- get_states_gender_jail_pop(states) %>%
    ggplot(aes(x=year, y=female_pop, color=state)) +
    geom_line()+
    ggtitle("Female Populations in Jails per States") +
    ylab("Jail Population") +
    xlab("Year")
  return(female_plot)
}

plot_male_jail_pop <- function(states) {
  male_plot <- get_states_gender_jail_pop(states) %>%
    ggplot(aes(x=year, y=male_pop, color=state)) +
    geom_line()+
    ggtitle("Male Populations in Jails per States") +
    ylab("Jail Population") +
    xlab("Year")
  return(male_plot)
}
  

#----------------------------------------------------------------------------#

## Section 6  ----
female_jail_pop_2018 <- incarceration %>%
  select(year, female_jail_pop,state) %>%
  filter(state %in% states) %>%
  group_by(year, state) %>%
  drop_na() %>%
  summarise(female_jail_pop=sum(female_jail_pop), .groups = 'drop') %>%
  filter(year == 2018)
  
plot_usmap(regions = "states", data = female_jail_pop_2018, values = "female_jail_pop", color="red") +
  scale_fill_continuous(low = "white", high = "red",name="Jail Population", label = scales::comma)

male_jail_pop_2018 <- incarceration %>%
  select(year, male_jail_pop,state) %>%
  filter(state %in% states) %>%
  group_by(year, state) %>%
  drop_na() %>%
  summarise(male_jail_pop=sum(male_jail_pop), .groups = 'drop') %>%
  filter(year == 2018)

plot_usmap(regions = "states", data = male_jail_pop_2018, values = "male_jail_pop", color="red") +
  scale_fill_continuous(low = "white", high = "red",name="Jail Population", label = scales::comma)


