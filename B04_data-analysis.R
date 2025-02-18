## --------------------------------------------------------------------------
#| label: setup
#| include: false

# set options
knitr::opts_chunk$set(
  tidy = FALSE, 
  message = FALSE,
	warning = FALSE)

options(htmltools.dir.version = FALSE)



## --------------------------------------------------------------------------

library(readr)
library(janitor)
library(here)
library(dplyr)
library(tidyr)
library(knitr)
library(ggplot2)


# read in data set
ph <- read_delim(here("data", "tidy_ph.csv"),  # specify folder, file
                 delim = ",")


# take a look at first few rows
head(ph)



## --------------------------------------------------------------------------

mean_ph <- ph |>
  group_by(milk_type, age) |>
  summarize(mean_ph = mean(ph, na.rm = TRUE))



## --------------------------------------------------------------------------

mean_ph |>
  pivot_wider(names_from = milk_type, values_from = mean_ph) |>
  kable(digits = 2)



## --------------------------------------------------------------------------

mean_ph |> 
  pivot_wider(names_from = age, values_from = mean_ph) |> 
  kable(digits = 2)



## --------------------------------------------------------------------------

ggplot(mean_ph,
       aes(x = age, y = mean_ph,
           color = milk_type)) +
  geom_point()



## --------------------------------------------------------------------------

# pick your colors
col <- c("darkorange", "darkgreen", "cyan", "purple")

# pick your point size
pt_size <- 2

ggplot(mean_ph,
       aes(x = age, y = mean_ph,
           color = milk_type)) +
  geom_point(size = pt_size) +
  scale_color_manual(values = col) +
  labs(x = "better axis title",
       y = "better axis title") +
  theme_grey() +
  theme(legend.position = "left")



## --------------------------------------------------------------------------

ggplot(mean_ph,
aes(x = age, y = mean_ph,
           color = milk_type)) +
  geom_point(size = pt_size) +
  scale_color_manual(values = col) +
  labs(x = "better axis title",
       y = "better axis title") +
  theme_grey() +
  theme(legend.position = "left")



## --------------------------------------------------------------------------

# create a vector with milk types (modify accordingly)
milks <- c("whole", "buttermilk", "oat", "chocolate")

# create subset
my_ph_data <- mean_ph |>
  filter(milk_type %in% milks)



## --------------------------------------------------------------------------

ggplot(my_ph_data,
       aes(x = age, y = mean_ph,
           color = milk_type)) +
  geom_point(size = pt_size) +
  geom_line() +
  scale_color_manual(values = col) +
  labs(x = "better axis title",
       y = "better axis title") +
  theme_grey() +
  theme(legend.position = "left")



## --------------------------------------------------------------------------

# Calculate the mean pH difference between milk age 0 and milk age 8
change_ph <- mean_ph |>
  filter(age %in% c(0, 8)) |>
  pivot_wider(names_from = age, values_from = mean_ph, names_prefix = "age_") |>
  mutate(ph_difference = age_0 - age_8)

# take a look at the summary table
change_ph |>
  kable(digits = 2)



## --------------------------------------------------------------------------

ggplot(change_ph,
       aes(x = milk_type, y = ph_difference,
           fill = milk_type)) +
  geom_bar(stat = "identity")



## --------------------------------------------------------------------------

ggplot(change_ph,
       aes(x = milk_type, y = ph_difference,
           fill = milk_type)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = col) +
  labs(x = "better axis title",
       y = "better axis title") +
  theme_grey() +
  theme(legend.position = "left")



## --------------------------------------------------------------------------

# Calculate rate of change
rate_ph <- mean_ph |>
  arrange(milk_type, age) |>
  group_by(milk_type) |>
  mutate(rate_of_change = (mean_ph - lag(mean_ph)) / (age - lag(age))) |>
  ungroup() |>
  filter(!is.na(rate_of_change))

# print summary table
rate_ph |>
  kable(digits = 4)



## --------------------------------------------------------------------------

# levels microbial growth
levels_growth <- c("none", "minimal", "moderate", "substantial", "extensive")

# levels cells
levels_cells <- c("absent", "few", "moderate", "abundant", "dominant")

# levels fungi
levels_fungi <- c("none", "trace", "moderate", "abundant", "extensive")

# read data
microbes <- read_delim(here("data", "microbe-community.csv"),
                       delim = ",") |>
  clean_names() |>
  mutate(microbial_growth = ordered(microbial_growth, levels = levels_growth),
         cocci = ordered(cocci, levels = levels_cells),
         bacilli = ordered(bacilli, levels = levels_cells),
         spirilla = ordered(spirilla, levels = levels_cells),
         yeast = ordered(yeast, levels = levels_cells),
         mold = ordered(mold, levels_fungi),
         age = as.character(age))

# print overview
microbes |>
  kable()



## --------------------------------------------------------------------------

# subset data
growth <- microbes |>
  select(milk_type, age, microbial_growth)

# create heatplot
ggplot(growth, 
       aes(x = age, y = milk_type,
           fill = microbial_growth)) +
  geom_tile(color = "black") +
  scale_fill_viridis_d() +
  labs(x = "milk age", y = "milk type") +
  theme_bw() +
  theme(legend.position = "bottom")



## --------------------------------------------------------------------------

# subset data
mold <- microbes |>
  select(milk_type, age, mold)

# create heatplot
ggplot(mold, 
       aes(x = age, y = milk_type,
           fill = mold)) +
  geom_tile(color = "black") +
  scale_fill_viridis_d() +
  labs(x = "milk age", y = "milk type") +
  theme_bw() +
  theme(legend.position = "bottom")



## --------------------------------------------------------------------------

# create a vector with milk types (modify accordingly)
milks <- c("whole", "buttermilk", "oat", "chocolate")

# create subset
cells <- microbes |>
  select(milk_type, age, cocci, bacilli, spirilla, yeast) |>
  pivot_longer(names_to = "cell_type", values_to = "abundance", 3:6) |>
  filter(milk_type %in% milks)

# plot multifaceted plot
ggplot(cells, 
       aes(x = age, y = cell_type,
           fill = abundance)) +
  geom_tile(color = "black") +
  facet_wrap(. ~ milk_type) +
  scale_fill_viridis_d() +
  labs(x = "milk age", y = "cell type") +
  theme_bw() +
  theme(legend.position = "bottom")


