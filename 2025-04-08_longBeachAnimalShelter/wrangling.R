# Using R
# Option 1: tidytuesdayR R package 
##install.packages("tidytuesdayR")
library(tidyverse)
library(showtext)
library(ggtext)
library(waffle)
library(ggrepel)
tuesdata <- tidytuesdayR::tt_load('2025-03-04')
longbeach <- tuesdata$longbeach

# Explore -----------------------------------------------------------------
View(longbeach)
# Some of the names have asterisks by them. What does that mean?
# Could explore animal names
# There are also types of animals--how many different ones?
length(unique(longbeach$animal_type))
sort(unique(longbeach$animal_type)) # oh nice, these are already pretty clean.

# Clean data and add useful columns
longbeach <- longbeach %>%
  mutate(animal_type_short = case_when(animal_type %in% c("amphibian", "reptile", "livestock", "other", "wild", "rabbit", "guinea pig") ~ "other",
                                       .default = animal_type),
         year_intake = lubridate::floor_date(intake_date, unit = "years"),
         year_outcome = lubridate::floor_date(outcome_date, unit = "years"),
         ym_intake = lubridate::floor_date(intake_date, unit = "months"))
table(longbeach$animal_type_short)

# Preliminary plotting ----------------------------------------------------
longbeach %>%
  ggplot(aes(x = year_intake, fill = animal_type_short))+
  geom_bar()+
  theme_classic() # maybe this can be a line plot instead

longbeach %>%
  group_by(year_intake, animal_type_short) %>%
  mutate(count_year = n()) %>%
  ungroup() %>%
  group_by(ym_intake, year_intake, count_year, animal_type_short) %>%
  summarize(count_ym = n()) %>%
  ggplot(aes(color = animal_type_short))+
  geom_line(aes(x = ym_intake, y = count_ym), linewidth = 1, 
            alpha = 0.75)+
  geom_line(aes(x = year_intake, y = count_year), linewidth = 2)+
  theme_classic() 

# What about different cat colors?
cats <- longbeach %>% filter(animal_type == "cat")
catcolors <- unique(cats$primary_color)

# Simplify the colors
cats <- cats %>%
  mutate(color_simple = case_when(primary_color %in% c("orange", "orange tabby", "gold", "orange tiger", "peach", "wheat", "red", "flame point", "fawn", "tan", "buff", "yellow", "blonde", "cream", "cream tabby", "cream tiger") ~ "orange",
                                  primary_color %in% c("calico", "tortie", "torbi", "tortie dilute", "calico dilute", "calico tabby", "blue cream", "tricolor", "calico point", "tortie point") ~ "calico_tortie",
                                  primary_color == "black tabby" ~ "black",
                                  primary_color %in% c("gray", "blue point", "blue", "gray tabby", "silver tabby", "silver", "blue tabby") ~ "gray_silver_blue",
                                  primary_color %in% c("chocolate", "brown") ~ "brown",
                                  primary_color %in% c("black smoke", "brown  tiger", "gray tiger", "seal", "black tiger", "snowshoe", "point") ~ "fancy",
                                  primary_color == "brown  tabby" ~ "brown tabby",
                                  str_detect(primary_color, "point") ~ "fancy",
                                  .default = primary_color))

cat_color_palette <-c("black", "chocolate4", "burlywood3", "red", "purple", "gray", "orange", "ivory")
cats %>%
  filter(color_simple != "unknown") %>%
  group_by(year_intake, color_simple) %>%
  mutate(count_year = n()) %>%
  ungroup() %>%
  group_by(ym_intake, year_intake, count_year, color_simple) %>%
  summarize(count_ym = n()) %>%
  ggplot(aes(color = color_simple))+
  geom_line(aes(x = ym_intake, y = count_ym), linewidth = 1, 
            alpha = 0.9)+
  theme_classic()+
  scale_color_manual(values = cat_color_palette)+
  labs(y = "Count per month", x = "Intake date", color = "Cat color")

# Okay, let's look at outcomes and conditions
# Proportion that died
prop_died <- cats %>%
  group_by(color_simple, ym_intake) %>%
  summarize(prop_died = sum(outcome_is_dead)/n())
prop_died <- cats %>%
  select(animal_id, color_simple, ym_intake, year_intake, year_outcome, outcome_is_dead)

# Of the ones that adopted, how long they spent in the shelter
# Intake condition
