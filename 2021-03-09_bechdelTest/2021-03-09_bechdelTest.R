# 2021-03-09
# Bechdel Test

library(tidytuesdayR) # to get the data
library(tidyverse) # duh
library(ggthemes) # more theme choices
library(ggtext) # adding annotations
library(ggExtra) # easily remove gridlines

devtools::install_github("ciannabp/inauguration") # inauguration color palette data
library(inauguration)


# Get the data ------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load('2021-03-09')
bechdel <- tuesdata$raw_bechdel
movies <- tuesdata$movies

# Explore -----------------------------------------------------------------
table(bechdel$rating, exclude = NULL) # all have a rating, 0 through 3


# Join --------------------------------------------------------------------
all <- movies %>%
  select(imdb_id, rated) %>%
  left_join(bechdel, by = "imdb_id")

# Remove unrated and recode ------------------------------------------------
all <- all %>%
  filter(!is.na(rated)) %>%
  select(-id) %>%
  mutate(rated = fct_recode(rated,
                            "NA" = "N/A",
                            "NA" = "Not Rated",
                            "NA" = "Unrated"),
         rated = factor(rated, levels = c("G", "PG", "TV-PG", "PG-13", "TV-14", "R", "NC-17", "X", "NA")))

# Compute averages --------------------------------------------------------
avgs <- all %>%
  group_by(year, rated) %>%
  mutate(avg = mean(rating))

# Filter it down to the ratings with the largest number of movies
avgs %>%
  group_by(rated) %>%
  summarize(n = n())

# Ok, we have WAY more movies rated PG, PG-13, and R than anything else. Could also maybe include G and NA.

avgs <- avgs %>%
  filter(rated %in% c("PG", "PG-13", "R"))


# Plot --------------------------------------------------------------------
avgs %>%
  ggplot(aes(x = year, y = avg, color = rated))+
  geom_point()+
  geom_smooth(method = "lm")
# ok no I don't like this.

# Aggregate by decade and find proportions --------------------------------
all <- all %>%
  mutate(decade = case_when(year >= 1970 & year < 1980 ~ "70s",
                            year >= 1980 & year < 1990 ~ "80s",
                            year >= 1990 & year < 2000 ~ "90s",
                            year >= 2000 & year < 2010 ~ "00s"))

yrs <- all %>%
  filter(!is.na(decade))

# Compute proportions of each rating for each decade. There's probably a better way to do this...
frq <- yrs %>%
  group_by(decade) %>%
  mutate(ndec = n()) %>%
  ungroup() %>%
  group_by(decade, rating) %>%
  summarize(prop = n()/ndec) %>%
  distinct() %>%
  ungroup()

# reorder factors so that 00's doesn't end up before 70's.
frq <- frq %>%
  mutate(decade = fct_relevel(decade, # had to look this up: it's fct_relevel, not fct_reorder, confusingly.
                             c("70s", "80s", "90s", "00s")))


# Plot --------------------------------------------------------------------
# get kamala's purple
kamala <- inauguration_palettes$inauguration_2021[1]

# function to generate a palette
colfunc <- colorRampPalette(c("black", kamala))
mycolors <- colfunc(4) # I couldn't figure out how to use this in ggplot but still be able to manually set the colors for the text.

frq %>%
ggplot(aes(x = decade, y = prop, fill = rating)) + 
  geom_bar(position = "fill", stat = "identity")+
  xlab("Decade")+
  ylab("")+
  theme_pander()+
  theme(legend.position = "none")+
  scale_fill_gradient(
    low = "black",
    high = kamala,
  )+
  removeGrid()+
  theme(text=element_text(family="PT Sans"))+
  annotate("text", x = 5, y = 0.7, 
           label= "The share of movies that \npass the Bechdel test \nhas increased over time.", 
           family = "PT Sans",
           size = 4,
           #hjust = 0,
           color = kamala)+
  annotate("text", x = 5, y = 0.2, 
           label= "The trend is mostly driven \nby a decrease in movies \nwith a rating of 1.", 
           family = "PT Sans",
           size = 4,
           #hjust = 0,
           color = mycolors[2])+
  theme(plot.margin = unit(c(0.2, 7, 0.2, 0.2), "lines")) +
  coord_cartesian(clip = "off") # to allow room in the margins for the text
