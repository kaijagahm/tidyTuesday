# 2021-04-13
# Post offices

library(tidytuesdayR) # to get the data
library(tidyverse) # duh
library(sf) # for spatial work
library(here)
library(gganimate) # for animation across time
library(gifski) # to make a gif
library(av) # to make a video

# Get the data ------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load('2021-04-13')
po <- tuesdata$post_offices

# Read up on it -----------------------------------------------------------
# Read the US Post Offices blurb in the tidy tuesday materials
# Clicked on the link to the website: https://cblevins.github.io/us-post-offices/
# I had already been thinking of doing something with gganimate, since I've never done that before. Maybe I'll closely re-create the graphic on the website.
# I like the idea of coloring by stamp scarcity index instead of by random vs. non-random assignment. Maybe shapes could be random vs. non-random?

head(po)

# Get the random coords data ----------------------------------------------
# I downloaded this from the website
por <- read.csv(here("2021-04-13_postOffices", "us-post-offices-random-coords.csv"))
head(por)
nrow(po) 
nrow(por) # ah, same number of rows, so I can just use this one instead.

por <- por %>%
  select(ID, Name, State, Established, Discontinued, Continuous, StampIndex, Coordinates, Latitude, Longitude, RandomCoordsFlag) %>%
  rename(id = ID, name = Name, state = State, established = Established, discontinued = Discontinued, continuous = Continuous, stampIndex = StampIndex, coordinates = Coordinates, lat = Latitude, long = Longitude, random = RandomCoordsFlag)

# if discontinued is NA, add 2021 as the year
por <- por %>%
  mutate(discontinued = case_when(is.na(discontinued) ~ as.integer(2021),
                                  TRUE ~ as.integer(2021)))

# are any NA for established?
por %>% filter(is.na(established)) # yeah we're going to have to remove those.
por <- por %>%
  filter(!is.na(established),
         coordinates == TRUE) # only the ones that have valid coordinates

names(por) # okay, cool
dim(por) # 165318 rows, 11 columns

# Transform to an sf object
## Remove any missing coordinate values
# pors <- por %>%
#   st_as_sf(., coords = c("long", "lat"))
# dim(pors)
# head(pors)

# Initial plot ------------------------------------------------------------
# Going to just try MA first because the whole dataset is too large
ma <- por %>% filter(state == "MA")
p1 <- ma %>%
  ggplot(aes(x = long, y = lat)) +
  geom_point()
ggsave("p1.png", plot = p1, path = here("2021-04-13_postOffices", "plots"))

p2 <- ma %>%
  ggplot(aes(x = long, y = lat, col = stampIndex)) +
  geom_point()
ggsave("p2.png", plot = p2, path = here("2021-04-13_postOffices", "plots"))

p3 <- ma %>%
  ggplot(aes(x = long, y = lat, col = as.numeric(stampIndex))) +
  geom_point()+
  scale_color_viridis_c()+
  theme_minimal()
ggsave("p3.png", plot = p3, path = here("2021-04-13_postOffices", "plots"))

p4 <- ma %>%
  ggplot(aes(x = long, y = lat, col = as.numeric(stampIndex))) +
  geom_point()+
  scale_color_viridis_c()+
  theme_minimal()+
  labs(col = "Stamp \nscarcity \nindex") 
ggsave("p4.png", plot = p4, path = here("2021-04-13_postOffices", "plots"))

# Let's try out gganimate -------------------------------------------------
# Here's the code from the vignette
# ggplot(mtcars, aes(factor(cyl), mpg)) + 
#   geom_boxplot() + 
#   # Here comes the gganimate code
#   transition_states(
#     gear,
#     transition_length = 2,
#     state_length = 1
#   ) +
#   enter_fade() + 
#   exit_shrink() +
#   ease_aes('sine-in-out')

# I'm not clear on what my data needs to look like to be animated.
# In mtcars, each frame is a boxplot for one gear
# By analogy, each frame of my plot needs to be the full map for a single year, which I think means I need to expand the data to have one row per year?
head(ma)
maExp <- ma %>%
  mutate(year = map2(established, discontinued, seq)) %>%
  unnest(cols = c(year)) %>%
  select(-established, -discontinued, -continuous) %>%
  mutate(stampIndex = as.numeric(stampIndex))

# Attempt a MA gganimate plot ---------------------------------------------
maExp %>%
  ggplot(aes(x = long, y = lat, col = as.numeric(stampIndex))) +
  geom_point()+
  scale_color_viridis_c()+
  theme_minimal()+
  labs(col = "Stamp \nscarcity \nindex") +
  transition_states(
    year
  )
