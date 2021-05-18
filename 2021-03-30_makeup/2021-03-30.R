# 2021-03-30
# Makeup shades

library(tidytuesdayR) # to get the data
library(tidyverse) # duh
library(ggthemes) # more theme choices
library(ggtext) # adding annotations
library(ggExtra) # easily remove gridlines
library(scales)
library(cowplot)
library(sysfonts)

# Get the data ------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load('2021-03-30')
shades <- tuesdata$allShades

shades %>%
  select(name, hue, sat, lightness) %>%
  filter(!is.na(name)) %>%
  arrange(name)

# Split by first letter ---------------------------------------------------
l <- shades %>%
  select(name, hue, sat, lightness) %>%
  filter(!is.na(name)) %>%
  arrange(name) %>%
  mutate(letter = toupper(substr(name, 1, 1)))

l %>%
  ggplot(aes(x = reorder(letter, -lightness), y = lightness))+
  geom_boxplot()
