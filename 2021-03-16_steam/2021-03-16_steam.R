# 2021-03-16
# Steam games

library(tidytuesdayR) # to get the data
library(tidyverse) # duh
library(ggthemes) # more theme choices
library(ggtext) # adding annotations
library(ggExtra) # easily remove gridlines
library(scales)
library(cowplot)

# Get the data ------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load('2021-03-16')
games <- tuesdata$games

# Tidy the dates ----------------------------------------------------------
# Months as factors
games <- games %>%
  mutate(month = factor(month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")))

# Clean percents ----------------------------------------------------------
games <- games %>%
  mutate(avg_peak_perc = as.numeric(str_remove(avg_peak_perc, "%")),
         avg_peak_prop = avg_peak_perc/100) %>%
  select(-avg_peak_perc)

# Let's look at raw numbers of players over time --------------------------
games <- games %>%
  mutate(my = lubridate::ym(paste(year, month)))

games %>%
  filter(year %in% c(2016, 2017, 2018, 2019, 2020, 2021)) %>%
  ggplot(aes(x = factor(my), y = avg))+
  geom_jitter(size = 0.05, width = 0.2, alpha = 0.8)+
  geom_line(aes(group = gamename), size = 0.1)

games %>%
  filter(year %in% c(2016, 2017, 2018, 2019, 2020, 2021)) %>%
  ggplot(aes(x = factor(my), y = peak))+
  geom_jitter(size = 0.05, width = 0.2, alpha = 0.8)+
  geom_line(aes(group = gamename), size = 0.1)
# okay, from this we can see that things are really dominated by just three games.

topthree <- games %>%
  group_by(gamename) %>%
  summarize(peakpeak = max(peak)) %>%
  arrange(-peakpeak) %>%
  pull(gamename) %>%
  head(3)

df <- games %>%
  filter(gamename %in% topthree)

df %>%
  pivot_longer(cols = c(peak, avg), names_to = "parameter", values_to = "value") %>%
  ggplot(aes(x = factor(my), y = value, group = interaction(gamename, parameter)))+
  geom_line(aes(col = gamename, lty = parameter)) # this is getting there.

# Start from when playerunknown's battlegrounds started
df <- df %>%
  filter(my >= (df %>% filter(gamename == "PLAYERUNKNOWN'S BATTLEGROUNDS") %>% pull(my) %>% min))

df %>%
  group_by(my) %>%
  mutate(peak = case_when(peak == max(peak) ~ peak, # keep only the highest peaks for a given month
                          TRUE ~ NA_real_)) %>%
  ggplot(aes(x = factor(my), y = avg, col = gamename, group = gamename))+
  geom_line() + # this is getting there.
  geom_point(aes(x = factor(my), y = peak, col = gamename), size = 1) # oh this is interesting. Let's not visualize the raw time series anymore.

# Relative peaks ----------------------------------------------------------
rel <- games %>%
  group_by(my) %>%
  arrange(-peak) %>%
  slice(1:2) %>%
  mutate(advantage = peak[1]-peak[2],
         pct_advantage = 100*(peak[1] - peak[2])/peak[2]) %>%
  slice(1) %>%
  ungroup()

# Try a bar graph? --------------------------------------------------------
brewer.pal(5, "Set1") # get the colors we'll use for the plot

rawplot <- rel %>%
  ggplot(aes(x = my, y = advantage/1000000, fill = gamename))+
  geom_col()+
  theme_minimal()+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        text=element_text(family="mono"))+
  ylab("Raw peak lead (millions)") +
  labs(title = glue::glue("<span style ='size:16px'>Size of lead: top-ranked vs. 2nd game on Steam (monthly peak player numbers)</span><br>"),
       subtitle = glue::glue("<span style='color:#4DAF4A;'>**Dota 2**</span>,
       <span style='color:#984EA3;'>**PLAYERUNKNOWN'S BATTLEGROUNDS**</span>,
       and <span style='color:#377EB8;'>**Counter-Strike: Global Offensive**</span> <br>have consistently had the **highest monthly peak player numbers**, with large absolute <br>and relative leads over the game with the next-highest peak."))+
  theme(plot.subtitle = element_markdown(),
        plot.title = element_markdown())+
  scale_fill_brewer(type = "qual", palette = "Set1")+
  annotate(geom = "richtext", 
           x = lubridate::ymd("2018-06-01"), y = 1.75, 
           label = "<span style='color:#984EA3;'>At its biggest lead, PLAYERUNKNOWN'S <br>BATTLEGROUNDS had nearly 2.5 million <br>more players than the game <br>with the next-highest peak.</span>", 
           family = "mono", size = 3, hjust = 0,
           fill = NA,
           label.color = NA)+
  NULL

pctplot <- rel %>%
  ggplot(aes(x = my, y = pct_advantage, fill = gamename))+
  geom_col()+
  theme_minimal()+
  theme(legend.position = "none")+
  ylab("% peak lead")+
  theme(axis.title.x = element_blank())+
  theme(text=element_text(family="mono"))+
  labs(caption = "Source: SteamCharts, via Kaggle and the tidytuesdayR package | Viz by Kaija Gahm")+
  theme(
    plot.caption = element_markdown()
  )+
  scale_fill_brewer(type = "qual", palette = "Set1")+
  annotate(geom = "richtext", 
           x = lubridate::ymd("2014-08-01"), y = 500, 
           label = "<span style='color:#4DAF4A;'>But Dota 2 had a greater relative lead, <br>at one point achieving 600% of its nearest <br>contender's peak player count.</span>", 
           family = "mono", size = 3, hjust = 0,
           fill = NA,
           label.color = NA)+
  NULL

cowplot::plot_grid(rawplot, pctplot, nrow = 2, align = "v")

