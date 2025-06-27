# Using R
library(tidyverse)
library(lme4)
library(lmerTest)
# Option 1: tidytuesdayR R package 
## install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2025-05-20')

water_quality <- tuesdata$water_quality
weather <- tuesdata$weather

# What is the date range?
glimpse(weather)
glimpse(water_quality)
range(water_quality$date)

water_quality <- water_quality %>% mutate(year = lubridate::year(date), month = lubridate::month(date), day = lubridate::day(date))

water_quality %>% 
  group_by(year, month) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = month, y = n))+
  geom_line() # clearly we have multiple sites grouped together
glimpse(water_quality)
table(water_quality$region)

# divided by region
water_quality %>%
  group_by(region, year, month) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = month, y = n, col = region))+
  geom_line() # this still doesn't look right
glimpse(water_quality)

water_quality %>% 
  group_by(region, council, year, month) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = month, y = n, col = council))+
  facet_wrap(~region, scales = "free")+
  geom_line()+
  theme(legend.position = "none") # why does this still look weird? oh, because I have years merged
glimpse(water_quality)

water_quality %>% 
  group_by(region, council, year, month) %>%
  mutate(n = n()) %>%
  ggplot(aes(x = date, y = n, col = council))+
  facet_wrap(~region, scales = "free")+
  geom_line()+
  theme(legend.position = "none") # why does this still look weird? oh, because I have years merged
glimpse(water_quality)

# Let's focus on temperature in the years 2010-2020
temp_subset <- water_quality %>% filter(year >= 2010 & year <= 2020)
temp_subset %>%
  ggplot(aes(x = interaction(year, month), y = water_temperature_c))+
  geom_boxplot() # we have some insane outliers... that can't be right

# Remove temp > 40c
temp_subset <- temp_subset %>%
  filter(water_temperature_c < 40)
temp_subset %>%
  ggplot(aes(x = interaction(year, month), y = water_temperature_c))+
  geom_boxplot()+
  facet_wrap(~region, scales = "free")+
  theme_classic()+
  theme(axis.text.x = element_blank()) # looks pretty similar

# Matt predicts: the one that's the most ocean-y is going to be the coldest
# quick map:
ggplot(temp_subset, aes(x = longitude, y = latitude, col = region))+geom_point()
library(mapview)
library(sf)
temp_subset_sf <- sf::st_as_sf(temp_subset, coords = c("longitude", "latitude"), remove = F, crs = "WGS84")
mapview(temp_subset_sf, zcol = "region")

# should exclude western sydney. sydney harbour is going to be really messy probably

temp_subset_east <- temp_subset %>%
  filter(region != "Western Sydney")
temp_subset_east_sf <- sf::st_as_sf(temp_subset_east, coords = c("longitude", "latitude"), remove = F, crs = "WGS84")
mapview(temp_subset_east_sf, zcol = "region")

# What pollutant measures do we have?
glimpse(temp_subset_east) # we have conductivity, temperature, enterococci
# higher conductivity means more dissolved substances

# curious about enterococci--gram positive lactic acid bacteria
temp_subset_east %>%
  ggplot(aes(x = date, y = enterococci_cfu_100ml, col = region))+
  geom_point()+
  geom_smooth()+
  theme_classic() # maybe log-transform

temp_subset_east %>%
  ggplot(aes(x = date, y = log(enterococci_cfu_100ml), col = region))+
  geom_point()+
  geom_smooth()+
  theme_classic() # this isn't helpful--need it by year

# averaging by month
temp_subset_east <- temp_subset_east %>% mutate(log_entero = log(enterococci_cfu_100ml))
temp_subset_east %>%
  group_by(region, year, month) %>%
  summarize(mn = mean(log_entero)) %>%
  ggplot(aes(x = lubridate::ym(paste(year, month)), y = mn, col = region))+
  geom_point() # this is super weird

# conductivity should correlate negatively with entero because entero is happening when there are big intrusions of freshwater into sydney harbour which would drive down the conductivity
sh <- temp_subset_east %>%
  filter(region == "Sydney Harbour")

sh %>%
  ggplot(aes(x = conductivity_ms_cm, y = log_entero, col = factor(year)))+
  geom_point(alpha = 0.5, pch = 1)+
  geom_smooth(method = "lm")+
  theme_classic() # aha!!

# Do the really coastal and really inland stuff follow the same 7-year cycle? Does it follow a cycle at all?
water_quality %>%
  ggplot(aes(x = date, y = log(enterococci_cfu_100ml), col = region))+
  geom_point(alpha = 0.2, size = 0.5)+
  theme_classic()+
  geom_smooth() # this is cool! once again, western sydney is weird; what happens if we omit it?

water_quality %>%
  filter(region != "Western Sydney") %>%
  ggplot(aes(x = date, y = log(enterococci_cfu_100ml), col = region))+
  geom_point(alpha = 0.2, size = 0.5)+
  theme_classic()+
  geom_smooth() # when were the el nino years?

# got some el nino data off of google
elnino <- tribble(~start, ~end, ~category,
        2015, 2016, "very strong",
        2002, 2003, "moderate",
        2009, 2010, "moderate",
        2004, 2005, "weak",
        2006, 2007, "weak",
        2014, 2015, "weak",
        1994, 1995, "moderate",
        1991, 1992, "strong",
        1997, 1998, "very strong") %>%
  mutate(category = factor(category, levels = c("weak", "moderate", "strong", "very strong")))
# when does this apply to? october through feb ish, but let's just look at the whole year for simplicity, centered around october through feb
elnino_dates <- elnino %>%
  mutate(start_date = lubridate::ymd(paste(start, 07, 1)),
         end_date = lubridate::ymd(paste(end, 06, 30)))

wc <- water_quality %>% filter(region != "Western Sydney")
ggplot(data = elnino_dates)+
  geom_rect(aes(xmin = start_date, xmax = end_date, ymin = 0, ymax = 15, alpha = category))+
  geom_point(data = wc, aes(x = date, y = log(enterococci_cfu_100ml), col = region),
             size = 0.2, pch = 1, alpha = 0.5)+
  theme_classic() # this is suggestive but we're not totally sure. let's join them together and see

elnino_tojoin <- elnino_dates %>% select(start_date, end_date, category) %>%
  mutate(date = map2(start_date, end_date, ~seq(.x, .y, by = "day"))) %>%
  unnest(date) %>%
  select(date, category)

joined <- wc %>%
  left_join(elnino_tojoin, by = "date") %>%
  mutate(category = case_when(is.na(category) ~ "none", .default = category)) %>%
  mutate(category = factor(category, levels = c("none", "weak", "moderate", "strong", "very strong")))
nrow(wc) == nrow(joined) # sanity check

# okay now we can compare
joined %>%
  filter(water_temperature_c < 40) %>%
  ggplot(aes(x = category, y = water_temperature_c, fill = region))+
  geom_boxplot()+
  theme_classic() # see a trend in sydney harbour more than anything

# different viz:
joined %>%
  filter(water_temperature_c < 40) %>%
  ggplot(aes(x = category, y = water_temperature_c, fill = region, col = region))+
  geom_violin(alpha = 0.2)+
  theme_classic()

# now let's look at just sydney harbour
sh <- joined %>%
  filter(water_temperature_c < 40) %>%
  filter(region == "Sydney Harbour")
sh %>% ggplot(aes(x = category, y = water_temperature_c, col = swim_site))+
  geom_violin()+
  theme_classic()+
  theme(legend.position = "none") # would be nice to see lines--let's fake it by coding the categories as numbers

sh <- sh %>%
  mutate(category_number = case_when(category == "none" ~ 0,
                                      category == "weak" ~ 1,
                                      category == "moderate" ~ 2,
                                      category == "strong" ~ 3,
                                      category == "very strong" ~ 4, .default = NA)) 

sh %>%
  ggplot(aes(x = category_number, y = water_temperature_c, col = swim_site))+
  geom_jitter(width = 0.1, alpha = 0.5, size = 0.2)+
  geom_smooth(method = "lm")+
  theme_classic()+
  theme(legend.position = "none") # maybe a slightly positive slope

temp_mod <- lmer(water_temperature_c ~ category_number + (1|swim_site), data = sh)
summary(temp_mod) # really small impact. 

# Harbour would be impacted less than the others
joined <- joined %>%
  mutate(category_number = case_when(category == "none" ~ 0,
                                     category == "weak" ~ 1,
                                     category == "moderate" ~ 2,
                                     category == "strong" ~ 3,
                                     category == "very strong" ~ 4, .default = NA)) 

temp_mod_all <- lmer(water_temperature_c ~ category_number + month + (1|region/swim_site) , data = joined)
summary(temp_mod_all)

# let's look at correlation between el nino and water temp by month, for each swim site
# before we do that, let's look at this by month
joined <- joined %>% filter(water_temperature_c < 40)
max <- max(joined$water_temperature_c)
min <- min(joined$water_temperature_c)
joined %>%
  ggplot(aes(x = category_number, y = water_temperature_c, col = region, group = swim_site))+
  geom_jitter(width = 0.1, alpha = 0.1, size = 0.2)+
  geom_smooth(method = "lm", se = F, linewidth = 0.5)+
  theme_classic()+
  facet_wrap(~month, scales = "free")+
  scale_color_manual(values = c("red", "blue", "yellowgreen", "purple"))+
  scale_y_continuous(limits =c(min, max))

# separate graphs for each of them
joined %>%
  filter(region == "Northern Sydney") %>%
  ggplot(aes(x = category_number, y = water_temperature_c, group = swim_site))+
  geom_jitter(width = 0.1, alpha = 0.1, size = 0.2, col = "red")+
  geom_smooth(method = "lm", se = F, linewidth = 0.5, col = "red")+
  theme_classic()+
  facet_wrap(~month, scales = "free")+
  labs(title = "Northern Sydney", y = "Water temp (C)", x = "Strength of El Nino")+
  scale_y_continuous(limits =c(min, max))

joined %>%
  filter(region == "Southern Sydney") %>%
  ggplot(aes(x = category_number, y = water_temperature_c, group = swim_site))+
  geom_jitter(width = 0.1, alpha = 0.1, size = 0.2, col = "blue")+
  geom_smooth(method = "lm", se = F, linewidth = 0.5, col = "blue")+
  theme_classic()+
  facet_wrap(~month, scales = "free")+
  labs(title = "Southern Sydney", y = "Water temp (C)", x = "Strength of El Nino")+
  scale_y_continuous(limits =c(min, max))

joined %>%
  filter(region == "Sydney City") %>%
  ggplot(aes(x = category_number, y = water_temperature_c, group = swim_site))+
  geom_jitter(width = 0.1, alpha = 0.1, size = 0.2, col = "yellowgreen")+
  geom_smooth(method = "lm", se = F, linewidth = 0.5, col = "yellowgreen")+
  theme_classic()+
  facet_wrap(~month, scales = "free")+
  labs(title = "Sydney City", y = "Water temp (C)", x = "Strength of El Nino")+
  scale_y_continuous(limits =c(min, max))

joined %>%
  filter(region == "Sydney Harbour") %>%
  ggplot(aes(x = category_number, y = water_temperature_c, group = swim_site))+
  geom_jitter(width = 0.1, alpha = 0.1, size = 0.2, col = "purple")+
  geom_smooth(method = "lm", se = F, linewidth = 0.5, col = "purple")+
  theme_classic()+
  facet_wrap(~month, scales = "free")+
  labs(title = "Sydney Harbour", y = "Water temp (C)", x = "Strength of El Nino")+
  scale_y_continuous(limits =c(min, max))

# Let's do the exact same thing but for bacteria instead of temperature
max_entero <- max(joined$enterococci_cfu_100ml)
min_entero <- min(joined$enterococci_cfu_100ml)
joined %>%
  ggplot(aes(x = category_number, y = log(enterococci_cfu_100ml), col = region, group = swim_site))+
  geom_jitter(width = 0.1, alpha = 0.1, size = 0.2)+
  geom_smooth(method = "lm", se = F, linewidth = 0.1)+
  theme_classic()+
  facet_wrap(~month, scales = "free")+
  scale_color_manual(values = c("red", "blue", "yellowgreen", "purple"))+
  scale_y_continuous(limits =c(min_entero, max_entero))

# separate graphs for each of them--added overall trendline and standardized y axes
joined %>%
  filter(region == "Northern Sydney") %>%
  ggplot(aes(x = category_number, y = log(enterococci_cfu_100ml)))+
  geom_smooth(method = "lm", se = F, linewidth = 0.5, col = "black", linetype = 2)+
  geom_jitter(width = 0.1, alpha = 0.1, size = 0.2, col = "red", aes(group = swim_site))+
  geom_smooth(method = "lm", se = F, linewidth = 0.1, col = "red", aes(group = swim_site))+
  theme_classic()+
  facet_wrap(~month)+
  labs(title = "Northern Sydney", y = "Enterococci concentration (log-transformed)", x = "Strength of El Nino")+
  scale_y_continuous(limits =c(min_entero, max_entero))

joined %>%
  filter(region == "Southern Sydney") %>%
  ggplot(aes(x = category_number, y = log(enterococci_cfu_100ml)))+
  geom_smooth(method = "lm", se = F, linewidth = 0.5, col = "black", linetype = 2)+
  geom_jitter(width = 0.1, alpha = 0.1, size = 0.2, col = "blue", aes(group = swim_site))+
  geom_smooth(method = "lm", se = F, linewidth = 0.1, col = "blue", aes(group = swim_site))+
  theme_classic()+
  facet_wrap(~month)+
  labs(title = "Southern Sydney", y = "Enterococci concentration (log-transformed)", x = "Strength of El Nino")+
  scale_y_continuous(limits =c(min_entero, max_entero))

joined %>%
  filter(region == "Sydney City") %>%
  ggplot(aes(x = category_number, y = log(enterococci_cfu_100ml)))+
  geom_smooth(method = "lm", se = F, linewidth = 0.5, col = "black", linetype = 2)+
  geom_jitter(width = 0.1, alpha = 0.1, size = 0.2, col = "yellowgreen", aes(group = swim_site))+
  geom_smooth(method = "lm", se = F, linewidth = 0.1, col = "yellowgreen", aes(group = swim_site))+
  theme_classic()+
  facet_wrap(~month)+
  labs(title = "Sydney City", y = "Enterococci concentration (log-transformed)", x = "Strength of El Nino")+
  scale_y_continuous(limits =c(min_entero, max_entero))

joined %>%
  filter(region == "Sydney Harbour") %>%
  ggplot(aes(x = category_number, y = log(enterococci_cfu_100ml)))+
  geom_smooth(method = "lm", se = F, linewidth = 0.5, col = "black", linetype = 2)+
  geom_jitter(width = 0.1, alpha = 0.1, size = 0.2, col = "purple", aes(group = swim_site))+
  geom_smooth(method = "lm", se = F, linewidth = 0.1, col = "purple", aes(group = swim_site))+

  theme_classic()+
  facet_wrap(~month)+
  labs(title = "Sydney Harbour", y = "Enterococci concentration (log-transformed)", x = "Strength of El Nino")+
  scale_y_continuous(limits =c(min_entero, max_entero))

# Temporal pattern of bacteria in general
joined %>%
  group_by(region, year, month) %>%
  summarize(mn = mean(enterococci_cfu_100ml, na.rm = T)) %>%
  ungroup() %>%
  ggplot(aes(x = month, y = log(mn)))+
  geom_point(aes(col = region), alpha = 0.5, pch = 1)+
  theme_classic()+
  geom_smooth(aes(group = region, col = region))+
  scale_x_continuous(breaks = 1:12)+
  labs(y = "Mean bacteria concentration (log-transformed)",
       x = "Calendar month")
