# 2021-05-18
# Ask a Manager

library(tidytuesdayR) # to get the data
library(tidyverse) # duh
library(ggbeeswarm)
library(wesanderson)
library(ggtext)
library(here)

# Load the data -----------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load('2021-05-18')
aam <- tuesdata$survey

dim(aam)

# Cleaning ----------------------------------------------------------------
# Let's clean up the country
aam <- as.data.frame(aam)
aam <- aam %>%
  mutate(country = case_when(grepl("us\\s", country)|grepl("usa\\s", country) ~ "USA",
                             TRUE ~ country),
         country = tolower(country),
         country = case_when(country %in% c("united states", "us", "usa", "u.s.", "united states of america", "u.s>", "isa", "united state", "u.s.a", "u.s.a.", "america", "the united states", "united state of america", "united stated", "united statws", "u.s", "unites states", "u. s.", "united sates", "united states of american", "uniited states", "united sates of america", "united states (i work from home and my clients are all over the us/canada/pr", "unted states", "united statesp", "united stattes", "united statea", "united statees", "uniyed states", "uniyes states", "united states of americas", "us of a", "u.sa", "united status", "uniteed states", "united stares", "unite states", "the us", "unitedstates", "united statues", "untied states", "usa (company is based in a us territory, i work remote)", "unitied states", "united sttes", "uniter statez", "u. s", "usa tomorrow", "united stateds", "unitef stated", "usaa", "united states- puerto rico", "usd", "usa, but for foreign gov't", "united statss", "united  states", "usa-- virgin islands", "united statew", "usat", "ua", "united y") ~ "usa",
                             country %in% c("united kingdom", "uk", "england", "great britain", "england/uk", "england, uk.", "britain", "united kingdom (england)", "united kingdom.", "u.k.", "united kindom", "england, uk", "uk for u.s. company", "united kingdomk", "england, gb", "u.k. (northern england)", "u.k", "england, united kingdom", "englang", "uk (england)", "uk, remote", "unites kingdom", "uk, but for globally fully remote company") ~ "uk",
                             country %in% c("canada", "canada, ottawa, ontario", "canadw", "can", "i am located in canada but i work for a company in the us", "canda", "canada and usa", "csnada", "canad", "canada") ~ "canada",
                             TRUE ~ country))

# Remove any countries that have fewer than 10 people
reduced <- aam %>%
  group_by(country) %>%
  filter(n() > 10) %>%
  ungroup()

# Remove industries with fewer than 10 people
reduced2 <- reduced %>%
  group_by(industry) %>%
  filter(n() > 10) %>%
  ungroup()

nrow(reduced2) # we still have over 24,000 responses

# Grab only the usa, and the top 10 industries
usaTop10Ind <- reduced2 %>%
  filter(country == "usa") %>%
  group_by(industry) %>%
  mutate(count = n()) %>%
  arrange(-count) %>%
  filter(industry %in% unique(.$industry)[1:10])

nrow(usaTop10Ind)

# Make a plot
usaTop10Ind %>%
  ggplot(aes(x = industry))+
  geom_bar()

# Just look at computing and tech
compTech <- usaTop10Ind %>%
  filter(industry == "Computing or Tech",
         currency == "USD")

# Get rid of that one weird outlier salary
compTech <- compTech %>%
  filter(annual_salary < 1500000)

compTech <- compTech %>%
  mutate(gender = case_when(is.na(gender)|gender == "Other or prefer not to answer" ~ "Other",
                            TRUE ~ gender),
         other_monetary_comp = as.numeric(other_monetary_comp),
         other_monetary_comp = case_when(is.na(other_monetary_comp) ~ 0,
                                         TRUE ~ other_monetary_comp),
         money = annual_salary + other_monetary_comp) %>%
  mutate(overall_years_of_professional_experience = fct_relevel(overall_years_of_professional_experience, c("1 year or less", "2 - 4 years", "5-7 years", "8 - 10 years", "11 - 20 years", "21 - 30 years", "31 - 40 years", "41 years or more")))

compTech %>%
  filter(money > 0) %>%
  ggplot(aes(x = how_old_are_you, y = money, col = gender))+
  geom_quasirandom(alpha = 0.5)+
  xlab("Age")+
  ylab("Salary")+
  facet_wrap(~gender)

compTech %>%
  filter(money > 0) %>%
  ggplot(aes(x = overall_years_of_professional_experience, y = money, col = gender))+
  geom_quasirandom(alpha = 0.5)+
  facet_wrap(~gender)

compTech %>%
  filter(money > 0) %>%
  ggplot(aes(x = overall_years_of_professional_experience, y = money, col = gender))+
  geom_quasirandom(alpha = 0.5)+
  coord_flip()

# Simplified plot  --------------------------------------------------------
rawProf <- compTech %>%
  mutate(gender = tolower(gender)) %>%
  mutate(gender = case_when(gender %in% c("non-binary", "other") ~ "non-binary or other",
                            TRUE ~ gender)) %>%
  ungroup() %>%
  select(how_old_are_you, overall_years_of_professional_experience, gender, money) %>%
  rename("age" = how_old_are_you,
         "experience" = overall_years_of_professional_experience) %>%
  mutate(experience = fct_recode(experience,
                                 "0.5" = "1 year or less",
                                 "3" = "2 - 4 years",
                                 "6" = "5-7 years",
                                 "9" = "8 - 10 years",
                                 "15.5" = "11 - 20 years",
                                 "25.5" = "21 - 30 years",
                                 "35.5" = "31 - 40 years",
                                 "45.5" = "41 years or more"),
         experience = as.numeric(as.character(experience)),
         money = money/1000) %>%
  mutate(gender = factor(gender))

summaryProf <- rawProf %>%
  group_by(gender, experience) %>%
  summarize(n = n(),
            mn = mean(money, na.rm = T),
            se = sd(money, na.rm = T)/sqrt(n),
            lowerQuart = quantile(money)[2],
            upperQuart = quantile(money)[4],
            lowerSE = mn - se,
            upperSE = mn + se)

# Relevant experience -----------------------------------------------------
rawField <- compTech %>%
  mutate(gender = tolower(gender)) %>%
  mutate(gender = case_when(gender %in% c("non-binary", "other") ~ "non-binary or other",
                            TRUE ~ gender)) %>%
  ungroup() %>%
  select(how_old_are_you, years_of_experience_in_field, gender, money) %>%
  rename("age" = how_old_are_you,
         "experience" = years_of_experience_in_field) %>%
  mutate(experience = fct_recode(experience,
                                 "0.5" = "1 year or less",
                                 "3" = "2 - 4 years",
                                 "6" = "5-7 years",
                                 "9" = "8 - 10 years",
                                 "15.5" = "11 - 20 years",
                                 "25.5" = "21 - 30 years",
                                 "35.5" = "31 - 40 years",
                                 "45.5" = "41 years or more"),
         experience = as.numeric(as.character(experience)),
         money = money/1000) %>%
  mutate(gender = factor(gender))

summaryField <- rawField %>%
  group_by(gender, experience) %>%
  summarize(n = n(),
            mn = mean(money, na.rm = T),
            se = sd(money, na.rm = T)/sqrt(n),
            lowerQuart = quantile(money)[2],
            upperQuart = quantile(money)[4],
            lowerSE = mn - se,
            upperSE = mn + se)


# Combine field and overall professional ----------------------------------
summaries <- summaryProf %>%
  mutate(type = "overall") %>%
  bind_rows(summaryField %>%
              mutate(type = "field")) %>%
  mutate(type = factor(type, levels = c("overall", "field")))

# Make a plot -------------------------------------------------------------
# Manually get colors for annotations
colors <- as.vector(wes_palette(3, name = "Darjeeling1", type = "discrete"))

annotationText <- data.frame(
  label = factor(c("overall", "industry-specific"), levels = c("overall", "industry-specific")),
  type = factor(c("overall", "field"), levels = c("overall", "field")),
  x = c(20, 20),
  y = c(75, 75)
)

p <- summaries %>%
  ggplot(aes(x = experience, col = gender))+
  facet_wrap(~type)+
  geom_line(aes(y = mn, group = gender), size = 2)+
  geom_ribbon(aes(ymin = lowerSE, ymax = upperSE, group = gender, 
                  fill = gender), alpha = 0.2, size = 0.3)+
  scale_color_manual(values = wes_palette(3, name = "Darjeeling1", 
                                          type = "discrete"))+
  scale_fill_manual(values = wes_palette(3, name = "Darjeeling1", 
                                         type = "discrete"))+
  theme_minimal()+
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        plot.margin = margin(l = 25, r = 25, t = 25, b = 25),
        text = element_text(family = "PT Mono"))+
  ylab("Money earned, USD (thousands)")+
  xlab("Years of experience")+
  ylim(c(60, 240))+
  labs(title = glue::glue("<b><span style='font-size:18pt'>US Tech Salaries</span></b>"),
       subtitle = glue::glue("<span style='font-size:14pt;'><span style='color:#FF0000'>**Men**</span> in tech tend to earn more than <span style='color:#F2AD00'>**women**</span>, after adjusting for years of professional experience. People who reported their gender as <span style='color:#00A08A'>**nonbinary**</span> or <span style='color:#00A08A'>**'other'**</span> fell somewhere in between, patterning more closely with men. Patterns were similar when analyzed by overall experience and industry-specific experience.</span> <i><br><span style = 'font-size:12pt'><b>Bold lines</b> connect mean values for each age bracket. The translucent ribbon represents standard error.</i></span>"),
       caption = "Created by <b>Kaija Gahm</b>. Data from the 2021 <b>Ask a Manager salary survey</b>, via Tidy Tuesday. X values are the midpoints of experience bins: e.g. '5-7 years' is recoded as 6; '11-20 years' is recoded as 15.5. On the y axis, 'money earned' is the sum of annual salary and any additional monetary compensation (bonuses etc.)")+
  theme(plot.title = element_markdown(margin = margin(b = 10)),
        plot.subtitle = element_textbox_simple(lineheight = 1.5),
        plot.caption = element_textbox_simple(margin = margin(t = 20, r = 5, 
                                                              l = 0)))+
  geom_text(
    data = annotationText,
    mapping = aes(x = x, y = y, label = label),
    inherit.aes = F,
    family = "PT Mono",
    size = 5,
    col = "gray"
  )

ggsave(p, filename = "askAManager.png", path = here("2021-05-18_askAManager"), 
       width = 10, height = 7)
