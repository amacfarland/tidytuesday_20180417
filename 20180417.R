library(tidyverse)
library(ggthemes)
library(colorspace)

df <- read_csv("dat/annual-number-of-deaths-by-cause.csv")

names(df) <- str_replace(names(df), " \\(.*\\)", "")

df_all <- df %>% 
  mutate_if(is.double, as.integer) %>% 
  filter(Year == 2016) %>% 
  gather('cause', 'deaths', 'Dementia':'Terrorism') 

df_all %>% 
  group_by(cause) %>% 
  summarise(deaths = sum(deaths, na.rm = TRUE)) %>%
  ggplot(., aes(reorder(cause, deaths), deaths)) +
  geom_bar(stat = "identity", fill = rainbow_hcl(33)) +
  coord_flip() +
  scale_y_continuous(name = "Deaths", labels = scales::comma) +
  scale_x_discrete(name = "Causes") +
  ggtitle("Annual number of deaths by cause, World, 2016") +
  scale_fill_ptol() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = -2.3, vjust=2))

df_comp1 <- df_all %>% 
  group_by(cause) %>% 
  summarise(deaths = sum(deaths, na.rm = TRUE)) %>% 
  mutate(freq = deaths / sum(deaths)) %>% 
  arrange(cause) %>% 
  select('Cause' = cause, 'Worldwide' = freq)
  
df_comp2 <- df_all %>% 
  filter(Entity == "United States") %>%
  group_by(cause) %>% 
  summarise(deaths = sum(deaths, na.rm = TRUE)) %>%
  mutate(freq = deaths / sum(deaths)) %>%
  arrange(cause) %>% 
  select('Cause' = cause, 'United States' = freq)
    
df_comp <- left_join(df_comp1, df_comp2)

rm(df_comp1, df_comp2)

df_comp %>% 
  mutate(Difference = .[[3]] - .[[2]]) %>%
  ggplot(., aes(reorder(Cause, Difference), Difference)) +
  geom_bar(stat = "identity", aes(fill = Difference), show.legend = FALSE) +
  coord_flip() +
  scale_y_continuous(name = "", labels = scales::percent) +
  scale_x_discrete(name = "") +
  ggtitle("Percent difference in share of deaths by cause, United States vs. Worldwide, 2016") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 1.2))
