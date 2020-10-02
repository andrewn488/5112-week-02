# WHO Data Wrangling Practice

library(tidyverse)
tidyr::who
View(who)

# gather columns 

who1 <- who %>% 
  gather(key = "key", value = "cases", new_sp_m014:newrel_f65)
View(who1)

# check counts

who1 %>% 
  count(key)

# make consistent variables

who2 <- who1 %>% 
  mutate(names_from = stringr::str_replace(key, "newrel", "new_rel"))
View(who2)

# separate variables in key column to be individual variables

who3 <- who2 %>% 
  separate(key, c("new", "type", "sexage"), sep = "_")
View(who3)

# drop unnecessary variables, drop na's

who4 <- who3 %>% 
  select(-new, -iso2, -iso3) %>% 
  filter(!is.na(cases))
View(who4)

# separate first character from sexage variable (m014 = male, 0-14 years age)

who5 <- who4 %>% 
  separate(sexage, c("sex", "age"), sep = 1) %>% 
  filter(!is.na(sex))
View(who5)

# practice plotting this data:


# histogram

ggplot(data = who5) +
  geom_histogram(mapping = aes(x = year)) + 
  ggtitle("Case Count of TB - Histrogram")

# point

ggplot(data = who5) + 
  geom_point(mapping = aes(x = year, y = cases, color = sex)) + 
  ggtitle("Case count of TB - Point")

# boxplot
ggplot(data = who5) +
  geom_boxplot(mapping = aes(x = factor(sex), y = year)) +
  ggtitle("Case count of TB based on Gender - Boxplot") + 
  scale_x_discrete(labels = c("female", "male"))
View(who_boxplot)
  

# smooth graph
ggplot(data = who5) +
  geom_smooth(mapping = aes(x = year, y = cases, linetype = sex)) +
  ggtitle("Case counts of TB for Male and Female - Smooth")
View(who_smooth)

# facet

ggplot(data = who5) +
  geom_smooth(mapping = aes(x = year, y = cases)) +
  facet_wrap(~sex, nrow = 2) +
  ggtitle("Facet on TB counts between M and F")

# smooth and point overlay
ggplot(data = who5) + 
  geom_smooth(mapping = aes(x = year, y = cases, linetype = sex)) + 
  geom_point(mapping = aes(x = year, y = cases, color = sex)) + 
  ggtitle("Attempt at an overlay of smooth and point")
  

  