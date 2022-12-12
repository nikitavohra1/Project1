# Project1

library(tidyverse)
install.packages("ggthemes")
library(ggthemes)
install.packages("plotly")
library(plotly)

library(readr)
df <- read_csv("2022.csv")

#Column Names:
names(df)

#Summary Statistics of the data set:
summary(df)

#* Potential Questions to be Answered: 
#* Top 10 Happiest Countries in the world
#* Unhappiest countries in the World,
#* GDP Distribution
#* Perceptions of Corruption Distribution
#* Freedom to make life choices and Happiness Score Relation
#* GDP per Capita and Perceptions of Corruption relation
#* Healthy Life Expectancy and Generosity relation

#Parsing Columns as Numeric Vector and Dropping Missing Values:
library(tidyr)
df_nona <- df %>%
  mutate(GDP= as.numeric(gsub(",", "" , `Explained by: GDP per capita`))) %>%
  mutate(Social_support= as.numeric(gsub(",", "" , `Explained by: Social support`))) %>%
  mutate(Healthy= as.numeric(gsub(",", "" , `Explained by: Healthy life expectancy`))) %>%
  mutate(Freedom= as.numeric(gsub(",", "" , `Explained by: Freedom to make life choices`))) %>%
  mutate(Generosity= as.numeric(gsub(",", "" , `Explained by: Generosity`))) %>%
  mutate(Corruption= as.numeric(gsub(",", "" , `Explained by: Perceptions of corruption`))) %>%
  drop_na()
summary(df_nona)

#Check for data- Clean or not?
hist(df_nona$`Happiness score`, col = "Blue")
hist(df_nona$GDP, col = "Red")
hist(df_nona$Social_support, col = "gold")
hist(df_nona$Healthy, col = "green")
hist(df_nona$Freedom, col = "deeppink")
hist(df_nona$Generosity, col = "darkorange")
hist(df_nona$Corruption, col = "coral")

boxplot(df_nona$`Happiness score`, col = "Blue")
boxplot(df_nona$GDP, col = "Red")
boxplot(df_nona$Social_support, col = "gold")
boxplot(df_nona$Healthy, col = "green")
boxplot(df_nona$Freedom, col = "deeppink")
boxplot(df_nona$Generosity, col = "darkorange")
boxplot(df_nona$Corruption, col = "coral")

sum(is.na(df_nona$`Happiness score`))
sum(is.na(df_nona$GDP))
sum(is.na(df_nona$Social_support))
sum(is.na(df_nona$Healthy))
sum(is.na(df_nona$Freedom))
sum(is.na(df_nona$Generosity))
sum(is.na(df_nona$Corruption))


# 1.) TOP 10 HAPPIEST COUNTRIES IN THE WORLD:
library(forcats)
df_nona %>%
  group_by(Country) %>%
  arrange(desc('Happiness score'))%>%
  mutate(Country = fct_reorder(Country, 'Happiness score'))%>%
  head(10)%>%
  summarise(Country, HappinessScore = max(df_nona$`Happiness score`))

#2.) UNHAPPIEST COUNTRIES IN THE WORLD:
df_nona %>%
  group_by(Country) %>%
  arrange((`Happiness score`))%>%
  head(5)%>%
  summarise(Country, HappinessScore = min(`Happiness score`))

#3.) GDP DISTRIBUTION:
#3.1.) Average GDP per capita of all the Countries is 1410.445: 
df_nona %>%
  summarise(Average_GDP = mean(`GDP`))

#3.2.) And here is the graphical visualisation of GDP distribuation:
ggplot(df_nona) + 
  geom_density(aes(x = GDP), fill = '#8ec54b') +
  labs( 
    title = 'GDP distribution of Countries',
    x = 'GDP', 
    y = 'Frequency'
  ) + 
  ggthemes::theme_few()

df_nona %>%
  select(GDP, `Happiness score`)%>%
  cor()


#4.) PERCEPTION OF CORRUPTION DISTRIBUTION:
#4.1.) The Country where people believe there is less corruption is Finland:
df_nona %>%
  head(1)%>%
  summarise(Country, Perception_of_Corruption = max(`Corruption`))

#4.2.) Top 20 Countries Wherein people believe there is not much corruption:
df_nona %>%
  head(20)%>%
  ggplot() +
  geom_col(aes(x=reorder(Country, Freedom), y= Freedom), fill='#cf7ea6') +
  geom_label(aes(x=reorder(Country, Freedom), y= Freedom, label= Freedom)) +
  coord_flip()

#4.3.) Perceptions of Corruption Corruption Distribution of countries using Histogram:
ggplot(df_nona) +
  geom_histogram(aes(x= Corruption), fill= '#CC0000') +
  labs(
    title = 'Perceptions of Corruption Distribution of Countries',
    x= 'Perceptions of Corruption',
    y= 'Frequency'
  ) +
  ggthemes::theme_few()

df_nona %>%
  select(Corruption, `Happiness score`)%>%
  cor()

#5.) HAPPINESS SCORE AND FREEDOM TO MAKE LIFE CHOICES RELATION:
#5.1.) There exists a high positive correlation between Happiness Score and Freedom to make Life Choices:
df_nona %>%
  select(Freedom, `Happiness score`)%>%
  cor()

#5.2.) Graphical Visualization of Happiness Score and Freedom to make Life Choices:
df_nona %>%
  select(Freedom, `Happiness score`)%>%
  ggplot(aes(x= `Happiness score`, y= Freedom)) +
  geom_boxplot(colour = '#8ec54b') +
  geom_smooth(method= lm) +
  labs(
    title = 'Happiness score and Freedom to make Life choices',
    x= 'Happiness Score',
    y= 'Freedom to make Life choices'
  ) +
  ggthemes::theme_few()


#6.) GENEROSITY AND Happiness:
df_nona %>%
  select(Generosity, `Happiness score`)%>%
  ggplot(aes(x= Generosity, y= `Happiness score`)) +
  geom_point(colour = '#960808') +
  geom_smooth(method= lm) +
  labs(
    title = 'Generosity and Happiness',
    x= 'Generostiy',
    y= 'Happiness'
  ) +
  ggthemes::theme_few()

#7.) Healthy and Happiness
df_nona %>%
  select(Generosity, `Happiness score`)%>%
  cor()

GENEROSITY AND Happiness:
  df_nona %>%
  select(Healthy, `Happiness score`)%>%
  ggplot(aes(x= Healthy, y= `Happiness score`)) +
  geom_point(colour = '#960808') +
  geom_smooth(method= lm) +
  labs(
    title = 'Healthy and Happiness',
    x= 'Healthy',
    y= 'Happiness'
  ) +
  ggthemes::theme_few()

df_nona %>%
  select(Healthy, `Happiness score`)%>%
  cor()

#8.) Social support and Happiness
df_nona %>%
  select(Social_support, `Happiness score`)%>%
  cor()

Social_support AND Happiness:
  df_nona %>%
  select(Social_support, `Happiness score`)%>%
  ggplot(aes(x= Social_support, y= `Happiness score`)) +
  geom_point(colour = '#960808') +
  geom_smooth(method= lm) +
  labs(
    title = 'Social Support and Happiness',
    x= 'Social Support',
    y= 'Happiness'
  ) +
  ggthemes::theme_few()

df_nona %>%
  select(Social_support, `Happiness score`)%>%
  cor()

#7.1.) Generosity and Healthy Life Expectancy are negatively related to eath other:
df_nona %>%
  select(Generosity, Healthy)%>%
  cor()

#7.2.) Graphical Visualization of Generosity and Healthy Life Expectancy:
df_nona %>%
  select(Generosity, Healthy)%>%
  ggplot(aes(x= Healthy, y= Generosity)) +
  geom_point(colour = '#960808') +
  geom_smooth(method= lm) +
  labs(
    title = 'Healthy Life Expectancy and Generosity',
    x= 'Healthy Life Expectancy',
    y= 'Generosity'
  ) +
  ggthemes::theme_few()

#8
lm(`Happiness score` ~ GDP + Social_support + Healthy + Freedom , data = df_nona)
