library(tidyverse)
df <- read_csv("~/Downloads/Exercise_POR/ESS11e04_1-subset.csv")

glimpse(df)
table(df$agegroup,df$pstplonl)
prop.table(table(df$agegroup,df$pstplonl))
round(prop.table(table(df$agegroup, df$pstplonl)) * 100,1)
round(prop.table(table(df$agegroup, df$pstplonl), margin = 1) * 100,1)


summary(df)
summary(df$pstplonl)


hist(df$yrbrn)
table(df$yrbrn)
unique(df$yrbrn)

df <- df %>% 
  filter(!yrbrn %in% c(7777,9999, 8888))
table(df$yrbrn)
unique(df$yrbrn)
hist(df$yrbrn)
  

table(df$pstplonl)
df <- df %>%
  mutate(pstplonl_new = case_when(
    pstplonl == 1 ~ "Yes",
    pstplonl == 2 ~ "No",
    TRUE      ~ NA_character_
  ))
table(df$pstplonl_new)


table(df$trstplt)
df <- df %>%
  mutate(trstplt_new = case_when(
    trstplt %in% c(7,8,9,10) ~ "High",
    trstplt %in% c(4,5,6) ~ "Medium",
    trstplt %in% c(0,1,2,3) ~ "Low",
    TRUE      ~ NA_character_
  ))
table(df$trstplt_new)


table(df$pstplonl_new, df$trstplt_new)
prop.table(table(df$pstplonl_new, df$trstplt_new))*100
round(prop.table(table(df$pstplonl_new, df$trstplt_new))*100,2)
round(prop.table(table(df$pstplonl_new, df$trstplt_new), margin = 1)*100,2)


df %>%
  group_by(pstplonl_new) %>%
  na.omit() %>% 
  summarize(across(c(trstprl, trstlgl, trstplc, trstplt, trstprt, trstep, trstun), 
                   ~ mean(.x, na.rm = TRUE)))
df %>%
  group_by(pstplonl_new) %>%
  na.omit() %>% 
  filter(if_all(c(trstprl, trstlgl, trstplc, trstplt, trstprt, trstep, trstun), 
                ~ .x >= 0 & .x <= 10)) %>%
  summarize(across(c(trstprl, trstlgl, trstplc, trstplt, trstprt, trstep, trstun), 
                   ~ mean(.x, na.rm = TRUE)))



library(ggplot2)
library(scales)


df %>% 
  filter(trstplt_new %in% c("High", "Medium", "Low") ) %>% 
  ggplot(aes(x= trstplt_new,fill=pstplonl_new)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("darkred", "darkgreen", "grey"))+ 
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~cntry)+
  labs(y = "Percentage", 
       x = "Country",
       title = "Response by Country",
       fill = "Response") +
  theme_bw() 




df2 <- df %>%
  filter(trstplt_new %in% c("High", "Medium", "Low")) %>%
  group_by(cntry, trstplt_new, pstplonl_new) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(cntry, trstplt_new) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>% 
  na.omit() %>% 
  mutate(
    trstplt_new = factor(trstplt_new, levels = c("Low", "Medium", "High")),
    pstplonl_new = factor(pstplonl_new, levels = c("Yes", "No"))
  )

df2 %>%
  ggplot(aes(x = trstplt_new, y = prop, fill = pstplonl_new)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = scales::percent(prop, accuracy = 1)),
            position = position_fill(vjust = 0.5),
            size = 3,
            color = "white") +
  scale_fill_manual(values = c("darkgreen", "darkred", "darkgrey")) + 
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~cntry) +
  labs(y = "Percentage", 
       x = "Trust Level (politicians)",
       title = "Posted Political Content by Country and Trust",
       fill = "Posted Political \nContent") +
  theme_bw()

