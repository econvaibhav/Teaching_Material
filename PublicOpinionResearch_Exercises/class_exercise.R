library(tidyverse)
df <- read_csv("~/Downloads/ESS11e04_1-subset (3)/ESS11e04_1-subset.csv")


glimpse(df)
table(df$pstplonl)
unique(df$pstplonl)

table(df$pstplonl, df$gndr)

table(df$pstplonl)
df <- df %>% 
  mutate(pstplonl_new = case_when(
    pstplonl == 1 ~ "Yes",
    pstplonl == 2 ~ "No",
    TRUE      ~ NA_character_
  ))
table(df$pstplonl_new)



table(df$gndr)
df <- df %>% 
  mutate(gender_new = case_when(
    gndr == 1 ~ "Male",
    gndr == 2 ~ "Female",
    TRUE      ~ NA_character_
  ))
table(df$gender_new)

table(df$pstplonl_new, df$gender_new)
prop.table(table(df$pstplonl_new, df$gender_new))


round(prop.table(table(df$pstplonl_new, df$gender_new, df$cntry), margin = 2)* 100, 2)


df_DE <- df %>% 
  filter(cntry == "DE")

round(prop.table(table(df_DE$pstplonl_new, df_DE$gender_new), margin = 2)* 100, 2)


summary(df$trstprt)


plot(df$pstplonl)
hist(df$netustm)


table(df$trstplt)
df <- df %>% 
  mutate(trust_pol_new= case_when(
    trstplt %in% c(0,1,2,3)  ~ "Low",
    trstplt %in% c(4,5,6,7) ~ "Medium",
    trstplt %in% c(8,9,10) ~ "High",
    TRUE      ~ NA_character_
  ))
table(df$pstplonl_new,df$trust_pol_new)


df_germany <- df %>% 
  filter(cntry == "DE")


df_3CONT <- df %>% 
  filter(cntry %in% c("DE", "FI", "AT") )


df_interest <- df %>% 
  filter(cntry %in% c("DE", "FI", "AT")) %>% 
  filter(trust_pol_new == "High")



df_interest2 <- df %>% 
  filter(cntry %in% c("DE", "FI", "AT") & trust_pol_new == "High")  


library(tidyverse)
library(scales)

# Viz Theme 
theme_graphs <- theme(
  axis.text.x = element_text(face = "bold"),
  axis.title.x = element_text(color = "black", size = 10, face = "bold"),
  axis.title.y = element_text(color = "black", size = 10, face = "bold"),
  plot.title = element_text(color = "black", size = 12, hjust = 0.5),
  plot.subtitle = element_text(color = "black", size = 8, hjust = 0.5),
  plot.caption = element_text(face = "italic"),
  legend.position = 'bottom'
  
)

df %>% 
  filter(trust_pol_new %in% c("High", "Medium", "Low") ) %>% 
  ggplot(aes(x=trust_pol_new, fill = pstplonl_new))+
  scale_fill_manual(values = c("#FF5555", "#CAEEC2", "darkgrey"))+
  scale_y_continuous(labels = scales::percent) +
  geom_bar(position = "fill") + 
  facet_wrap(~cntry)+
  labs(y= "Percentage",
       x = "Trust in Politicians",
       fill = "12 Month \nActivity SM",
       title = "Trust in Politicians by SM Act.",
       subtitle = "Made in POR Class",
       caption = "ESS Dataset") +
  theme_bw()+
  theme_graphs


df2 <- df %>%
  filter(trust_pol_new %in% c("High", "Medium", "Low")) %>%
  group_by(cntry, trust_pol_new, pstplonl_new) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(cntry, trust_pol_new) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>% 
  na.omit() %>% 
  mutate(
    trust_pol_new = factor(trust_pol_new, levels = c("Low", "Medium", "High")),
    pstplonl_new = factor(pstplonl_new, levels = c("Yes", "No"))
  )


df2 %>% 
  filter(trust_pol_new %in% c("High", "Medium", "Low") ) %>% 
  ggplot(aes(x=trust_pol_new, y = prop, fill = pstplonl_new))+
  geom_bar(stat= "identity",position = "fill") +
  geom_text(aes(label = scales::percent(prop, accuracy = 1)),
            position = position_fill(vjust = 0.5),
            size = 3,
            color = "black") +
  scale_fill_manual(values = c("#CAEEC2", "pink", "darkgrey"))+
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~cntry)+
  labs(y= "Percentage",
       x = "Trust in Politicians",
       fill = "12 Month \nActivity SM",
       title = "Trust in Politicians by SM Act.",
       subtitle = "Made in POR Class",
       caption = "ESS Dataset") +
  theme_bw()+
  theme_graphs


