# SOT86131
# Public Opinion Research Methods - Seminar
# David Karpa and Vaibhav Agarwal
# Sample .R file for Task 1

# Loading the Required Packages
library(tidyverse)
library(ggcorrplot)
library(ggthemes)
library(scales)
library(stargazer)
library(kableExtra)
library(ggrepel)

# Loading the Data
wvs_df <- read_csv("~/Downloads/F00011356-WVS_Cross-National_Wave_7_csv_v6_0/WVS_Cross-National_Wave_7_csv_v6_0.csv")

# Cleaning Code
clean_wvs <- wvs_df %>%
  select(
    A_YEAR, B_COUNTRY_ALPHA, W_WEIGHT,
    Q196, Q197, Q198,            # Surveillance Acceptance Outcome Var
    Q65, Q71, Q70, Q69,          # Institutional Trust
    Q131,                        # Perceived Threat
    num_range("Q", 139:141),      # Worried about Safety
    Q235, Q45,                        # Authoritarian disposition
    Q275, Q288                   # Socio-demographics (Education, Income)
  ) %>%
  mutate(across(where(is.numeric), ~ ifelse(.x < 0, NA, .x))) %>% # Unlike ESS, which might need different filters for non response as they are 77, 88, 99 etc., all non-response in WVS is stored as negative numbers, so we can directly convert those to NA
  mutate(
    surv_acceptance     = rowMeans(pick(Q196, Q197, Q198), na.rm = TRUE),
    safey_worried     = rowMeans(pick(Q139, Q140, Q141), na.rm = TRUE),
    ins_trust     = rowMeans(pick(Q65, Q71, Q70, Q69), na.rm = TRUE),
    
    surv_acceptance_rev = 5 - surv_acceptance,
    safey_worried_rev = 5 - safey_worried,
    ins_trust_rev = 5 - ins_trust,
    
    perc_threat = 5 - Q131, 

    auth_disp           = 5 - Q235,
    education           = Q275,
    income              = Q288
  )


################################################################################
# Corrplot = Graph 1 

wvs_full_subset <- clean_wvs %>%
  select(
    surv_acceptance_rev,
    ins_trust_rev,
    safey_worried_rev,
    perc_threat,
    auth_disp,
    Q45,
    education,
    income
  ) %>%
  mutate(across(everything(), as.numeric)) %>%
  set_names(c(
    "Surveillance Acceptance",
    "Institutional Trust",
    "Worried About Safety",
    "Perceived Threat (Q131)",
    "Auth: Strong Leader",
    "Auth: Respect for Authority",
    "Education Level", 
    "Income Scale"
  ))

wvs_cor_mat_full <- round(cor(wvs_full_subset, use = "pairwise.complete.obs"), 2)
wvs_cor_pmat_full <- cor_pmat(wvs_full_subset)

ggcorrplot(wvs_cor_mat_full,
           type = "lower",
           p.mat = wvs_cor_pmat_full,
           sig.level = 0.05,
           insig = "blank",
           outline.col = "black",
           ggtheme = theme_clean(),
           colors = c("black", "white", "grey70"), 
           lab = TRUE,
           tl.cex = 10,
           show.legend = FALSE,
           title = 'Correlation Plot of Imp. Variables')

################################################################################
# Scatter + lm = Graph 2 

scatter_data_perc <- clean_wvs %>%
  mutate(
    high_surv   = ifelse(surv_acceptance_rev >= 3, 1, 0),
    high_auth   = ifelse(auth_disp >= 3, 1, 0),
    high_trust  = ifelse(ins_trust_rev >= 3, 1, 0)
  ) %>%
  group_by(B_COUNTRY_ALPHA) %>%
  summarise(
    perc_surv   = mean(high_surv, na.rm = TRUE),
    perc_auth   = mean(high_auth, na.rm = TRUE),
    perc_trust  = mean(high_trust, na.rm = TRUE)
  ) %>%
  drop_na() %>%
  pivot_longer(
    cols = c(perc_auth, perc_trust),
    names_to = "y_variable",
    values_to = "y_value"
  ) %>%
  mutate(
    # Clean up labels for easy naming
    y_variable = case_when(
      y_variable == "perc_auth"   ~ "Prefer Strong Leader",
      y_variable == "perc_trust"  ~ "High Institutional Trust"
    ))


# Taking the same theme from exercise classes, with few modifications. 
theme_graphs <- theme(
  axis.text.x = element_text(color = "black", size = 10),
  axis.title.x = element_text(color = "black", size = 10),
  axis.title.y = element_text(color = "black", size = 10),
  plot.title = element_text(color = "black", size = 16, hjust = 0.5,face = "bold"),
  plot.subtitle = element_text(color = "black", size = 12, hjust = 0.5),
  plot.caption = element_text(face = "italic"),
  legend.position = 'none',
  strip.background = element_rect(fill = "black"),
  strip.text = element_text(color = "white", face = "bold", size = 11),
  panel.grid.minor = element_blank()
  
)

# Can also use data %>%  ggplot method. Both have the same results. 
ggplot(scatter_data_perc, aes(x = y_value, y = perc_surv)) +
  geom_smooth(method = "lm", color = "red", linewidth = 1, se = TRUE, alpha = 0.2) +
  geom_text_repel(
    aes(label = B_COUNTRY_ALPHA, color = y_variable), 
    size = 3.5,           
    max.overlaps = 20,  
    segment.color = "grey50", 
    color = "black"
  ) +
  facet_wrap(~ y_variable, scales = "free_y") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  
  theme_bw() +
  labs(
    x = "% of Population Agreeing (Refer to Facet Title)",
    y = "% of Population Accepting High Surveillance",
    title = "Authoritarian Disposition and State Survelliance"
  ) +
  theme_graphs

################################################################################
# stacked bar graph = Graph 3

target_countries <- c("DEU","CHN","IND")

bar_plot_base <- clean_wvs %>%
  filter(B_COUNTRY_ALPHA %in% target_countries) %>%
  drop_na(surv_acceptance_rev) %>%
  mutate(
    Surv_Attitude = ntile(surv_acceptance_rev, 3),
    Surv_Attitude = factor(case_when(
      Surv_Attitude == 1 ~ "Low",
      Surv_Attitude == 2 ~ "Moderate",
      Surv_Attitude == 3 ~ "High"
    ), levels = c("Low", "Moderate", "High"))
  )

# Left side - auth desp

plot_data_auth <- bar_plot_base %>%
  drop_na(auth_disp) %>%
  mutate(
    Auth_Level = factor(case_when(
      ntile(auth_disp, 3) == 1 ~ "Low Auth.",
      ntile(auth_disp, 3) == 2 ~ "Moderate Auth.",
      ntile(auth_disp, 3) == 3 ~ "High Auth."
    ), levels = c("Low Auth.", "Moderate Auth.", "High Auth."))
  ) %>%
  count(B_COUNTRY_ALPHA, Surv_Attitude, Auth_Level) %>%
  group_by(B_COUNTRY_ALPHA, Surv_Attitude) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

a <- ggplot(plot_data_auth, aes(x = Surv_Attitude, y = prop, fill = Auth_Level)) +
  geom_col(color = "black", linewidth = 0.2) + 
  geom_text(
    aes(label = scales::percent(prop, accuracy = 1)), 
    position = position_stack(vjust = 0.5), size = 3.5, color = "black"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_grey(start = 0.4, end = 1) + 
  theme_bw() +
  theme_graphs +
  theme( 
    legend.position = "right" 
  ) +
  labs(
    x = "Surveillance Acceptance", 
    y = "Percentage of Respondents",
    title = "Surveillance Acceptance by Authoritarian Disposition",
    fill = "Preference for Strong Leader:"
  ) +
  facet_wrap(~ B_COUNTRY_ALPHA)


# right side
plot_data_trust <- bar_plot_base %>%
  drop_na(ins_trust_rev) %>%
  mutate(
    Trust_Level = factor(case_when(
      ntile(ins_trust_rev, 3) == 1 ~ "Low Trust",
      ntile(ins_trust_rev, 3) == 2 ~ "Moderate Trust",
      ntile(ins_trust_rev, 3) == 3 ~ "High Trust"
    ), levels = c("Low Trust", "Moderate Trust", "High Trust"))
  ) %>%
  count(B_COUNTRY_ALPHA, Surv_Attitude, Trust_Level) %>%
  group_by(B_COUNTRY_ALPHA, Surv_Attitude) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

b <- ggplot(plot_data_trust, aes(x = Surv_Attitude, y = prop, fill = Trust_Level)) +
  geom_col(color = "black", linewidth = 0.2) +
  geom_text(
    aes(label = scales::percent(prop, accuracy = 1)), 
    position = position_stack(vjust = 0.5), size = 3.5, color = "black"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_grey(start = 0.4, end = 1) + 
  theme_bw() +
  theme_graphs + 
  theme(
    legend.position = "right"
  ) +
  labs(
    x = "Surveillance Acceptance", 
    y = "Percentage of Respondents",
    title = "Surveillance Acceptance by Institutional Trust",
    fill = "Institutional Trust Level:"
  ) +
  facet_wrap(~ B_COUNTRY_ALPHA)



library(patchwork)
a / b

###############################################################################
# desc. stats = Table 1

reg_data <- clean_wvs %>%
  drop_na(surv_acceptance_rev, ins_trust_rev, auth_disp)

raw_cat_data <- reg_data %>%
  mutate(
    `Surveillance Acceptance`   = round(surv_acceptance_rev),
    `Institutional Trust`       = round(ins_trust_rev),
    `Authoritarian Disposition` = round(auth_disp)
  ) %>%
  select(`Surveillance Acceptance`, `Institutional Trust`, `Authoritarian Disposition`)

summary_stats_raw <- raw_cat_data %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Score") %>%
  group_by(Variable, Score) %>%
  summarise(Count = n(), .groups = "drop_last") %>%
  mutate(Percentage = Count / sum(Count)) %>%
  ungroup() %>%
  mutate(
    Description = case_when(
      Variable == "Surveillance Acceptance"   ~ "Mean index of WVS items Q196-Q198 (higher = more acceptance)",
      Variable == "Institutional Trust"       ~ "Mean index of confidence in Gov, Police, Courts, Armed Forces",
      Variable == "Authoritarian Disposition" ~ "Preference for a strong unconstrained leader"
    ),
    Percentage = percent(Percentage, accuracy = 0.1),
    Score = as.character(Score)
  ) %>%
  select(Variable, Description, Score, Count, Percentage) %>%
  arrange(Variable, Score)

summary_stats_raw %>%
  kbl(
    col.names = c("Variable", "Description", "Score", "N (Count)", "Percentage"),
    caption = "<b>Table 1: Percentage Distribution of Key Variables (1-4 Scale)</b>",
    align = c("l", "l", "c", "c", "c"),
    escape = FALSE
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"), 
    full_width = FALSE,
    position = "center",
    font_size = 14
  ) %>%
  row_spec(0, bold = TRUE, color = "white", background = "black") %>%
  collapse_rows(columns = 1:2, valign = "top") %>%
  column_spec(1, bold = TRUE, width = "12em") %>%
  column_spec(2, width = "18em", italic = TRUE) %>%
  column_spec(3, width = "5em") %>%
  footnote(
    general = "Averages were rounded off to nearest whole number to map to the original 1-4 scale. \n Data: WVS Wave 7",
    general_title = "Note: "
  )

################################################################################
# linear regression = Table 2

reg_data <- clean_wvs %>%
  drop_na(surv_acceptance_rev, ins_trust_rev, safey_worried_rev, perc_threat, 
          auth_disp, education, income, W_WEIGHT)

fit_baseline <- lm(surv_acceptance_rev ~ ins_trust_rev + safey_worried_rev, 
                   data = reg_data, weights = W_WEIGHT)

fit_full <- lm(surv_acceptance_rev ~ ins_trust_rev + safey_worried_rev + perc_threat + 
                 auth_disp + education + income, 
               data = reg_data, weights = W_WEIGHT)

stargazer(
  fit_baseline, fit_full,
  type = "latex",
  title = "Regression Results: Acceptance of State Surveillance",
  dep.var.labels = c("Surveillance Acceptance Index"),
  covariate.labels = c("Institutional Trust Index", "Safety Worries Index", "Perceived Threat (Q131)", 
                       "Authoritarian Disposition", "Education Level", "Income Scale"),
  omit.stat = c("f", "ser"),
  star.cutoffs = c(0.05, 0.01, 0.001),
  notes = "Weighted OLS Regression."
)



