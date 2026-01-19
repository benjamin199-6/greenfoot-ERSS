###############################################################################
# 02_descriptives_figures.R
#
# Purpose:
# Create descriptives and figures for the paper.
# Saves ALL figures to: paper/figures
# Saves ALL tables/text outputs to: paper/tables
#
# Assumes 00_setup.R and 01_load_data.R have been sourced first.
###############################################################################

message("Running 02_descriptives_figures.R ...")

# ---------------------------------------------------------------------------
# 0. Sanity check
# ---------------------------------------------------------------------------
if (!exists("data")) stop("Object 'data' not found. Run 01_load_data.R first.")

# ---------------------------------------------------------------------------
# Helpers: save plots/tables to correct folders
# ---------------------------------------------------------------------------
save_plot <- function(filename, plot = ggplot2::last_plot(),
                      width = 7, height = 5, dpi = 300, units = "in", bg = "white") {
  ggplot2::ggsave(
    filename = file.path(paths$output_figures, filename),
    plot = plot,
    width = width,
    height = height,
    dpi = dpi,
    units = units,
    bg = bg
  )
}

save_txt <- function(filename, x) {
  out <- file.path(paths$output_tables, filename)
  cat(paste0(x, collapse = "\n"), file = out)
}

save_capture <- function(filename, expr) {
  out <- file.path(paths$output_tables, filename)
  capture.output(expr, file = out)
}

# ---------------------------------------------------------------------------
# 1. Build df (1:1 from your script)
# ---------------------------------------------------------------------------
df <- data %>%
  dplyr::select(
    CHOIX, Country, AgeGroup, Gender, EmploymentStatus, HasChildren, AnnualNetIncome,
    InterestInSports, InterestInFootball, FootballSupportFrequency, FavouriteFootballTeam,
    ClimateChangeConcern, ReducedElectricityUse, EnvConcernEnergyChoices, SustainableTravel,
    LifestyleChanges, LessIntensiveDiet, NoEmissionReduction, UnsureEmissionReduction,
    CrowdfundingFamiliarity, SavingsAccount, StocksBonds, NoSavings, UnsureSavings,
    CrowdfundingReturnPreference, FinancialBenefitType, MainParticipationReason, SecondReason,
    LeastImportantReason, CrowdfundingLikelihoodSchool, MainReasonParticipateSchool,
    MoreLikelyCarbonReduction, MoreLikelyLocalSupport, CrowdfundingLikelihoodFootball,
    MainReasonParticipateFootball, MoreLikelyCarbonReductionFootball, MoreLikelyLocalSupportFootball,
    ParticipationAmount
  )

df <- as.data.frame(df)

df <- df %>%
  mutate(
    LTP = coalesce(CrowdfundingLikelihoodFootball, CrowdfundingLikelihoodSchool),
    Building = ifelse(CHOIX == 2, 1, 0),
    Fan = ifelse(InterestInFootball >= 4, 1, 0),
    Crowdfunding_participated = ifelse(CrowdfundingFamiliarity == 1, 1, 0),
    Crowdfunding_heard_not_participated = ifelse(CrowdfundingFamiliarity == 2, 1, 0),
    Crowdfunding_else = ifelse(CrowdfundingFamiliarity > 2, 1, 0),
    France = ifelse(Country == 1, 1, 0),
    Ireland = ifelse(Country == 2, 1, 0),
    Sweden = ifelse(Country == 3, 1, 0),
    Azerbaijan = ifelse(Country == 4, 1, 0),
    female = ifelse(Gender == 1, 2, 0),
    Empl_fulltime = ifelse(EmploymentStatus == 1, 1, 0),
    Empl_part_time = ifelse(EmploymentStatus == 2, 1, 0),
    Empl_self_employed = ifelse(EmploymentStatus == 3, 1, 0),
    Empl_retired = ifelse(EmploymentStatus == 4, 1, 0),
    Emp_unemployed = ifelse(EmploymentStatus == 5, 1, 0),
    Empl_student = ifelse(EmploymentStatus == 6, 1, 0),
    Emp_other = ifelse(EmploymentStatus == 7, 1, 0),
    income_1 = ifelse(AnnualNetIncome == 1, 1, 0),
    income_2 = ifelse(AnnualNetIncome == 2, 1, 0),
    income_3 = ifelse(AnnualNetIncome == 2, 1, 0),
    income_4 = ifelse(AnnualNetIncome == 3, 1, 0)
  ) %>%
  filter(Gender != 3)

df$Concerns_binary <- ifelse(df$ClimateChangeConcern > 4, 1, 0)

df <- df %>%
  dplyr::mutate(Group = case_when(
    Building == 1 & Fan == 1 ~ "Football_Fan",
    Building == 1 & Fan == 0 ~ "Football_NoFan",
    Building == 0 & Fan == 1 ~ "Neutral_Fan",
    Building == 0 & Fan == 0 ~ "Neutral_NoFan"
  ))

df <- df %>%
  mutate(
    Reason_Local_support = coalesce(MoreLikelyLocalSupportFootball, MoreLikelyLocalSupport),
    Reason_Emission_reduction = coalesce(MoreLikelyCarbonReductionFootball, MoreLikelyCarbonReduction)
  )


# Figure 3

ci_data <- df %>%
  group_by(Fan, Building) %>%
  summarise(count = n()) %>%
  mutate(percent = (count / sum(count)) * 100,
         se = sqrt(percent * (100 - percent) / sum(count)),
         ci_low = percent - 1.96 * se,
         ci_high = percent + 1.96 * se,
         Building_label = ifelse(Building == "1", "Football", "Neutral"))


ci_data$Fan=as.factor(ci_data$Fan)
ci_data$Building=as.factor(ci_data$Building)


# Choose a Wes Anderson palette
# For example, the "GrandBudapest1" for a pink and brown scheme
palette <- wes_palette("Royal1", n = 2, type = "discrete")  # Ensuring discrete colors for categorical data


# Convert 'Fan' from numeric to factor with labels
ci_data <- ci_data %>%
  mutate(Fan = factor(Fan, levels = c(0, 1), labels = c("No Fan", "Fan")))

p_ci_fb=ggplot(ci_data, aes(x = Fan, y = percent, fill = factor(Building, labels = c("Neutral", "Football")))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                position = position_dodge(width = 0.9), width = 0.25) +
  geom_text(aes(label = paste0(round(percent, 1), "%"),
                y = percent + 2), # Adjust the position of labels above the bars
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5) +
  labs(title = "",
       x = "", y = "", fill = "Building") +
  scale_fill_manual(values = palette) +  # Use Wes Anderson color palette
  theme_minimal() +
  theme(axis.title.y = element_blank(),  # Remove y-axis label
        axis.text.y = element_blank(),   # Remove y-axis text
        axis.ticks.y = element_blank(),  # Remove y-axis ticks
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))  # Adjust plot margins



print(p_ci_fb)

# Save
ggsave(
  filename = file.path(paths$output_figures, "Figure3_ci_fan_building.png"),
  plot = p_ci_fb,
  width = 7,
  height = 5,
  dpi = 300,
  units = "in",
  bg = "white"
)


# Figure 4:
# Data preparation for Climate Change Concern
env_data <- df %>%
  group_by(Group) %>%
  summarise(
    mean_prop = mean(ClimateChangeConcern),
    se_prop = sd(ClimateChangeConcern) / sqrt(n()),  # Corrected to use its own SD
    ci_lower = mean_prop - 1.96 * se_prop,  # 95% CI Lower Bound
    ci_upper = mean_prop + 1.96 * se_prop   # 95% CI Upper Bound
  )


plot_env <- ggplot(env_data, aes(x = Group, y = mean_prop, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                position = position_dodge(width = 0.9), width = 0.25) +
  labs(
    x = "", y = "Mean Response Likert Scale from 1-5") +
  scale_x_discrete(labels = c("Football_Fan" = "Football Fan", "Football_NoFan" = "No Football Fan",
                              "Neutral_Fan" = "Neutral Fan", "Neutral_NoFan" = "No Neutral Fan")) +
  scale_fill_manual(values = royal_palette) +
  theme_minimal() +
  theme(
    legend.position = "none",
    #axis.title.y = element_blank(), # Remove y-axis title
    axis.text.y = element_blank(),  # Remove y-axis text
    axis.ticks.y = element_blank(), # Remove y-axis ticks
    panel.grid.major.y = element_blank(), # Remove major grid lines on y-axis
    panel.grid.minor.y = element_blank()  # Remove minor grid lines on y-axis
  ) +
  geom_text(aes(label = round(mean_prop, 2)), 
            position = position_dodge(width =0.5), 
            vjust = -1.5)  # Display values above bars

# Print the plot
print(plot_env)




# ---------------------------------------------------------------------------
# 4. Crowdfunding + environment barplots with CI (1:1)
# ---------------------------------------------------------------------------
crowdfunding_data <- df %>%
  group_by(Group) %>%
  summarise(
    mean_prop = mean(CrowdfundingFamiliarity),
    se_prop = sd(CrowdfundingFamiliarity) / sqrt(n()),
    ci_lower = mean_prop - 1.96 * se_prop,  # 95% CI Lower Bound
    ci_upper = mean_prop + 1.96 * se_prop   # 95% CI Upper Bound
  )
# Plot for crowdfunding data
# Plot for environmental concern data

royal_palette <- wes_palette("Royal1", 4, type = "discrete")


crowdf <- ggplot(crowdfunding_data, aes(x = Group, y = mean_prop, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                position = position_dodge(width = 0.9), width = 0.25) +
  labs(
    x = "", y = "Mean Response Likert Scale from 1-5") +
  scale_x_discrete(labels = c("Football_Fan" = "Football Fan", "Football_NoFan" = "No Football Fan",
                              "Neutral_Fan" = "Neutral Fan", "Neutral_NoFan" = "No Neutral Fan")) +
  scale_fill_manual(values = royal_palette) +
  theme_minimal() +
  theme(
    legend.position = "none",
    #axis.title.y = element_blank(), # Remove y-axis title
    axis.text.y = element_blank(),  # Remove y-axis text
    axis.ticks.y = element_blank(), # Remove y-axis ticks
    panel.grid.major.y = element_blank(), # Remove major grid lines on y-axis
    panel.grid.minor.y = element_blank()  # Remove minor grid lines on y-axis
  ) +
  geom_text(aes(label = round(mean_prop, 2)), 
            position = position_dodge(width =0.5), 
            vjust = -1.5)  # Display values above bars

# Print the plot
print(crowdf)

# Save the plot (paper/figures)
ggsave(
  filename = file.path(paths$output_figures, "Figure5_crowdfunding_by_group.png"),
  plot = crowdf,
  width = 7,
  height = 5,
  dpi = 300,
  units = "in",
  bg = "white"
)


##########################################


env_data <- df %>%
  group_by(Group) %>%
  summarise(
    mean_prop = mean(ClimateChangeConcern),
    se_prop = sd(ClimateChangeConcern) / sqrt(n()),
    ci_lower = mean_prop - 1.96 * se_prop,
    ci_upper = mean_prop + 1.96 * se_prop
  )

royal_palette4 <- wesanderson::wes_palette("Royal1", 4, type = "discrete")

crowdf <- ggplot(crowdfunding_data, aes(x = Group, y = mean_prop, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                position = position_dodge(width = 0.9), width = 0.25) +
  labs(x = "", y = "Mean Response Likert Scale from 1-5") +
  scale_x_discrete(labels = c("Football_Fan" = "Football Fan", "Football_NoFan" = "No Football Fan",
                              "Neutral_Fan" = "Neutral Fan", "Neutral_NoFan" = "No Neutral Fan")) +
  scale_fill_manual(values = royal_palette4) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  geom_text(aes(label = round(mean_prop, 2)),
            position = position_dodge(width = 0.5),
            vjust = -1.5)

print(crowdf)
save_plot("crowdfunding_by_group.png", crowdf)

plot_env <- ggplot(env_data, aes(x = Group, y = mean_prop, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                position = position_dodge(width = 0.9), width = 0.25) +
  labs(x = "", y = "Mean Response Likert Scale from 1-5") +
  scale_x_discrete(labels = c("Football_Fan" = "Football Fan", "Football_NoFan" = "No Football Fan",
                              "Neutral_Fan" = "Neutral Fan", "Neutral_NoFan" = "No Neutral Fan")) +
  scale_fill_manual(values = royal_palette4) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  geom_text(aes(label = round(mean_prop, 2)),
            position = position_dodge(width = 0.5),
            vjust = -1.5)

print(plot_env)
save_plot("env_concern_by_group.png", plot_env)

# ---------------------------------------------------------------------------
# 5. ANOVA output (save to tables)
# ---------------------------------------------------------------------------
anova_fan <- aov(ClimateChangeConcern ~ Group, data = df)
save_capture("anova_climateconcern_by_group.txt", summary(anova_fan))
save_capture("tukey_climateconcern_by_group.txt", TukeyHSD(anova_fan))

# ---------------------------------------------------------------------------
# 6. Appendix plot: Fan x Building x Country (1:1)
# ---------------------------------------------------------------------------
ci_data2 <- df %>%
  group_by(Fan, Building, Country) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Fan, Building) %>%
  mutate(
    total_count = sum(count),
    percent = (count / total_count) * 100,
    se = sqrt(percent * (100 - percent) / total_count),
    ci_low = percent - 1.96 * se,
    ci_high = percent + 1.96 * se,
    Building_label = ifelse(Building == 1 | Building == "1", "Football", "Neutral"),
    Country_label = case_when(
      Country == "France" ~ "France",
      Country == "Ireland" ~ "Ireland",
      Country == "Sweden" ~ "Sweden",
      Country == "Azerbaijan" ~ "Azerbaijan",
      TRUE ~ as.character(Country)
    )
  )

ci_data2$Fan <- as.factor(ci_data2$Fan)

plot_ci <- ggplot(ci_data2, aes(x = Fan, y = percent, fill = Building_label)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                position = position_dodge(width = 0.9), width = 0.25) +
  geom_text(aes(label = paste0(round(percent, 1), "%"),
                y = percent + 2),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5) +
  labs(title = "", x = "Fan", y = "", fill = "Building") +
  scale_fill_manual(values = c("Football" = "black", "Neutral" = "grey"),
                    labels = c("Football", "Neutral")) +
  facet_grid(rows = vars(Country)) +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

print(plot_ci)
save_plot("fan_building_by_country.png", plot_ci, width = 10, height = 8)

# ---------------------------------------------------------------------------
# 7. TableOne: Age/Gender/Income by Country (save to tables)
# ---------------------------------------------------------------------------
vars <- c("AgeGroup", "Gender", "AnnualNetIncome")
factorVars <- c("AgeGroup", "Gender", "AnnualNetIncome")

table_one <- tableone::CreateTableOne(vars = vars, strata = "Country", data = df, factorVars = factorVars)
save_capture("tableone_by_country.txt", print(table_one, test = FALSE))

# ---------------------------------------------------------------------------
# 8. Marginal effects bar plot (data entered manually in your script)
# ---------------------------------------------------------------------------
me_data <- data.frame(
  LTP = factor(c("Very Unlikely", "Unlikely", "Indifferent", "Likely", "Very Likely"),
               levels = c("Very Unlikely", "Unlikely", "Indifferent", "Likely", "Very Likely")),
  Margin = c(-0.1128, -0.0461, -0.0109, 0.0957, 0.0741),
  SE = c(0.0152, 0.0073, 0.0046, 0.0130, 0.0138)
)

p_me <- ggplot(me_data, aes(x = LTP, y = Margin)) +
  geom_bar(stat = "identity", fill = wesanderson::wes_palette("Royal1")[2]) +
  geom_errorbar(aes(ymin = Margin - SE, ymax = Margin + SE), width = 0.2) +
  labs(x = "Likelihood to Participate", y = "Marginal Effect") +
  theme_minimal() +
  coord_flip()

print(p_me)
save_plot("marginal_effects_ltp.png", p_me, width = 7, height = 5)

# ---------------------------------------------------------------------------
# 9. Balance tables (save)
# ---------------------------------------------------------------------------
df$Treatment <- ifelse(df$Building == 1, "Football", "Neutral")

balance_vars1 <- c(
  "AgeGroup", "Gender", "AnnualNetIncome",
  "ClimateChangeConcern", "EnvConcernEnergyChoices",
  "SustainableTravel", "ReducedElectricityUse", "LifestyleChanges"
)

factor_vars1 <- c("AgeGroup", "Gender", "AnnualNetIncome", "ClimateChangeConcern", "EnvConcernEnergyChoices")

balance_table1 <- tableone::CreateTableOne(vars = balance_vars1, strata = "Group",
                                           data = df, factorVars = factor_vars1)
save_capture("balance_table_by_group.txt", print(balance_table1, smd = TRUE, test = FALSE))

balance_vars2 <- c(
  "AgeGroup", "Gender", "EmploymentStatus", "AnnualNetIncome", "EnvConcernEnergyChoices",
  "ReducedElectricityUse", "SustainableTravel", "LessIntensiveDiet",
  "CrowdfundingReturnPreference", "MainParticipationReason"
)

factor_vars2 <- c("AgeGroup", "Gender", "EmploymentStatus", "AnnualNetIncome",
                  "CrowdfundingReturnPreference", "MainParticipationReason")

balance_table2 <- tableone::CreateTableOne(vars = balance_vars2, strata = "Treatment",
                                           data = df, factorVars = factor_vars2)
save_capture("balance_table_by_treatment.txt", print(balance_table2, smd = TRUE, test = TRUE))

# ---------------------------------------------------------------------------
# 10. WTP plots (Fans vs Non-Fans; then distributions; then by country) (1:1)
# ---------------------------------------------------------------------------
wtp_midpoints <- c(0, 30, 75, 200, 400, 750, 3000, 7500, 15000)

df <- df %>%
  mutate(
    Fan = factor(Fan, labels = c("Non-Fan", "Fan")),
    WTP_num = wtp_midpoints[ParticipationAmount]
  )

summary_df <- df %>% filter(CHOIX == 2) %>%
  group_by(Fan) %>%
  summarise(
    mean_wtp = mean(WTP_num, na.rm = TRUE),
    sd_wtp = sd(WTP_num, na.rm = TRUE),
    n = n(),
    se = sd_wtp / sqrt(n),
    ci_low = mean_wtp - qt(0.975, n - 1) * se,
    ci_high = mean_wtp + qt(0.975, n - 1) * se
  )

t_test <- t.test(WTP_num ~ Fan, data = df)
p_value <- t_test$p.value

stars <- dplyr::case_when(
  p_value < 0.001 ~ "***",
  p_value < 0.01  ~ "**",
  p_value < 0.05  ~ "*",
  TRUE ~ "ns"
)

p_wtp_simple <- ggplot(summary_df, aes(x = Fan, y = mean_wtp, fill = Fan)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.15, size = 0.8) +
  annotate("text", x = 1.5, y = max(summary_df$ci_high) * 1.05, label = stars,
           size = 7, fontface = "bold") +
  scale_fill_manual(values = wesanderson::wes_palette("Darjeeling1", 2, type = "discrete")) +
  labs(x = "", y = "Estimated contribution (EUR)", fill = "Group") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

print(p_wtp_simple)
save_plot("WTP_Fans_vs_NonFans.png", p_wtp_simple, width = 7, height = 5)

# ---- WTP detailed recoding for df_football (1:1) ----
df_football <- df %>% filter(CHOIX == 2)
df_football2 <- df_football %>% filter(Country != 4)

df_football <- df_football %>%
  mutate(
    ParticipationAmount = as.numeric(ParticipationAmount),
    WTP = case_when(
      ParticipationAmount == 1 ~ 0,
      ParticipationAmount == 2 ~ 30,
      ParticipationAmount == 3 ~ 75,
      ParticipationAmount == 4 ~ 200,
      ParticipationAmount == 5 ~ 400,
      ParticipationAmount == 6 ~ 750,
      ParticipationAmount == 7 ~ 3000,
      ParticipationAmount == 8 ~ 7500,
      ParticipationAmount == 9 ~ 15000,
      TRUE ~ NA_real_
    ),
    WTP_Category = case_when(
      ParticipationAmount == 1 ~ "None",
      ParticipationAmount %in% c(2, 3) ~ "Low",
      ParticipationAmount %in% c(4, 5) ~ "Medium",
      ParticipationAmount %in% c(6, 7) ~ "High",
      ParticipationAmount %in% c(8, 9) ~ "Very high",
      TRUE ~ NA_character_
    ),
    WTP_Category = factor(WTP_Category, levels = c("None", "Low", "Medium", "High", "Very high"))
  )

# Fans vs Non-Fans (again, 1:1)
df_football <- df_football %>%
  mutate(Fan = factor(Fan, levels = c("Non-Fan", "Fan"), labels = c("Non-Fan", "Fan")))

summary_df2 <- df_football %>%
  group_by(Fan) %>%
  summarise(
    mean_wtp = mean(WTP, na.rm = TRUE),
    sd_wtp = sd(WTP, na.rm = TRUE),
    n = n(),
    se = sd_wtp / sqrt(n),
    ci_low = mean_wtp - qt(0.975, n - 1) * se,
    ci_high = mean_wtp + qt(0.975, n - 1) * se
  )

t_test2 <- t.test(WTP ~ Fan, data = df_football)
p_value2 <- t_test2$p.value
stars2 <- dplyr::case_when(
  p_value2 < 0.001 ~ "***",
  p_value2 < 0.01  ~ "**",
  p_value2 < 0.05  ~ "*",
  TRUE ~ "ns"
)

p_wtp2 <- ggplot(summary_df2, aes(x = Fan, y = mean_wtp, fill = Fan)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.15, size = 0.8) +
  annotate("text", x = 1.5, y = max(summary_df2$ci_high) * 1.1,
           label = stars2, size = 7, fontface = "bold") +
  scale_fill_manual(values = wesanderson::wes_palette("Darjeeling1", 2, type = "discrete")) +
  labs(x = "", y = "Estimated contribution (EUR)", title = "", subtitle = "Fans vs. Non-Fans", fill = "Group") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

print(p_wtp2)
save_plot("WTP_Fans_vs_NonFans_2.png", p_wtp2, width = 7, height = 5)

# ---- by Country facet (1:1) ----
df_football <- df_football %>%
  mutate(Country = factor(Country, levels = c(1, 2, 3, 4),
                          labels = c("France", "Ireland", "Sweden", "Azerbaijan")))

summary_country <- df_football %>%
  group_by(Country, Fan) %>%
  summarise(
    mean_wtp = mean(WTP, na.rm = TRUE),
    sd_wtp = sd(WTP, na.rm = TRUE),
    n = n(),
    se = sd_wtp / sqrt(n),
    ci_low = mean_wtp - qt(0.975, n - 1) * se,
    ci_high = mean_wtp + qt(0.975, n - 1) * se,
    .groups = "drop"
  )

p_values <- df_football %>%
  group_by(Country) %>%
  summarise(
    n_total = n(),
    p_value = ifelse(length(unique(Fan)) == 2, t.test(WTP ~ Fan)$p.value, NA_real_),
    .groups = "drop"
  ) %>%
  mutate(
    stars = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      TRUE ~ "ns"
    )
  )

summary_country <- left_join(summary_country, p_values, by = "Country")

label_positions <- summary_country %>%
  group_by(Country) %>%
  summarise(y_pos = max(ci_high, na.rm = TRUE) * 1.1, .groups = "drop")

summary_country <- left_join(summary_country, label_positions, by = "Country")

p_wtp_country <- ggplot(summary_country, aes(x = Fan, y = mean_wtp, fill = Fan)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6, color = "black") +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                position = position_dodge(width = 0.7), width = 0.15, size = 0.8) +
  geom_text(
    data = summary_country %>% distinct(Country, stars, y_pos),
    aes(x = 1.5, y = y_pos, label = stars),
    inherit.aes = FALSE, size = 6, fontface = "bold"
  ) +
  geom_text(
    data = summary_country %>% distinct(Country, n_total),
    aes(x = 1.5, y = -max(summary_country$mean_wtp, na.rm = TRUE) * 0.05,
        label = paste0("N = ", n_total)),
    inherit.aes = FALSE, size = 4
  ) +
  facet_wrap(~ Country, scales = "free_y") +
  scale_fill_manual(values = wesanderson::wes_palette("Darjeeling1", 2, type = "discrete")) +
  labs(
    x = "", y = "Estimated contribution (EUR)",
    title = "Mean Willingness to Participate by Country and Fan Status",
    subtitle = "95% Confidence Intervals and Significance Levels",
    fill = "Group"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    strip.text = element_text(face = "bold", size = 12),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )

print(p_wtp_country)
save_plot("WTP_Fans_vs_NonFans_byCountry_N.png", p_wtp_country, width = 10, height = 6)

# ---- Wilcoxon + WTP category distribution plot (1:1) ----
df_football <- df_football %>%
  mutate(WTP_num = as.numeric(WTP_Category))

wilcox_test <- wilcox.test(WTP_num ~ Fan, data = df_football)
p_value_w <- wilcox_test$p.value

stars_w <- case_when(
  p_value_w < 0.001 ~ "***",
  p_value_w < 0.01  ~ "**",
  p_value_w < 0.05  ~ "*",
  TRUE ~ "ns"
)

save_capture("wilcoxon_wtp_category_fan.txt", wilcox_test)

dist_df <- df_football %>%
  group_by(Fan, WTP_Category) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Fan) %>%
  mutate(share = n / sum(n))

p_wtp_dist <- ggplot(dist_df, aes(x = Fan, y = share, fill = WTP_Category)) +
  geom_bar(stat = "identity", width = 0.7, color = "black") +
  annotate("text", x = 1.5, y = 1.05, label = stars_w, size = 7, fontface = "bold") +
  scale_fill_manual(values = wesanderson::wes_palette("Darjeeling1", 5, type = "discrete")) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Distribution of Contribution Levels by Fan Status",
    subtitle = paste0("Wilcoxon rank-sum test: ", stars_w, " (p = ", formatC(p_value_w, format = "f", digits = 3), ")"),
    x = "", y = "Share of respondents", fill = "Contribution level"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", size = 16),
        axis.text.x = element_text(size = 12))

print(p_wtp_dist)
save_plot("WTP_Category_Distribution_Fans_vs_NonFans.png", p_wtp_dist, width = 7, height = 5)

tab <- df_football %>%
  count(Fan, WTP_Category) %>%
  group_by(Fan) %>%
  mutate(share = n / sum(n) * 100) %>%
  tidyr::pivot_wider(names_from = Fan, values_from = share)

readr::write_csv(tab, file.path(paths$output_tables, "wtp_category_shares_by_fan.csv"))

chi <- chisq.test(table(df_football$Fan, df_football$WTP_Category))
save_capture("chisq_wtp_category_fan.txt", chi)

# ---------------------------------------------------------------------------
# 11. “Summary Tables using tableone” block (reads CSV) — make path portable + save tables
# ---------------------------------------------------------------------------
# Your script reads: greenfoot_data_ERSS_resubmission.csv
# Put that file in data/processed/ and load from there.
csv2 <- file.path(paths$data_processed, "greenfoot_data_ERSS_resubmission.csv")
if (file.exists(csv2)) {
  
  df_football_csv <- readr::read_csv(csv2, show_col_types = FALSE)
  
  df_football_csv <- df_football_csv %>%
    mutate(
      fan_yes = factor(fan_yes, levels = c(0, 1), labels = c("Non-Fan", "Fan")),
      WTP_Category = factor(WTP_Category, levels = c("None", "Low", "Medium", "High", "Very high"))
    )
  
  vars_ltp <- c("LTP_FOOTBALL")
  ltp_table <- tableone::CreateTableOne(vars = vars_ltp, strata = "fan_yes", data = df_football_csv, test = FALSE)
  save_capture("tableone_ltp_by_fan.txt", print(ltp_table, test = FALSE))
  
  df_football_csv$WTP_f <- as.factor(df_football_csv$WTP)
  vars_wtp_all <- c("WTP_f")
  wtp_table_all <- tableone::CreateTableOne(vars = vars_wtp_all, strata = "fan_yes", data = df_football_csv, test = FALSE)
  save_capture("tableone_wtp_by_fan.txt", print(wtp_table_all, test = FALSE))
  
  # Recode LTP labels (1:1) and plot mean WTP by LTP and fan_yes
  df_football_csv$LTP_FOOTBALL2 <- factor(
    df_football_csv$LTP_FOOTBALL,
    levels = c(
      "Very unlikely to participate",
      "Unlikely to participate",
      "Neither likely nor unlikely",
      "Likely to participate",
      "Very likely to participate"
    ),
    labels = c("Very unlikely", "Unlikely", "Indifferent", "Likely", "Very likely"),
    ordered = TRUE
  )
  
  mean_wtp <- df_football_csv %>%
    group_by(fan_yes, LTP_FOOTBALL2) %>%
    summarise(
      mean_WTP = mean(WTP, na.rm = TRUE),
      sd_WTP   = sd(WTP, na.rm = TRUE),
      n        = n(),
      se_WTP   = sd_WTP / sqrt(n),
      .groups  = "drop"
    ) %>%
    group_by(fan_yes) %>%
    mutate(share = round(100 * n / sum(n), 1)) %>%
    ungroup()
  
  p_ltp_wtp_bar <- ggplot(mean_wtp, aes(x = LTP_FOOTBALL2, y = mean_WTP, fill = fan_yes)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    geom_errorbar(
      aes(ymin = mean_WTP - se_WTP, ymax = mean_WTP + se_WTP),
      position = position_dodge(width = 0.8), width = 0.2
    ) +
    geom_text(
      aes(label = paste0(n, "N")),
      position = position_dodge(width = 0.8),
      vjust = -0.3,
      size = 3
    ) +
    scale_fill_manual(values = wesanderson::wes_palette("Darjeeling1", 2, type = "discrete")) +
    labs(
      title = "Mean Willingness to Pay (WTP) by Likelihood to Participate (LTP)",
      x = "Likelihood to Participate (LTP)",
      y = "Mean Willingness to Pay (EUR)",
      fill = "Fan Status"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "top",
      legend.title = element_text(face = "bold"),
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.text.x = element_text(angle = 30, hjust = 1)
    )
  
  print(p_ltp_wtp_bar)
  save_plot("LTP_Fans_vs_NonFans_WTP.png", p_ltp_wtp_bar, width = 7, height = 5)
  
  # LTP distribution plot (1:1)
  ltp_probs <- df_football_csv %>%
    group_by(fan_yes, LTP_FOOTBALL2) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(fan_yes) %>%
    mutate(prob = n / sum(n) * 100) %>%
    ungroup()
  
  p_ltp_dist <- ggplot(ltp_probs, aes(x = LTP_FOOTBALL2, y = prob, fill = fan_yes)) +
    geom_col(position = "dodge", width = 0.7) +
    geom_text(aes(label = paste0(round(prob, 1), "%")),
              position = position_dodge(width = 0.7),
              vjust = -0.3, size = 3) +
    scale_fill_manual(values = wesanderson::wes_palette("Darjeeling1", 2)) +
    labs(title = "", x = "LTP", y = "Share within Group (%)", fill = "Fan Status") +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.text.x = element_text(angle = 30, hjust = 1)
    )
  
  print(p_ltp_dist)
  save_plot("LTP_distribution_by_fan.png", p_ltp_dist, width = 7, height = 5)
  
  # WTP probs plot (1:1)
  wtp_probs <- df_football_csv %>%
    group_by(fan_yes, WTP_Category) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(fan_yes) %>%
    mutate(prob = n / sum(n) * 100) %>%
    ungroup()
  
  p_wtp_probs <- ggplot(wtp_probs, aes(x = WTP_Category, y = prob, fill = fan_yes)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6) +
    geom_text(
      aes(label = paste0(round(prob, 1), "%")),
      position = position_dodge(width = 0.7),
      vjust = -0.3,
      size = 3
    ) +
    scale_fill_manual(values = wesanderson::wes_palette("Darjeeling1", 2)) +
    labs(title = "", x = "WTP Category", y = "Share within Group (%)", fill = "Fan Status") +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.text.x = element_text(angle = 30, hjust = 1)
    )
  
  print(p_wtp_probs)
  save_plot("WTP_probs_by_fan.png", p_wtp_probs, width = 7, height = 5)
  
} else {
  warning("File not found: data/processed/greenfoot_data_ERSS_resubmission.csv. Skipping that block.")
}

message("02_descriptives_figures.R completed successfully.")
