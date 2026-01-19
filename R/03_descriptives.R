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
    ParticipationAmount,LTP_national_campaing
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

royal_palette <- wes_palette("Royal1", n = 4, type = "discrete")  # Ensuring discrete colors for categorical data

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

# Save
ggsave(
  filename = file.path(paths$output_figures, "Figure4_climate_concern.png"),
  plot = plot_env,
  width = 7,
  height = 5,
  dpi = 300,
  units = "in",
  bg = "white"
)



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


########################################## Plot for appendix #############

ci_data_country <- df %>%
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
    Country_label = factor(
      case_when(
        Country == 1 ~ "France",
        Country == 2 ~ "Ireland",
        Country == 3 ~ "Sweden",
        Country == 4 ~ "Azerbaijan",
        TRUE ~ as.character(Country)
      ),
      levels = c("France", "Ireland", "Sweden", "Azerbaijan")
    )
  )

# Convert Fan to factor (as you did)
ci_data_country$Fan <- as.factor(ci_data_country$Fan)

# Plot
plot_ci <- ggplot(ci_data_country, aes(x = Fan, y = percent, fill = Building_label)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                position = position_dodge(width = 0.9), width = 0.25) +
  geom_text(aes(label = paste0(round(percent, 1), "%"),
                y = percent + 2),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5) +
  labs(title = "",
       x = "Fan", y = "", fill = "Building") +
  scale_fill_manual(values = c("Football" = "black", "Neutral" = "grey"),
                    labels = c("Football", "Neutral")) +
  facet_grid(rows = vars(Country_label)) +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# Display the plot
print(plot_ci)

# Save to paper/figures
ggsave(
  filename = file.path(paths$output_figures, "A9_fan_building_by_country.png"),
  plot = plot_ci,
  width = 10,
  height = 8,
  dpi = 300,
  units = "in",
  bg = "white"
)


# ---------------------------------------------------------------------------
# 8. Marginal effects bar plot (data entered manually in your script)
# ---------------------------------------------------------------------------
##Note the effects come from the STATA Model Outcome.... see analyis file and Do-file!

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


ggsave(
  filename = file.path(paths$output_figures, "Figure6_marginal_effects_ltp.png"),
  plot = p_me,
  width = 10,
  height = 8,
  dpi = 300,
  units = "in",
  bg = "white"
)



# ---------------------------------------------------------------------------
# 5. ANOVA output (save to tables)
# ---------------------------------------------------------------------------

anova_fan <- aov(ClimateChangeConcern ~ Group, data = df)
summary(anova_fan)
TukeyHSD(anova_fan)
#save_capture("anova_climateconcern_by_group.txt", summary(anova_fan))
#save_capture("tukey_climateconcern_by_group.txt", TukeyHSD(anova_fan))

# ---------------------------------------------------------------------------
# 7. TableOne: Age/Gender/Income by Country (save to tables)
# ---------------------------------------------------------------------------
vars <- c("AgeGroup", "Gender", "AnnualNetIncome")
factorVars <- c("AgeGroup", "Gender", "AnnualNetIncome")

table_one <- tableone::CreateTableOne(vars = vars, strata = "Country", data = df, factorVars = factorVars)
table_one
save_capture("Table_1_tableone_by_country.txt", print(table_one, test = FALSE))


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
save_capture("Table_A7_balance_table_by_group.txt", print(balance_table1, smd = TRUE, test = FALSE))

balance_vars2 <- c(
  "AgeGroup", "Gender", "EmploymentStatus", "AnnualNetIncome", "EnvConcernEnergyChoices",
  "ReducedElectricityUse", "SustainableTravel", "LessIntensiveDiet",
  "CrowdfundingReturnPreference", "MainParticipationReason"
)

factor_vars2 <- c("AgeGroup", "Gender", "EmploymentStatus", "AnnualNetIncome",
                  "CrowdfundingReturnPreference", "MainParticipationReason")

balance_table2 <- tableone::CreateTableOne(vars = balance_vars2, strata = "Treatment",
                                           data = df, factorVars = factor_vars2)
save_capture("Table_A7_balance_table_by_group.txt", print(balance_table2, smd = TRUE, test = TRUE))



###### WTP 


table(df$CrowdfundingLikelihoodFootball, useNA = "ifany")


df_football <- df %>%
  filter(CHOIX == 2) %>%
  mutate(
    # fan indicator (match your later convention 0/1 if needed)
    fan_yes = ifelse(Fan %in% c(1, "1", "Fan"), 1, 0),
    
    # country (numeric codes -> labels)
    country = factor(
      Country,
      levels = c(1, 2, 3, 4),
      labels = c("France", "Ireland", "Sweden", "Azerbaijan")
    ),
    
    # WTP (from ParticipationAmount; same mapping you used)
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
    WTP_Category = factor(WTP_Category,
                          levels = c("None", "Low", "Medium", "High", "Very high")),
    
    # LTP (use the football one when available, otherwise coalesce)
    LTP_FOOTBALL = LTP_national_campaing,
    LTP_FOOTBALL2 = factor(
      LTP_FOOTBALL,
      levels = c(1, 2, 3, 4, 5),
      labels = c("Very unlikely", "Unlikely", "Indifferent", "Likely", "Very likely"),
      ordered = TRUE
    )
  )
df_football$fan_yes <- factor(df_football$fan_yes, levels = c(0, 1), labels = c("Non-Fan", "Fan"))


# --- 4. Create TableOne for WTP ---

vars_wtp <- c("WTP_Category")

wtp_table <- CreateTableOne(
  vars = vars_wtp,
  strata = "fan_yes",
  data = df_football,
  test = FALSE
)

# Save printed output
capture.output(
  print(wtp_table, showAllLevels = TRUE, quote = FALSE, noSpaces = TRUE),
  file = file.path(paths$output_tables, "Table_3_WTP_by_Fan.txt")
)


df_football$LTP_FOOTBALL2 <- factor(
  df_football$LTP_FOOTBALL,
  levels = c(1, 2, 3, 4, 5),
  labels = c(
    "Very unlikely",
    "Unlikely",
    "Indifferent",
    "Likely",
    "Very likely"
  ),
  ordered = TRUE
)


#table(df_football$ParticipationAmount, useNA = "ifany")
#table(df_football$WTP_Category, useNA = "ifany")



# Summarize mean WTP and shares per fan group × LTP category
mean_wtp <- df_football %>%
  group_by(fan_yes, LTP_FOOTBALL2) %>%
  summarise(
    mean_WTP = mean(WTP, na.rm = TRUE),
    sd_WTP   = sd(WTP, na.rm = TRUE),
    n        = n(),
    se_WTP   = sd_WTP / sqrt(n),
    .groups  = "drop"
  ) %>%
  group_by(fan_yes) %>%
  mutate(
    share = round(100 * n / sum(n), 1)  # % within each fan group
  ) %>%
  ungroup()






# Compute share of each LTP category within each fan group
ltp_probs <- df_football %>%
  group_by(fan_yes, LTP_FOOTBALL2) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(fan_yes) %>%
  mutate(prob = n / sum(n) * 100) %>%
  ungroup()

# Plot the distribution (stacked or side-by-side)
LTP2=ggplot(ltp_probs, aes(x = LTP_FOOTBALL2, y = prob, fill = fan_yes)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0(round(prob, 1), "%")),
            position = position_dodge(width = 0.7),
            vjust = -0.3, size = 3) +
  scale_fill_manual(values = wesanderson::wes_palette("Darjeeling1", 2)) +
  labs(
    title = "",
    x = "LTP",
    y = "Share within Group (%)",
    fill = "Fan Status"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )






ggsave(
  filename = file.path(paths$output_figures, "Fig7_LTP_Fans_vs_NonFans_WTP.png"),
  plot = LTP2,
  width = 10,
  height = 8,
  dpi = 300,
  units = "in",
  bg = "white"
)





# Compute share of each WTP category within fan/non-fan group
wtp_probs <- df_football %>%
  group_by(fan_yes, WTP_Category) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(fan_yes) %>%
  mutate(prob = n / sum(n) * 100) %>%
  ungroup()

# Plot
wtp=ggplot(wtp_probs, aes(x = WTP_Category, y = prob, fill = fan_yes)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(
    aes(label = paste0(round(prob, 1), "%")),
    position = position_dodge(width = 0.7),
    vjust = -0.3,
    size = 3
  ) +
  scale_fill_manual(values = wesanderson::wes_palette("Darjeeling1", 2)) +
  labs(
    title = "",
    x = "WTP Category",
    y = "Share within Group (%)",
    fill = "Fan Status"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )



ggsave(
  filename = file.path(paths$output_figures, "Figure8_WTP_Fans_vs_NonFans.png"),
  plot = wtp,
  width = 10,
  height = 8,
  dpi = 300,
  units = "in",
  bg = "white"
)

CreateTableOne(vars = c("LTP_FOOTBALL2", "WTP", "WTP_Category"),
               strata = "fan_yes", data = df_football)

CreateTableOne(vars = c("WTP", "fan_yes"), strata = "LTP_FOOTBALL2", data = df_football)






message("02_descriptives_figures.R completed successfully.")
