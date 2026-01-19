### Replicaton package Crowdfunding paper 




# Modell wie in stata
library(haven)
library(sjPlot)
library(sjlabelled)
library(tidyverse)
library(fastDummies)


### Define working directory 
setwd("/Users/benjamin/Library/CloudStorage/OneDrive-UniversitätWien/PhD/Paper/Crowdfundign_/R")

data <- read_sav("Greenfoot_survey_rawdata.sav")

view_df(data)
data <- data %>%
  rename(
    SeedID = IDSEED,
    Country = PAYS,
    Language = LANGUE,
    AgeGroup = Q1.1,
    Gender = Q1.2,
    EmploymentStatus = Q1.3,
    RelationshipStatus = Q1.4,
    HasChildren = Q1.5,
    AnnualNetIncome = Q1.6,
    InterestInSports = Q1.7,
    InterestInFootball = Q1.8,
    FootballSupportFrequency = Q1.9,
    FavouriteFootballTeam = Q1.10,
    ClimateChangeConcern = Q2.1,
    EnvConcernEnergyChoices = Q2.2,
    ReducedElectricityUse = Q2.3_1,
    SustainableTravel = Q2.3_2,
    LessIntensiveDiet = Q2.3_3,
    LifestyleChanges = Q2.3_4,
    NoEmissionReduction = Q2.3_5,
    UnsureEmissionReduction = Q2.3_6,
    HomeOwnership = Q2.4,
    HomeRenovationsEE = Q2.5,
    LEDLighting = Q2.6_1,
    ApplianceUpgrade = Q2.6_2,
    HVACUpgrade = Q2.6_3,
    WaterHeaterUpgrade = Q2.6_4,
    SunShades = Q2.6_5,
    SolarPower = Q2.6_6,
    Insulation = Q2.6_7,
    OtherEEMeasures = Q2.6_8,
    UnsureEEMeasures = Q2.6_9,
    CrowdfundingFamiliarity = Q3.1,
    SavingsAccount = Q3.2_1,
    StocksBonds = Q3.2_2,
    NoSavings = Q3.2_3,
    UnsureSavings = Q3.2_4,
    CrowdfundingReturnPreference = Q3.3,
    FinancialBenefitType = Q3.4,
    MainParticipationReason = Q3.5,
    SecondReason = Q3.6,
    LeastImportantReason = Q3.7,
    PreferredEnergyProjectRenewable = Q3.8_1,
    PreferredEnergyProjectRefurbishment = Q3.8_2,
    EVChargingStations = Q3.8_3,
    EnergyEfficientLighting = Q3.8_4,
    SmartBuildingTech = Q3.8_5,
    HVACRenewal = Q3.8_6,
    NoPreferenceProject = Q3.8_7,
    CrowdfundingLikelihoodSchool = Q4.1,
    MainReasonParticipateSchool = Q4.2,
    MainReasonNotParticipateSchool = Q4.3,
    MoreLikelyCarbonReduction = Q4.4,
    MoreLikelyLocalSupport = Q4.5,
    CrowdfundingLikelihoodFootball = Q5.1,
    MainReasonParticipateFootball = Q5.2,
    MainReasonNotParticipateFootball = Q5.3,
    MoreLikelyCarbonReductionFootball = Q5.4,
    MoreLikelyLocalSupportFootball = Q5.5,
    MoreLikelyClubCompetitiveness = Q5.6,
    SupportIndividualAthletes = Q5.7_1,
    SupportAmateurClub = Q5.7_2,
    SupportSemiProClub = Q5.7_3,
    SupportMajorProClub = Q5.7_4,
    SupportNationalTeam = Q5.7_5,
    SupportUnknownOther = Q5.7_6,
    RewardSpecialTShirt = Q5.8_1,
    RewardSpecialJersey = Q5.8_2,
    RewardAutographedMerchandise = Q5.8_3,
    RewardMatchTickets = Q5.8_4,
    RewardGreenClubMembership = Q5.8_5,
    RewardStadiumTour = Q5.8_6,
    RewardMeetPlayer = Q5.8_7,
    RewardOther = Q5.8_8,
    RewardUnsure = Q5.8_9,
    OtherRewardText = Q5.8AUTRE,
    PreferredPaybackOption = Q6.1,
    WouldJoinWithPreferredReward = Q6.2,
    ParticipationAmount = Q6.3,
    InterestOptionPlain = Q6.4A,
    InterestOptionLottery = Q6.4B
  )


data <- data %>%
  mutate(
    # Country, coded as a factor with labels
    Country = factor(Country, levels = 1:4, labels = c("France", "Ireland", "Sweden", "Azerbaijan")),
    
    # Language, coded as a factor with labels
    Language = factor(Language, levels = 1:4, labels = c("French", "English", "Swedish", "Azerbaijani")),
    
    # Age Group, as an ordered factor to preserve ordinal nature
    AgeGroup = factor(AgeGroup, levels = 1:7, labels = c("18 - 24 years", "25 - 34 years", "35 - 44 years", "45 - 54 years", "55 - 64 years", "65 - 74 years", "75 years and older"), ordered = TRUE),
    
    # Gender, with an option for 'Other / prefer not to answer'
    Gender = factor(Gender, levels = 1:3, labels = c("Female", "Male", "Other / prefer not to answer")),
    
    # Employment Status, as a categorical variable
    EmploymentStatus = factor(EmploymentStatus, levels = 1:7, labels = c("Full time", "Part time", "Self-employed", "Retired/pensioned", "Unemployed", "Student", "Other")),
    
    # Relationship Status, as a categorical variable
    RelationshipStatus = factor(RelationshipStatus, levels = 1:5, labels = c("Married / Partnership", "Single", "Widow", "Divorced or Separated", "Other / Prefer not to answer")),
    
    # Children presence as a binary categorical variable
    HasChildren = factor(HasChildren, levels = 1:2, labels = c("Yes", "No")),
    
    # Annual Net Household Income, as an ordered factor
    AnnualNetIncome = factor(AnnualNetIncome, levels = 1:4, labels = c("Under 30 000 €", "Between 30 000 and 40 000 €", "Between 40 000 and 55 000 €", "More than 55 000 €"), ordered = TRUE),
    
    # Interest in Sports, as an ordinal scale
    InterestInSports = factor(InterestInSports, levels = 1:5, labels = c("Not at all interested", "Not interested", "A little bit interested", "Interested", "Very interested"), ordered = TRUE),
    
    # Interest in Football, as an ordinal scale
    InterestInFootball = factor(InterestInFootball, levels = 1:5, labels = c("Not at all interested", "Not interested", "A little bit interested", "Interested", "Very interested"), ordered = TRUE),
    
    # Frequency of Financial Support to Football, as an ordinal scale
    FootballSupportFrequency = factor(FootballSupportFrequency, levels = 1:5, labels = c("Very often", "Often", "Sometimes", "Rarely", "Never"), ordered = TRUE),
    
    # Binary questions regarding environmental behavior changes, coded directly as factors
    ReducedElectricityUse = factor(ReducedElectricityUse, levels = 0:1, labels = c("No", "Yes")),
    SustainableTravel = factor(SustainableTravel, levels = 0:1, labels = c("No", "Yes")),
    LessIntensiveDiet = factor(LessIntensiveDiet, levels = 0:1, labels = c("No", "Yes")),
    LifestyleChanges = factor(LifestyleChanges, levels = 0:1, labels = c("No", "Yes")),
    NoEmissionReduction = factor(NoEmissionReduction, levels = 0:1, labels = c("No", "Yes")),
    UnsureEmissionReduction = factor(UnsureEmissionReduction, levels = 0:1, labels = c("No", "Yes")),
    
    # Crowdfunding familiarity, with ordinal levels
    CrowdfundingFamiliarity = factor(CrowdfundingFamiliarity, levels = 1:4, labels = c("Participated before", "Heard but not participated", "Not familiar", "Unsure"), ordered = TRUE),
    
    # Binary questions related to savings and investments
    SavingsAccount = factor(SavingsAccount, levels = 0:1, labels = c("No", "Yes")),
    StocksBonds = factor(StocksBonds, levels = 0:1, labels = c("No", "Yes")),
    NoSavings = factor(NoSavings, levels = 0:1, labels = c("No", "Yes")),
    UnsureSavings = factor(UnsureSavings, levels = 0:1, labels = c("No", "Yes")),
    
    # Preferences for crowdfunding returns
    CrowdfundingReturnPreference = factor(CrowdfundingReturnPreference, levels = 1:7, labels = c("Financial return", "Reward", "Financial return & decision making", "Nothing", "Would not participate", "Mix of returns", "Don't know"), ordered = TRUE),
    FinancialBenefitType = factor(FinancialBenefitType, levels = 1:5, labels = c("Variable interest rate", "Fixed interest rate", "Electricity bill discount", "Not interested in returns", "Don't know")),
    
    # Reasons for participation, using ordinal scales where appropriate
    MainParticipationReason = factor(MainParticipationReason, levels = 1:7, ordered = TRUE),
    SecondReason = factor(SecondReason, levels = 1:7, ordered = TRUE),
    LeastImportantReason = factor(LeastImportantReason, levels = 1:7, ordered = TRUE),
    
    # Preferences for types of energy projects, converted to binary factors
    PreferredEnergyProjectRenewable = factor(PreferredEnergyProjectRenewable, levels = 0:1, labels = c("No", "Yes")),
    PreferredEnergyProjectRefurbishment = factor(PreferredEnergyProjectRefurbishment, levels = 0:1, labels = c("No", "Yes")),
    EVChargingStations = factor(EVChargingStations, levels = 0:1, labels = c("No", "Yes")),
    EnergyEfficientLighting = factor(EnergyEfficientLighting, levels = 0:1, labels = c("No", "Yes")),
    SmartBuildingTech = factor(SmartBuildingTech, levels = 0:1, labels = c("No", "Yes")),
    HVACRenewal = factor(HVACRenewal, levels = 0:1, labels = c("No", "Yes")),
    NoPreferenceProject = factor(NoPreferenceProject, levels = 0:1, labels = c("No", "Yes")),
    
    # Likelihood of participation in crowdfunding campaigns
    CrowdfundingLikelihoodSchool = factor(CrowdfundingLikelihoodSchool, levels = 1:5, ordered = TRUE),
    CrowdfundingLikelihoodFootball = factor(CrowdfundingLikelihoodFootball, levels = 1:5, ordered = TRUE),
    
    # Reasons for participating or not in the campaigns
    MainReasonParticipateSchool = factor(MainReasonParticipateSchool, levels = 1:5),
    MainReasonNotParticipateSchool = factor(MainReasonNotParticipateSchool, levels = 1:5),
    MoreLikelyCarbonReduction = factor(MoreLikelyCarbonReduction, levels = 1:5, ordered = TRUE),
    MoreLikelyLocalSupport = factor(MoreLikelyLocalSupport, levels = 1:5, ordered = TRUE),
    MainReasonParticipateFootball = factor(MainReasonParticipateFootball, levels = 1:6),
    MainReasonNotParticipateFootball = factor(MainReasonNotParticipateFootball, levels = 1:5),
    MoreLikelyCarbonReductionFootball = factor(MoreLikelyCarbonReductionFootball, levels = 1:5, ordered = TRUE),
    MoreLikelyLocalSupportFootball = factor(MoreLikelyLocalSupportFootball, levels = 1:5, ordered = TRUE),
    MoreLikelyClubCompetitiveness = factor(MoreLikelyClubCompetitiveness, levels = 1:5, ordered = TRUE),
    
    # Support for various groups
    SupportIndividualAthletes = factor(SupportIndividualAthletes, levels = 0:1, labels = c("No", "Yes")),
    SupportAmateurClub = factor(SupportAmateurClub, levels = 0:1, labels = c("No", "Yes")),
    SupportSemiProClub = factor(SupportSemiProClub, levels = 0:1, labels = c("No", "Yes")),
    SupportMajorProClub = factor(SupportMajorProClub, levels = 0:1, labels = c("No", "Yes")),
    SupportNationalTeam = factor(SupportNationalTeam, levels = 0:1, labels = c("No", "Yes")),
    SupportUnknownOther = factor(SupportUnknownOther, levels = 0:1, labels = c("No", "Yes")),
    
    # Types of rewards for crowdfunding participation
    RewardSpecialTShirt = factor(RewardSpecialTShirt, levels = 0:1, labels = c("No", "Yes")),
    RewardSpecialJersey = factor(RewardSpecialJersey, levels = 0:1, labels = c("No", "Yes")),
    RewardAutographedMerchandise = factor(RewardAutographedMerchandise, levels = 0:1, labels = c("No", "Yes")),
    RewardMatchTickets = factor(RewardMatchTickets, levels = 0:1, labels = c("No", "Yes")),
    RewardGreenClubMembership = factor(RewardGreenClubMembership, levels = 0:1, labels = c("No", "Yes")),
    RewardStadiumTour = factor(RewardStadiumTour, levels = 0:1, labels = c("No", "Yes")),
    RewardMeetPlayer = factor(RewardMeetPlayer, levels = 0:1, labels = c("No", "Yes")),
    RewardOther = factor(RewardOther, levels = 0:1, labels = c("No", "Yes"))
  )

names(data)
table(data$`filter_$`)
table(data$CHOIX)
table(data$CrowdfundingLikelihoodFootball)

data <- data %>%
  mutate(LTP = coalesce(CrowdfundingLikelihoodFootball, CrowdfundingLikelihoodSchool),
         Contion = factor(ifel]
))


data <- data %>%
  mutate(
    Condition = ifelse(!is.na(CrowdfundingLikelihoodFootball), 1, 0),
    Condition = factor(Condition, levels = c(0, 1), labels = c("Public Building", "Football"))
  )




model_data <- data %>%
  mutate(
    EnvironmentalAttitude = as.numeric(ClimateChangeConcern),  # or any other variable that best represents environmental attitude
    Attitude = as.numeric(InterestInFootball),  # Assuming this reflects general attitude towards crowdfunding in the context of football
    SubjectiveNorm = as.numeric(InterestInSports),  # Example for subjective norm
    PerceivedControl = as.numeric(CrowdfundingFamiliarity))  # Individual's perception of their ability to participate  )

model_data$Condition
str(data$LTP)

model_data$LTP=as.numeric(model_data$LTP)


tpb_model <- lm(LTP ~ Condition*Attitude+ PerceivedControl, data = model_data)
tab_model(tpb_model)



library(lavaan)

# Define the SEM model
# Note: Adjust the observed variable names (e.g., Attitude1, SN1, PC1) to match your dataset
sem_model <- '
  # Define latent variables
  Attitude =~ ClimateChangeConcern
  SubjectiveNorm =~ InterestInSports
  PerceivedControl =~ CrowdfundingFamiliarity

  # Regression paths
  LTP ~ Attitude + SubjectiveNorm + PerceivedControl  + Condition
  LTP ~ Age + Income + EducationLevel
  
  # Covariances among latent variables
  Attitude ~~ SubjectiveNorm + PerceivedControl + AttitudeTowardsEnvironment
  SubjectiveNorm ~~ PerceivedControl + AttitudeTowardsEnvironment
  PerceivedControl ~~ AttitudeTowardsEnvironment
'

# Fit the SEM model
# Make sure 'model_data' contains all the variables used in the 'sem_model' string
fit <- sem(model = sem_model, data = model_data, model.type = "sem")

# Summary of the fit
summary(fit, standardized = TRUE, fit.measures = TRUE)

