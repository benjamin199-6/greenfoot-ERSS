###############################################################################
# 01_load_data.R
#
# Purpose:
# Load raw survey data and apply variable renaming
# (as in the submitted paper code).
###############################################################################

message("Running 01_load_data.R ...")

# ---------------------------------------------------------------------------
# 1. Define raw data file
# ---------------------------------------------------------------------------
raw_file <- file.path(paths$data_raw, "Greenfoot_survey_rawdata.sav")

if (!file.exists(raw_file)) {
  stop("Raw data file not found: ", raw_file,
       "\nExpected location: data/raw/Greenfoot_survey_rawdata.sav")
}

# ---------------------------------------------------------------------------
# 2. Load raw data (SPSS .sav) into object 'data'
# ---------------------------------------------------------------------------
data <- haven::read_sav(raw_file)
sjPlot::view_df(data)
# ---------------------------------------------------------------------------
# 3. Rename variables (1:1 from your submitted script)
# ---------------------------------------------------------------------------
data <- data %>%
  rename(
    ID = IDSEED,
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
    InterestOptionLottery = Q6.4B,
    LTP_national_campaing= Q6.2
  )

# ---------------------------------------------------------------------------
# 4. Exclude other / prefer not to answer for gender (1:1)
# ---------------------------------------------------------------------------
data_full=data
data <- data %>% filter(Gender %in% c(1, 2))

# ---------------------------------------------------------------------------
# 5. Quick sanity output
# ---------------------------------------------------------------------------
message("Rows: ", nrow(data), " | Cols: ", ncol(data))
message("01_load_data.R completed successfully.")
