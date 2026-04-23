# Importing datasets --------------------------------------------------------- #

load(
  "Data_2018\\Raw_data_RData\\CO2_Data.RData"
)

load(
  "Data_2018\\Raw_data_RData\\Agriculture_Data.RData"
)

load(
  "Data_2018\\Raw_data_RData\\Country_Indicators.RData"
)

# Filtering datasets --------------------------------------------------------- #

## Co2 variable
Co2 <- Co2_Data |>
  dplyr::filter(
    year == 2018
  ) |>
  dplyr::select(
    country,
    co2_per_capita
  ) |>
  dplyr::filter_all(
    dplyr::all_vars(!is.na(.))
  ) |>
  dplyr::rename(
    CO2 = co2_per_capita
  )

## Oil rents, Daily gas consumption per capita and Daily oil consumption per capita
CI <- Country_indicators |>
  dplyr::select(
    c(3, 12, 18, 19)
  ) |>
  dplyr::rename(
    OR = `Oil rents (% of GDP) [NY.GDP.PETR.RT.ZS]`,
    DGCC = `Per capita daily gas consumption (thousand cubic feet per capita per day) [CC.EG.CONS.GAS.PC]`,
    DOCC = `Per capita daily oil consumption (barrels per capita per day) [CC.EG.CONS.OIL.PC]`,
    country = `Country Name`
  ) |>
  dplyr::mutate(
    OR = as.numeric(OR),
    DGCC = as.numeric(DGCC),
    DOCC = as.numeric(DOCC)
  ) |>
  dplyr::filter_all(
    dplyr::all_vars(!is.na(.))
  )

# Joining all variables in a dataset ----------------------------------------- #

Models_Data <- Co2 |>
  dplyr::full_join(
    Agriculture_Data,
    by = "country"
  ) |>
  dplyr::full_join(
    CI,
    by = "country"
  ) |>
  na.omit() |>
  dplyr::slice(
    # Not a country
    -91
  ) |>
  dplyr::mutate(
    DOCC = (DOCC - mean(DOCC)) / sd(DOCC)
  )

# Saving the filtered dataset ------------------------------------------------ #

write.csv(Models_Data, "Data_2018\\data_2018.csv", row.names = F)

# or

save(Models_Data, file = "Data_2018\\data_2018.RData")
