# Importing datasets --------------------------------------------------------- #

load(
  "Data_2018\\Raw_data_RData\\EFAD_Normalized.RData"
)
load(
  "Data_2018\\Raw_data_RData\\ELAD_Normalized.RData"
)
load(
  "Data_2018\\Raw_data_RData\\PIAD_Normalized.RData"
)

# Filtering datasetes --------------------------------------------------------- #

## Use of Nutrient nitrogen N per area of cropland
UNNC <- EFAD_Normalized |>
  dplyr::filter(
    Year == 2018,
    Element == "Use per area of cropland",
    Item == "Nutrient nitrogen N (total)"
  ) |>
  dplyr::select(
    c(3, 5, 7, 11)
  ) |>
  dplyr::rename(
    'UNNC' = Value
  ) |>
  dplyr::select(
    1,
    4
  )

## Use of cropland area per capita
UCAC <- ELAD_Normalized |>
  dplyr::filter(
    Year == 2018,
    Element == "Area per capita"
  ) |>
  dplyr::select(
    c(3, 5, 7, 11)
  ) |>
  dplyr::rename(
    'UCAC' = Value
  ) |>
  dplyr::select(
    c(1, 4)
  )

## Gross per Capita Crop Production Index
GCCP <- PIAD_Normalized |>
  dplyr::filter(
    Year == 2018,
    Item == "Crops",
    Element == "Gross per capita Production Index Number (2014-2016 = 100)"
  ) |>
  dplyr::select(
    c(3, 6, 8, 12)
  ) |>
  dplyr::rename(
    'GCCP' = Value
  ) |>
  dplyr::select(
    1,
    4
  )

# Joining all variables in a dataset ----------------------------------------- #

Agriculture_Data <- UNNC |>
  dplyr::full_join(
    UCAC,
    by = "Area"
  ) |>
  dplyr::full_join(
    GCCP,
    by = "Area"
  ) |>
  dplyr::filter_all(
    dplyr::all_vars(
      !is.na(.)
    )
  ) |>
  dplyr::rename(
    country = Area
  )

# Saving the filtered dataset ------------------------------------------------ #

# write.csv(Agriculture_Data, "Data_2018\\Raw_data_csv\\Agriculture_Data.csv", row.names = F)

# or

# save(Agriculture_Data, file = "Data_2018\\Raw_data_RData\\Agriculture_Data.RData")
