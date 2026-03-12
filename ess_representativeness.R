library(haven)
library(dplyr)

DATA_DIR <- "."
ESS_DIR <- file.path(DATA_DIR, "ESS_2018-19")

cover <- read_dta(file.path(ESS_DIR, "sect_cover_hh_w4.dta"))
ess <- cover %>%
  mutate(
    pw_w4 = as.numeric(pw_w4),
    sector = case_when(
      as.numeric(saq14) == 1 ~ "Rural",
      as.numeric(saq14) == 2 ~ "Urban",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(pw_w4), pw_w4 > 0) %>%
  select(household_id, pw_w4, sector)

phone <- read_dta(file.path(ESS_DIR, "sect11b2_hh_w4.dta"))
hh_phone <- unique(phone$household_id)
ess <- ess %>% mutate(in_phone_frame = household_id %in% hh_phone)

geo <- read_dta(file.path(ESS_DIR, "ETH_HouseholdGeovariables_Y4.dta")) %>%
  select(household_id, dist_market)
ess <- ess %>% left_join(geo, by = "household_id")

assets <- read_dta(file.path(ESS_DIR, "sect11_hh_w4.dta")) %>%
  mutate(acd = as.numeric(asset_cd), n_own = as.numeric(s11q01)) %>%
  filter(!is.na(n_own), n_own > 0) %>%
  group_by(household_id) %>%
  summarise(
    own_radio = any(acd == 8),
    own_tv = any(acd == 9),
    own_bicycle = any(acd == 13),
    .groups = "drop"
  )

ess <- ess %>%
  left_join(assets, by = "household_id") %>%
  mutate(
    own_radio = if_else(is.na(own_radio), FALSE, own_radio),
    own_tv = if_else(is.na(own_tv), FALSE, own_tv),
    own_bicycle = if_else(is.na(own_bicycle), FALSE, own_bicycle)
  )

frame <- ess %>% filter(in_phone_frame)
excl <- ess %>% filter(!in_phone_frame)

wmean <- function(x, w) {
  keep <- !is.na(x) & !is.na(w) & w > 0
  sum(x[keep] * w[keep]) / sum(w[keep])
}

wpct <- function(x, w) wmean(as.numeric(x), w) * 100

urban_f <- wpct(frame$sector == "Urban", frame$pw_w4)
urban_e <- wpct(excl$sector == "Urban", excl$pw_w4)

rural_f <- wpct(frame$sector == "Rural", frame$pw_w4)
rural_e <- wpct(excl$sector == "Rural", excl$pw_w4)

dist_f <- wmean(frame$dist_market, frame$pw_w4)
dist_e <- wmean(excl$dist_market, excl$pw_w4)

radio_f <- wpct(frame$own_radio, frame$pw_w4)
radio_e <- wpct(excl$own_radio, excl$pw_w4)

tv_f <- wpct(frame$own_tv, frame$pw_w4)
tv_e <- wpct(excl$own_tv, excl$pw_w4)

bike_f <- wpct(frame$own_bicycle, frame$pw_w4)
bike_e <- wpct(excl$own_bicycle, excl$pw_w4)

n_f <- nrow(frame)
n_e <- nrow(excl)

tab <- data.frame(
  Characteristic = c(
    "Urban (%)",
    "Rural (%)",
    "Distance to market (km), mean",
    "Own radio (%)",
    "Own TV (%)",
    "Own bicycle (%)",
    "N households"
  ),
  In_phone_frame = c(
    urban_f,
    rural_f,
    dist_f,
    radio_f,
    tv_f,
    bike_f,
    n_f
  ),
  Excluded = c(
    urban_e,
    rural_e,
    dist_e,
    radio_e,
    tv_e,
    bike_e,
    n_e
  )
)

tab$Difference <- with(tab, In_phone_frame - Excluded)

num_cols <- c("In_phone_frame", "Excluded", "Difference")
tab[num_cols] <- lapply(tab[num_cols], function(x) round(x, 2))

print(tab)
write.csv(tab, file.path(DATA_DIR, "ESS_table.csv"), row.names = FALSE)
