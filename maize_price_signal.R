library(haven)
library(dplyr)
library(tidyr)
library(sandwich)
library(lmtest)

DATA_DIR <- "."

prices <- list()
for (r in 13:19) {
  f <- file.path(DATA_DIR, paste0("wb_lsms_hfpm_hh_survey_round", r, "_price_public.dta"))
  if (!file.exists(f)) next
  d <- read_dta(f)
  wcol <- paste0("phw", r)
  if (!wcol %in% names(d)) wcol <- names(d)[grepl("^phw", names(d))][1]
  d <- d %>%
    filter(grepl("Maize", as.character(fp_01), ignore.case = TRUE)) %>%
    mutate(round = r, phw = as.numeric(!!sym(wcol)), fp3_price = as.numeric(fp3_price), fp2_unit_num = as.numeric(fp2_unit)) %>%
    select(household_id, round, fp3_price, fp2_unit_num, phw)
  # if hh has maize in both kg and quntal same round, keep kg (unit 1 before 3 in arrange)
  d <- d %>% arrange(household_id, round, fp2_unit_num) %>% group_by(household_id, round) %>% slice(1) %>% ungroup()
  prices[[length(prices) + 1]] <- d
}
prices <- bind_rows(prices)

prices <- prices %>%
  mutate(price_per_kg = case_when(fp2_unit_num == 1 ~ fp3_price, fp2_unit_num == 3 ~ fp3_price / 100, TRUE ~ NA_real_))
prices <- prices %>% filter(!is.na(price_per_kg), price_per_kg > 0)
# unit 8 is Jog in codebook, there is no kg conversion so it's in the dropped set


# flag outliers with 1.5*IQR within each round, not pooled, otherwise real cross-round swings would get dropped as outliers
prices <- prices %>%
  group_by(round) %>%
  mutate(
    q1 = quantile(price_per_kg, 0.25, na.rm = TRUE),
    q3 = quantile(price_per_kg, 0.75, na.rm = TRUE),
    iqr = q3 - q1,
    lower = q1 - 1.5 * iqr, upper = q3 + 1.5 * iqr,
    outlier = price_per_kg < lower | price_per_kg > upper
  ) %>%
  ungroup()
prices <- prices %>% filter(!outlier) %>% select(-q1, -q3, -iqr, -lower, -upper, -outlier)
print(paste(nrow(prices), "rows after outlier drop"))

by_hh <- prices %>%
  group_by(household_id) %>%
  summarise(
    n_rounds = n(),
    mean_price = mean(price_per_kg, na.rm = TRUE),
    sd_price = sd(price_per_kg, na.rm = TRUE),
    cv_price = if_else(mean_price > 0, sd_price / mean_price, NA_real_),
    phw_avg = mean(phw, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_rounds >= 2, !is.na(sd_price))


wmean_sd <- sum(by_hh$sd_price * by_hh$phw_avg, na.rm = TRUE) / sum(by_hh$phw_avg, na.rm = TRUE)
ord_sd <- order(by_hh$sd_price)
wi <- cumsum(by_hh$phw_avg[ord_sd]) / sum(by_hh$phw_avg, na.rm = TRUE)
med_idx <- which(wi >= 0.5)[1]
wmed_sd <- if (!is.na(med_idx)) by_hh$sd_price[ord_sd][med_idx] else NA_real_
p25 <- by_hh$sd_price[ord_sd][which(wi >= 0.25)[1]]
p75 <- by_hh$sd_price[ord_sd][which(wi >= 0.75)[1]]

print(paste("variability (weighted): mean SD =", round(wmean_sd, 2), ", median =", round(wmed_sd, 2), ", p25 =", round(p25, 2), ", p75 =", round(p75, 2)))
print(paste("N hh with variability (>= 2 rounds):", nrow(by_hh)))

r11 <- read_dta(file.path(DATA_DIR, "wb_lsms_hfpm_hh_survey_round11_clean_microdata.dta"))
r11 <- r11 %>% select(household_id, pho3_signal, wfinal, cs4_sector, cs1_region) %>%
  mutate(pho3_signal = as_factor(pho3_signal), wfinal = as.numeric(wfinal))

dat <- by_hh %>% inner_join(r11, by = "household_id") %>%
  filter(!is.na(pho3_signal), !is.na(wfinal), wfinal > 0)
dat$signal_num <- as.numeric(dat$pho3_signal)
print(paste("overlap (variability + R11 signal):", nrow(dat)))


sig_means <- dat %>%
  group_by(pho3_signal) %>%
  summarise(
    n = n(),
    wtd_sd = sum(sd_price * wfinal, na.rm = TRUE) / sum(wfinal, na.rm = TRUE),
    wtd_cv = sum(cv_price * wfinal, na.rm = TRUE) / sum(wfinal, na.rm = TRUE),
    .groups = "drop"
  )
print(sig_means)

fit <- lm(sd_price ~ signal_num, data = dat, weights = wfinal)
print(summary(fit))
print(lmtest::coeftest(fit, vcov. = sandwich::vcovHC(fit, type = "HC2")))

fit2 <- lm(sd_price ~ signal_num + as_factor(cs4_sector), data = dat, weights = wfinal)
print(summary(fit2))

r11_sig <- r11 %>% filter(!is.na(pho3_signal), wfinal > 0) %>%
  group_by(pho3_signal) %>% summarise(n = n(), wtd = sum(wfinal), .groups = "drop") %>%
  mutate(pct = 100 * wtd / sum(wtd))
overlap_sig <- dat %>% group_by(pho3_signal) %>% summarise(n = n(), wtd = sum(wfinal), .groups = "drop") %>%
  mutate(pct = 100 * wtd / sum(wtd))
print("R11 signal %:"); print(r11_sig)
print("overlap signal %:"); print(overlap_sig)

# redo relationship requiring >=3 rounds per hh; R15 was thin in some regions so decent robustness check
hh3 <- by_hh %>% filter(n_rounds >= 3)
dat3 <- hh3 %>% inner_join(r11 %>% select(household_id, pho3_signal, wfinal), by = "household_id") %>%
  filter(!is.na(pho3_signal), wfinal > 0) %>% mutate(signal_num = as.numeric(pho3_signal))
print(paste(nrow(dat3), "hh in sensitivity"))
fit3 <- lm(sd_price ~ signal_num, data = dat3, weights = wfinal)
summary(fit3)
