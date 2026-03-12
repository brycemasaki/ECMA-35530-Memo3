# ECMA 35530, Policy Memo 3

Analysis code for a policy memo evaluating whether mobile phone coverage expansion could stabilize agricultural prices in Ethiopia, using Aker (2010) as the base paper.

## Scripts

- **`maize_price_signal.R`** - Cleans maize price data from HFPS rounds 13–19, computes within-household price variability, merges with round 11 signal strength, and estimates the regressions in Table 1.
- **`ess_representativeness.R`** - Compares characteristics of ESS 2018/19 households in the HFPS sampling frame vs. those excluded (Table 2).

## Data

Microdata must be downloaded separately (free registration, just explain what you're using for):

- [Ethiopia HFPS 2020–2024](https://microdata.worldbank.org/index.php/catalog/3716)
- [Ethiopia ESS 2018/19](https://microdata.worldbank.org/index.php/catalog/3823)

## Requirements

```r
install.packages(c("haven", "dplyr", "tidyr", "sandwich", "lmtest"))
```
