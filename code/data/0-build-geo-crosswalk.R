# 0-build-geo-crosswalk.R  --  RUNS LOCALLY (not on the VRDC seat)
# ---------------------------------------------------------------------------
# Bake a ZIP -> MAC-treatment crosswalk from public geography files plus
# Riley's contractor crosswalk (data/mac_transitions.csv). The result is a
# small CSV uploaded to the seat; the seat-side merge is then a single join on
# the provider's MD-PPAS practice ZIP, no readxl / heavy files in the enclave.
#
# Chain: ZIP --HUD--> dominant county FIPS --county-fips-cw--> SSA state
#        --mac_transitions--> area, first transition year, ever-treated.
# Virginia is overridden at the county level (the DC carve-out).
#
# Output: data/xwalk_zip_treatment.csv

library(tidyverse)
library(readxl)

geo <- "D:/research-data/geography/"

# DC carve-out: NoVA jurisdictions that sit with DC/Novitas (never-treated),
# not the rest of Virginia (TrailBlazer -> Palmetto, 2011). Riley names
# Arlington, Fairfax County, and Alexandria; CMS also groups the independent
# cities of Fairfax (51600) and Falls Church (51610). Edit if needed.
nova_fips <- c("51013", "51059", "51510", "51600", "51610")

# --- HUD ZIP -> county, keep the dominant county per ZIP (business address) ---
zc <- read_excel(paste0(geo, "ZIP_COUNTY_032018.xlsx")) %>%
  transmute(zip5 = str_pad(as.character(zip), 5, pad = "0"),
            fipscounty = str_pad(as.character(county), 5, pad = "0"),
            bus_ratio = as.numeric(bus_ratio),
            tot_ratio = as.numeric(tot_ratio)) %>%
  arrange(zip5, desc(bus_ratio), desc(tot_ratio)) %>%
  group_by(zip5) %>% slice(1) %>% ungroup() %>%
  select(zip5, fipscounty)
cat("HUD zip5 (dominant county):", nrow(zc), "\n")

# --- county FIPS -> SSA state + abbreviation ---
cf <- read.csv(paste0(geo, "county-fips-cw.csv"), colClasses = "character") %>%
  transmute(fipscounty = str_pad(fipscounty, 5, pad = "0"),
            ssastate   = suppressWarnings(as.integer(ssastate)),
            state_abbr = state) %>%
  filter(!is.na(ssastate)) %>%
  distinct(fipscounty, .keep_all = TRUE)
cat("county-fips rows:", nrow(cf), "\n")

# --- Riley treatment timing, collapsed to SSA state (non-VA splits agree) ---
trt_state <- read.csv("data/output/mac_transitions.csv") %>%
  filter(state_name != "Virginia") %>%
  group_by(state) %>%
  summarise(area = first(area),
            first_treat_year = suppressWarnings(min(first_treat_year, na.rm = TRUE)),
            ever_treated = any(as.logical(ever_treated)), .groups = "drop") %>%
  mutate(first_treat_year = ifelse(is.finite(first_treat_year), first_treat_year, NA_integer_))

# --- assemble ZIP-level crosswalk ---
xw <- zc %>%
  left_join(cf, by = "fipscounty")

cat("zip5 with no SSA-state match (territories/military):",
    sum(is.na(xw$ssastate)), "\n")

# Virginia override by county; everything else by SSA state
xw <- xw %>%
  left_join(trt_state, by = c("ssastate" = "state")) %>%
  mutate(
    is_va = state_abbr == "VA",
    nova  = is_va & fipscounty %in% nova_fips,
    area  = case_when(nova ~ "VANorth", is_va ~ "VASouth", TRUE ~ area),
    first_treat_year = case_when(nova ~ NA_integer_, is_va ~ 2011L, TRUE ~ first_treat_year),
    ever_treated     = case_when(nova ~ FALSE, is_va ~ TRUE, TRUE ~ ever_treated)
  ) %>%
  select(zip5, fipscounty, ssastate, state_abbr, area, first_treat_year, ever_treated)

# --- validation before writing ---
cat("\n--- VA split check ---\n")
xw %>% filter(state_abbr == "VA") %>% count(area, first_treat_year, ever_treated) %>% print()
cat("\n--- treatment coverage (zip5 level) ---\n")
xw %>% count(ever_treated) %>% print()
cat("zip5 with no area assigned:", sum(is.na(xw$area)), "\n")

write.csv(xw, "data/output/xwalk_zip_treatment.csv", row.names = FALSE)
cat("\nwrote data/output/xwalk_zip_treatment.csv (", nrow(xw), "ZIPs )\n")
