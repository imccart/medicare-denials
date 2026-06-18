# 1-build-panel.R  --  runs on the VRDC seat
# ---------------------------------------------------------------------------
# Assemble the physician-year analysis panel from the SAS exports plus the
# uploaded crosswalk. Packages are loaded by _analyze-all.R (no library() here).
#
# Expected inputs in data/input/ (SAS PROC EXPORT + uploaded crosswalk):
#   denials_panel.csv  npi, year, tot_claims, den_claims, tot_lines,
#                      den_lines, sbmt_charge, den_charge,
#                      den_rate_clm, den_rate_line
#   mdppas_panel.csv   npi, year, zip5 (phy_zip_perf1), specialty
#                      (spec_prim_1_name), pos_inpat, pos_opd,
#                      npi_unq_benes, tin1_unq_benes
#   xwalk_zip_treatment.csv  zip5, ssastate, state_abbr, area,
#                      first_treat_year, ever_treated
#
# Output: data/output/analysis_panel.csv  (internal to VRDC, not an export)

den <- fread("data/input/denials_panel.csv",
             colClasses = c(npi = "character"))
mdp <- fread("data/input/mdppas_panel.csv",
             colClasses = c(npi = "character", zip5 = "character"))
xw  <- fread("data/input/xwalk_zip_treatment.csv",
             colClasses = c(zip5 = "character"))

# --- join denials + MD-PPAS on npi x year (expect 1:1; verify) ---
stopifnot(!any(duplicated(den[, .(npi, year)])),
          !any(duplicated(mdp[, .(npi, year)])))
panel <- merge(den, mdp, by = c("npi", "year"), all.x = TRUE)
cat(sprintf("denials rows %d; matched to MD-PPAS %d (%.1f%%)\n",
            nrow(den), sum(!is.na(panel$zip5)),
            100 * mean(!is.na(panel$zip5))))

# --- fix each physician's MAC jurisdiction at their BASELINE (first) year ---
# Treatment is a jurisdiction's contractor changing while the provider stays
# put; assigning by baseline ZIP avoids endogenous moves. A `moved` flag lets
# robustness drop providers whose area changes. (Time-varying assignment is the
# alternative: join xw on each year's zip5 instead.)
setorder(panel, npi, year)
base_zip <- panel[!is.na(zip5), .(zip5 = zip5[1]), by = npi]
base_zip <- merge(base_zip, xw, by = "zip5", all.x = TRUE)

# detect movers (area changes across observed years)
yr_area <- merge(panel[!is.na(zip5), .(npi, year, zip5)], xw[, .(zip5, area)],
                 by = "zip5", all.x = TRUE)
moved <- yr_area[, .(moved = uniqueN(area[!is.na(area)]) > 1), by = npi]

panel <- merge(panel, base_zip[, .(npi, ssastate, state_abbr, area,
                                    first_treat_year, ever_treated)],
               by = "npi", all.x = TRUE)
panel <- merge(panel, moved, by = "npi", all.x = TRUE)

# --- moderators (MD-PPAS) ---
panel[, `:=`(
  hosp_based = pos_inpat + pos_opd,          # facility-based share of services
  log_volume = log1p(npi_unq_benes),         # provider Medicare volume
  log_tin    = log1p(tin1_unq_benes)         # practice (TIN) size proxy
)]

# --- event-study timing ---
# sunab cohort: never-treated coded 10000 so they serve as the comparison group.
panel[, cohort   := fifelse(ever_treated == TRUE & !is.na(first_treat_year),
                            as.integer(first_treat_year), 10000L)]
panel[, rel_year := fifelse(cohort == 10000L, NA_integer_, year - cohort)]

# --- drop providers we can't place (territories/military ZIPs) ---
n0 <- nrow(panel)
panel <- panel[!is.na(area)]
cat(sprintf("dropped %d rows with no MAC area (%.1f%%); %d remain\n",
            n0 - nrow(panel), 100 * (n0 - nrow(panel)) / n0, nrow(panel)))

cat("\n--- panel checks ---\n")
print(panel[, .(.N, n_npi = uniqueN(npi)), by = year][order(year)])
cat("cohorts (first_treat_year):\n"); print(panel[, .N, by = cohort][order(cohort)])
cat("movers:", panel[, uniqueN(npi[moved == TRUE])], "of", panel[, uniqueN(npi)], "\n")
cat("den_rate_clm summary:\n"); print(summary(panel$den_rate_clm))

fwrite(panel, "data/output/analysis_panel.csv")
cat("\nwrote data/output/analysis_panel.csv\n")
