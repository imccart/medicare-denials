# 2-event-study.R  --  runs on the VRDC seat
# ---------------------------------------------------------------------------
# Staggered event study of denial rates around MAC transitions, Sun & Abraham
# (2021) via fixest::sunab with never-treated as the comparison group.
# Packages loaded by _analyze-all.R. Only estimates leave the enclave; the
# underlying panel does not, and any exported summary table must mask N < 11.

panel <- fread("data/output/analysis_panel.csv",
               colClasses = c(npi = "character"))

# ---- main dynamic specification ----
# den rate on cohort x relative-time interactions, physician + year FE,
# weighted by claim volume, clustered on state (the treatment level).
est_main <- feols(den_rate_clm ~ sunab(cohort, year) | npi + year,
                  data = panel, weights = ~tot_claims, cluster = ~ssastate)

cat("\n===== dynamic event-study coefficients =====\n")
print(est_main)

# overall post-treatment ATT (aggregates the dynamic terms)
att_main <- summary(est_main, agg = "att")
cat("\n===== overall ATT =====\n")
print(att_main)

# event-study plot -> results/ (PNG is exportable after disclosure review)
png("results/event_study_main.png", width = 1000, height = 650, res = 130)
iplot(est_main,
      main = "Denial rate around MAC transition",
      xlab = "Years since contractor change", ref.line = -0.5)
dev.off()

# ---- robustness: drop movers (providers whose MAC area changes) ----
est_nomove <- feols(den_rate_clm ~ sunab(cohort, year) | npi + year,
                    data = panel[moved == FALSE],
                    weights = ~tot_claims, cluster = ~ssastate)

# ---- heterogeneity by provider characteristics (MD-PPAS) ----
# Re-estimate the overall ATT within subgroups. Template shown for two
# moderators; copy the block for specialty, TIN size, etc.
het_att <- function(dt, label) {
  m <- feols(den_rate_clm ~ sunab(cohort, year) | npi + year,
             data = dt, weights = ~tot_claims, cluster = ~ssastate)
  a <- summary(m, agg = "att")$coeftable
  data.frame(group = label, att = a[1, 1], se = a[1, 2],
             n_npi = uniqueN(dt$npi))
}

med_hosp <- panel[, median(hosp_based, na.rm = TRUE)]
vol_terc <- quantile(panel$log_volume, c(1/3, 2/3), na.rm = TRUE)

het <- rbind(
  het_att(panel[hosp_based <= med_hosp], "hosp-based: low"),
  het_att(panel[hosp_based >  med_hosp], "hosp-based: high"),
  het_att(panel[log_volume <= vol_terc[1]], "volume: low tercile"),
  het_att(panel[log_volume >  vol_terc[2]], "volume: high tercile")
)
cat("\n===== heterogeneity in overall ATT =====\n")
print(het)

# ---- collected output for review/export ----
etable(est_main, est_nomove,
       headers = c("Main", "No movers"),
       file = "results/event_study_tables.tex", replace = TRUE)
fwrite(het, "results/heterogeneity_att.csv")
cat("\nwrote results/event_study_main.png, results/event_study_tables.tex, results/heterogeneity_att.csv\n")
