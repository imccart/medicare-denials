/*==============================================================================
  1-build-denials.sas
  --------------------------------------------------------------------------
  Build a performing-physician x year denial panel from the Carrier (Part B)
  RIF, 2009-2018. DUA 027710; runs on the VRDC seat.

  Replaces the original year-by-year, denied-only extract. Two changes matter:
    (1) keeps a DENOMINATOR -- the denial flag is a counted indicator, not a
        WHERE filter, so each cell carries total AND denied claims (a rate);
    (2) one pass per year (12 monthly aggregates -> one npi x year row),
        appended to a single panel, instead of 120 stacked monthly tables.

  Denial measure: claim-level CARR_CLM_PMT_DNL_CD = '0' = "Denied" (ResDAC
  "Carrier Claim Payment Denial Code"). Attached to each line via CLM_ID x
  BENE_ID, matching the join in the original macro.

  Output (stays in VRDC; not an export):
    PL027710.denials_panel  -- schema documented in the project CLAUDE.md.

  Libraries RIF<yyyy> and PL027710 are auto-mounted on the seat; do NOT add
  LIBNAME for them. MD-PPAS (geography + moderators) is joined in 2-merge-*.sas.

  Note on summing distinct-claim counts across months: a CLM_ID lives in a
  single monthly file, so summing per-month COUNT(DISTINCT CLM_ID) equals the
  annual distinct-claim count. This keeps intermediate tables small.
==============================================================================*/

%let year_start = 2009;
%let year_end   = 2018;

/* row-count helper for the validation log (per project data discipline) */
%macro row_count(ds, label);
  proc sql noprint; select count(*) into :nr from &ds; quit;
  %put NOTE: ROWCHECK &label = &nr;
%mend;

%macro build_year(yr);
  %do m = 1 %to 12;
    %let mm = %sysfunc(putn(&m, z2.));

    /* claim-level denied flag */
    proc sql;
      create table work.clm_&mm as
      select distinct BENE_ID, CLM_ID,
             (CARR_CLM_PMT_DNL_CD = '0') as denied
      from RIF&yr..BCARRIER_CLAIMS_&mm;
    quit;

    /* lines joined to the claim flag, aggregated to npi within this month */
    proc sql;
      create table work.npi_&mm as
      select l.PRF_PHYSN_NPI                                          as npi,
             count(distinct l.CLM_ID)                                 as tot_claims,
             count(distinct case when c.denied=1 then l.CLM_ID end)   as den_claims,
             count(*)                                                 as tot_lines,
             sum(c.denied)                                            as den_lines,
             sum(l.LINE_SBMTD_CHRG_AMT)                               as sbmt_charge,
             sum(c.denied * l.LINE_SBMTD_CHRG_AMT)                    as den_charge
      from RIF&yr..BCARRIER_LINE_&mm as l
      inner join work.clm_&mm as c
        on l.CLM_ID = c.CLM_ID and l.BENE_ID = c.BENE_ID
      where l.PRF_PHYSN_NPI is not null
        and l.PRF_PHYSN_NPI not in ('', '0000000000', '9999999999')
      group by l.PRF_PHYSN_NPI;
    quit;

    proc datasets lib=work nolist; delete clm_&mm; quit;
  %end;

  /* stack the 12 monthly npi aggregates and collapse to npi x year */
  data work.stack_&yr;
    set %do m=1 %to 12; %let mm=%sysfunc(putn(&m,z2.)); work.npi_&mm %end; ;
  run;

  proc sql;
    create table work.year_&yr as
    select npi, &yr as year length=8,
           sum(tot_claims)  as tot_claims,
           sum(den_claims)  as den_claims,
           sum(tot_lines)   as tot_lines,
           sum(den_lines)   as den_lines,
           sum(sbmt_charge) as sbmt_charge,
           sum(den_charge)  as den_charge
    from work.stack_&yr
    group by npi;
  quit;

  %row_count(work.year_&yr, year &yr npi-rows);

  proc datasets lib=work nolist;
    delete %do m=1 %to 12; %let mm=%sysfunc(putn(&m,z2.)); npi_&mm %end; stack_&yr;
  quit;
%mend;

/* ---- run all years, append into one panel ---- */
proc datasets lib=PL027710 nolist; delete denials_panel; quit;

%macro run_all;
  %do y = &year_start %to &year_end;
    %build_year(&y);
    proc append base=PL027710.denials_panel data=work.year_&y force; run;
    proc datasets lib=work nolist; delete year_&y; quit;
    %put NOTE: ===== year &y appended =====;
  %end;
%mend;
%run_all;

/* ---- denial rates + spot-check distributions before anything downstream ---- */
data PL027710.denials_panel;
  set PL027710.denials_panel;
  den_rate_clm  = den_claims / tot_claims;
  den_rate_line = den_lines  / tot_lines;
run;

%row_count(PL027710.denials_panel, FINAL panel npi-years);

proc means data=PL027710.denials_panel n nmiss mean min p10 p50 p90 max maxdec=4;
  class year;
  var tot_claims den_claims den_rate_clm den_rate_line sbmt_charge;
run;
