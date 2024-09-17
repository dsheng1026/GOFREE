
#' OM_JOB
#'
#' @param GW_activity Output from GCAM_GW()
#' @import dplyr tidyr tibble
#'
#' @return A data frame of OM job by subsector
#' @export
#'
#' @examples
#' job.OM <- OM_JOB(GW_activity)
OM_JOB <- function(GW_activity){

  ## adjustment for OM EF ----

  cap_fac_join <- rgcam::getQuery(prj, 'GCAM_USA elec cap-fac by cooling tech') %>%
     select(scenario, region, year, sector, subsector = subsector...5,
                  subsector.1 = subsector...6, technology, value) %>%
     mutate(fuel = subsector,
                  fuel = ifelse(grepl("offshore", subsector.1), "wind_offshore", fuel),
                  subsector = gsub(",depth=1", "", subsector.1),
                  technology = gsub(" ", "", technology),
                  fuel = ifelse(grepl("CSP", subsector), "CSP", fuel), # separate CSP and PV from solar
                  fuel = ifelse(grepl("PV", subsector), "PV", fuel))

  cap_fac_join %>%
     left_join(OM_cost %>%  rename(sector = sector.name, subsector = subsector.name), # TODO: assumption: OM_cost
                     by = c("year", "sector", "subsector", "technology")) %>%
    # convert the OM_var cost from 1975$/MWh to 1975$/kW/yr
     mutate(`OM-var` = `OM-var` * 8760 * value / convMW_kW) %>%
     mutate(fixed = `OM-fixed`/(`OM-fixed` +`OM-var`),
                  variable = `OM-var`/(`OM-fixed` +`OM-var`)) %>%
     select(-`OM-fixed`, -`OM-var`) ->
    OM_share_all

  OM_share_all %>%
     filter(!grepl("steam/CT", subsector)) %>%
     filter(sector != "elect_td_bld") %>%
     arrange(region, year, subsector, subsector.1, technology) %>%
     group_by(region, year, technology) %>%
    # for as_subpeak_steam/CT, there is no base load
    # in this case, when the baseline value is not for base load
    # use the capacity factor as the scaler
     mutate(scaler = value /  first(value)) %>%
     bind_rows(OM_share_all %>%
                        filter(grepl("steam/CT", subsector)) %>%
                        mutate(scaler = value)) %>%
     bind_rows(OM_share_all %>%
                        filter(sector == "elect_td_bld") %>%
                        mutate(scaler = 1)) %>%  # TODO: do we need to change the scaler for elect_td_bld?
     mutate(fixed_adj = fixed,
                  # fixed_adj = fixed * scaler, # don't adjust for the nameplate capacity
                  variable_adj = variable * scaler) ->
    OM_share_adj

  GW_activity %>%
     filter(activity == "installed") %>%
     filter(Year >= 2020) %>%
     left_join(EF.JEDI %>%  select(region, unit, OM, fuel), # TODO: assumption EF.JEDI
              by = c("region", "fuel")) %>%
     left_join(OM_share_adj %>%  select(region, Year = year, sector, subsector, technology, fixed_adj),
              by = c("region", "subsector", "technology", "Year")) %>%
     mutate(fixed_adj = ifelse(is.na(fixed_adj), 1, fixed_adj), # for technologies that does not have assumption in GCAM, assign all OM to fixed_OM
           value = OM * value * convGW_MW * fixed_adj, # GW to people, round to integer when aggregate OM job
           job = "OM_fixed",
           Units = "ppl") %>%
     group_by(scenario, region, Year, fuel, subsector, job, Units) %>%
     summarise(value = sum(value, na.rm = T), .groups = "drop") ->
    fuel_OM_fixed_ppl

  GW_activity %>%
     filter(activity == "running") %>%
     filter(Year >= 2020) %>%
     left_join(EF.JEDI %>%  select(region, unit, OM, fuel),
              by = c("region", "fuel")) %>%
     left_join(OM_share_adj %>%  select(region, Year = year, sector, subsector, technology, variable_adj),
              by = c("region", "subsector", "technology", "Year")) %>%
     mutate(variable_adj = ifelse(is.na(variable_adj), 0, variable_adj),
           value = OM * value * convGW_MW * variable_adj, # GW to people, round to integer when aggregate OM job
           job = "OM_var",
           Units = "ppl") %>%
     group_by(scenario, region, Year, fuel, subsector, job, Units) %>%
     summarise(value = sum(value, na.rm = T), .groups = "drop") ->
    fuel_OM_variable_ppl

  fuel_OM_fixed_ppl %>%
     bind_rows(fuel_OM_variable_ppl) %>%
     mutate(value = ceiling(value)) -> # round to next integer
    fuel_OM_ppl

  fuel_OM_ppl %>%
    spread(job, value) %>%
    mutate(OM_total = OM_fixed + OM_var) %>%
    gather(job, value, OM_fixed:OM_total) ->
    fuel_OM_ppl

  return(fuel_OM_ppl)
}



#' CON_JOB
#'
#' @param GW_activity Output from GCAM_GW()
#' @param method Specify method for construction job.
#' "Net": in a given year, a technology pre-mature retirement and newly installed net out.
#' "Total": in a given year, a technology pre-mature retirement and newly installed can happen at the same time.
#' @import dplyr tidyr tibble
#'
#' @return A data frame of construction job by subsector
#' @export
#'
#' @examples
#' job.CON <- CON_JOB(GW_activity, "Net")
#' job.CON <- CON_JOB(GW_activity)
CON_JOB <- function(GW_activity, method = NULL){
  if (is.null(method)||method %in% c("Total", "total")){
    ACTIVITY <-  "additions"
  } else if (method %in% c("Net", "net")){
    ACTIVITY <-  "add_adj"
  } else {stop("Not a valid input, use either 'Net' or 'Total'")
  }

  GW_activity %>%
     filter(Year >= 2020) %>%
     filter(activity == ACTIVITY) %>%
     left_join(EF.JEDI %>%  select(region, unit, CON.L, fuel, Construction.Period.Y),
              by = c("region", "fuel")) %>%
    # filter the plant with construction period no longer than GCAM model step year (5 years)
     filter(Construction.Period.Y <= GCAM_model_step) %>%
     mutate(value = CON.L * value * convGW_MW,
           job = "Construction-onsite",
           Units = "ppl") %>%
     group_by(scenario, region, Year, fuel, subsector, job, Units) %>%
     summarise(value = sum(value, na.rm = T), .groups = "drop") %>%
     bind_rows(GW_activity %>%
                        filter(Year >= 2020) %>%
                        filter(activity == ACTIVITY) %>%
                        left_join(EF.JEDI %>%  select(region, unit, CON.RL, fuel, Construction.Period.Y),
                          by = c("region", "fuel")) %>%
                        filter(Construction.Period.Y <= GCAM_model_step) %>%
                        mutate(value = CON.RL * value * convGW_MW,
                                     job = "Construction-related",
                                     Units = "ppl") %>%
                        group_by(scenario, region, Year, fuel, subsector, job, Units) %>%
                        summarise(value = sum(value, na.rm = T), .groups = "drop")) %>%
     mutate(value = ceiling(value)) -> # round to next integer
    fuel_CON_ppl_short

  GW_activity %>%
     filter(Year >= 2020) %>%
     filter(activity == ACTIVITY) %>%
     left_join(EF.JEDI %>%  select(region, unit, CON.L, CON.RL, fuel, Construction.Period.Y),
                     by = c("region", "fuel")) %>%
    # filter the plant with construction period no longer than GCAM model step year (5 years)
     filter(Construction.Period.Y > GCAM_model_step) ->
    GW_long

  GW_long %>%  select(-Year, -value) %>%
     distinct() %>%
    repeat_add_columns(tibble::tibble(Year = c(seq(GCAM_futr_start, GCAM_futr_end, by = GCAM_model_step)))) %>%
     left_join(GW_long %>%  select(scenario, region, subsector, technology, Year, value),
                     by = c("scenario", "region", "subsector", "technology", "Year")) %>%
     arrange( across(-c(Year, value, CON.L, CON.RL))) %>%
     group_by( across(-c(Year, value, CON.L, CON.RL))) %>%
     mutate(# construction for the capacity addition in t
                  value_con = CON.L * value * convGW_MW,
                  # construction for the capacity addition in t + 1
                  value_lead_con = CON.L *  lead(value) * convGW_MW * (Construction.Period.Y - GCAM_model_step)/GCAM_model_step,
                  value = tidyr::replace_na(value_con,0) + tidyr::replace_na(value_lead_con, 0),
                  job = "Construction-onsite",
                  Units = "ppl") %>%
     group_by(scenario, region, Year, fuel, subsector, job, Units) %>%
     summarise(value = sum(value, na.rm = T), .groups = "drop") %>%
     bind_rows(GW_long %>%  select(-Year, -value) %>%  distinct() %>%
                       repeat_add_columns(tibble(Year = c(seq(GCAM_futr_start, GCAM_futr_end, by = GCAM_model_step)))) %>%
                        left_join(GW_long %>%  select(scenario, region, subsector, technology, Year, value),
                                        by = c("scenario", "region", "subsector", "technology", "Year")) %>%
                        arrange( across(-c(Year, value, CON.L, CON.RL))) %>%
                        group_by( across(-c(Year, value, CON.L, CON.RL))) %>%
                        mutate(# construction for the capacity addition in t
                                     value_con = CON.RL * value * convGW_MW,
                                     # construction for the capacity addition in t + 1
                                     value_lead_con = CON.RL *  lead(value) * convGW_MW * (Construction.Period.Y - GCAM_model_step)/GCAM_model_step,
                                     value = tidyr::replace_na(value_con,0) + tidyr::replace_na(value_lead_con, 0),
                         job = "Construction-related",
                         Units = "ppl") %>%
                        group_by(scenario, region, Year, fuel, subsector, job, Units) %>%
                        summarise(value = sum(value, na.rm = T), .groups = "drop")) %>%
     mutate(value = ceiling(value)) %>%  # round to next integer
     bind_rows(fuel_CON_ppl_short) ->
    fuel_CON_ppl
  return(fuel_CON_ppl)
}


#' DECOM_JOB
#'
#' @param GW_activity Output from GCAM_GW()
#' @param method Specify method for construction job.
#' "Net": in a given year, a technology pre-mature retirement and newly installed net out.
#' "Total": in a given year, a technology pre-mature retirement and newly installed can happen at the same time.
#' @import dplyr tidyr tibble
#' @return A data frame of decommission job by subsector
#' @export
#'
#' @examples
#' job.DECOM <- DECOM_JOB(GW_activity, "Net")
#' job.DECOM <- DECOM_JOB(GW_activity)
DECOM_JOB <- function(GW_activity, method = NULL){

  if (is.null(method)||method %in% c("Total", "total")){
    ACTIVITY <-  "retirements"
  } else if (method %in% c("Net", "net")){
    ACTIVITY <-  "ret_adj"
  } else {stop("Not a valid input, use either 'Net' or 'Total'")
  }


  GW_activity %>%
     filter(Year >= 2020) %>%
     filter(activity == ACTIVITY) %>%
     left_join(EF.JEDI %>%  select(region, unit, DECON, fuel, Construction.Period.Y),
              by = c("region", "fuel")) %>%
     filter(Construction.Period.Y <= GCAM_model_step) %>%
     mutate(value = DECON * value * convGW_MW,
           job = "Decommission",
           Units = "ppl") %>%
     group_by(scenario, region, Year, fuel, subsector, job, Units) %>%
     summarise(value = sum(value, na.rm = T), .groups = "drop") %>%
     mutate(value = ceiling(value))-> # round to next integer
    fuel_DECOM_ppl_short


  GW_activity %>%
     filter(Year >= 2020) %>%
     filter(activity == ACTIVITY) %>%
     left_join(EF.JEDI %>%  select(region, unit, DECON, fuel, Construction.Period.Y),
                     by = c("region", "fuel")) %>%
     filter(Construction.Period.Y > GCAM_model_step) %>%
     arrange( across(-c(Year, value, DECON ))) %>%
     group_by( across(-c(Year, value, DECON ))) %>%
     mutate(# construction for the capacity addition in t
                  value_decon = DECON * value * convGW_MW,
                  # construction for the capacity addition in t + 1
                  value_lead_decon = DECON *  lead(value) * convGW_MW * (Construction.Period.Y - GCAM_model_step) / GCAM_model_step,
                  value = value_decon + value_lead_decon,
                  job = "Decommission",
                  Units = "ppl") %>%
     group_by(scenario, region, Year, fuel, subsector, job, Units) %>%
     summarise(value = sum(value, na.rm = T), .groups = "drop") %>%
     mutate(value = ceiling(value)) %>%
     bind_rows(fuel_DECOM_ppl_short) -> # round to next integer
    fuel_DECOM_ppl

  return(fuel_DECOM_ppl)
}



#' GCAM_JOB
#'
#' @param GW_activity Output from GCAM_GW()
#' @param method Specify method for construction job.
#' "Net": in a given year, a technology pre-mature retirement and newly installed net out.
#' "Total": in a given year, a technology pre-mature retirement and newly installed can happen at the same time.
#' @import dplyr tidyr tibble
#' @return A data frame of all power sector direct job by subsector
#' @export
#'
#' @examples
#' JOB_activity <- GCAM_JOB(GW_activity, "Net")
#' JOB_activity <- GCAM_JOB(GW_activity)
GCAM_JOB <- function(GW_activity, method = NULL){

  job.OM <-  OM_JOB(GW_activity)
  job.CON <-  CON_JOB(GW_activity, method)
  job.DECOM <-  DECOM_JOB(GW_activity, method)

  job.OM %>%
     bind_rows(job.CON) %>%
     bind_rows(job.DECOM) ->
    job.GCAM

  return(job.GCAM)
}
