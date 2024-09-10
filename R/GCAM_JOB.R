
#' OM_JOB
#'
#' @param GW_activity Output from GCAM_GW()
#'
#' @return A data frame of OM job by subsector
#' @export
#'
#' @examples
#' job.OM <- OM_JOB(GW_activity)

OM_JOB <- function(GW_activity){

  ## adjustment for OM EF ----

  cap_fac_join <- rgcam::getQuery(prj, 'GCAM_USA elec cap-fac by cooling tech') %>%
    dplyr::select(scenario, region, year, sector, subsector = subsector...5,
                  subsector.1 = subsector...6, technology, value) %>%
    dplyr::mutate(fuel = subsector,
                  fuel = ifelse(grepl("offshore", subsector.1), "wind_offshore", fuel),
                  subsector = gsub(",depth=1", "", subsector.1),
                  technology = gsub(" ", "", technology),
                  fuel = ifelse(grepl("CSP", subsector), "CSP", fuel), # separate CSP and PV from solar
                  fuel = ifelse(grepl("PV", subsector), "PV", fuel))

  cap_fac_join %>%
    dplyr::left_join(OM_cost %>% dplyr::rename(sector = sector.name, subsector = subsector.name), # TODO: assumption: OM_cost
                     by = c("year", "sector", "subsector", "technology")) %>%
    # convert the OM_var cost from 1975$/MWh to 1975$/kW/yr
    dplyr::mutate(`OM-var` = `OM-var` * 8760 * value / convMW_kW) %>%
    dplyr::mutate(fixed = `OM-fixed`/(`OM-fixed` +`OM-var`),
                  variable = `OM-var`/(`OM-fixed` +`OM-var`)) %>%
    dplyr::select(-`OM-fixed`, -`OM-var`) ->
    OM_share_all

  OM_share_all %>%
    dplyr::filter(!grepl("steam/CT", subsector)) %>%
    dplyr::filter(sector != "elect_td_bld") %>%
    dplyr::arrange(region, year, subsector, subsector.1, technology) %>%
    dplyr::group_by(region, year, technology) %>%
    # for as_subpeak_steam/CT, there is no base load
    # in this case, when the baseline value is not for base load
    # use the capacity factor as the scaler
    dplyr::mutate(scaler = value / dplyr::first(value)) %>%
    dplyr::bind_rows(OM_share_all %>%
                       dplyr::filter(grepl("steam/CT", subsector)) %>%
                       dplyr::mutate(scaler = value)) %>%
    dplyr::bind_rows(OM_share_all %>%
                       dplyr::filter(sector == "elect_td_bld") %>%
                       dplyr::mutate(scaler = 1)) %>%  # TODO: do we need to change the scaler for elect_td_bld?
    dplyr::mutate(fixed_adj = fixed,
                  # fixed_adj = fixed * scaler, # don't adjust for the nameplate capacity
                  variable_adj = variable * scaler) ->
    OM_share_adj

  GW_activity %>%
    dplyr::filter(activity == "installed") %>%
    dplyr::filter(Year >= 2020) %>%
    dplyr::left_join(EF.JEDI %>% dplyr::select(region, unit, OM, fuel), # TODO: assumption EF.JEDI
              by = c("region", "fuel")) %>%
    dplyr::left_join(OM_share_adj %>% dplyr::select(region, Year = year, sector, subsector, technology, fixed_adj),
              by = c("region", "subsector", "technology", "Year")) %>%
    dplyr::mutate(fixed_adj = ifelse(is.na(fixed_adj), 1, fixed_adj), # for technologies that does not have assumption in GCAM, assign all OM to fixed_OM
           value = OM * value * convGW_MW * fixed_adj, # GW to people, round to integer when aggregate OM job
           job = "OM_fixed",
           Units = "ppl") %>%
    dplyr::group_by(scenario, region, Year, fuel, subsector, job, Units) %>%
    dplyr::summarise(value = sum(value, na.rm = T), .groups = "drop") ->
    fuel_OM_fixed_ppl

  GW_activity %>%
    dplyr::filter(activity == "running") %>%
    dplyr::filter(Year >= 2020) %>%
    dplyr::left_join(EF.JEDI %>% dplyr::select(region, unit, OM, fuel),
              by = c("region", "fuel")) %>%
    dplyr::left_join(OM_share_adj %>% dplyr::select(region, Year = year, sector, subsector, technology, variable_adj),
              by = c("region", "subsector", "technology", "Year")) %>%
    dplyr::mutate(variable_adj = ifelse(is.na(variable_adj), 0, variable_adj),
           value = OM * value * convGW_MW * variable_adj, # GW to people, round to integer when aggregate OM job
           job = "OM_var",
           Units = "ppl") %>%
    dplyr::group_by(scenario, region, Year, fuel, subsector, job, Units) %>%
    dplyr::summarise(value = sum(value, na.rm = T), .groups = "drop") ->
    fuel_OM_variable_ppl

  fuel_OM_fixed_ppl %>%
    dplyr::bind_rows(fuel_OM_variable_ppl) %>%
    dplyr::mutate(value = ceiling(value)) -> # round to next integer
    fuel_OM_ppl

  return(fuel_OM_ppl)
}




#' CON_JOB
#'
#' @param GW_activity Output from GCAM_GW()
#' @param method Specify method for construction job.
#' "Net": in a given year, a technology pre-mature retirement and newly installed net out.
#' "Total": in a given year, a technology pre-mature retirement and newly installed can happen at the same time.
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
    dplyr::filter(Year >= 2020) %>%
    dplyr::filter(activity == ACTIVITY) %>%
    dplyr::left_join(EF.JEDI %>% dplyr::select(region, unit, CON.L, fuel),
              by = c("region", "fuel")) %>%
    dplyr::mutate(value = CON.L * value * convGW_MW,
           job = "Construction-onsite",
           Units = "ppl") %>%
    dplyr::group_by(scenario, region, Year, fuel, subsector, job, Units) %>%
    dplyr::summarise(value = sum(value, na.rm = T), .groups = "drop") %>%
    dplyr::bind_rows(GW_activity %>%
                       dplyr::filter(Year >= 2020) %>%
                       dplyr::filter(activity == ACTIVITY) %>%
                       dplyr::left_join(EF.JEDI %>% dplyr::select(region, unit, CON.RL, fuel),
                          by = c("region", "fuel")) %>%
                       dplyr::mutate(value = CON.RL * value * convGW_MW,
                                     job = "Construction-related",
                                     Units = "ppl") %>%
                       dplyr::group_by(scenario, region, Year, fuel, subsector, job, Units) %>%
                       dplyr::summarise(value = sum(value, na.rm = T), .groups = "drop")) %>%
    dplyr::mutate(value = ceiling(value)) -> # round to next integer
    fuel_CON_ppl
  return(fuel_CON_ppl)
}



#' DECOM_JOB
#'
#' @param GW_activity Output from GCAM_GW()
#' @param method Specify method for construction job.
#' "Net": in a given year, a technology pre-mature retirement and newly installed net out.
#' "Total": in a given year, a technology pre-mature retirement and newly installed can happen at the same time.
#'
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
    dplyr::filter(Year >= 2020) %>%
    dplyr::filter(activity == ACTIVITY) %>%
    dplyr::left_join(EF.JEDI %>% dplyr::select(region, unit, DECON, fuel),
              by = c("region", "fuel")) %>%
    dplyr::mutate(value = DECON * value * convGW_MW,
           job = "Decommission",
           Units = "ppl") %>%
    dplyr::group_by(scenario, region, Year, fuel, subsector, job, Units) %>%
    dplyr::summarise(value = sum(value, na.rm = T), .groups = "drop") %>%
    dplyr::mutate(value = ceiling(value))-> # round to next integer
    fuel_DECOM_ppl
  return(fuel_DECOM_ppl)
}




#' GCAM_JOB
#'
#' @param GW_activity Output from GCAM_GW()
#' @param method Specify method for construction job.
#' "Net": in a given year, a technology pre-mature retirement and newly installed net out.
#' "Total": in a given year, a technology pre-mature retirement and newly installed can happen at the same time.
#'
#' @return A data frame of all power sector direct job by subsector
#' @export
#'
#' @examples
#' JOB_activity <- GCAM_JOB(GW_activity, "Net")
#' JOB_activity <- GCAM_JOB(GW_activity)

GCAM_JOB <- function(GW_activity, method = NULL){

  job.OM <- GOFREE::OM_JOB(GW_activity)
  job.CON <- GOFREE::CON_JOB(GW_activity, method)
  job.DECOM <- GOFREE::DECOM_JOB(GW_activity, method)

  job.OM %>%
    dplyr::bind_rows(job.CON) %>%
    dplyr::bind_rows(job.DECOM) ->
    job.GCAM

  return(job.GCAM)
}
