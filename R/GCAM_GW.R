
#' GCAM_GW
#'
#' @param elec_gen_activity Output from GCAM_EJ()
#' @import dplyr tidyr tibble
#' @return A data frame with state-level capacity activity
#' @export
#'
#' @examples
#' GW_activity <- GCAM_GW(EJ_activity)

GCAM_GW <- function(elec_gen_activity){

  cap_fac_join <- rgcam::getQuery(prj, 'GCAM_USA elec cap-fac by cooling tech') %>%
     select(scenario, region, year, sector, subsector = subsector...5,
           subsector.1 = subsector...6, technology, value) %>%
     mutate(fuel = subsector,
           fuel = ifelse(grepl("offshore", subsector.1), "wind_offshore", fuel),
           subsector = gsub(",depth=1", "", subsector.1),
           technology = gsub(" ", "", technology),
           fuel = ifelse(grepl("CSP", subsector), "CSP", fuel), # separate CSP and PV from solar
           fuel = ifelse(grepl("PV", subsector), "PV", fuel))


  elec_gen_activity %>%
     filter(grepl("hydro", subsector)) %>%
     mutate(technology = gsub(" ", "", technology)) %>%
     left_join(hydro_cf %>%  rename(capacity.factor = value), by = "region") %>%
     mutate(value = (value / GCAMUSAJobs::CONV_GWH_EJ) / (8760 * capacity.factor), # EJ to GW
                  Units = "GW",
                  fuel = ifelse(grepl("CCS", technology), paste0(fuel, "_CCS"), fuel)) ->
    tech_GW_hydro

  elec_gen_activity %>%
     filter(grepl("geo", subsector)) %>%
     mutate(technology = gsub(" ", "", technology)) %>%
     mutate(fuel = ifelse(grepl("geo", subsector), "geo", fuel),
                  capacity.factor = geo_cf) %>% # TODO: assumption: geo_cf
     mutate(value = (value / GCAMUSAJobs::CONV_GWH_EJ) / (8760 * capacity.factor), # EJ to GW
                  Units = "GW",
                  fuel = ifelse(grepl("CCS", technology), paste0(fuel, "_CCS"), fuel)) ->
    tech_GW_geo

  elec_gen_activity %>%
     filter(!grepl("hydro", subsector)) %>%
     filter(!grepl("geo", subsector)) %>%
     mutate(technology = gsub(" ", "", technology)) %>%
     left_join(cap_fac_join %>% rename(Year = year, capacity.factor = value),
                     by = c("scenario", "region", "subsector", "technology", "Year")) %>%
     mutate(fuel = ifelse(grepl("gas_backup", subsector), "gas", fuel),
                  capacity.factor = ifelse(grepl("gas_backup", subsector), backup_cf, capacity.factor),
                  value = (value / GCAMUSAJobs::CONV_GWH_EJ) / (8760 * capacity.factor), # EJ to GW
                  Units = "GW",
                  fuel = ifelse(grepl("CCS", technology), paste0(fuel, "_CCS"), fuel)) %>%
     select(names(tech_GW_hydro)) %>%
     bind_rows(tech_GW_hydro) %>%
     bind_rows(tech_GW_geo) ->
    GW_activity


  # OUTPUT : GW_activity ----
  return(GW_activity)
}
