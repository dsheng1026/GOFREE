
#' GCAM_GW
#'
#' @param elec_gen_activity Output from GCAM_EJ()
#'
#' @return A data frame with state-level capacity activity
#' @export
#'
#' @examples
#' GW_activity <- GCAM_GW(EJ_activity)

GCAM_GW <- function(elec_gen_activity){

  cap_fac_join <- rgcam::getQuery(prj, 'GCAM_USA elec cap-fac by cooling tech') %>%
    dplyr::select(scenario, region, year, sector, subsector = subsector...5,
           subsector.1 = subsector...6, technology, value) %>%
    dplyr::mutate(fuel = subsector,
           fuel = ifelse(grepl("offshore", subsector.1), "wind_offshore", fuel),
           subsector = gsub(",depth=1", "", subsector.1),
           technology = gsub(" ", "", technology),
           fuel = ifelse(grepl("CSP", subsector), "CSP", fuel), # separate CSP and PV from solar
           fuel = ifelse(grepl("PV", subsector), "PV", fuel))


  elec_gen_activity %>%
    dplyr::filter(grepl("hydro", subsector)) %>%
    dplyr::mutate(technology = gsub(" ", "", technology)) %>%
    dplyr::left_join(hydro_cf %>% dplyr::rename(capacity.factor = value), by = "region") %>%
    dplyr::mutate(value = (value / GOFREE::CONV_GWH_EJ) / (8760 * capacity.factor), # EJ to GW
                  Units = "GW",
                  fuel = ifelse(grepl("CCS", technology), paste0(fuel, "_CCS"), fuel)) ->
    tech_GW_hydro

  elec_gen_activity %>%
    dplyr::filter(grepl("geo", subsector)) %>%
    dplyr::mutate(technology = gsub(" ", "", technology)) %>%
    dplyr::mutate(fuel = ifelse(grepl("geo", subsector), "geo", fuel),
                  capacity.factor = geo_cf) %>% # TODO: assumption: geo_cf
    dplyr::mutate(value = (value / GOFREE::CONV_GWH_EJ) / (8760 * capacity.factor), # EJ to GW
                  Units = "GW",
                  fuel = ifelse(grepl("CCS", technology), paste0(fuel, "_CCS"), fuel)) ->
    tech_GW_geo

  elec_gen_activity %>%
    dplyr::filter(!grepl("hydro", subsector)) %>%
    dplyr::filter(!grepl("geo", subsector)) %>%
    dplyr::mutate(technology = gsub(" ", "", technology)) %>%
    dplyr::left_join(cap_fac_join %>% dplyr::rename(Year = year, capacity.factor = value),
                     by = c("scenario", "region", "subsector", "technology", "Year")) %>%
    dplyr::mutate(fuel = ifelse(grepl("gas_backup", subsector), "gas", fuel),
                  capacity.factor = ifelse(grepl("gas_backup", subsector), backup_cf, capacity.factor),
                  value = (value / GOFREE::CONV_GWH_EJ) / (8760 * capacity.factor), # EJ to GW
                  Units = "GW",
                  fuel = ifelse(grepl("CCS", technology), paste0(fuel, "_CCS"), fuel)) %>%
    dplyr::select(names(tech_GW_hydro)) %>%
    dplyr::bind_rows(tech_GW_hydro) %>%
    dplyr::bind_rows(tech_GW_geo) ->
    GW_activity


  # OUTPUT : GW_activity ----
  return(GW_activity)
}
