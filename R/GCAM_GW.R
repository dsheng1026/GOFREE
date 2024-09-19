
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

  elec_gen_activity %>%
     filter(grepl("hydro", subsector)) %>%
     mutate(value = (value / GCAMUSAJobs::CONV_GWH_EJ) / (8760 * capacity.factor), # EJ to GW
                  Units = "GW",
                  fuel = ifelse(grepl("CCS", technology), paste0(fuel, "_CCS"), fuel)) ->
    tech_GW_hydro

  elec_gen_activity %>%
     filter(grepl("geo", subsector)) %>%
     mutate(value = (value / GCAMUSAJobs::CONV_GWH_EJ) / (8760 * capacity.factor), # EJ to GW
                  Units = "GW",
                  fuel = ifelse(grepl("CCS", technology), paste0(fuel, "_CCS"), fuel)) ->
    tech_GW_geo

  elec_gen_activity %>%
     filter(!grepl("hydro", subsector)) %>%
     filter(!grepl("geo", subsector)) %>%
     mutate(technology = gsub(" ", "", technology)) %>%
     mutate(value = (value / GCAMUSAJobs::CONV_GWH_EJ) / (8760 * capacity.factor), # EJ to GW
            Units = "GW",
            fuel = ifelse(grepl("CCS", technology), paste0(fuel, "_CCS"), fuel)) %>%
     select(names(tech_GW_hydro)) %>%
     bind_rows(tech_GW_hydro) %>%
     bind_rows(tech_GW_geo) ->
    GW_activity


  # OUTPUT : GW_activity ----
  return(GW_activity)
}
