library(usethis)


read.my.data <- function(mydata, N) {
  # Get the path to the CSV file
  csv_file_path <- system.file("extdata", paste0(mydata,".csv"), package = "GOFREE")
  # Read the CSV file using read.csv or readr::read_csv
  data <- read.csv(csv_file_path, skip = N)
  return(data)
}

hydro_cf <- read.my.data("hydro_CF_USA", 3)

read.GCAM.csv <- function(basename, na.strings="") {
  csv_file_path <- system.file("extdata", paste0("GCAM/",basename,".csv"), package = "GOFREE")
  data <- read.csv(csv_file_path, na.strings=na.strings, stringsAsFactors=F, comment.char = "#")
  return(data)
}

elec_tech_water_map <- read.GCAM.csv("elec_tech_water_map") %>%
  dplyr::select(technology, to.technology)

A23.elecS_tech_mapping_cool <- read.GCAM.csv("A23.elecS_tech_mapping_cool")

# retirement assumptions
A23.globaltech_retirement <- read.GCAM.csv("A23.globaltech_retirement")
A23.elec_tech_coal_retire_SCurve <- read.GCAM.csv("A23.elec_tech_coal_retire_SCurve")
L2244.StubTechSCurve_nuc_gen2_USA <- read.GCAM.csv("L2244.StubTechSCurve_nuc_gen2_USA") %>%
  dplyr::select(region, subsector, lifetime, steepness, half.life) %>%
  dplyr::distinct()

s_curve_shutdown_global_final <- tibble::as_tibble(A23.globaltech_retirement) %>%
  dplyr::mutate(year = dplyr::case_when(year == "final-historical-year" ~ "final-calibration-year",
                          year == "initial-nonhistorical-year" ~ "initial-future-year",
                          T ~ year)) %>%
  dplyr::filter(grepl("final", year)) %>%
  dplyr::left_join(A23.elecS_tech_mapping_cool, by = c("supplysector", "subsector", "technology")) %>%
  dplyr::select(subsector = Electric.sector.technology,
         technology = to.technology,
         year, lifetime, shutdown.rate, half.life, steepness, median.shutdown.point, profit.shutdown.steepness)

s_curve_shutdown_global_initial <- tibble::as_tibble(A23.globaltech_retirement) %>%
  dplyr::mutate(year = dplyr::case_when(year == "final-historical-year" ~ "final-calibration-year",
                          year == "initial-nonhistorical-year" ~ "initial-future-year",
                          T ~ year)) %>%
  dplyr::filter(grepl("initial", year)) %>%
  dplyr::left_join(A23.elecS_tech_mapping_cool, by = c("supplysector", "subsector", "technology")) %>%
  dplyr::select(subsector = Electric.sector.technology,
         technology = to.technology,
         year, lifetime, shutdown.rate, half.life, steepness, median.shutdown.point, profit.shutdown.steepness)

s_curve_shutdown_global_final %>%
  dplyr::bind_rows(s_curve_shutdown_global_initial) ->
  s_curve_shutdown_global


# read in capital cost assumptions
cap_cost <- read.GCAM.csv("L2234.GlobalTechCapital_elecS_USA")
cap_cost_int <- read.GCAM.csv("L2234.GlobalIntTechCapital_elecS_USA")
cap_cost_coal_vintage <- read.GCAM.csv("L2241.GlobalTechCapital_coal_vintage_USA")
cap_cost_coal_retire <- read.GCAM.csv("L2241.GlobalTechCapital_elec_coalret_USA")


cap_cost_cool <- read.GCAM.csv("L2233.GlobalTechCapital_elecS_cool_USA")
cap_cost_int_cool_raw <- read.GCAM.csv("L2233.GlobalIntTechCapital_elecS_cool_USA")

cap_cost_int_cool <- cap_cost_int_cool_raw %>%
  dplyr::filter(grepl("CSP", subsector) & input.capital == "cooling system") %>%
  # recirculating = 10, dry_hybrid = 62
  dplyr::filter( (grepl("dry_hybrid", intermittent.technology) & capital.overnight == 62) |
            (grepl("recirculating", intermittent.technology) & capital.overnight == 10)) %>%
  dplyr::bind_rows(cap_cost_int_cool_raw %>%
                     dplyr::filter(!(grepl("CSP", subsector) & input.capital == "cooling system")))


elec_capital_cool <- rbind(cap_cost_cool %>%
                                    dplyr::rename(supplysector = sector.name,
                                                  subsector = subsector.name,
                                                  subsector0 = subsector.name0),
                           cap_cost_int_cool %>%
                             dplyr::rename(technology = intermittent.technology)) %>%
  dplyr::select(supplysector, subsector, technology, year, input.capital, capital.overnight) %>%
  dplyr::distinct() %>%
  dplyr::filter(input.capital != "capital") %>%
  dplyr::rename(capital.cooling = capital.overnight) %>%
  dplyr::select(subsector, technology, year, capital.cooling)

# technology capital
elec_capital_tech <- rbind(cap_cost,
                           cap_cost_coal_retire,
                           cap_cost_coal_vintage %>% dplyr::rename(supplysector = sector.name,
                                                            subsector = subsector.name),
                           cap_cost_int %>% dplyr::rename(technology = intermittent.technology)) %>%
  dplyr::select(supplysector, subsector = technology, year, input.capital, capital.overnight) %>%
  dplyr::distinct() %>%
  dplyr::rename(capital.tech = capital.overnight) %>%
  dplyr::select(subsector, year, capital.tech)

# combine together, now we have a full table of capital cost for all technologies
elec_capital <- elec_capital_tech %>%
  dplyr::left_join(elec_capital_cool, by = c("subsector", "year")) %>%
  tidyr::replace_na(list(capital.cooling = 0)) %>%
  dplyr::mutate(capital = capital.cooling + capital.tech) %>%
  dplyr::mutate(technology = ifelse(is.na(technology), subsector, technology)) %>%
  dplyr::select(subsector, technology, year, capital)

s_curve_shutdown_coal <- elec_capital %>%
  # first create a mapping file for coal vintage technologies
  dplyr::filter(subsector %in% A23.elec_tech_coal_retire_SCurve$Electric.sector.technology) %>%
  dplyr::select(subsector, technology) %>%
  dplyr::distinct() %>%
  dplyr::as_tibble() %>%
  left_join_error_no_match(A23.elec_tech_coal_retire_SCurve %>% dplyr::select(-Electric.sector),
                           by = c("subsector" = "Electric.sector.technology")) %>%
  dplyr::mutate(shutdown.rate = NA,
         median.shutdown.point = NA,
         profit.shutdown.steepness = NA) %>%
  repeat_add_columns(tibble::tibble(year = c("final-calibration-year", "initial-future-year")) ) %>%
  dplyr::select(names(s_curve_shutdown_global))

s_curve_shutdown <- s_curve_shutdown_global %>%
  dplyr::filter(!subsector %in% unique(s_curve_shutdown_coal$subsector)) %>%
  dplyr::bind_rows(s_curve_shutdown_coal) %>%
  dplyr::distinct()

# copy to all states, then adjust nuclear S-curve by states
s_curve_shutdown_states <- s_curve_shutdown %>%
  repeat_add_columns(tibble::tibble(region = gcamusa.STATES)) %>%
  # update nuclear state-level assumptions
  dplyr::left_join(L2244.StubTechSCurve_nuc_gen2_USA, by = c("region", "subsector")) %>%
  dplyr::mutate(lifetime = ifelse(is.na(lifetime.y), lifetime.x, lifetime.y),
         steepness = ifelse(is.na(steepness.y), steepness.x, steepness.y),
         half.life = ifelse(is.na(half.life.y), half.life.x, half.life.y)) %>%
  dplyr::select(region, names(s_curve_shutdown))


# O&M cost share: fixed vs var
L113.globaltech_OMvar_ATB <- read.GCAM.csv("L113.globaltech_OMvar_ATB") %>% dplyr::rename(OM = input.OM.var)
L113.globaltech_OMfixed_ATB <- read.GCAM.csv("L113.globaltech_OMfixed_ATB") %>% dplyr::rename(OM = input.OM.fixed)

L113.globaltech_OMvar_ATB %>%
  dplyr::bind_rows(L113.globaltech_OMfixed_ATB) %>%
  dplyr::select(supplysector, subsector, technology, OM, value = X2015) %>%
  tidyr::spread(OM, value) %>%
  dplyr::mutate(fixed = `OM-fixed`/(`OM-fixed` +`OM-var`),
         variable = `OM-var`/(`OM-fixed` +`OM-var`)) %>%
  #TODO: group by technology
  dplyr::group_by(fuel = subsector) %>%
  dplyr::summarise(fixed = mean(fixed),
            variable = mean(variable)) ->
  OM_share

use_data(OM_share, overwrite = TRUE)
use_data(s_curve_shutdown_states, overwrite = TRUE)
use_data(hydro_cf, overwrite = TRUE)
