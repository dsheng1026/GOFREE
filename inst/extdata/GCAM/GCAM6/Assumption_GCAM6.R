# GCAM assumptions ----

read.GCAM.csv <- function(basename, na.strings="") {
  pathname <- file.path('Assumptions/GCAM', paste0(basename, ".csv"))
  return( read.csv(pathname, na.strings=na.strings, stringsAsFactors=F, comment.char = "#") )
}

elec_tech_water_map <- read.GCAM.csv("elec_tech_water_map") %>% 
  select(technology, to.technology)

A23.elecS_tech_mapping_cool <- read.GCAM.csv("A23.elecS_tech_mapping_cool") 

# retirement assumptions
A23.globaltech_retirement <- read.GCAM.csv("A23.globaltech_retirement")
A23.elec_tech_coal_retire_SCurve <- read.GCAM.csv("A23.elec_tech_coal_retire_SCurve")
L2244.StubTechSCurve_nuc_gen2_USA <- read.GCAM.csv("L2244.StubTechSCurve_nuc_gen2_USA") %>% 
  select(region, subsector, lifetime, steepness, half.life) %>% 
  distinct()

s_curve_shutdown_global_final <- tibble::as_tibble(A23.globaltech_retirement) %>%
  mutate(year = case_when(year == "final-historical-year" ~ "final-calibration-year",
                          year == "initial-nonhistorical-year" ~ "initial-future-year",
                          T ~ year)) %>% 
  filter(grepl("final", year)) %>% 
  left_join(A23.elecS_tech_mapping_cool, by = c("supplysector", "subsector", "technology")) %>% 
  select(subsector = Electric.sector.technology,
         technology = to.technology,
         year, lifetime, shutdown.rate, half.life, steepness, median.shutdown.point, profit.shutdown.steepness)

s_curve_shutdown_global_initial <- tibble::as_tibble(A23.globaltech_retirement) %>%
  mutate(year = case_when(year == "final-historical-year" ~ "final-calibration-year",
                          year == "initial-nonhistorical-year" ~ "initial-future-year",
                          T ~ year)) %>% 
  filter(grepl("initial", year)) %>% 
  left_join(A23.elecS_tech_mapping_cool, by = c("supplysector", "subsector", "technology")) %>% 
  select(subsector = Electric.sector.technology,
         technology = to.technology,
         year, lifetime, shutdown.rate, half.life, steepness, median.shutdown.point, profit.shutdown.steepness)

s_curve_shutdown_global_final %>% 
  bind_rows(s_curve_shutdown_global_initial) ->
  s_curve_shutdown_global


# read in capital cost assumptions
cap_cost <- read.GCAM.csv("L2234.GlobalTechCapital_elecS_USA")
cap_cost_int <- read.GCAM.csv("L2234.GlobalIntTechCapital_elecS_USA")
cap_cost_coal_vintage <- read.GCAM.csv("L2241.GlobalTechCapital_coal_vintage_USA")
cap_cost_coal_retire <- read.GCAM.csv("L2241.GlobalTechCapital_elec_coalret_USA")


cap_cost_cool <- read.GCAM.csv("L2233.GlobalTechCapital_elecS_cool_USA")
cap_cost_int_cool_raw <- read.GCAM.csv("L2233.GlobalIntTechCapital_elecS_cool_USA")

cap_cost_int_cool <- cap_cost_int_cool_raw %>% 
  filter(grepl("CSP", subsector) & input.capital == "cooling system") %>% 
  # recirculating = 10, dry_hybrid = 62 
  filter( (grepl("dry_hybrid", intermittent.technology) & capital.overnight == 62) |
            (grepl("recirculating", intermittent.technology) & capital.overnight == 10)) %>% 
  bind_rows(cap_cost_int_cool_raw %>% 
              filter(!(grepl("CSP", subsector) & input.capital == "cooling system")))


elec_capital_cool <- rbind(cap_cost_cool %>% rename(supplysector = sector.name,
                                                    subsector = subsector.name,
                                                    subsector0 = subsector.name0), 
                           cap_cost_int_cool %>% rename(technology = intermittent.technology)) %>% 
  select(supplysector, subsector, technology, year, input.capital, capital.overnight) %>% 
  distinct() %>% 
  filter(input.capital != "capital") %>% 
  rename(capital.cooling = capital.overnight) %>% 
  select(subsector, technology, year, capital.cooling)

# technology capital
elec_capital_tech <- rbind(cap_cost, 
                           cap_cost_coal_retire,
                           cap_cost_coal_vintage %>% rename(supplysector = sector.name,
                                                            subsector = subsector.name),
                           cap_cost_int %>% rename(technology = intermittent.technology)) %>% 
  select(supplysector, subsector = technology, year, input.capital, capital.overnight) %>% 
  distinct() %>% 
  rename(capital.tech = capital.overnight) %>% 
  select(subsector, year, capital.tech)

# combine together, now we have a full table of capital cost for all technologies
elec_capital <- elec_capital_tech %>% 
  left_join(elec_capital_cool, by = c("subsector", "year")) %>% 
  replace_na(list(capital.cooling = 0)) %>% 
  mutate(capital = capital.cooling + capital.tech) %>% 
  mutate(technology = ifelse(is.na(technology), subsector, technology)) %>% 
  select(subsector, technology, year, capital)

s_curve_shutdown_coal <- elec_capital %>% 
  # first create a mapping file for coal vintage technologies
  filter(subsector %in% A23.elec_tech_coal_retire_SCurve$Electric.sector.technology) %>% 
  select(subsector, technology) %>% 
  distinct() %>% 
  as_tibble() %>% 
  left_join_error_no_match(A23.elec_tech_coal_retire_SCurve %>% select(-Electric.sector),
                           by = c("subsector" = "Electric.sector.technology")) %>% 
  mutate(shutdown.rate = NA,
         median.shutdown.point = NA,
         profit.shutdown.steepness = NA) %>%
  repeat_add_columns(tibble::tibble(year = c("final-calibration-year", "initial-future-year")) ) %>% 
  select(names(s_curve_shutdown_global))

s_curve_shutdown <- s_curve_shutdown_global %>% 
  filter(!subsector %in% unique(s_curve_shutdown_coal$subsector)) %>% 
  bind_rows(s_curve_shutdown_coal) %>% 
  distinct()

# copy to all states, then adjust nuclear S-curve by states
s_curve_shutdown_states <- s_curve_shutdown %>%
  repeat_add_columns(tibble::tibble(region = gcamdata:::gcamusa.STATES)) %>% 
  # update nuclear state-level assumptions
  left_join(L2244.StubTechSCurve_nuc_gen2_USA, by = c("region", "subsector")) %>% 
  mutate(lifetime = ifelse(is.na(lifetime.y), lifetime.x, lifetime.y),
         steepness = ifelse(is.na(steepness.y), steepness.x, steepness.y),
         half.life = ifelse(is.na(half.life.y), half.life.x, half.life.y)) %>% 
  select(region, names(s_curve_shutdown))


# O&M cost share: fixed vs var
L113.globaltech_OMvar_ATB <- read.GCAM.csv("L113.globaltech_OMvar_ATB") %>% rename(OM = input.OM.var)
L113.globaltech_OMfixed_ATB <- read.GCAM.csv("L113.globaltech_OMfixed_ATB") %>% rename(OM = input.OM.fixed)

L113.globaltech_OMvar_ATB %>% 
  bind_rows(L113.globaltech_OMfixed_ATB) %>% 
  select(supplysector, subsector, technology, OM, value = X2015) %>% 
  spread(OM, value) %>% 
  mutate(fixed = `OM-fixed`/(`OM-fixed` +`OM-var`),
         variable = `OM-var`/(`OM-fixed` +`OM-var`)) %>% 
  #TODO: group by technology
  group_by(fuel = subsector) %>% 
  summarise(fixed = mean(fixed),
            variable = mean(variable)) ->
  OM_share
