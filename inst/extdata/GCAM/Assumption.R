

# Constants ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

CONV_EJ_KWH <- 277777777777.78

convGW_MW <- 1e3
convMW_kW <- 1e3
convGW_kW <- 1e6

convEJ2TWh<-277.77777777778
convEJ2GW<-convEJ2TWh*1000/8760
convEJ2GWh <- 277777.778

GCAM_model_step <- 5

# Capacity factors --------------------------------------------------------------------------------------------------------------
# Data source for capacity factor:
# https://hub.globalccsinstitute.com/publications/renewable-power-generation-costs-2012-overview/52-capacity-factors-hydropower
# Data source for hydropower capital cost
# https://hub.globalccsinstitute.com/publications/renewable-power-generation-costs-2012-overview/51-hydropower-capital-costs

#TODO: hydro and geothermal capacity factor for USA or USA states

# used in plutus
# hydro_cap_fact <- 0.38 

# https://www.irena.org/-/media/Files/IRENA/Agency/Publication/2012/RE_Technologies_Cost_Analysis-HYDROPOWER.pdf
# P16. range from 23% to 95%, average is 0.5
# P6, key findings
# large hydro Cap.Fac.(%) ranges from 25 to 90
# small hydro Cap.Fac.(%) ranges from 20 to 95

# P16 
# One option: a high installed capacity and low capacity factor to provide electricity predominantly, nonbase
# Alternate: a low installed capacity and high capacity factors less flexibility, base

# hydro_cf <- 0.9 # upper 
# hydro_cf <- 0.5 # average

# https://www.ipcc.ch/site/assets/uploads/2018/03/Chapter-5-Hydropower-1.pdf
# P10 
# capacity factor average for North_Am 47%
# hydro_cf <- 0.47

# Hall et al., 2003 
# Estimation of economic parameters of U.S. hydropower resources
# https://www.osti.gov/servlets/purl/1218138
# USA ranges from 41%-61%
# Table 8 has the state level monthly capacity factor (exported as csv, see Assumptions/hydro_CF_USA.csv)

hydro_cf <- read.csv("Assumptions/hydro_CF_USA.csv", skip = 3, header = T) %>% 
  select(region = Abbr, value = Annual) %>% 
  mutate(fuel = "hydro")

# https://www.eia.gov/todayinenergy/detail.php?id=42036
# "Geothermal capacity factors in the United States averaged 76% in 2018"
geo_cf <- 0.76 

# another source of hydrogen, solar and wind capacity factor in the U.S.
#https://energy.mit.edu/wp-content/uploads/2023/04/MITEI-WP-2023-02.pdf


# Decommission ----
#TODO: gather non JEDI job data into a table
# currently use construction EF (JEDI based) as decom EF
# decomm.EF <- 1.65 # job-yrs/MW
# use the construction EF from JEDI 

# backup electricity capacity factor
backup_cf <- 0.05 # assumption in GCAM


read.GCAM.csv
# GCAM7.1 USA s-curve assumption

A23.globaltech_retirement <- read.GCAM.csv("A23.globaltech_retirement")
A23.elecS_tech_mapping_cool <- read.GCAM.csv("A23.elecS_tech_mapping_cool")

L2233.StubTechSCurve_elecS_cool_USA <- read.GCAM.csv("L2233.StubTechSCurve_elecS_cool_USA") %>% 
  dplyr::select(-X) %>% 
  tidyr::as_tibble()

L2233.GlobalTechSCurve_elecS_cool_USA <- read.GCAM.csv("L2233.GlobalTechSCurve_elecS_cool_USA") %>% 
  dplyr::select(-X) %>% 
  dplyr::filter(subsector.name != "battery") %>% 
  tidyr::as_tibble()

L2233.GlobalTechSCurve_elecS_cool_USA %>% 
  gcamdata::repeat_add_columns(tibble::tibble(region = gcamdata:::gcamusa.STATES)) %>% 
  dplyr::rename(subsector0 = subsector.name0, subsector = subsector.name) %>% 
  dplyr::full_join(L2233.StubTechSCurve_elecS_cool_USA %>% dplyr::rename(technology = stub.technology),
            by = c("region", "supplysector", "subsector0", "subsector", "year", "technology")) %>% 
  # use info from L2233.StubTechSCurve_elecS_cool_USA when available 
  dplyr::mutate(lifetime = ifelse(is.na(lifetime.y), lifetime.x, lifetime.y),  
                steepness = ifelse(is.na(steepness.y), steepness.x, steepness.y),
                half.life = ifelse(is.na(half.life.y), half.life.x, half.life.y)) %>% 
  dplyr::select(-lifetime.x, -steepness.x, -half.life.x, -lifetime.y, -steepness.y, -half.life.y, 
                -supplysector, -subsector0) ->
  SCurve_baseyear

# for non-base year vintage, natural retirement happens when reaching lifetime
# and assign NA for steepness and half.life for future calculation

SCurve_future <- tibble::as_tibble(A23.globaltech_retirement) %>%
  dplyr::mutate(year = dplyr::case_when(year == "final-historical-year" ~ "final-calibration-year",
                                        year == "initial-nonhistorical-year" ~ "initial-future-year",
                                        T ~ year)) %>% 
  dplyr::filter(grepl("initial", year)) %>% 
  dplyr::left_join(A23.elecS_tech_mapping_cool, by = c("supplysector", "subsector", "technology")) %>% 
  dplyr::select(subsector = Electric.sector.technology, 
                technology = to.technology,
                lifetime, half.life, steepness) %>% 
  gcamdata::repeat_add_columns(tibble::tibble(region = gcamdata:::gcamusa.STATES)) %>% 
  gcamdata::repeat_add_columns(tibble::tibble(year = gcamdata:::MODEL_FUTURE_YEARS)) 
  

SCurve_baseyear %>% 
  dplyr::bind_rows(SCurve_future) %>% 
  dplyr::rename(vintage = year) ->
  SCurve

# GCAM7.1 USA O&M assumption

# Electricity Load Segments Technology Fixed OM Costs (1975$/kW/yr)
L2233.GlobalTechOMfixed_elecS_cool_USA <- read.GCAM.csv("L2233.GlobalTechOMfixed_elecS_cool_USA") %>% 
  dplyr::select(-X)
L2233.GlobalIntTechOMfixed_elecS_cool_USA <- read.GCAM.csv("L2233.GlobalIntTechOMfixed_elecS_cool_USA") %>% 
  dplyr::select(-X)

# Electricity Load Segments Technology Variable OM Costs (1975$/MWh)
L2233.GlobalTechOMvar_elecS_cool_USA <- read.GCAM.csv("L2233.GlobalTechOMvar_elecS_cool_USA") %>% 
  dplyr::select(-X)
L2233.GlobalIntTechOMvar_elecS_cool_USA <- read.GCAM.csv("L2233.GlobalIntTechOMvar_elecS_cool_USA") %>% 
  dplyr::select(-X)

L2233.GlobalTechOMvar_elecS_cool_USA %>% 
  bind_rows(L2233.GlobalIntTechOMvar_elecS_cool_USA %>% rename(technology = intermittent.technology)) %>% 
  dplyr::mutate(unit = "1975$/MWh",
                technology = gsub(" ", "", technology)) %>% 
  dplyr::rename(value = OM.var,
                input = input.OM.var) ->
  OM_var 


L2233.GlobalTechOMfixed_elecS_cool_USA %>% 
  bind_rows(L2233.GlobalIntTechOMfixed_elecS_cool_USA %>% rename(technology = intermittent.technology)) %>% 
  dplyr::mutate(unit = "1975$/kW/yr",
                technology = gsub(" ", "", technology)) %>% 
  dplyr::rename(value = OM.fixed,
                input = input.OM.fixed) ->
  OM_fix

# CSP technology OM_var info does not have further technology information
# include more detailed technology-level info , in later versions of GCAM, this might change when the assumption is better specified

OM_fix %>% 
  dplyr::bind_rows(OM_var) %>% 
  dplyr::select(-unit) %>% spread(input, value) %>% 
  dplyr::group_by(sector.name, subsector.name0, subsector.name, year) %>% 
  dplyr::mutate(`OM-fixed` = ifelse(is.na(`OM-fixed`), mean(`OM-fixed`, na.rm = TRUE), `OM-fixed`),
                `OM-var` = ifelse(is.na(`OM-var`), mean(`OM-var`, na.rm = TRUE), `OM-var`)) ->
  # tidyr::gather(input, value, `OM-fixed`:`OM-var`)
  # dplyr::mutate(unit = ifelse(input == "OM-fixed", "1975$/kW/yr", "1975$/MWh")) ->
  OM_cost

# save(OM_cost, file = "OM_cost.rda")
# save(SCurve, file = "SCurve.rda")
# save(hydro_cf, file = "hydro_cf.rda")

# Load map file for plotting ----
load("mapUS52Compact.rda")