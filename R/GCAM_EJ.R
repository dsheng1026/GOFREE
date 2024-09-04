
#' GCAM_EJ
#'
#' @param prj A list of queried outcome
#' This function takes +20 seconds for a single scenario
#'
#' @return A data frame with state-level power output from capacity activity
#' @export
#'
#' @examples
#' EJ_activity <- GCAM_EJ(prj)


GCAM_EJ <- function(prj){

  elec_input <- rgcam::getQuery(prj, 'elec energy input by elec gen tech by vintage') %>%
    dplyr::select(scenario, region, subsector = subsector...5, subsector.1 = subsector...6,
           technology, input, Year = year, value, Units) %>%
    dplyr::mutate(subsector = gsub(",depth=1", "", subsector.1)) %>%
    dplyr::select(-subsector.1) %>%
    tidyr::separate(technology, c("technology", "temp"), sep = ",") %>%
    tidyr::separate(temp, c("temp", "vintage"), sep = "=") %>%
    dplyr::select(-temp) %>%
    dplyr::mutate(vintage = as.numeric(vintage)) %>%
    dplyr::filter(vintage >= 2015,
                  Year >= vintage)

  ## conventional ----
  elec_vintage0 <- rgcam::getQuery(prj, 'elec gen by gen tech and cooling tech (incl cogen) by vintage') %>%
    dplyr::select(scenario, region, subsector = subsector...5, subsector.1 = subsector...6,
           technology, Year = year, value, Units) %>%
    dplyr::mutate(subsector = gsub(",depth=1", "", subsector.1)) %>%
    dplyr::group_by(scenario, region, subsector, technology, Year, Units) %>%
    dplyr::summarise(value = sum(value), .groups = "drop") %>%
    tidyr::separate(technology, c("technology", "temp"), sep = ",") %>%
    tidyr::separate(temp, c("temp", "vintage"), sep = "=") %>%
    dplyr::select(-temp) %>%
    dplyr::mutate(vintage = as.numeric(vintage)) %>%
    dplyr::filter(vintage >= 2015,
                  Year >= vintage)

  YEAR_RANGE <- sort(unique(elec_vintage0$Year))

  ## renewable resource ----
  # annual, by subsector, technology and vintage, EJ

  ### elec_gen & vintage calculation for rooftop_PV ----

  # use the input to query correct rooftop PV elec_gen output
  elec_input %>%
    dplyr::filter(grepl("rooftop", subsector)) %>%
    dplyr::filter(input !="backup_electricity") %>%
    dplyr::group_by(scenario, region, subsector, technology, input, Year, Units) %>%
    dplyr::summarise(value = sum(value), .groups = "drop") ->
    elec_input_RTPV

  YEAR_RANGE_RTPV <- sort(unique(elec_input_RTPV$Year))

  # starting Year
  A <- min(YEAR_RANGE_RTPV)
  # set lifetime of rooftop = 20 years
  RTPVLT <- 20

  elec_input_RTPV %>%
    dplyr::group_by(scenario, region, subsector, technology) %>%
    dplyr::mutate(Vin2020 = value[Year == A],
                  Vin2020 = ifelse(Year >= A & Year < A + RTPVLT, Vin2020[Year == A], 0 )) %>%
    dplyr::ungroup() ->
    df
  N <- length(unique(df$Year))

  for (i in 2:N) {
    TIME = A + (i-1)*5;
    df %>%
      dplyr::mutate(!!paste0("Vin", TIME) := ifelse(value - rowSums(dplyr::across(tidyr::starts_with("Vin"))) > 0, # only add new vintage capacity if older vintage is not enough
                                                    value - rowSums(dplyr::across(tidyr::starts_with("Vin"))),
                                                    0)) -> # if previous vintage is enough, then 0 new vintage
      df
    df %>%
      dplyr::group_by(scenario, region, subsector, technology) %>%
      dplyr::mutate(!!paste0("Vin", TIME) := ifelse(Year >= TIME & Year < TIME + RTPVLT, .data[[paste0("Vin", TIME)]][Year == TIME], 0)) ->
      df
  }
  df %>%
    dplyr::rename(running = value) %>%
    tidyr::gather(vintage, value, c(starts_with("Vin"))) %>%
    dplyr::mutate(vintage = gsub("Vin", "", vintage),
                  vintage = as.numeric(vintage)) %>%
    dplyr::select(names(elec_vintage0)) %>%
    dplyr::filter(Year >= vintage) ->
    elec_gen_RTPV_vintage

  ### elec_gen calculation for PV and CSP ----
  elec_input %>%
    dplyr::filter(grepl("PV", subsector)|grepl("CSP", subsector)) %>%
    dplyr::filter(input !="backup_electricity") %>%
    dplyr::group_by(scenario, region, subsector, technology, vintage, Year, Units) %>%
    dplyr::summarise(value = sum(value), .groups = "drop") %>%
    dplyr::select(names(elec_vintage0)) %>%
    dplyr::filter(Year >= vintage) ->
    elec_gen_SOLAR_vintage

  ### elec_gen calculation for wind ----
  elec_input %>%
    dplyr::filter(grepl("wind", subsector)) %>%
    dplyr::filter(input !="backup_electricity") %>% dplyr::group_by(scenario, region, subsector, technology, vintage, Year, Units) %>%
    dplyr::summarise(value = sum(value), .groups = "drop") %>%
    dplyr::select(names(elec_vintage0)) %>%
    dplyr::filter(Year >= vintage) ->
    elec_gen_WIND_vintage


  # combine elec_gen by vintage across fuel types  ----
  # solar and wind are calculated separately for the backup_elec adjustment
  # rooftop is calculated separately for the backup_elec adjustment and vintage structure

  elec_vintage0 %>%
    dplyr::filter(!grepl("CSP", subsector)) %>% # solar calculated separately
    dplyr::filter(!grepl("PV", subsector)) %>% # solar calculated separately
    dplyr::filter(!grepl("rooftop", subsector)) %>% # rooftop pv calculated and vintaged separately
    dplyr::filter(!grepl("wind", subsector)) %>% # wind calculated separately
    dplyr::bind_rows(elec_gen_RTPV_vintage) %>% # add rooftop PV elec_vintage
    dplyr::bind_rows(elec_gen_SOLAR_vintage) %>% # add rooftop CSP and PV elec_vintage
    dplyr::bind_rows(elec_gen_WIND_vintage) -> # add rooftop wind elec_vintage
    elec_vintage
  ##  elec_gen from installed capacity ----

  elec_vintage %>%
    dplyr::filter(Year >= vintage) %>%
    dplyr::arrange(scenario, region, subsector, technology, vintage, desc(Year)) %>%
    dplyr::group_by(scenario, region, subsector, technology, vintage) %>%
    dplyr::mutate(real_cap = cummax(value)) %>% # use max of future operating capacity as the real capacity
    dplyr::arrange(scenario, region, subsector, technology, vintage, Year) %>%
    dplyr::ungroup() ->
    elec_vintage_real

  # Calculate s-curve output fraction: GCAM 7.1 ----

  elec_vintage_real %>%
    dplyr::left_join(SCurve,
              by = c("region", "subsector", "technology", "vintage")) %>%
    dplyr::mutate(half.life = as.numeric(half.life),
           steepness = as.numeric(steepness),
           half.life = ifelse(is.na(half.life), 0, half.life),
           steepness = ifelse(is.na(steepness), 0, steepness),
           s_curve_frac = ifelse(Year > vintage & half.life != 0,
                                  (1 / (1 + exp( steepness * ((Year - vintage) - half.life )))),
                                  1)) %>%
    dplyr::distinct() ->
    s_curve_frac


  # Adjust s-curve output fraction to ensure that all of the capacity is retired at the end of lifetime
  s_curve_frac %>%
    dplyr::mutate(s_curve_adj = dplyr::if_else(Year - vintage >= lifetime, 0, s_curve_frac),
                  s_curve_adj = dplyr::if_else(is.na(s_curve_adj), 1, s_curve_adj)) %>%
    dplyr::select(scenario, region, subsector, technology, vintage, Units, Year, s_curve_adj) ->
    s_curve_frac_adj

  s_curve_frac_adj %>%
    dplyr::select(-Year, -s_curve_adj) %>% dplyr::distinct() %>%
    repeat_add_columns(tibble::tibble(Year = YEAR_RANGE)) %>% # expand the data to include years w/o production
    dplyr::left_join(s_curve_frac_adj,
              by = c("scenario", "region", "subsector", "technology", "vintage", "Units", "Year")) %>%
    tidyr::replace_na(list(s_curve_adj = 0)) %>%
    # assume hydro does not retire
    dplyr::mutate(s_curve_adj = ifelse(grepl("hydro", subsector), 1, s_curve_adj),
           # assume rooftop_PV has a 20 years lifetime, so s-curve-frac = 0 in year vintage + 20
           s_curve_adj = ifelse(grepl("rooftop", subsector), 0, s_curve_adj),
           s_curve_adj = ifelse(grepl("rooftop", subsector) & (Year < vintage + 20), 1, s_curve_adj)) %>%
    dplyr::filter(Year >= vintage) ->
    s_curve_frac_adj_full

  ### elec_gen from newly installed capacity ----
  elec_vintage_real %>%
    dplyr::filter(!grepl("geo", subsector)) %>%
    dplyr::mutate(additions = dplyr::if_else(vintage == Year, value, 0),
                  additions = dplyr::if_else(grepl("hydro", subsector), 0, additions)) ->
    elec_vintage_add_nogeo

  elec_vintage_add_nogeo %>%
    dplyr::group_by(scenario, region, subsector, technology, Units, Year) %>%
    dplyr::summarise(additions = sum(additions), .groups = "drop") ->
    elec_total_add_nogeo

  ### elec_gen from retired capacity ----

  s_curve_frac_adj_full %>%
    dplyr::left_join(elec_vintage_real %>%
                       dplyr::filter(!grepl("geo", subsector)) %>% # calculate additions and retirement of geothermal separately
                       dplyr::filter(vintage == Year) %>%
                       dplyr::select(-Year, -value) %>%
                       dplyr::rename(OG_gen = real_cap),
                     by = c("scenario", "region", "subsector", "technology", "vintage", "Units")) %>%
    dplyr::mutate(gen_expect = OG_gen * s_curve_adj) ->
    elec_gen_expect

  elec_gen_expect %>%
    dplyr::group_by(scenario, region, subsector, technology, Units, vintage) %>%
    dplyr::mutate(prev_yr_expect = dplyr::lag(gen_expect, n = 1L),
                  natural_retire = dplyr::if_else(Year > vintage & prev_yr_expect > gen_expect,
                                                  prev_yr_expect - gen_expect, 0)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-s_curve_adj, -OG_gen, -gen_expect, -prev_yr_expect) ->
    elec_retire_expect

  elec_vintage_real %>%
    dplyr::filter(!grepl("geo", subsector)) %>% # calculate additions and retirement of geothermal separately
    dplyr::select(-Year, -value, -real_cap) %>% dplyr::distinct() %>%
    repeat_add_columns(tibble::tibble(Year = YEAR_RANGE)) %>%  # expand the data to include years w/o production
    dplyr::left_join(elec_vintage_real,
                     by = c("scenario", "region", "subsector", "technology", "vintage", "Units", "Year")) %>%
    tidyr::replace_na(list(real_cap = 0)) %>%  # NA means 0 production from a vintage in this year
    dplyr::filter(Year >= vintage) %>%
    dplyr::arrange(scenario, region, subsector, technology, vintage, Units, Year) %>%
    dplyr::group_by(scenario, region, subsector, technology, Units, vintage) %>%
    dplyr::mutate(prev_year = dplyr::lag(real_cap, n = 1L)) %>%
    dplyr::mutate(retirements = ifelse(is.na(prev_year), 0, prev_year - real_cap),
                  retirements = ifelse(retirements < 0, 0, retirements),
                  retirements = ifelse(grepl("hydro", subsector), 0, retirements)) %>%
    dplyr::arrange(vintage, technology, region) %>%
    dplyr::ungroup() ->
    elec_vintage_ret

  elec_vintage_ret %>%
    dplyr::select(-value, - real_cap, -prev_year) %>%
    left_join_error_no_match(elec_retire_expect,
                             by = c("scenario", "region", "subsector", "technology", "vintage", "Units", "Year")) %>%
    # when there is early retirement in previous periods, the expected natural retirement when reaching lifetime can be larger than the observed retirement
    # therefore, adjust the natural retirement of each period, cap by the observed retirement
    dplyr::mutate(natural_retire = ifelse(natural_retire > retirements, retirements, natural_retire),
           early_retire = retirements - natural_retire) ->
    elec_retire_all_nogeo

  # Total retirements per region/ technology/ year (in EJ)
  elec_retire_all_nogeo %>%
    dplyr::group_by(scenario, region, subsector, technology, Units, Year) %>%
    dplyr::summarise(retirements = sum(retirements),
                     natural_retire = sum(natural_retire),
                     early_retire = sum(early_retire),
                     .groups = "drop") ->
    elec_total_ret_nogeo

  # Adjusted (Net) additions and retirements
  # The key question here is do we allow capacity addition and retirement to happen in the same year for a technology in a region
  # Natural retirement is not affected by this adjustment
  # If not allowed:
  # when early retirement is larger than addition, then net retire
  # when early retirement is smaller than addition, then net add
  # Merge total additions and retirements data tables
  elec_total_ret_nogeo %>%
    dplyr::left_join(elec_total_add_nogeo, by = c("scenario", "region", "subsector", "technology", "Units", "Year")) %>%
    tidyr::replace_na(list(additions = 0)) %>%  #
    dplyr::mutate(add_adj = dplyr::if_else(additions >= early_retire, additions - early_retire, 0),
                  early_ret_adj = dplyr::if_else(early_retire > additions, early_retire - additions, 0),
                  ret_adj = early_ret_adj + natural_retire) -> # add the natural retire to get the updated total ret
    elec_add_ret_nogeo

  ## elec_gen activities of geothermal ----

  elec_vintage %>%
    dplyr::filter(Year >= vintage) %>%
    dplyr::filter(grepl("geo", subsector)) %>%
    dplyr::arrange(scenario, region, subsector, technology, vintage, desc(Year)) %>%
    dplyr::group_by(scenario, region, subsector, technology) %>%
    dplyr::mutate(real_cap = cummax(value)) %>% # use max of future operating capacity as the real capacity
    dplyr::arrange(scenario, region, subsector, technology, Year) %>%
    dplyr::mutate(additions = ifelse(Year %in% c(2015, 2045, 2075), real_cap, 0),
                  retirements = ifelse(Year %in% c(2045, 2075), real_cap, 0),
                  natural_retire = ifelse(Year %in% c(2045, 2075), real_cap, 0),
                  early_retire = 0,
                  add_adj = additions,
                  early_ret_adj = early_retire,
                  ret_adj = retirements) %>%
    dplyr::select(names(elec_add_ret_nogeo)) ->
    elec_add_ret_geo

  elec_add_ret_nogeo %>%
    dplyr::bind_rows(elec_add_ret_geo) ->
    elec_add_ret

  elec_vintage %>%
    dplyr::group_by(scenario, region, subsector, technology, Year, Units) %>%
    dplyr::summarise(value = sum(value, na.rm = T), .groups = "drop") %>%
    dplyr::mutate(activity = "running") %>%
    dplyr::bind_rows(elec_vintage_real %>%
                       dplyr::group_by(scenario, region, subsector, technology, Year, Units) %>%
                       dplyr::summarise(value = sum(real_cap, na.rm = T), .groups = "drop") %>%
                       dplyr::mutate(activity = "installed")) %>%
    dplyr::bind_rows(elec_add_ret %>% tidyr::gather(activity, value, retirements:ret_adj)) ->
    elec_activity
  # OUTPUT : elec_activity ----
  return(elec_activity)
}
