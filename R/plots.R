
#' PLOT_EF
#' @param state "state abbreviation" or null
#' @import ggplot2 dplyr tidyr
#' @return A plot of employment factor by fuel technology and state
#' @export
#'
#' @examples
#' PLOT_EF()
PLOT_EF <- function(state = NULL){

  SEC <- c("gas","gas_CCS",  "coal", "coal_CCS",
           "biomass", "biomass_CCS",
           "refined liquids","refined liquids_CCS",
           "wind","wind_offshore","CSP", "PV", "rooftop_pv",
           "nuclear", "hydro","geo")


  if (is.null(state)){
    EF.JEDI.RAW %>%
      select(region, unit, fuel, Construction.Period.Y, CON.L, CON.RL, OM, DECON) %>% # reorder columns
      gather(job, value, CON.L:DECON) %>%
      mutate(job = gsub("CON.L","Construction-onsite", job),
             job = gsub("CON.RL","Construction-related", job),
             job = gsub("OM","O&M", job),
             job = gsub("DECON","Decommissioning total", job),
             fuel2 = paste0(fuel, "(",Construction.Period.Y*12,")"),
             fuel2 = ifelse(job == "O&M", fuel, fuel2),
             fuel2 = ifelse(job == "Decommissioning total", paste0(fuel, "(",60,")"), fuel2),
             fuel = factor(fuel, levels = SEC)) %>%
      drop_na() %>%
      ggplot() +
      geom_point(ggplot2::aes(x = fuel2, y = value)) +
      facet_wrap(~ job, ncol = 4, scales = "free_x") +
      labs(x = "", y = "Annual jobs per MW",
           title = "Average annual jobs during the project period: USA states\n(project period in months by job type is in parentheses)") +
      theme_bw() + theme0 + theme1 %>% return()
  }
  else if (state %in% gcamusa.STATES){
    EF.JEDI.RAW %>%
      filter(region == state) %>%
      select(region, unit, fuel, Construction.Period.Y, CON.L, CON.RL, OM, DECON) %>% # reorder columns
      gather(job, value, CON.L:DECON) %>%
      mutate(job = gsub("CON.L","Construction-onsite", job),
             job = gsub("CON.RL","Construction-related", job),
             job = gsub("OM","O&M", job),
             job = gsub("DECON","Decommissioning total", job),
             fuel2 = paste0(fuel, "(",Construction.Period.Y*12,")"),
             fuel2 = ifelse(job == "O&M", fuel, fuel2),
             fuel2 = ifelse(job == "Decommissioning total", paste0(fuel, "(",60,")"), fuel2),
             fuel = factor(fuel, levels = SEC)) %>%
      drop_na() %>%
      ggplot() +
      geom_point(ggplot2::aes(x = fuel2, y = value)) +
      facet_wrap(~ job, ncol = 4, scales = "free_x") +
      labs(x = "", y = "Annual jobs per MW",
           title = paste0("Average annual jobs during the project period: ", state, "\n(project period in months by job type is in parentheses)")) +
      theme_bw() + theme0 + theme1 %>% return()
  }
  else {stop("Not a valid input, use a capitalized 2-letter state abbreviations or NULL")
  }
}



#' PLOT_GW
#'
#' @param GW_activity Output from GCAM_GW()
#' @param method "Net" or "Total"
#' @import ggplot2 dplyr tidyr
#'
#' @return A trend plot of capacity activity by fuel technology at the national level
#' @export
#'
#' @examples
#' PLOT_GW(GW_activity, "Net")
#' PLOT_GW(GW_activity, "Total")
PLOT_GW <- function(GW_activity, state = NULL, method = NULL){

  if (is.null(method)||method %in% c("Total", "total")){
    ACTIVITY <-  c("installed", "additions", "retirements")
  } else if (method %in% c("Net", "net")){
    ACTIVITY <- c("installed", "add_adj", "ret_adj")
  } else {stop("Not a valid input, use either 'Net' or 'Total'")
  }

  if (is.null(state)){
    GW_activity %>%
      filter(Year >= 2020) %>%
      filter(activity %in% ACTIVITY) %>%
      mutate(activity = ifelse(grepl("add", activity), "Newly installed", activity),
             activity = ifelse(grepl("ret", activity), "Retired", activity)) %>%
      group_by(scenario, Year, fuel, activity, Units) %>%
      summarise(value = sum(value, na.rm = T), .groups = "drop") %>%
      spread(activity, value) %>%
      mutate(across(contains("Newly installed"), ~ installed - .x, .names = "Previously Installed"),
             Retired = -Retired) %>%
      select(-installed) %>%
      gather(activity, value, `Newly installed`:`Previously Installed`) %>%
      drop_na() %>%
      ggplot() +
      geom_bar(aes(x = Year, y = value, fill = activity),
               stat = "identity", position = "stack") +
      facet_wrap(~ fuel, ncol = 4, scales = "free_y") +
      labs(x = "", y = "GW", title = paste0("USA ", method)) +
      theme_bw() + theme0 + theme1 +
      theme(legend.position = "bottom")  %>% return()
  }
  else if (state %in% gcamusa.STATES){
    GW_activity %>%
      filter(region == state) %>%
      filter(Year >= 2020) %>%
      filter(activity %in% ACTIVITY) %>%
      mutate(activity = ifelse(grepl("add", activity), "Newly installed", activity),
             activity = ifelse(grepl("ret", activity), "Retired", activity)) %>%
      group_by(scenario, Year, fuel, activity, Units) %>%
      summarise(value = sum(value, na.rm = T), .groups = "drop") %>%
      spread(activity, value) %>%
      mutate(across(contains("Newly installed"), ~ installed - .x, .names = "Previously Installed"),
             Retired = -Retired) %>%
      select(-installed) %>%
      gather(activity, value, `Newly installed`:`Previously Installed`) %>%
      drop_na() %>%
      ggplot() +
      geom_bar(aes(x = Year, y = value, fill = activity),
               stat = "identity", position = "stack") +
      facet_wrap(~ fuel, ncol = 4, scales = "free_y") +
      labs(x = "", y = "GW", title = paste0(state, " ", method)) +
      theme_bw() + theme0 + theme1 +
      theme(legend.position = "bottom") %>% return()
  }
  else {stop("Not a valid input, use a capitalized 2-letter state abbreviation or NULL")
  }
}



#' PLOT_JOB
#'
#' @param JOB_activity Output from GCAM_JOB()
#' @import ggplot2 dplyr
#'
#' @return A trend plot of job by fuel technology and job type at the national level
#' @export
#'
#' @examples
#' PLOT_JOB(JOB_activity)
PLOT_JOB <- function(JOB_activity, state = NULL){
  if(is.null(state)){
    JOB_activity %>%
      filter(!job %in% c("OM_fixed", "OM_var")) %>%
      filter(Year >= 2020) %>%
      group_by(scenario, Year, fuel, job, Units) %>%
      summarise(value = sum(value, na.rm = T), .groups = "drop") %>%
      drop_na() %>%
      ggplot() +
      geom_bar(aes(x = Year, y = value/1000, fill = job),
               stat = "identity", position = "stack") +
      facet_wrap(~ fuel, ncol = 4, scales = "free_y") +
      labs(x = "", y = "Thousand people", title = "USA") +
      scale_fill_manual(values = colors) +
      theme_bw() + theme0 + theme1 +
      theme(legend.position = "bottom") %>%
      return()
  }
  else if (state %in% gcamusa.STATES){
    JOB_activity %>%
      filter(region == state) %>%
      filter(!job %in% c("OM_fixed", "OM_var")) %>%
      filter(Year >= 2020) %>%
      group_by(scenario, Year, fuel, job, Units) %>%
      summarise(value = sum(value, na.rm = T), .groups = "drop") %>%
      drop_na() %>%
      ggplot() +
      geom_bar(aes(x = Year, y = value/1000, fill = job),
               stat = "identity", position = "stack") +
      facet_wrap(~ fuel, ncol = 4, scales = "free_y") +
      labs(x = "", y = "Thousand people", title = state) +
      scale_fill_manual(values = colors) +
      theme_bw() + theme0 + theme1 +
      theme(legend.position = "bottom") %>% return()
  }
  else {stop("Not a valid input, use a capitalized 2-letter state abbreviation or NULL")
  }
}


#' PLOT_JOB_TYPE
#'
#' @param JOB_activity Output of GCAM_JOB()
#' @import ggplot2 dplyr
#' @return A trend plot of job by job type at the national level
#' @export
#'
#' @examples
#' PLOT_JOB_TYPE(JOB_activity)
PLOT_JOB_TYPE <- function(JOB_activity, state = NULL){

  if (is.null(state)){
    JOB_activity %>%
      filter(Year >= 2020) %>%
      filter(!job %in% c("OM_fixed", "OM_var")) %>%
      group_by(scenario, Year, job, Units) %>%
      summarise(value = sum(value, na.rm = T), .groups = "drop") %>%
      drop_na() %>%
      ggplot() +
      geom_bar(aes(x = Year, y = value/10^6, fill = job),
               stat = "identity", position = "stack") +
      labs(x = "", y = "Million people", title = "USA") +
      scale_fill_manual(values = colors) +
      theme_bw() + theme0 + theme1 +
      theme(legend.position = "bottom") %>%
      return()
  }
  else if (state %in% gcamusa.STATES){
    JOB_activity %>%
      filter(region == state) %>%
      filter(Year >= 2020) %>%
      filter(!job %in% c("OM_fixed", "OM_var")) %>%
      group_by(scenario, Year, job, Units) %>%
      summarise(value = sum(value, na.rm = T), .groups = "drop") %>%
      drop_na() %>%
      ggplot() +
      geom_bar(aes(x = Year, y = value/10^3, fill = job),
               stat = "identity", position = "stack") +
      labs(x = "", y = "Thousand people", title = state) +
      scale_fill_manual(values = colors) +
      theme_bw() + theme0 + theme1 +
      theme(legend.position = "bottom") %>% return()
  }
  else {stop("Not a valid input, use a capitalized 2-letter state abbreviation or NULL")}
}


#' MAP_JOB
#'
#' @param JOB_activity Output of GCAM_JOB()
#' @param year Year of interest, a numeric
#' @import ggplot2 dplyr
#'
#' @return A map of state level total direct power sector job of a given year
#' @export
#'
#' @examples
#' MAP_JOB(JOB_activity, 2050)
MAP_JOB <- function(JOB_activity, year){

  JOB_activity %>%
    filter(!job %in% c("OM_fixed", "OM_var")) %>%
    filter(Year == year) %>%
    group_by(scenario, region, Year, Units) %>%
    summarise(value = sum(value, na.rm = T), .groups = "drop") ->
    job.state

  values <- job.state$value
  names(values) <- job.state$region
  df.map <- mapUS52Compact #Puerto Rico is not included
  df.map$value <- values[df.map$subRegion]

  df.map$value <- df.map$value / 1000
  df.map$bin <- ifelse(df.map$value <= 1, "(0, 1k]",
                       ifelse(df.map$value > 1 & df.map$value <= 5, "(1k, 5k]",
                              ifelse(df.map$value > 5 & df.map$value <= 10, "(5k, 10k]",
                                     ifelse(df.map$value > 10 & df.map$value <= 25, "(10k, 25k]",
                                            ifelse(df.map$value > 25 & df.map$value <= 50, "(25k, 50k]",
                                                   ifelse(df.map$value > 50 & df.map$value <= 75, "(50k, 75k]",
                                                          ifelse(df.map$value > 75 & df.map$value <= 100, "(75k, 100k]",
                                                                 ifelse(df.map$value > 100 & df.map$value <= 200, "(100k, 200k]",
                                                                        ifelse(df.map$value > 200 & df.map$value <= 300, "(200k, 300k]",
                                                                               ifelse(df.map$value > 300, "> 300k", NA_character_))))))))))


  df.map$bin <- factor(df.map$bin, levels = c("(0, 1k]", "(1k, 5k]", "(5k, 10k]",
                                              "(10k, 25k]", "(25k, 50k]", "(50k, 75k]",
                                              "(75k, 100k]", "(100k, 200k]", "(200k, 300k]",
                                              "> 300k"))
  df.map %>%
    ggplot() +
    geom_sf(ggplot2::aes(fill = bin)) +
    scale_fill_manual(values = map.colors) +
    # scale_fill_brewer(palette = "Blues")+
    geom_sf_text(aes(label = paste0(subRegion, ":\n", round(value,1))),  color = "black", size = 3) +
    labs(x = "", y = "", fill = "People", title = paste0("state-level power sector employment: ", year)) +
    theme_bw() + theme0 + theme1 %>%
    return()
}
