
#' PLOT_EF
#'
#' @return A plot of employment factor by fuel technology and state
#' @export
#'
#' @examples
#' PLOT_EF()
PLOT_EF <- function(){
  SEC <- c("gas","gas_CCS",  "coal", "coal_CCS",
           "biomass", "biomass_CCS",
           "refined liquids","refined liquids_CCS",
           "wind","wind_offshore","CSP", "PV", "rooftop_pv",
           "nuclear", "hydro","geo")
  GOFREE::EF.JEDI.RAW %>%
    dplyr::select(region, unit, fuel, Construction.Period.Y, CON.L, CON.RL, OM, DECON) %>% # reorder columns
    tidyr::gather(job, value, CON.L:DECON) %>%
    dplyr::mutate(job = gsub("CON.L","Construction-onsite", job),
           job = gsub("CON.RL","Construction-related", job),
           job = gsub("OM","O&M", job),
           job = gsub("DECON","Decommissioning total", job),
           fuel2 = paste0(fuel, "(",Construction.Period.Y*12,")"),
           fuel2 = ifelse(job == "O&M", fuel, fuel2),
           fuel2 = ifelse(job == "Decommissioning total", paste0(fuel, "(",60,")"), fuel2),
           fuel = factor(fuel, levels = SEC)) %>%
    ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(x = fuel2, y = value)) +
    ggplot2::facet_wrap(~ job, ncol = 4, scales = "free_x") +
    ggplot2::labs(x = "", y = "Annual jobs per MW",
                  title = "Average annual job during the practice period\n(practice period in month by job type is in parentheses)") +
    ggplot2::theme_bw() + GOFREE::theme0 + GOFREE::theme1
}




#' PLOT_GW
#'
#' @param GW_activity Output from GCAM_GW()
#' @param method "Net" or "Total"
#'
#' @return A trend plot of capacity activity by fuel technology at the national level
#' @export
#'
#' @examples
#' PLOT_GW(GW_activity, "Net")
#' PLOT_GW(GW_activity, "Total")
PLOT_GW <- function(GW_activity, method = NULL){

  if (is.null(method)||method %in% c("Total", "total")){
    ACTIVITY <-  c("installed", "additions", "retirements")
  } else if (method %in% c("Net", "net")){
    ACTIVITY <- c("installed", "add_adj", "ret_adj")
  } else {stop("Not a valid input, use either 'Net' or 'Total'")
  }
  GW_activity %>%
    dplyr::filter(Year >= 2020) %>%
    dplyr::filter(activity %in% ACTIVITY) %>%
    dplyr::mutate(activity = ifelse(grepl("add", activity), "Newly installed", activity),
                  activity = ifelse(grepl("ret", activity), "Retired", activity)) %>%
    dplyr::group_by(scenario, Year, fuel, activity, Units) %>%
    dplyr::summarise(value = sum(value), .groups = "drop") %>%
    tidyr::spread(activity, value) %>%
    dplyr::mutate(across(contains("Newly installed"), ~ installed - .x, .names = "Previously Installed"),
                  Retired = -Retired) %>%
    dplyr::select(-installed) %>%
    tidyr::gather(activity, value, `Newly installed`:`Previously Installed`) %>%
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Year, y = value, fill = activity),
             stat = "identity", position = "stack") +
    ggplot2::facet_wrap(~ fuel, ncol = 4, scales = "free_y") +
    # facet_grid(scenario~ fuel) +
    ggplot2::labs(x = "", y = "GW") +
    ggplot2::theme_bw() + GOFREE::theme0 + GOFREE::theme1 +
    ggplot2::theme(legend.position = "bottom") %>%
    return()
}


#' PLOT_JOB
#'
#' @param JOB_activity Output from GCAM_JOB()
#'
#' @return A trend plot of job by fuel technology and job type at the national level
#' @export
#'
#' @examples
#' PLOT_JOB(JOB_activity)
PLOT_JOB <- function(JOB_activity){
  JOB_activity %>%
    dplyr::filter(Year >= 2020) %>%
    dplyr::group_by(scenario, Year, fuel, job, Units) %>%
    dplyr::summarise(value = sum(value), .groups = "drop") %>%
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Year, y = value/1000, fill = job),
             stat = "identity", position = "stack") +
    ggplot2::facet_wrap(~ fuel, ncol = 4, scales = "free_y") +
    # facet_grid(scenario~ fuel) +
    ggplot2::labs(x = "", y = "Thousand people") +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::theme_bw() + GOFREE::theme0 + GOFREE::theme1 +
    ggplot2::theme(legend.position = "bottom") %>%
    return()
}


#' PLOT_JOB_TYPE
#'
#' @param JOB_activity Output of GCAM_JOB()
#'
#' @import ggplot2 dplyr
#' @return A trend plot of job by job type at the national level
#' @export
#'
#' @examples
#' PLOT_JOB_TYPE(JOB_activity)
PLOT_JOB_TYPE <- function(JOB_activity){
  JOB_activity %>%
    filter(Year >= 2020) %>%
    group_by(scenario, Year, job, Units) %>%
    summarise(value = sum(value), .groups = "drop") %>%
    ggplot() +
    geom_bar(aes(x = Year, y = value/10^6, fill = job),
             stat = "identity", position = "stack") +
    labs(x = "", y = "Million people") +
    scale_fill_manual(values = colors) +
    theme_bw() + GOFREE::theme0 + GOFREE::theme1 +
    theme(legend.position = "bottom") %>%
    return()
}



#' MAP_JOB
#'
#' @param JOB_activity Output of GCAM_JOB()
#' @param year Year of interest, a numeric
#'
#' @return A map of state level total direct power sector job of a given year
#' @export
#'
#' @examples
#' MAP_JOB(JOB_activity, 2050)
MAP_JOB <- function(JOB_activity, year){

  JOB_activity %>%
    dplyr::filter(Year == year) %>%
    dplyr::group_by(scenario, region, Year, Units) %>%
    dplyr::summarise(value = sum(value, na.rm = T), .groups = "drop") ->
    job.state

  values <- job.state$value
  names(values) <- job.state$region
  df.map <- GOFREE::mapUS52Compact #Puerto Rico is not included
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

  # top and tail 5 states
#
#   df.map <- df.map[order(-df.map$value), ]
#   df.map$value <- round(df.map$value, 1)

#   df.text.head <- head(df.map, 5)
#   df.text.tail <- tail(df.map, 5)


  df.map %>%
    ggplot2::ggplot() +
    ggplot2::geom_sf(ggplot2::aes(fill = bin)) +
    ggplot2::scale_fill_brewer(palette = "Blues")+
    ggplot2::geom_sf_text(ggplot2::aes(label = paste0(subRegion, ":\n", round(value,1))),  color = "black", size = 3) +
    # ggplot2::geom_sf_text(data = df.text.head, ggplot2::aes(label = paste0(subRegion, ":\n", value)),  color = "yellow", size = 4) +
    # ggplot2::geom_sf_text(data = df.text.tail, ggplot2::aes(label = paste0(subRegion, ":\n ", value)),  color = "black", size = 4) +
    ggplot2::labs(x = "", y = "", fill = "People", title = paste0("state-level power sector employment: ", year)) +
    ggplot2::theme_bw() + GOFREE::theme0 + GOFREE::theme1 %>%
    return()
}
