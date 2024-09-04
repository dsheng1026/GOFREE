# constant.R
#
# This file holds the various constants used in this package
#
# List of constants

# unit conversion ----
CONV_EJ_KWH <- 277777777777.78
CONV_GWH_EJ <- 3.6e-06
convGW_MW <- 1e3
convMW_kW <- 1e3
GCAM_model_step <- 5

# capacity factor ----
geo_cf <- 0.76

# years mapping ----
years_mapping <- data.frame(year = c(rep("final-calibration-year", 1),
                                     rep("initial-future-year",
                                         length(seq(2020, 2100, by = 5)))),
                            vintage = c(seq(2015, 2100, by = 5))) %>%
  dplyr::mutate(year = as.character(year))


# GCAM-USA state name ----
gcamusa.STATES <- c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "HI",
                    "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN",
                    "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH",
                    "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VT", "WA",
                    "WI", "WV", "WY")


# plot theme ----

theme0 <- ggplot2::theme(
  panel.border = ggplot2::element_rect(colour = "black", linewidth =1),
  text = ggplot2::element_text(family= "Arial", size = 15),
  axis.text.y = ggplot2::element_text(angle = 0, color = "black", size = 15, margin = ggplot2::margin(r = 10)),
  axis.text.x = ggplot2::element_text(angle = 0, color = "black", size = 15, margin = ggplot2::margin(t = 10), vjust= 0.5),
  axis.title.y = ggplot2::element_text(size = 15, margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
  axis.title.x = ggplot2::element_text(size = 15, margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)),
  axis.text.x.top =  ggplot2::element_blank(),  axis.title.x.top = ggplot2::element_blank(),
  strip.background = ggplot2::element_rect(fill="grey95"),
  strip.text = ggplot2::element_text(size = 16),
  plot.title = ggplot2::element_text(hjust = 0, face = "bold"),
  plot.margin = ggplot2::margin(t = 10, r = 15, b = 10, l = 10) #panel.spacing = unit(1, "lines"),
)

theme_leg <- ggplot2::theme(legend.position="right", legend.justification = "center",
                   legend.key.size = ggplot2::unit(1.5, "cm"),
                   legend.key.height = ggplot2::unit(1.5,"line"),
                   legend.spacing.x = ggplot2::unit(1, 'cm'), #legend.spacing.y = unit(5, 'cm'),
                   legend.text = ggplot2::element_text(margin = ggplot2::margin(l = -25,t=0, b=0), size = 15),
                   legend.box.margin = ggplot2::margin(-10, 10,-8,10),
                   legend.background = ggplot2::element_blank())

theme1 <- ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 40, hjust = 0.9, vjust = 1),
                legend.text = ggplot2::element_text(hjust = 0),
                strip.background = ggplot2::element_rect(fill="grey99"),
                strip.text = ggplot2::element_text(size = 12),
                axis.text.x.bottom = ggplot2::element_text(size = 12),
                axis.text.y = ggplot2::element_text(size = 12),
                panel.grid.minor = ggplot2::element_blank(),
                panel.grid.major = ggplot2::element_line(linetype = 2, color = "grey80", linewidth = 0.3),
                panel.spacing.y = ggplot2::unit(0.5, "lines"),
                panel.spacing.x = ggplot2::unit(0.5, "lines"))

colors <- c("OM_fixed" = "#1f78b4", "OM_var" = "#a6cee3",  # Group 1 colors (blue shades)
            "Construction-onsite" = "#33a02c", "Construction-related" = "#b2df8a",  # Group 2 colors (green shades)
            "Decommission" = "#e31a1c")

