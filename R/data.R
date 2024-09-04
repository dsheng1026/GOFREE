#' SCurve
#'
#' @description
#' This data provides the shut-down assumptions by fuel and technology at the state level
#'
#' @details
#' Follow package plutus to gather the assumptions
#'
#' @source
#' GCAM assumptions used in GCAM-USA 7.1; see scripts for details
#'
#' @examples
#' data(SCurve)
"SCurve"


#' hydro_cf
#'
#' @description
#' This data provides a state-level annual capacity factor of hydro power
#'
#' @format
#' A data frame with `49` rows and `3` variables:
#'
#' @source
#' Hall et al., 2003. Table 8, exported as GODEEP/Assumptions/hydro_CF_USA.csv
#' state level monthly capacity factor
#'
#' @examples
#' data(hydro_cf)
"hydro_cf"


#' OM_cost
#'
#' @description
#' This data technology level OM fixed and variable cost information
#'
#' @source
#' GCAM data
#'
#' @examples
#' data(OM_cost)
"OM_cost"


#' mapUS52Compact
#'
#' @description
#' This data presents the spatial information of USA states
#'
#' @source
#' https://github.com/JGCRI/rmap/tree/main/data
#'
#' @examples
#' data(mapUS52Compact[, 1:6])
"mapUS52Compact"



#' colors
#'
#' @description
#' specify the fill color by job type fr plot functions
"colors"


#' theme0
#'
#' @description
#' specify plot theme0
"theme0"

#' theme1
#'
#' @description
#' specify plot theme1
"theme1"

#' theme_leg
#'
#' @description
#' specify plot theme_leg
"theme_leg"
