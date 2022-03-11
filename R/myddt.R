#' @title This function plots length vs. width of a given species.
#'
#' @param df A data frame, must be the DDT data frame.
#' @param SPECIES A species listed in the DDT data frame.
#'
#' @return Returns a plot of the length vs. the weight of a given species, color coded by river. Also returns the entire data frame, the subsetted data frame, and a RIVER relative frequency table.
#' @importFrom dplyr %>%
#'
#' @export
#'
#' @examples
#' \dontrun{myddt(df = DDT.csv, SPECIES = "CCATFISH")}
#'

myddt <- function(df, SPECIES)
{
  WEIGHT <- NULL
  LENGTH <- NULL
  RIVER <- NULL

  sp.df <- df %>% dplyr::filter( SPECIES == {{ SPECIES }} )

  gscat <- ggplot2::ggplot(sp.df, ggplot2::aes(x = WEIGHT, y = LENGTH)) +
    ggplot2::geom_point(ggplot2::aes(color = RIVER)) +
    ggplot2::ggtitle("Trey Hille") +
    ggplot2::geom_smooth(method = "lm", formula = y ~ poly(x, 2))

  base::print(gscat)

  rt <- base::round(base::table(df$RIVER)/base::length(df$RIVER), 5)
  outpt <- base::list(df, sp.df, rt)
  base::names(outpt) <- c("Data Before Subsetting", "Data After Subsetting", "River Relative Frequencies")
  base::print(outpt)

  utils::write.csv(sp.df, file = base::paste("LvsWfor", SPECIES, ".csv", sep = ""))
}
