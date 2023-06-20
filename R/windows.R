
#' Tibble of all dashboards, worksheets and storyboards
#'
#' @param twb_file a Tableau TWB file
#'
#' @return a tibble with a list of dashboards, worksheets and storyboards
#' @export
#'
#' @examples
#'  \dontrun{
#'    all_windows(twb_file = "test.xml")
#' }
#'
all_windows <- function(twb_file){
    all_windows <- convert_cols_xml_to_tbl(twb_file, "//window") %>%
        dplyr::select(-c(hidden, maximized))

}
