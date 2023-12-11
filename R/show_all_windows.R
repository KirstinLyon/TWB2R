
#' Tibble of all dashboards, worksheets and storyboards
#'
#' @param twb_file a WB file
#'
#' @return a tibble with a list of dashboards, worksheets and storyboards
#' @export
#'
#' @examples
#'  \dontrun{
#'    show_all_windows(twb_file = "test.twb")
#' }
#'
show_all_windows <- function(twb_file){
    all_windows <- convert_cols_xml_to_tbl(twb_file, "//window") %>%
        dplyr::select(-c(maximized))
    return(all_windows)

}
