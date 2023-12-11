
#' Tibble of all datasources connected to workbook
#'
#' @param twb_file a TWB file
#'
#' @return a tibble of datasources connected to workbook
#' @export
#'
#' @examples
#'  \dontrun{
#'    show_all_datasources(twb_file = "test.twb")
#' }
#'
show_all_datasources <- function(twb_file){
    datasources <- convert_cols_xml_to_tbl(twb_file, "//connection") %>%
        dplyr::filter(!is.na(filename)) %>%
        dplyr::mutate(directory = dplyr::case_when(is.na(directory) ~ filename,
                                                   TRUE ~ directory),
                      filename = stringr::word(filename, -1, sep = fixed("/"))) %>%
        dplyr::select(class, directory, filename) %>%
        dplyr::distinct()
    return(datasources)

}
