
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
    datasources <- convert_cols_xml_to_tbl(twb_file, "//named-connection/connection")

    #Tag only exists if there are datsources on a server
    if("dbname" %in% colnames(datasources)){
        datasources <- datasources %>%
            dplyr::mutate(full_path       = dplyr::case_when(is.na(dbname) ~ filename,
                                                       .default = dbname),
                          is_server       = dplyr::case_when(is.na(dbname) ~ 0,
                                                        .default = 1))
    }

    #tag only exists if all datasources are local
    if("directory" %in% colnames(datasources)){
        datasources <- datasources %>%
            dplyr::mutate(full_path = dplyr::case_when(is.na(directory) ~ filename,
                                                       .default = paste(directory, filename, sep = "/")),

                          is_server = 0)
    }

    datasources <- datasources %>%
        dplyr::mutate(datasource_name = stringr::word(full_path, -1, sep = fixed("/"))) %>%
        dplyr::select(class, datasource_name, is_server, full_path)  %>%
        dplyr::distinct()

    return(datasources)

}
