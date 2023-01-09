#' Helper function for pulling out information from an xml file using a given xpath
#'
#' @param data an xml file
#' @param twb_xpath the name of the xpath tag
#'
#' @return returns wanted xpath fields as a tibble
#' @export
#'
convert_cols_xml_to_tbl <- function(data,twb_xpath){

    test    <- xml2::xml_find_all(data, xpath = twb_xpath)
    test_xml <- purrr::map(xml2::xml_attrs(test), base::as.list) %>% purrr::reduce(dplyr::bind_rows)
    return(test_xml)
}


#' Create Data Dictionary from column and calculation tags
#'
#' @param twb_file An xml file
#'
#' @return returns a data dictionary
#' @export
#'
#' @examples
#'  \dontrun{
#'    data_dictionary(twb_file = "test.xml")
#' }
#'
data_dictionary <- function (twb_file){


    #Scenario 1:  Raw data ----
    all_cols <- convert_cols_xml_to_tbl(twb_file, "..//column")

    all_raw_tbl <- all_cols %>%
        dplyr::filter(!is.na(type),is.na(caption)) %>%
        dplyr::mutate(formula = NA, tableau_name = NA, name = stringr::str_remove_all(name, "[\\[\\]]")) %>%
        dplyr::select(-ordinal, -caption, -value)

    # Scenario 2:  calculations and parameters ----

    all_calcs <- convert_cols_xml_to_tbl(twb_file, "//column[@caption]")
    all_calcs_details <- convert_cols_xml_to_tbl(twb_file, ".//column/calculation")

    all_calcs_tbl <- dplyr::bind_cols(all_calcs, all_calcs_details) %>%
        dplyr::distinct() %>%
        dplyr::select(-class, -value) %>%
        dplyr::rename("name" = "caption", "tableau_name" = "name")


    # Combine raw and calculations and output data ----

    data_dictionary_tbl <- all_raw_tbl %>%
        dplyr::union(all_calcs_tbl) %>%
        dplyr::select(name, datatype, role, type, 'default-format', formula, alias, tableau_name, 'param-domain-type')
}
