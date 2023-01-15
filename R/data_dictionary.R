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
        dplyr::distinct(.) %>%
        dplyr::filter(!is.na(type)) %>%
        dplyr::mutate(field_type = dplyr::case_when(base::startsWith(name,"[Parameter") ~ "Parameter",
                                      base::startsWith(name, "[Calculation") ~"Calculation",
                                      base::endsWith(name, "(group)]") ~ "Group",
                                      TRUE ~ "other"),
               has_alias = !is.na(caption)) %>%
        dplyr::filter(field_type != "Parameter", field_type != "Calculation")


    # Scenario 2:  calculations and parameters ----

    all_calcs <- convert_cols_xml_to_tbl(twb_file, "//column[@caption]") %>%
        dplyr::distinct(.) %>%
        dplyr::mutate(field_type = dplyr::case_when(base::startsWith(name,"[Parameter") ~ "Parameter",
                                      base::startsWith(name, "[Calculation") ~"Calculation",
                                      base::endsWith(name, "(group)]") ~ "Group",
                                      TRUE ~ "other")) %>%
        dplyr::filter(field_type != "other")



    all_calcs_details <- convert_cols_xml_to_tbl(twb_file, ".//column/calculation") %>%
        dplyr::distinct(.) %>%
        dplyr::filter(!is.na(formula))


    all_calcs_tbl <- dplyr::bind_cols(all_calcs, all_calcs_details) %>%
        dplyr::select(-class, -value) %>%
        dplyr::mutate(has_alias = FALSE)


    # Combine raw and calculations and output data ----

    data_dictionary_tbl <- all_raw_tbl %>%
        dplyr::union_all(all_calcs_tbl) %>%
        dplyr::mutate(tableau_name = dplyr::case_when(!is.na(caption) ~ caption,
                                        TRUE ~ name)) %>%
        dplyr::mutate(tableau_name = stringr::str_remove_all(tableau_name, "[\\[\\]]")) %>%
        dplyr::select(tableau_name, caption, name,  datatype, 'default-format', formula, has_alias)

}

#' Title Create a table of Contents based on the workbook
#'
#' @param twb_file an xml file
#'
#' @return Table of contents
#' @export
#'
#' @examples
#'   \dontrun{
#'    create_toc(twb_file = "test.xml")
#' }
create_toc <- function(twb_file){
    TOC <- convert_cols_xml_to_tbl(twb_file, "//window") %>%
        dplyr::rename("type" = "class") %>%
        dplyr::select(-hidden, -maximized)

}
