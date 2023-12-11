#' Helper function for pulling out information from an xml file using a given xpath
#'
#' @param data a TWB file
#' @param twb_xpath the name of the xpath tag
#'
#' @return returns wanted xpath fields as a tibble
#' @keywords internal

#' @examples
#'  \dontrun{
#'    convert_cols_xml_to_tbl(twb_file, twb_xpath)
#' }#'
convert_cols_xml_to_tbl <- function(twb_file,twb_xpath){

    tryCatch(
        {
            test    <- xml2::xml_find_all(twb_file, xpath = twb_xpath)
            test_xml <- purrr::map(xml2::xml_attrs(test), base::as.list) %>% purrr::reduce(dplyr::bind_rows)
            return(test_xml)
        },
        error = function(e){
            stop("This type of created field does not exist in your TWB file.")
        }
    )

}


#' Checks the "caption" created to ensure no duplicates
#'
#' @param twb_file a TWB file
#' @param a_name a name for a calculation
#' @export

#' @return returns TRUE/FALSE depending on if the name exists in the dataset
#' @keywords internal

#' @examples
#'  \dontrun{
#'    check_name(twb_file, a_name)
#' }
check_name <- function(twb_file, a_name){

    all_calc_names <- twb_file %>%
        TWB2R::show_all_other_created()

    all_param_names <- twb_file %>%
        TWB2R::show_all_parameters()

    all_raw_names <- twb_file %>%
        TWB2R::show_all_raw_fields()

    all_tbl <- list(all_calc_names, all_param_names, all_raw_names)
    all_names <- purrr::compact(all_tbl) %>%
        purrr::reduce(dplyr::bind_rows, .init = dplyr::tibble()) %>%
        dplyr::select(name) %>%
        dplyr::pull()

    return (a_name %in% all_names)
}



#' create unique ID for the calculation.
#'
#' @param twb_file twb_file

#' @return a unique code for a calculation

#' @keywords internal
#' @examples
#'  \dontrun{
#'    create_calc_id(twb_file)
#' }

create_calc_id <- function(twb_file){
    an_id <- twb_file %>%
        TWB2R::show_all_other_created() %>%
        dplyr::select(unique_id) %>%
        dplyr::mutate(unique_id = stringr::str_replace_all(unique_id, "[\\[|\\]]", ""),

                      unique_id = stringr::str_split_i(unique_id, "_", 2)) %>%
        dplyr::summarise(new_unique_id = max(unique_id)) %>%
        dplyr::pull()


    new_id <- VeryLargeIntegers::as.vli(an_id) + 1
    return(glue::glue("[Calculation_{new_id}]"))

}
