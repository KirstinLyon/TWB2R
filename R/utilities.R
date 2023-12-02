#' Helper function for pulling out information from an xml file using a given xpath
#'
#' @param data an xml file
#' @param twb_xpath the name of the xpath tag
#'
#' @return returns wanted xpath fields as a tibble
#' @export
#' @examples
#'  \dontrun{
#'    convert_cols_xml_to_tbl(data; twb_xpath)
#' }#'
convert_cols_xml_to_tbl <- function(data,twb_xpath){

    tryCatch(
        {
            test    <- xml2::xml_find_all(data, xpath = twb_xpath)
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
#' @param twb_file a twb file
#' @param a_name a name for a calculation
#'
#' @return returns TRUE/FALSE depending on if the name exists in the dataset
#' @export
#'
#' @examples
#'  \dontrun{
#'    check_name(twb_file, a_name)
#' }
check_name <- function(twb_file, a_name){

    all_calc_names <- twb_file %>%
        TWB2R::all_other_created() %>%
        dplyr::select(name)

    all_param_names <- twb_file %>%
        TWB2R::all_parameters() %>%
        dplyr::select(name)

    all_raw_names <- twb_file %>%
        TWB2R::all_raw_fields() %>%
        dplyr::select(name)

    all_names <- dplyr::bind_rows(all_calc_names,
                                  all_param_names,
                                  all_raw_names) %>%
        dplyr::pull(name)

    return (a_name %in% all_names)
}

#' create unique ID for the calculation.
#'
#' @param twb_file twb_file
#'
#' @return a unique code for a calculation
#' @export
#'
#' @examples
#'  \dontrun{
#'    create_calc_id(twb_file)
#' }
create_calc_id <- function(twb_file){
    all_ids <- twb_file %>%
        TWB2R::all_other_created() %>%
        dplyr::select(unique_id) %>%
        dplyr::pull(unique_id)
}
