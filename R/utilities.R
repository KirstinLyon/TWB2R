#' Helper function for pulling out information from an xml file using a given xpath
#'
#' @param data an xml file
#' @param twb_xpath the name of the xpath tag
#'
#' @return returns wanted xpath fields as a tibble
#' @export
#'
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
