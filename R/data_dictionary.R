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


#' Create an overview of all visible fields from a twb file
#'
#' @param twb_file an XML file generated from a twb
#'
#' @return Returns a tibble with all fields coming from the datasource(s) used in the workbook.
#' @export
#'
#' @examples
#'  \dontrun{
#'    raw_fields_overview(twb_file = "test.xml")
#' }

raw_fields_overview <- function(twb_file) {
    # All raw fields ----
    # remove any duplicates and keep those with a "role", keep visible and update named column

    lookup <- c(original_name = "name", name = "caption")

    all_raw_fields <- convert_cols_xml_to_tbl(twb_file, "//column[boolean(@name) and not (.//calculation)]") %>%
        janitor::clean_names() %>%
        dplyr::distinct() %>%
        dplyr::filter(!is.na(role)) %>%
        dplyr::filter(
            dplyr::if_any(
                .cols = dplyr::any_of("hidden"),
                .fns = ~is.na(.x)
            )
        ) %>%

        dplyr::mutate(name = stringr::str_replace_all(name, "[\\[|\\]]", "")) %>%
        dplyr::mutate(caption = if("caption" %in% colnames(.))
            dplyr::case_when(is.na(caption) ~ name , TRUE ~ caption)
            else NULL

        ) %>%
        dplyr::mutate(has_alias = if("caption" %in% colnames(.))
            dplyr::case_when(caption == name ~ FALSE, TRUE ~ TRUE)
            else NULL
        ) %>%
        dplyr::rename(dplyr::any_of(lookup)) %>%
        janitor::remove_empty(which = "cols")

}


#' Create overview of parameters
#'
#' @param twb_file a twb file from tableau
#'
#' @return list of parameters
#' @export
#'
#' @examples
#'  \dontrun{
#'    parameters_overview(twb_file = "test.xml")
#' }


parameters_overview <- function(twb_file){


    #All created fields ---------------------
    #pull out all data - first calc attributes, and then calculations attributes from the calc fields.  This is both calcs and param
    all_created_cols <- convert_cols_xml_to_tbl(twb_file, "//column[boolean(@caption) and .//calculation]")
    all_created_calcs <- convert_cols_xml_to_tbl(twb_file, "//column[boolean(@caption)]//calculation")

    #combine created fields together and remove unnamed
    all_created <- all_created_cols %>%
        dplyr::bind_cols(all_created_calcs) %>%
        janitor::clean_names() %>%
        dplyr::select(-which(names(.) == 'folder_name')) %>%
        dplyr::filter(
            dplyr::if_any(
                .cols = dplyr::any_of("unnamed"),
                .fns = ~is.na(.x)
            )
        )%>%
        dplyr::mutate(col_type = if ("param_domain_type" %in% colnames(.))
            dplyr::case_when(is.na(param_domain_type) ~ 'other_created', TRUE ~ "parameter")
            else "other_created")


    #separate in to parameters and other
    lookup <- c(unique_id = "name", name = "caption", default_value_text = "alias", default_value = "value")

    all_param <- all_created %>%
        dplyr::filter(col_type == "parameter") %>%
        dplyr::distinct() %>%
        dplyr::rename(dplyr::any_of(lookup)) %>%
        janitor::remove_empty(which = "cols") %>%
        #dplyr::select(-which(names(.) == 'col_type'))

}

#' List of other fields created in tableau
#'
#' @param twb_file a tableau twb file
#'
#' @return list of other created files
#' @export
#'
#' @examples
#'  \dontrun{
#'    other_created_overview(twb_file = "test.xml")
#' }


other_created_overview <- function(twb_file){
    #All created fields ---------------------
    #pull out all data - first calc attributes, and then calculations attributes from the calc fields.  This is both calcs and param
    all_created_cols <- convert_cols_xml_to_tbl(twb_file, "//column[boolean(@caption) and .//calculation]")
    all_created_calcs <- convert_cols_xml_to_tbl(twb_file, "//column[boolean(@caption)]//calculation")

    #combine created fields together and remove unnamed
    all_created <- all_created_cols %>%
        dplyr::bind_cols(all_created_calcs) %>%
        janitor::clean_names() %>%
        dplyr::select(-which(names(.) == 'folder_name')) %>%
        dplyr::filter(
            dplyr::if_any(
                .cols = dplyr::any_of("unnamed"),
                .fns = ~is.na(.x)
            )
        )%>%
        dplyr::mutate(col_type = if ("param_domain_type" %in% colnames(.))
            dplyr::case_when(is.na(param_domain_type) ~ 'other_created', TRUE ~ "parameter")
            else "other_created")


    lookup <- c(unique_id = "name", name = "caption", default_value_text = "alias", default_value = "value")

    #separate in to parameters and other
    all_param <- all_created %>%
        dplyr::filter(col_type == "parameter") %>%
        dplyr::distinct() %>%
        dplyr::rename(dplyr::any_of(lookup))

    all_other <- all_created %>%
        dplyr::filter(col_type == "other_created") %>%
        dplyr::distinct() %>%
        dplyr::rename(dplyr::any_of(lookup)) %>%
        janitor::remove_empty(which = "cols") # remove cols that have everything that is null


    ## create a list of all unique_id and friendly_names from param and calc list ----
    param_name <- all_param

    tryCatch(
        {
            param_name <- param_name %>%
                dplyr::select(name, unique_id)
        },
        error = function(e){
            message("No parameters exist")
            return(NA)
        }
    )

     other_name <- all_other %>%
        dplyr::select(name, unique_id)


    # This is the list of user and friendly names
    all_name <- other_name %>%
        dplyr::bind_rows(param_name) %>%
        dplyr::mutate(unique_id = stringr::str_replace_all(unique_id, "[\\[|\\]]", ""))




    ## swapping in the right name ----


    pattern_vector <- stats::setNames(all_name$name, all_name$unique_id)
    all_other$formula <- stringr::str_replace_all(all_other$formula,
                                                  pattern = stringr::fixed(pattern_vector))

    calcs_only <- all_other

}
