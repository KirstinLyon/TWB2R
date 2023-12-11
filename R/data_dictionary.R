#' Create an overview of all visible fields from a TWB file
#'
#' @param twb_file a TWB file
#'
#' @return Returns a tibble of all fields that exist in datasource(s) used in the workbook.
#' @export
#'
#' @examples
#'  \dontrun{
#'    all_raw_fields(twb_file = "test.twb")
#' }

show_all_raw_fields <- function(twb_file) {
    # All raw fields ----
    # remove any duplicates and keep those with a "role", keep visible and update named column

    lookup <- c(original_name = "name", name = "caption")

    all_raw_fields <- convert_cols_xml_to_tbl(twb_file, "//column[boolean(@name) and not (.//calculation)]") %>%
        janitor::clean_names() %>%
        dplyr::distinct() %>%
        dplyr::filter(!is.na(role)) %>%
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
        janitor::remove_empty(which = "cols") %>%
        dplyr::select(-dplyr::any_of(c("datatype", "role", "type",
                                "semantic_role"))) %>%
        dplyr::select(name, original_name, dplyr::everything(.)) %>%
        dplyr::filter(name != ":Measure Names")

    return(all_raw_fields)

}


#' Creates an overview of parameters created in the workbook
#'
#' @param twb_file a TWB file
#'
#' @return Returns a tibble of all parameters created in the workbook.
#' @export
#'
#' @examples
#'  \dontrun{
#'    all_parameters(twb_file = "test.twb")
#' }


show_all_parameters <- function(twb_file){


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
                dplyr::select(-which(names(.) == 'col_type')) %>%
                dplyr::select(-dplyr::any_of(c("datatype", "role", "type",
                                        "semantic_role", "param_domain_type",
                                        "default_value", "datatype_customized",
                                        "class", "formula",
                                        "fcp_parameter_default_values_true_source_field"))) %>%
                dplyr::select(name, unique_id, dplyr::everything(.))

    return(all_param)
}

#' List of other fields created in tableau (e.g., calculations, bins and groups)
#'
#' @param twb_file a TWB file
#'
#' @return A tibble of other fields created in the workbook
#' @export
#'
#' @examples
#'  \dontrun{
#'    all_other_created(twb_file = "test.twb")
#' }


show_all_other_created <- function(twb_file){
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

    calcs_only <- all_other %>%
        dplyr::select(-which(names(.) == 'col_type'))%>%
        dplyr::select(-dplyr::any_of(c("datatype", "role", "type",
                                "semantic_role"))) %>%
        dplyr::select(name, unique_id, dplyr::everything(.))

    return(calcs_only)

}
