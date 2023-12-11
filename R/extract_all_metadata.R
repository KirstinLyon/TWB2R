#' Function that extracts all metadata available from functions as a list
#'
#' @param twb_file A TWB file
#'
#' @return a list of metadata extracted using the TWB2R functions
#' @export
#'
#' @examples
#'   \dontrun{
#'    extract_all_metadata(twb_file = "test.twb")
#' }
extract_all_metadata <- function(twb_file) {


    # Apply all_* functions and store results in a list
    result_list <- list(
        windows     = TWB2R::show_all_windows(twb_file),
        raw         = TWB2R::show_all_raw_fields(twb_file),
        parameters  = TWB2R::show_all_parameters(twb_file),
        created     = TWB2R::show_all_other_created(twb_file),
        datasources = TWB2R::show_all_datasources(twb_file)
    )

    return(result_list)
}

#' Returns the desired metadata based on the result
#'
#' @param result_list All metadata.  Use extract_all_metadata for the result_list
#' @param result_type select from windows, raw, parameters, created (calculations, groups and bins),
#' datasources
#'
#' @return meta-data based on a keyword
#' @export
#'
#' @examples
#'   \dontrun{
#'    get_metadata(twb_file = "test.twb")
#' }
get_metadata <- function(result_list, result_type) {
    # Extract the desired result from the list
    result <- result_list[[result_type]]
    return(result)
}
