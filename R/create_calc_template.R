#' Creates a template to enter all calculations for the workbook
#'
#' @param csv_file the location and file name for the template
#'
#' @return an empty csv file with all options for creating a calculation
#' @export
#'
#' @examples
#'  \dontrun{
#'    create_calc_template(csv_file)
#' }
create_calc_template <- function(csv_file){

    template <- dplyr::tibble(
        caption = "",
        datatype = "",
        role = "",
        type = "",
        class = "",
        formula = ""
    ) %>%
        readr::write_excel_csv2(csv_file, delim = ",")

}

