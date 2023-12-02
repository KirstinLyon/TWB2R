#' Add a calculation to a Tableau workbook
#'
#' @param twb_file A tableau file
#' @param file_location location of where the twb file is stored
#' @param caption user-friendly name
#' @param datatype string, number, date, ...
#' @param role measure or dimension
#' @param type nominal, ordinal or ...
#' @param class calculations are a type tableau
#' @param formula formula for the calculation
#'
#' @return an updated twb_file with an added calculation
#' @export
#'
#' @examples
#'  \dontrun{
#'    create_calc(twb_file, file_Location, caption, datatype,
#'    role, type, class, formula)
#' }
create_calc <- function(twb_file, file_location,caption, datatype,
                        role, type, class = "Tableau", formula ){

    temp_file <- twb_file
    name_check <- TWB2R::check_name(twb_file, caption)


    if(name_check == FALSE){

        datasource_nodes <- xml2::xml_find_all(file, "//datasources/datasource")

        new_tag <- xml2::xml_add_child(datasource_nodes[[1]], "column")
        child <- xml2::xml_add_child(new_tag, "calculation")
        xml2::xml_set_attr(new_tag, "caption", caption)
        xml2::xml_set_attr(new_tag, "datatype", datatype)
        xml2::xml_set_attr(new_tag, "name", "[Calculation_1097752430143991829]")
        xml2::xml_set_attr(new_tag, "role", role)
        xml2::xml_set_attr(new_tag, "type", type)

        xml2::xml_set_attr(child, "class", class)
        xml2::xml_set_attr(child, "formula", formula)

        xml2::write_xml(twb_file, file_location)

    }

    else{
        error <- "Name already exists"
    }

    return(temp_file)

}
