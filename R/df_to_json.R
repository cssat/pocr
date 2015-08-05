#' Export \code{data.frame} to \code{JSON}
#' 
#' This is based on the type of JSON file that the original nutrient Parallel Coordinates
#' was built off of. It seems to be sort of "row oriented", whereas \code{rjson::toJSON}
#' is "column oriented".
#' 
#' @param df Data frame to convert.
#' @param file name of file to save to.
#' @param varname name of variable to assign to the JSON object.
#' @export
df_to_json <- function(df, file, varname = "foods") {
    sink(file)
    cat("var", varname, "= [")
    classes <- sapply(df, class)
    rown <- nrow(df)
    coln <- ncol(df)
    dfnames <- names(df)
    for (rowi in 1:rown) {
        cat("{")
        for (colj in 1:coln) {
            cat('"', dfnames[colj], '":', sep = "")   ## "column name":
            if(classes[colj] != "numeric") cat('"') ## non-numeric values need to be quoted
            cat(df[rowi, colj])    ## value
            if(classes[colj] != "numeric") cat('"') ## non-numeric values need to be quoted
            if (colj < coln) cat(",")
        }
        cat("}")
        if (rowi < rown) cat(",")
    }
    cat("];")
    sink()
}