#' Returns an appropriate function to pass to the \code{breaks} argument
#' of any \code{continuous_scale}
#' 
#' @param n suggested number of breaks (defaults to 5)
#' @export
#' @examples
#' ggplot(mtcars, aes(x = disp, y = mpg)) +
#' geom_point() +
#'     scale_y_log10()
#'     
#'  ggplot(mtcars, aes(x = disp, y = mpg)) +
#'      geom_point() +
#'          scale_y_log10(breaks = break_setter())
#'          
#'  ggplot(mtcars, aes(x = disp, y = mpg)) +
#'      geom_point() +
#'      scale_y_log10(breaks = break_setter(10))
#' @export
break_setter <- function(n = 5) {
    function(lims) {pretty(x = as.numeric(lims), n = n)}
}