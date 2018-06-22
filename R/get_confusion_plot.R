#' GET_CONFUSION_TBL
#' 
#' This function takes in two arrays and produces a heatmap of the confusion
#' matrix. The heatmap squares include proportions depending on the align parameter
#' of actual (\code{align} = 1) or predicted (\code{align} = 2)
#' @param actual an array
#' @param predicted an array
#' @keywords confusion
#' @return a heatmap of confusion!
#' @export
#' @examples
#' get_confusion_plot(c(1,2,3,3,3,3,1,1,1,2,2,3,3), c(3,2,1,3,2,3,1,1,2,1,2,3,3))

get_confusion_plot <- function(actual, predicted, align = 1){
  require(reshape2)
  require(ggplot2)
  
  get_confusion_prop_tbl <- function(actual, predicted, align = 1){
    if (!(align %in% c(1,2))) {stop("align must be 1 or 2")}
    conf <- table(actual, predicted)
    if (align == 2) {
      conf <- t(conf)
      return (t(conf / rowSums(conf)))
    }
    return (conf / rowSums(conf))
  }
  
  conf_prop <- get_confusion_prop_tbl(actual, predicted, align)
  conf_prop_melt <- reshape2::melt(conf_prop, variable.name= predicted, id = actual)
  
  plt <- ggplot2::ggplot(data = conf_prop_melt, aes(x=predicted,y=actual)) +
    geom_tile(aes(fill=value)) +
    geom_text(aes(label = round(value,2))) + 
    scale_fill_gradient2(high = "darkblue", mid = "gainsboro",
                         midpoint = 0, limit = c(0,1), space = "Lab",
                         name = "Prop")
  
  print(plt)
}




