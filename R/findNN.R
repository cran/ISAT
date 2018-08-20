#' Find the nearest neighbour (NN) given a cell and a group of cells;
#' @param cell An object cell
#' @param cellList A list of cell types to be calculated for
#' @param XY_LABELS x and y positions of the cell
#' @return The Nearest Neibour Cell distance and calculate the distance
#' @export findNN
#' @examples
#' Cell.X.Position=sample (1:100,1)
#' Cell.Y.Position=sample (1:100,1)
#' Tcell=data.frame(Cell.X.Position,Cell.Y.Position)
#' Cell.X.Position=sample(1:500,5,replace=TRUE)
#' Cell.Y.Position=sample(1:500,5,replace=TRUE)
#' Tumor.cells=data.frame(Cell.X.Position,Cell.Y.Position)
#' findNN(Tcell,Tumor.cells,XY_LABELS=c('Cell.X.Position', 'Cell.Y.Position'))

findNN = function(cell, cellList,XY_LABELS=c('Cell.X.Position', 'Cell.Y.Position') ) {
  
  x_ = XY_LABELS[1]
  y_ = XY_LABELS[2]
  
  cell_x = cell[x_]
  cell_y = cell[y_]
  
  distances = sqrt((unlist(cellList[x_])-unlist(cell_x))^2 
                   + (unlist(cellList[y_])-unlist(cell_y))^2)
  distances_min = min(distances)
  NN = cellList[which(distances==distances_min), ]
  if (nrow(NN)!=1) {
    NN = NN[1, ]
  }
  
  NN$distance = distances_min
  return(NN)
}
