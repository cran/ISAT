#' Read a file and compute the NN through permutations;
#' @import stats
#' @import utils
#' @import gtools
#' @import stringr
#' @param filename the file to be processed
#' @param cell_names A list of cell types
#' @return The Nearest Neibour Cell distance in the file
#' @export findNNs

findNNs = function(filename, cell_names=NULL) {
   XY_LABELS=c('Cell.X.Position', 'Cell.Y.Position')
   Phenotype = NULL
   
   if (is.null(cell_names)) {
    # Default cell types
    cell_names = c('Melanoma','CD8 + T cell','CD4+ T cell','T regulatory cell','PDL1+ cell','CD3+ T cell','other')
  }
  
  dat = read.delim(filename, sep='\t')
  
  cell_types = list()
  for (eachType in cell_names) {
    subdat = subset(dat, Phenotype == eachType)
    cell_types[[eachType]] = subdat
  }
  
  permu = permutations(length(cell_names), 2)

  result = NULL
  for (i in 1:nrow(permu)) {
    eachPermu = permu[i,]
    name1 = cell_names[eachPermu[1]]
    name2 = cell_names[eachPermu[2]]
    data1 = cell_types[[name1]]
    data2 = cell_types[[name2]]
    
    NNs = NULL
    for (j in 1:nrow(data1)) {
      eachcell = data1[j, ]
      NN = findNN(eachcell, data2)
      if (is.null(NNs)) {
        NNs = NN
      } else {
        NNs = rbind(NNs, NN)
      }
    }
    
    eachResult = data1[, XY_LABELS]
    names(eachResult) = str_c('Ori.', XY_LABELS)
    eachResult = cbind(eachResult, NNs[, c(XY_LABELS, 'distance')])
    
    eachResultProc = procDist(eachResult)
    eachResultFinal = cbind(data.frame(
      Target = name1,
      Nearest = name2
    ), eachResultProc)
    
    if (is.null(result)) {
      result = eachResultFinal
    } else {
      result = rbind(result, eachResultFinal)
    }
  }
  return(result)
}
