#' Get the count number of each phenotype for each tissue category
#' @param dataFile File path to the data file
#' @return a data frame summary of count
#' @export



getFileCount = function(dataFile) {
    Tissue.Category = NULL
    Phenotype = NULL
    data = read.delim(dataFile)
    phenotype = levels (data$Phenotype)
    category = levels(data$Tissue.Category)
    phenotype.length = length(phenotype)+1
    category.length = length(category)+1
    allcells=data.frame(Tissue.Category="All",Phenotype="All", Count=nrow(data))
                        
#set up template file  
    template = NULL
    for (i in c(category,"All")) {
    templateRes=data.frame(Tissue.Category=rep(i,length(phenotype)+1),Phenotype=c(phenotype,"All"))
    if (is.null(template)) {
      template = templateRes
    } else {
      template = rbind(template, templateRes)
    }
    }
    
#calculate cell count in each Catogory   
    result = NULL
    for (eachCat in levels(data$Tissue.Category)) {   
      
        cell_in_cat = subset(data, Tissue.Category==eachCat)
        cell_pheno = subset(template, Tissue.Category==eachCat)
        phenotypes = as.character(cell_pheno$Phenotype)
        
        eachR = as.data.frame(table(cell_in_cat$Phenotype))
        colnames(eachR) = c('Phenotype', 'Count')
        eachR = rbind(eachR, data.frame(Phenotype='All', 
                                        Count=nrow(cell_in_cat)))
        eachR = eachR[match(as.character(cell_pheno$Phenotype), 
                            as.character(eachR$Phenotype)), ]
        
        cellPhenoResult = cbind(cell_pheno, Count=eachR$Count)
        
        if (is.null(result)) {
            result = cellPhenoResult
        } else {
            result = rbind(result, cellPhenoResult)
        }
    }
#calculate cell count in Catogory="All"    
    All_result=NULL
    for (each_pheno in phenotype) {
      each_pheno_subset = subset(result, Phenotype==each_pheno)
      each_pheno_all= data.frame(Tissue.Category="All",Phenotype=each_pheno, 
                                 Count=sum(each_pheno_subset$Count))
      if (is.null(All_result)) {
        All_result = each_pheno_all
      } else {
        All_result = rbind(All_result,each_pheno_all)
      }
    }
#binding results   
    file_result = rbind(result, All_result,allcells)
    return(file_result)
}
