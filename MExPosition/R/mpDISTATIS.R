mpDISTATIS <- function(data, sorting = 'No', normalization = 'None', masses= NULL, table = NULL, make_table_nominal = TRUE,DESIGN = NULL, make_design_nominal = TRUE, graphs=TRUE)
{
#######################
# Data preparation
#######################
  main <- deparse(substitute(data))		
  
  if(is.null(data))
  {   stop('You have not provided any data')
  }
	
  if(!is.matrix(data))
  {   data <- as.matrix(data)
  }
	
  if(sum(is.na(data)>0))
  { stop('Missing data not allowed')
  }
  
  DESIGN <- designCheck(data, DESIGN, make_design_nominal)
	
#########################
# Running DISTATIS
#########################

# Core processing of DISTATIS
   res.proc <- mpDISTATIS.core(data, sorting = sorting, normalization = normalization, masses = masses, table = table, make_table_nominal= TRUE)
 		
#########################
# Results
#########################

distatis.overview <- list(data = res.proc$data, sorting = res.proc$sorting, normalization = res.proc$normalization, table = res.proc$table,
					num.groups=dim(res.proc$table.partialFactorScores.array)[3])

distatis.innerproduct <- list(S = res.proc$S, C=res.proc$C, ci = res.proc$ci, cj = res.proc$cj, eigs.vector= res.proc$EigenVector, eigs = res.proc$eigenValue, 
                    fi = res.proc$factorScores, t = res.proc$percentVar, a = res.proc$alphaWeights) 

distatis.compromise <- list(Compromise = res.proc$Compromise, ci=res.proc$Compromise.ci, cj = res.proc$Compromise.cj, eigs.vector = res.proc$Compromise.EigenVector,
					eigs = res.proc$Compromise.EigenValues, fi = res.proc$Compromise.factorScores, t = res.proc$Compromise.percentVar)
					
distatis.table <- list( a = res.proc$alphaWeights,  m = res.proc$masses, eigs = res.proc$table.EigenValues, eigs.vector = res.proc$table.EigenVectors, 
            		fi = res.proc$table.FactorScores, partial.fi = res.proc$table.partialFactorScores,
         		    ci = res.proc$table.ci, t =res.proc$table.inertia, partial.fi.array = res.proc$table.partialFactorScores.array)  					

class(distatis.overview) <- c("distatis.overview","list")
class(distatis.innerproduct) <- c("distatis.innerproduct","list")
class(distatis.compromise) <- c("distatis.compromise","list")
class(distatis.table) <- c("distatis.table","list")
	
res<- list(Overview = distatis.overview, InnerProduct = distatis.innerproduct, Compromise = distatis.compromise, Table=distatis.table)	

class(res) <- c("mpDISTATIS","list")

mpPlotInfo <- NULL
if(graphs == TRUE)
  {   mpPlotInfo <- mpGraphHandler(res,res.proc$data, DESIGN, res.proc$table, main)
  }


return(mpOutputHandler(res=res, mpPlotInfo=mpPlotInfo))
}
