mpSTATIS.optimize <- function(X, num.obs, table , num.groups, optimization.option = 'STATIS') 
{ 
    ## Exception
    if(optimization.option != 'None' && optimization.option != 'STATIS' && optimization.option != 'RV_Matrix' && 
        optimization.option != 'STATIS_Power1' && optimization.option != 'ANISOSTATIS_Type1' && optimization.option != 'ANISOSTATIS_Type2')
    {	print(paste('WARNING: Optimization option ', optimization.option, ' not recognized. STATIS was set as default'))
        statis.processed <- mpSTATIS.core(X, num.obs = num.obs, table, num.groups = num.groups, optimization.option = "STATIS")	
    }

    # No optimization
    if(optimization.option == "None")
    {	print(paste('Optimizing using: ',optimization.option))
    	statis.processed <- mpSTATIS.core(X, num.obs = num.obs, table, num.groups = num.groups, optimization.option = "None")
    }

    #Optimization Option - STATIS
    if(optimization.option == "STATIS")
    {	print(paste('Optimizing using: ',optimization.option))	
	statis.processed <- mpSTATIS.core(X, num.obs = num.obs, table, num.groups = num.groups, optimization.option = "STATIS")
    }

    #Optimization Option - RV Matrix
    if(optimization.option == 'RV_Matrix')
    {	print(paste('Optimizing using: ',optimization.option))
	statis.processed <- mpSTATIS.core(X, num.obs = num.obs, table,num.groups = num.groups, optimization.option = 'RV_Matrix')
    }	

    #Optimization Option - STATIS Power 1
    if(optimization.option == 'STATIS_Power1')
    { 	print(paste('Optimizing using: ',optimization.option))
	statis.processed <- mpSTATIS.core(X, num.obs = num.obs, table,num.groups = num.groups, optimization.option = 'STATIS_Power1')
    }

    #Optimization Option - ANISOSTATIS Type 1
    if(optimization.option == 'ANISOSTATIS_Type1')
    {	print(paste('Optimizing using: ',optimization.option))
	statis.processed <- mpANISOSTATIS.core(X, num.obs = num.obs, table,num.groups = num.groups, optimization.option = 'ANISOSTATIS_Type1')
    }

    #Optimization Option - ANISOSTATIS Type 2
    if(optimization.option == 'ANISOSTATIS_Type2')
    {	print(paste('Optimizing using: ',optimization.option))
	statis.processed <- mpANISOSTATIS.core(X, num.obs = num.obs, table, num.groups = num.groups, optimization.option = 'ANISOSTATIS_Type2')
    }

# Results    
res.optimize <- list(S=statis.processed$S, RVMatrix = statis.processed$RVMatrix, C = statis.processed$C, ci = statis.processed$ci, cj = statis.processed$cj,
                    EigenVector = statis.processed$EigenVector, eigenValue = statis.processed$eigenValue, factorScores = statis.processed$factorScores, 
                    percentVar = statis.processed$percentVar, alphaWeights = statis.processed$alphaWeights, 
                    
                    table.col.names = statis.processed$table.col.names, weights = statis.processed$weights, masses = statis.processed$masses, 
                    table.ci = statis.processed$table.ci, table.cj = statis.processed$table.cj, table.EigenValues = statis.processed$table.EigenValues, 
                    table.EigenVectors = statis.processed$table.EigenVectors, table.partialFactorScores.array = statis.processed$table.partialFactorScores.array,
		    		table.loadings = statis.processed$table.loadings, table.SqLoadings = statis.processed$table.SqLoadings, 
		    		table.FactorScores = statis.processed$table.FactorScores, table.partialFactorScores = statis.processed$table.partialFactorScores,
		  			table.contribution.variable = statis.processed$table.contribution.variable, table.inertia=statis.processed$table.inertia)

return(res.optimize)   
print('Processing completed')
}
