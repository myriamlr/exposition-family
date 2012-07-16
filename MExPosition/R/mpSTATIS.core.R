mpSTATIS.core <- function (X, num.obs, table, num.groups, optimization.option = 'STATIS')
{ 	
##########################################
# Inner Product
##########################################

# Scalar Product Matrices (S)
   scalarProductMatrices = array(0,dim=c(num.obs,num.obs,num.groups))
   from = 1
   for(i in 1:num.groups)
   
   {   from = sum(table[i-1,])+from
       to = from + sum(table[i,])-1
       scalarProductMatrices[,,i] = X[,from:to] %*% t(X[,from:to])	
   }

# RV Matrix 	
   rvMatrix = diag(1,num.groups)	
   for(i in 1:(num.groups))
   {   for(j in i:num.groups)
       {   rv = rvCoeff(scalarProductMatrices[,,i],scalarProductMatrices[,,j],type=0)
           rvMatrix[i,j] = rv
           rvMatrix[j,i] = rv
       }
    }
	
# C Matrix	
   CMatrix = diag(1,num.groups)	
   for(i in 1:(num.groups))
   {   for(j in i:num.groups)
       {   c = matrixTrace((scalarProductMatrices[,,i]) %*% t(scalarProductMatrices[,,j]))
           CMatrix[i,j] = c
           CMatrix[j,i] = c
	}
   }

# eigen decomposition
   C.decomp = corePCA(CMatrix)
   decomp.C = C.decomp$pdq
   
# contribution
   ci = C.decomp$ci
   #rownames(ci)=paste('assessor',1:num.groups)
   	if(is.null(rownames(table))){
	   rownames(ci) <- paste("Table", 1:dim(table)[1], sep = "")
	   table.names <- rownames(ci)
	}else{
	  	rownames(ci) <- rownames(table)
	  	table.names <- rownames(ci)
	}

# contribution
	cj = C.decomp$cj
	rownames(cj) <- rownames(ci)
    
# eigen vectors
   P = decomp.C$p
   rownames(P) <- rownames(ci)

# eigen values
   D= (decomp.C$Dv)

# factor scores
   G = decomp.C$p %*%  diag(sqrt(D))
   rownames(G) <- rownames(P)

# percent of variance explained
   taus <- D/sum(D) * 100
	
# Alpha Weights
   if (optimization.option == "None")
   {	alphaWeights = matrix(1/num.groups,1,num.groups)
   }
	
   if (optimization.option == "Multitable")
   {	alphaWeights = matrix(1,1,num.groups)
   }
	
   if(optimization.option == 'RV_Matrix')
   {    alphaWeights = (corePCA(rvMatrix)$pdq$p)[,1]/sum((corePCA(rvMatrix)$pdq$p)[,1])
   }
	
   if(optimization.option == "STATIS")
   {	alphaWeights = P[,1] / sum(P[,1])	
   }
	
   if(optimization.option == 'STATIS_Power1')
   {	alphaWeights = (CMatrix %*% matrix(1,num.groups,1)) / sum(CMatrix %*% matrix(1,num.groups,1))
   }

##########################################
# Tables: Generalized PCA of X
##########################################	

# column names	
   table.colnames <- colnames(table)

# alpha weights
   table.alphaWeights <- alphaWeights

# weights and masses
   M = diag(1/(dim(X)[1]),dim(X)[1],dim(X)[1])
	
   w = c()
   for(i in 1:length(rowSums(table)))
   { w = c(w, rep(alphaWeights[i],rowSums(table)[i]))
   }
    
   W =diag(w)  

#general PDQ
	pdq.general = corePCA(X,M=M,W=W)
	general.pdq = pdq.general$pdq

# contribution
	table.ci = pdq.general$ci

# contribution
	table.cj = pdq.general$cj
   
# Eigen vectors of the tables
   gpdq.vectors = general.pdq$p

# Eigen values of the tables 
   gpdq.eigenvalues = (general.pdq$Dd)^2
	
# Inertia
   gpdq.inertia = ((general.pdq$Dv) / sum(general.pdq$Dv))*100

# Cumulative Inertia
   gpdq.cum.inertia = cumsum(gpdq.inertia)

# Loadings of the tables
   gpdq.loadings = general.pdq$q
   rownames(gpdq.loadings) = colnames(X)
   

# Squared loadings of the tables
   gpdq.loadingsSquared= gpdq.loadings^2

# Factor scores of the tables
   gpdq.factorscores =  general.pdq$p %*%  (general.pdq$Dd)
   rownames(gpdq.factorscores)=rownames(X)
   
# Partial Factor Scores
   gpdq.partial = array(0,dim=c(dim(X)[1],dim(gpdq.loadings)[2],num.groups))
   to_partial = 0
   from_partial = 1
   for(i in 1:dim(table)[1])
   {   from = sum(table[i-1,]) + from_partial
       to = sum(table[i,]) + to_partial
       to_partial = to
       from_partial = from
       gpdq.partial[,,i] = X[,from:to] %*% gpdq.loadings[from:to,]
   }
   
   gpdq.partialFS <- matrix(0,dim(X)[1]*num.groups,dim(gpdq.loadings)[2])
   to.total = 0
   for(i in 1:num.groups)
   {	from = to.total + 1
   		to = i*dim(gpdq.partial)[1]
   		to.total = to
   		gpdq.partialFS[from:to,]= gpdq.partial[,,i]
   	}
   	rownames(gpdq.partialFS) = paste(rep(table.names,each=dim(X)[1]),rep(rownames(X)))

# contribution of variable to dimension
   contribution.variable <- matrix(0,dim(table)[1], 2)
   load.to = 0
   load.from = 1
   for(j in 1:dim(contribution.variable)[2])
   {	for(i in 1:dim(table)[1])
	{   from = sum(table[i-1,]) + load.from
            to = sum(table[i,]) + load.to
            load.to = to
            load.from = from
            contribution.variable[i,j]= (sum(alphaWeights[i] * gpdq.loadingsSquared[from:to,j]))
	}
        load.to = 0
	load.from = 1
    }

##########################################
# Results
##########################################	

res.statis.core <- list(S=scalarProductMatrices, RVMatrix = rvMatrix, C = CMatrix, ci = ci, cj = cj, EigenVector = P, eigenValue = D, factorScores = G,  
			alphaWeights = alphaWeights, percentVar = taus,
			
			table.col.names = table.colnames, weights = W, masses = M, table.partialFactorScores.array = gpdq.partial,table.cj = table.cj, table.ci = table.ci, 
			table.EigenValues = gpdq.eigenvalues, table.inertia = gpdq.inertia,table.EigenVectors = gpdq.vectors, 
			table.loadings = gpdq.loadings, table.SqLoadings = gpdq.loadingsSquared, table.FactorScores = gpdq.factorscores, table.partialFactorScores = gpdq.partialFS,
			table.contribution.variable = contribution.variable
			
			)
    
return (res.statis.core)
}
