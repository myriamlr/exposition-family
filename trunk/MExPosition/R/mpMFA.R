mpMFA <- function(data, table, make_table_nominal = TRUE, DESIGN=NULL, make_design_nominal = TRUE, graphs = TRUE)
{
	main <- deparse(substitute(data))	
	DESIGN <- designCheck(data, DESIGN, make_design_nominal)
	
	res <- mpSTATIS(data, table, make_table_nominal = make_table_nominal, statis.prepro.option = 'MFA', graphs=FALSE)
	
	class(res) <- c("mpMFA", "list")
	
	mpPlotInfo = NULL
	if(graphs==TRUE)
	{
		mpPlotInfo <- mpGraphHandler(res,data,DESIGN,res$Overview$groupmatrix, main)
	}

	return (mpOutputHandler(res=res,mpPlotInfo=mpPlotInfo))
}