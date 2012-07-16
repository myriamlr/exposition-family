mpSTATIS.preprocess <-
function(data, table, row.preprocess = 'None', column.preprocess = 'None',  
							table.preprocess = 'None', make_table_nominal = TRUE)
							 
{  table <- mpTableCheck(data, t(table), make_table_nominal)
   
   X_dimensions = dim(data)
   Y_dimensions = dim(table)
   num.obs = X_dimensions[1]
   num_groups = dim(table)[1]
  
   # row preprocessing
   row.preproc.data <- mpSTATIS.rowPreproc(data, table, row.preprocess)

   # column preprocessing
   column.preproc.data <- mpSTATIS.columnPreproc(row.preproc.data$row.processed, table , column.preprocess)

   # table preprocessing
   table.preproc.data  <- mpSTATIS.tablePreproc(column.preproc.data$column.processed, table, table.preprocess)
      
   rownames(table.preproc.data$table.processed) <- rownames(data)
   colnames(table.preproc.data$table.processed) <-colnames(data)

   # results
   preprocess <- list(data = data, table = table, data.preprocessed = table.preproc.data$table.processed, num.obs = num.obs,
                       numgroups = num_groups, row.preprocess=row.preprocess, column.preprocess=column.preprocess, table.preprocess=table.preprocess)
	
   print('Preprocessing Completed')
   return(preprocess)
}
