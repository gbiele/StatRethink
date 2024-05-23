
load_or_execute <- function(file_path, execute_command) {
  
  if (file.exists(file_path)) {
    # Load the file into the global environment
    load(file_path, envir = .GlobalEnv)  
    message(paste("Loaded objects from:", file_path)) 
  } else {
    # Evaluate the command (whether it's a string or an expression)
    result <- eval(substitute(execute_command), envir = .GlobalEnv)
    
    # If the command returns objects, make them available
    if (!is.null(result)) {
      obj_name = 
        deparse(substitute(execute_command)) %>% 
        paste(collapse = "") %>% 
        gsub("\\{|\\}","",.) %>% 
        strsplit(" = ") %>% 
        .[[1]] %>% .[1] %>% 
        gsub(" ","",.)
      assign(obj_name, result, envir = .GlobalEnv)
      save(list = obj_name, file = file_path)
      message("Executed command and loaded resulting objects.")
    } else {
      message("Executed command (no objects returned).")
    }
  }
}

# Define a function to reorder a factor variable in a data.table
# based on the mean of another variable
reorder_dt = function(dt, f_var, o_var) {
  # Compute the mean of o_var for each level of f_var, 
  # and store the result in a temporary data table
  tmp = dt[, .(m = mean(get(o_var))), by = c(f_var)][order(-m)]
  
  # Reorder the factor levels of f_var in the original data table dt
  # The new order is based on the sorted means from the temporary data table tmp
  dt[, (f_var) := factor(get(f_var), levels = tmp[, get(f_var)])]
}

mcmc_intervals_sorted = function(draws) {
  dt = draws %>% mcmc_intervals_data()
  dt$parameter = factor(dt$parameter, levels = dt$parameter[order(-dt$m)])
  
  dt %>% 
    ggplot(aes(x = parameter, y = m)) + 
    geom_linerange(aes(ymin = ll, ymax = hh)) + 
    geom_linerange(aes(ymin = l, ymax = h), linewidth = .8) + 
    geom_point() + 
    coord_flip() + 
    theme_classic() +
    ylab("estimate")
}

get_stats = function(draws, id) {
  s = draws %>% summarise_draws() %>% data.table()
  s = s[variable == id, .(mean,q5, q95)] %>% as.numeric()
  sprintf("%.2f (%.2f, %.2f)", s[1], s[2], s[3])
}
