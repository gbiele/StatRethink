
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
