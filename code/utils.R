fxn_SeparateTrt <- function(f.data = data){

  f.new <- 
    f.data |> 
    mutate(
      herb = ifelse(grepl("x", trt_name), "none", "herbicide"),
      cctrt = ifelse(grepl("cc", trt_name), "cc", "none"),
      crop = ifelse(grepl("a", trt_name), "a", "p")
      )
  
  return(f.new)
    
}
