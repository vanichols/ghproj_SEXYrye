fxn_SeparateTrt <- function(f.data = data){

  f.new <- 
    f.data |> 
    mutate(
      herb = ifelse(grepl("x", trt_name), "none", "herbicide"),
      cctrt = ifelse(grepl("cc", trt_name), "cc", "none"),
      crop = case_when(
        grepl("mix", trt_name) ~ "mix",
        grepl("rows", trt_name) ~ "mix",
        grepl("a", trt_name) ~ "a",
        grepl("p", trt_name) ~ "p",
        TRUE ~ "uhoh"
      )
    )
  
  return(f.new)
    
}
