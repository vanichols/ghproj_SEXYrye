fxn_SeparateTrt <- function(f.data = data){
  
  f.new <- 
    f.data |> 
    mutate(
      herb = ifelse(grepl("^x", trt_name), "none", "herb"),
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

fxn_MakeNice <- function(f.data = data){
  
  f.new <- 
    f.data |> 
    mutate(
      herbNice = ifelse(herb == "none", "No herbicides", "Herbicides"),
      cctrtNice = ifelse(cctrt == "none", "No cover crop", "Fall cover crop"),
      cropNice = case_when(
        crop == "mix" ~ "A/P mix",
        crop == "a" ~ "Annual",
        crop == "p" ~ "Perennial",
        TRUE ~ "uhoh"
      ),
      cropNice = factor(cropNice, levels = c("Annual", "A/P mix", "Perennial"))
    )
  
  return(f.new)
  
}
