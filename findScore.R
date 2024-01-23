

findScore <- function(crSeq, goodSeq, badSeq) {
  library(tidyverse)
  library(here)
  library(Biostrings)
  
  Seq = crSeq
  names(Seq) = "Seq"
  
  Code_map = Biostrings::IUPAC_CODE_MAP		
  
  score = 0
  Seq <- str_split(string = Seq, pattern = "") %>% unlist()
  
  # Favorable nt [HIGH scores] at positions 1,2 and [LOW scores] at positions 11,12,15,16,17
  
  for (i in seq_along(Seq)) {
    
    temp = Code_map[str_which(Code_map %>% names(), 
                              str_split_i(string = goodSeq, pattern = boundary("character"), i = i))] %>% 
      str_split_1("") %>% str_c(collapse = "|")
    
    if(str_detect(Seq[i], temp)){
      # print(paste("Comparing Seq chr:", Seq[i], "with pattern", temp, "position:", i))
      if (i %in% c(1,2)) {
        score = score + 60
      }
      if (i %in% c(11,12,15,16,17)) {
        score = score + 5
      }
    }
  }
  
  # Penalised nt [HIGH scores] at positions 1,2 and [LOW scores] at positions 3,4,11,12,15,16,17
  
  for (i in seq_along(Seq)) {
    
    temp = Code_map[str_which(Code_map %>% names(), 
                              str_split_i(string = badSeq, pattern = boundary("character"), i = i))] %>% 
      str_split_1("") %>% str_c(collapse = "|")
    
    if(str_detect(Seq[i], temp)){
      # print(paste("Comparing Seq chr:", Seq[i], "with pattern", temp, "position:", i))
      if (i %in% c(1,2)) {
        score = score - 60
      }
      if (i %in% c(3,4,11,12,15,16,17)) {
        score = score - 5
      }
    }
  }
    
  # Step 2 to check for NOT "C" at the positions: 11, 12, 15, 16, 17
  score = ifelse (Seq[11] == "C",  score - 5, score + 5)
  score = ifelse (Seq[12] == "C",  score - 5, score + 5)
  score = ifelse (Seq[15] == "C",  score - 5, score + 5)
  score = ifelse (Seq[16] == "C",  score - 5, score + 5)
  score = ifelse (Seq[17] == "C",  score - 5, score + 5)
  
  
  return(score)
  
}
