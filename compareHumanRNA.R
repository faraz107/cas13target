compareHumanRNA <- function(crSeq, BUTTON){
  
  library(tidyverse)
  library(here)
  library(Biostrings)
  
  Seq = crSeq
  names(Seq) = "Seq"
  comparison = "None"
  c_on = "None"
  c_off = "None"

  ## ADD THE PATH TO DOWNLOADED NCBI BLAST DIRECTORY
  # To check for mismatches with Human RNA sequence data
  db_path = here("ncbi-blast-2.14.1+", "bin", "rnadb")
  blastn_cmd = here("ncbi-blast-2.14.1+", "bin", "blastn")
  
  if (BUTTON == "SUBMIT") {
    
    x = system (paste0("echo ", "'", Seq, "'", "| ", 
                       blastn_cmd, 
                       " -db ", db_path, 
                       " -task blastn -outfmt \"7 delim=, staxid sacc gaps mismatch nident length qcovs qcovhsp qcovus\"", 
                       " -word_size 15 ", 
                       " -reward 4 ", 
                       " -penalty -5 ", 
                       " -gapopen 12 ", 
                       " -gapextend 8 ", 
                       " -max_target_seqs 10 "), 
                intern = TRUE)
  }
  
  
  if (BUTTON == "EXAMPLE") {
      
    x = system (paste0("echo ", "'", Seq, "'", "| ", 
                       blastn_cmd, 
                       " -db ", db_path, 
                       " -task blastn -outfmt \"7 delim=, staxid sacc gaps mismatch nident length qcovs qcovhsp qcovus\"", 
                       " -word_size 15 ", 
                       " -reward 4 ", 
                       " -penalty -5 ", 
                       " -gapopen 12 ", 
                       " -gapextend 8 ", 
                       " -max_target_seqs 10 "), 
                intern = TRUE)
    
  }
  
  
  blastout <- x %>% data.frame()

  colnames(blastout) <- "Data"
  
  blastout <- blastout %>% 
    separate(col = Data, sep = ",", into = c("staxid", "sacc", "gaps", "mismatch",
                                             "nident", "length", "qcovs", "qcovhsp", 
                                             "qcovus")) %>% 
    rowwise() %>% 
    mutate(nonident = 30-as.numeric(nident)) %>% 
    ungroup() %>% 
    filter(!is.na(nonident)) 
 
  if(dim(blastout)[1] > 0){
    temp = blastout %>% filter(nonident == 0, str_detect(staxid, "#", negate = TRUE)) %>% head(1)
    if (!is.na(temp$nonident[1])) {
      c_on = paste0(scales::percent(1 - temp %>% pull(nonident)/30), " match with <br/> ", 
                          "<a href='https://www.ncbi.nlm.nih.gov/nuccore/", temp %>% pull(sacc),"' target='_blank'> ", temp %>% pull(sacc), " </a>")
    }
  }
  
  if(dim(blastout)[1] > 0){
    temp = blastout %>% filter(nonident > 0, str_detect(staxid, "#", negate = TRUE)) %>% head(1)
    if (!is.na(temp$nonident[1])) {
      c_off = paste0(scales::percent(1 - temp %>% pull(nonident)/30), " match. <br/> ", 
                    temp %>% pull(nonident), "-nt mismatches with ", 
                    "<a href='https://www.ncbi.nlm.nih.gov/nuccore/", temp %>% pull(sacc),"' target='_blank'> ", temp %>% pull(sacc), " </a>")
    }
  }
  
  comparison = list(c_on, c_off)
  
  return(comparison)
  
}
