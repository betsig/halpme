#' read in a list of files with the same column format
#' @param file_list vector file locations.
#' @param header Do the files have a header?
#' @return data.frame of all files concatenated using rbind()
#' @export
#' @import data.table
#' @author Beth Signal
#' @examples
read_and_join = function(file_list, header=TRUE){
  output = NULL
  for(i in seq_along(file_list)){
    out_part = fread(file_list[i], header=header, data.table=FALSE)
    output = rbind(output, out_part)
  }
  return(output)
}
#' read in a fasta file as a data.frame
#' @param file fasta file location.
#' @return data.frame fasta sequences. One row for each sequence
#' @export
#' @import data.table
#' @importFrom stringr str_sub
#' @author Beth Signal
#' @examples
read_fasta2df = function(file){

  fasta = fread(file, header = FALSE, data.table = FALSE, stringsAsFactors = FALSE)

  fasta_df = data.frame(seq_id = fasta[seq(1,nrow(fasta), 2),1],
                        seq = fasta[seq(2,nrow(fasta), 2),1])

  first_char = str_sub(fasta_df$seq_id, 1, 1)
  not_fasta_starts = length(which(first_char != ">"))
  if(not_fasta_starts > 0 ){
    message(paste0("WARNING: ", not_fasta_starts, " of ", nrow(fasta_df), " sequence ids did not start with '>'"))
    message(paste0("Check your data formatting."))
  }

  fasta_df$seq_id = as.character(fasta_df$seq_id)
  fasta_df$seq_id[which(first_char == ">")] = str_sub(fasta_df$seq_id[which(first_char == ">")],2,-1)
  return(fasta_df)

}
