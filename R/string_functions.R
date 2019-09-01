#' apply str_split to string vectors
#' @param string_vector vector of strings
#' @param split_by character delimiter to split strings by
#' @param position which position of the split to return
#' @return vector of strings split by the split_by character at the position
#' @export
#' @import stringr
#' @author Beth Signal
#' @examples
#' strv_split(c("ABC_DEF_HIJ_KLM", "NOP_QRS_TUV_WXY"), "_" , 2)
#'
#' strv_split(c("ABC_DEF_HIJ_KLM", "NOP_QRS_TUV"), "_" , 2)
#'
#' strv_split(c("ABC_DEF_HIJ_KLM", "NOP_QRS_TUV"), "_" , -1)

strv_split = function(string_vector, split_by, position){

  if(position > 0){
    out = unlist(lapply(str_split(string_vector, split_by), "[[", position))
  }else{
    position = (position*-1) - 1
    out = unlist(lapply(str_split(string_vector, split_by), function(x) x[length(x) - position]))
  }

  return(out)

}
#' apply str_split to string vectors to extract text between two delimters
#' @param string_vector vector of strings
#' @param split_by_1 first character delimiter to split strings by (at the start of the desired string to be captured)
#' @param split_by_2 second character delimiter to split strings by (at the end of the desired string to be captured)
#' @param position_1 which position of the first split to return
#' @param position_2 which position of the second split to return
#' @return vector of strings split by the split_by character at the position
#' @export
#' @import stringr
#' @author Beth Signal
#' @examples
#' strv_split2(c("ABC_DEF_HIJ_KLM", "ABC_QRS_HIJ_WXY"), "C_" , "_H")
strv_split2 = function(string_vector, split_by_1, split_by_2, position_1=NULL, position_2=NULL){

  if(is.null(position_1) & is.null(position_2)){

    out = unlist(lapply(str_split(lapply(str_split(string_vector, split_by_1), "[[", 2), split_by_2), "[[",1))

  }else{
    if(position_1 > 0){
      out = unlist(lapply(str_split(string_vector, split_by_1), "[[", position_1))
    }else{
      position_1 = (position_1*-1) - 1
      out = unlist(lapply(str_split(string_vector, split_by_1), function(x) x[length(x) - position_1]))
    }

    if(position_2 > 0){
      out = unlist(lapply(str_split(out, split_by_2), "[[", position_2))
    }else{
      position_2 = (position_2*-1) - 1
      out = unlist(lapply(str_split(out, split_by_2), function(x) x[length(x) - position_2]))
    }
  }

  return(out)

}

#' remove any stray quotes in strings
#' @param string string
#' @return string without quotes
#' @export
#' @author Beth Signal
#' @examples
#' remove_quotes("abcd'efgh")
#' remove_quotes('abcd"efgh')
remove_quotes = function(string){

  out = gsub('"','',gsub("'", "", string))
  return(out)

}
