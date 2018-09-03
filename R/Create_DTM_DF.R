#' @title func
#' @description  Creates a DTM Data Frame for your text data based on specified parametes
#' @param text
#' @param cust_stop_words
#' @return df_dtm
#' @export Create_DTM_DF

Create_DTM_DF<- function(text,cust_stop_words = c())
{
  regex_func = function(x){ return (gsub('[^a-z ]', '', x)) }
  common_stop_words = tm::stopwords()
  custom_stop_words = cust_stop_words
  all_stop_words = append(common_stop_words, custom_stop_words)
  docs = tm::VCorpus(tm::VectorSource(as.character(text)))
  docs = tm::tm_map(docs, tm::content_transformer(tolower))
  docs = tm::tm_map(docs, tm::content_transformer(regex_func))
  docs = tm::tm_map(docs, tm::stripWhitespace)
  docs = tm::tm_map(docs, tm::removeWords, all_stop_words)
  dtm = tm::DocumentTermMatrix(docs)
  df_dtm = as.data.frame(as.matrix(dtm))
  return(df_dtm)
}
