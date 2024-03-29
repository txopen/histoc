#' calculated HLA Evolutionary Divergence (HED) score.
#'
#' @description Given two HLA alleles (class I or II), calculates HLA Evolutionary Divergence, i.e,
#' divergence between allele sequences using Grantham distance metric.
#' @param hla1 first HLA allele.
#' @param hla2 second HLA allele.
#' @return A numerical values corresponding to the divergence between alleles.
#' @examples
#' cHED(hla1 = "A*02:653",
#'  hla2 = "A*02:654")
#' @export
cHED <- function(hla1, hla2){

  if(startsWith(hla1,'D') & startsWith(hla2,'D')){

    hla1 <- stringr::str_replace_all(hla1, c("\\*" = "", ":" = ""))
    hla2 <- stringr::str_replace_all(hla2, c("\\*" = "", ":" = ""))

    seq_hla1 <- seqs2[hla1]
    seq_hla2 <- seqs2[hla2]
  } else {
    seq_hla1 <- seqs[hla1]
    seq_hla2 <- seqs[hla2]
    }

  if(is.null(seq_hla1[[1]])){
    stop(cat("allele does not exist in sequence."))
  }

  if(is.null(seq_hla2[[1]])){
    stop(cat("allele does not exist in sequence."))
  }

  seq_len = length(seq_hla1[[1]])
  dis <- 0

  for(i in 1:seq_len){
    aa1 = seq_hla1[[1]][[i]]
    aa2 = seq_hla2[[1]][[i]]
    dis = dis + dst[toupper(aa1),toupper(aa2)]
  }

  dis = dis / seq_len
  return(dis)
}
