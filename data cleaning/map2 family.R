chi_score<-c(60,50,40)
eng_score<-c(60,50,40)

weight_score<-function(chi,eng){
    final_score<-chi+eng*2
    return(final_score)
}


weight_score_list<-
    map2_dbl(chi_score,
             eng_score,
             weight_score)

data.frame(chi_score,
           eng_score,
           weight_score_list)