#Score based on +ve and -ve words
score.sentiment = function(sentences, pos, neg, .progress='none')
{
  require(plyr)
  require(stringr)
  
  list = lapply(sentences, function(sentence, pos, neg)
  {
    sentence = gsub('[[:punct:]]',' ',sentence)
    sentence = gsub('[[:cntrl:]]','',sentence)
    sentence = gsub('\\d+','',sentence)
    sentence = gsub('\n','',sentence)
    
    sentence = tolower(sentence)
    word.list = str_split(sentence, '\\s+')
    words = unlist(word.list)
    pos.matches = match(words, pos)
    neg.matches = match(words, neg)
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    pp = sum(pos.matches)
    nn = sum(neg.matches)
    score = sum(pos.matches) - sum(neg.matches)
    list1=c(score, pp, nn)
    return (list1)
  }, pos, neg)
  
  score_new=lapply(list, `[[`, 1)
  pp1=score=lapply(list, `[[`, 2)
  nn1=score=lapply(list, `[[`, 3)
  
  scores.df = data.frame(score=score_new, text=sentences)
  positive.df = data.frame(Positive=pp1, text=sentences)
  negative.df = data.frame(Negative=nn1, text=sentences)
  
  list_df=list(scores.df, positive.df, negative.df)
  return(list_df)
}

############################################################################
# Clean the tweets
result = score.sentiment(sample, pos, neg)

library(reshape)
#Creating a copy of result data frame
test1=result[[1]]
test2=result[[2]]
test3=result[[3]]

#Creating three different data frames for Score, Positive and Negative

#Removing text column from data frame
test1$text=NULL
test2$text=NULL
test3$text=NULL

#Storing the first row(Containing the sentiment scores) in variable q
scores  = test1[1,]
pos_val = test2[1,]
neg_val = test3[1,]
q_sc = melt(scores , var='Score')
q_pv = melt(pos_val, var='Positive')
q_nv = melt(neg_val, var='Negative') 
q_sc['Score'] = NULL
q_pv['Positive'] = NULL
q_nv['Negative'] = NULL

#Creating data frame
table1 = data.frame(Text=result[[1]]$text, Score = q_sc)
table2 = data.frame(Text=result[[2]]$text, Score = q_pv)
table3 = data.frame(Text=result[[3]]$text, Score = q_nv)

#Merging three data frames into one
table_final=data.frame(Text=table1$Text, Score=table1$value, Positive=table2$value, Negative=table3$value)

############################################################################
#Defining categories of sentiments
Sc = table_final$Score

#Outstanding
outst = sapply(Sc, function(Sc) Sc >= 6)
value_outst = length(Sc[outst])

#Very good
vgood = sapply(Sc, function(Sc) Sc > 3 && Sc <6)
value_vgood = length(Sc[vgood])

#Good
good = sapply(Sc, function(Sc) Sc <= 3 && Sc > 0)
value_good = length(Sc[good])

#Neutral
neutral = sapply(Sc, function(Sc) Sc == 0) 
value_neutral = length(Sc[neutral])

#Bad
bad = sapply(Sc, function(Sc) Sc >= -3 && Sc < 0)
value_bad = length(Sc[bad])

#Very bad
vbad = sapply(Sc, function(Sc) Sc < -3 && Sc > -6)
value_vbad = length(Sc[vbad])

#Awful
awful = sapply(Sc, function(Sc) Sc <= -6)
value_awful = length(Sc[awful])