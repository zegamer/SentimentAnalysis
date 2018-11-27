library(ggplot2)

#Frequency of word category
hist(table_final$Positive, xlab = "Positive tweets", col=rainbow(10), main = 'Histogram of Positive tweets') #Positive
hist(table_final$Negative, xlab = "Negative tweets", col=rainbow(10), main = 'Histogram of Negative tweets') #Negative

#Pie : Sentiment Categories
pVal = c(value_outst, value_vgood, value_good, value_neutral, value_bad, value_vbad, value_awful)
Sentiment = c("Outstanding", "Great", "Good", "Neutral", "Bad", "Poor", "Awful")
Percent = c(round(pVal/sum(pVal)*100))

df = data.frame(Sentiment, Percent)
df$Sentiment = factor(df$Sentiment,
                      levels = c("Outstanding","Great","Good","Neutral", "Bad", "Poor", "Awful"))

p = ggplot(data = df, aes(Sentiment, Percent, fill=Sentiment)) + 
  ggtitle("Sentiment Category") +
  geom_bar(stat = "identity")

p + coord_polar("x", start=0)
p