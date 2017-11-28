# Análise de Dados de Redes Sociais

# Instalando e Carregando o Pacote twitteR
install.packages("twitteR")
install.packages("httr")
install.packages("devtools")
library(twitteR)
library(httr)
library(devtools)

# Criando autenticação no Twitter

api_key <- "xxx"
api_secret <- "yyy"
access_token <- "zzz"
access_token_secret <- "www"

# Autenticando no Twitter
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

# Capturando os tweets

wilson_tweets = searchTwitter("Wilson Tennis", n = 500, lang = "en")
nike_tweets = searchTwitter("NikeCourt", n = 500, lang = "en")
head_tweets = searchTwitter("Head Tennis", n = 500, lang = "en")
babolat_tweets = searchTwitter("Babolat", n = 500, lang = "en")

# Visualizando as primeiras linhas do objeto tweets
head(wilson_tweets)
head(nike_tweets)
head(head_tweets)
head(babolat_tweets)

# Instalando o pacote para Text Mining.
install.packages("tm")
install.packages("SnowballC")
library(SnowballC)
library(tm)

# Tratamento (limpeza, organização e transformação) dos dados coletados

wilson_tweetlist <- sapply(wilson_tweets, function(x) x$getText())
wilson_tweetcorpus <- Corpus(VectorSource(wilson_tweetlist))
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
wilson_tweetcorpus <- tm_map(wilson_tweetcorpus, toSpace, "/")
wilson_tweetcorpus <- tm_map(wilson_tweetcorpus, toSpace, "@")
wilson_tweetcorpus <- tm_map(wilson_tweetcorpus, toSpace, "\\|")
wilson_tweetcorpus <- tm_map(wilson_tweetcorpus, toSpace, "/|@|\\|")
wilson_tweetcorpus <- tm_map(wilson_tweetcorpus, removeNumbers)
wilson_tweetcorpus <- tm_map(wilson_tweetcorpus, removePunctuation)
wilson_tweetcorpus <- tm_map(wilson_tweetcorpus, toSpace, "\n")
wilson_tweetcorpus <- tm_map(wilson_tweetcorpus, function(x)removeWords(x, stopwords("english")))
wilson_tweetcorpus <- tm_map(wilson_tweetcorpus, removeWords, c("https","tco"))
wilson_tweetcorpus <- tm_map(wilson_tweetcorpus, stripWhitespace)
#wilson_tweetcorpus <- sapply(wilson_tweetcorpus,function(row) iconv(row, "latin1", "ASCII", sub=""))
wilson_tweetcorpus <- tm_map(wilson_tweetcorpus, content_transformer(tolower))

nike_tweetlist <- sapply(nike_tweets, function(x) x$getText())
nike_tweetcorpus <- Corpus(VectorSource(nike_tweetlist))
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
nike_tweetcorpus <- tm_map(nike_tweetcorpus, toSpace, "/")
nike_tweetcorpus <- tm_map(nike_tweetcorpus, toSpace, "@")
nike_tweetcorpus <- tm_map(nike_tweetcorpus, toSpace, "\\|")
nike_tweetcorpus <- tm_map(nike_tweetcorpus, toSpace, "/|@|\\|")
nike_tweetcorpus <- tm_map(nike_tweetcorpus, removeNumbers)
nike_tweetcorpus <- tm_map(nike_tweetcorpus, removePunctuation)
nike_tweetcorpus <- tm_map(nike_tweetcorpus, toSpace, "\n")
nike_tweetcorpus <- tm_map(nike_tweetcorpus, function(x)removeWords(x, stopwords("portuguese")))
nike_tweetcorpus <- tm_map(nike_tweetcorpus, removeWords, c("https","tco"))
nike_tweetcorpus <- tm_map(nike_tweetcorpus, stripWhitespace)
nike_tweetcorpus <- sapply(nike_tweetcorpus,function(row) iconv(row, "latin1", "ASCII", sub=""))
#nike_tweetcorpus <- tm_map(nike_tweetcorpus, content_transformer(tolower))

head_tweetlist <- sapply(head_tweets, function(x) x$getText())
head_tweetcorpus <- Corpus(VectorSource(head_tweetlist))
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
head_tweetcorpus <- tm_map(head_tweetcorpus, toSpace, "/")
head_tweetcorpus <- tm_map(head_tweetcorpus, toSpace, "@")
head_tweetcorpus <- tm_map(head_tweetcorpus, toSpace, "\\|")
head_tweetcorpus <- tm_map(head_tweetcorpus, toSpace, "/|@|\\|")
head_tweetcorpus <- tm_map(head_tweetcorpus, removeNumbers)
head_tweetcorpus <- tm_map(head_tweetcorpus, removePunctuation)
head_tweetcorpus <- tm_map(head_tweetcorpus, toSpace, "\n")
head_tweetcorpus <- tm_map(head_tweetcorpus, function(x)removeWords(x, stopwords("portuguese")))
head_tweetcorpus <- tm_map(head_tweetcorpus, removeWords, c("https","tco"))
head_tweetcorpus <- tm_map(head_tweetcorpus, stripWhitespace)
head_tweetcorpus <- sapply(head_tweetcorpus,function(row) iconv(row, "latin1", "ASCII", sub=""))
#head_tweetcorpus <- tm_map(head_tweetcorpus, content_transformer(tolower))

babolat_tweetlist <- sapply(babolat_tweets, function(x) x$getText())
babolat_tweetcorpus <- Corpus(VectorSource(babolat_tweetlist))
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
babolat_tweetcorpus <- tm_map(babolat_tweetcorpus, toSpace, "/")
babolat_tweetcorpus <- tm_map(babolat_tweetcorpus, toSpace, "@")
babolat_tweetcorpus <- tm_map(babolat_tweetcorpus, toSpace, "\\|")
babolat_tweetcorpus <- tm_map(babolat_tweetcorpus, toSpace, "/|@|\\|")
babolat_tweetcorpus <- tm_map(babolat_tweetcorpus, removeNumbers)
babolat_tweetcorpus <- tm_map(babolat_tweetcorpus, removePunctuation)
babolat_tweetcorpus <- tm_map(babolat_tweetcorpus, toSpace, "\n")
babolat_tweetcorpus <- tm_map(babolat_tweetcorpus, function(x)removeWords(x, stopwords("portuguese")))
babolat_tweetcorpus <- tm_map(babolat_tweetcorpus, removeWords, c("https","tco"))
babolat_tweetcorpus <- tm_map(babolat_tweetcorpus, stripWhitespace)
babolat_tweetcorpus <- sapply(babolat_tweetcorpus,function(row) iconv(row, "latin1", "ASCII", sub=""))
#babolat_tweetcorpus <- tm_map(babolat_tweetcorpus, content_transformer(tolower))

# Instalando o pacote wordcloud
install.packages("RColorBrewer")
install.packages("wordcloud")
library(RColorBrewer)
library(wordcloud)

# Gerando uma nuvem palavras
pal2 <- brewer.pal(8,"Dark2")
wordcloud(wilson_tweetcorpus, 
          min.freq = 2, 
          scale = c(5,1), 
          random.color = F, 
          max.word = 60, 
          random.order = F,
          colors = pal2)

pal2 <- brewer.pal(8,"Dark2")
wordcloud(nike_tweetcorpus, 
          min.freq = 2, 
          scale = c(5,1), 
          random.color = F, 
          max.word = 60, 
          random.order = F,
          colors = pal2)

pal2 <- brewer.pal(8,"Dark2")
wordcloud(head_tweetcorpus, 
          min.freq = 2, 
          scale = c(5,1), 
          random.color = F, 
          max.word = 60, 
          random.order = F,
          colors = pal2)

pal2 <- brewer.pal(8,"Dark2")
wordcloud(babolat_tweetcorpus, 
          min.freq = 2, 
          scale = c(5,1), 
          random.color = F, 
          max.word = 60, 
          random.order = F,
          colors = pal2)

## Etapa 5 - Análise de Sentimento

# Criando uma função para avaliar o sentimento
install.packages("stringr")
install.packages("plyr")
library(stringr)
library(plyr)

sentimento.score = function(sentences, pos.words, neg.words, .progress = 'none')
{
  
  # Criando um array de scores com lapply
  scores = laply(sentences,
                 function(sentence, pos.words, neg.words)
                 {
                   sentence = gsub("[[:punct:]]", "", sentence)
                   sentence = gsub("[[:cntrl:]]", "", sentence)
                   sentence =gsub('\\d+', '', sentence)
                   tryTolower = function(x)
                   {
                     y = NA
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     return(y)
                   }
                   
                   sentence = sapply(sentence, tryTolower)
                   word.list = str_split(sentence, "\\s+")
                   words = unlist(word.list)
                   pos.matches = match(words, pos.words)
                   neg.matches = match(words, neg.words)
                   pos.matches = !is.na(pos.matches)
                   neg.matches = !is.na(neg.matches)
                   score = sum(pos.matches) - sum(neg.matches)
                   return(score)
                 }, pos.words, neg.words, .progress = .progress )
  
  scores.df = data.frame(text = sentences, score = scores)
  return(scores.df)
}

# Mapeando as palavras positivas e negativas

pos = readLines("palavras_positivas.txt")
neg = readLines("palavras_negativas.txt")


# Obtendo texto
wilson_txt = sapply(wilson_tweets, function(x) x$getText())
nike_txt = sapply(nike_tweets, function(x) x$getText())
head_txt = sapply(head_tweets, function(x) x$getText())
babolat_txt = sapply(babolat_tweets, function(x) x$getText())

# Vetor de tweets das marcas
nd = c(length(wilson_txt), length(nike_txt), length(head_txt), length(babolat_txt))


# Juntando os textos
marcas = c(wilson_txt, nike_txt, head_txt, babolat_txt) 

# Aplicando função para calcular o score de sentimento
scores = sentimento.score(marcas, pos, neg, .progress = 'text')

# Calculando o score por marca

scores$marca = factor(rep(c("wilson", "nike", "head", "babolat"), nd))
scores$muito.pos = as.numeric(scores$score >= 2)
scores$muito.neg = as.numeric(scores$score <= -2)

# Calculando o total
numpos = sum(scores$muito.pos)
numneg = sum(scores$muito.neg)

# Score global
global_score = round( 100 * numpos / (numpos + numneg) )
#head(scores)

# colors
cols = c("#7CAE00", "#00BFC4", "#F8766D", "#C77CFF")
names(cols) = c("wilson", "nike", "head", "babolat")

# boxplot
ggplot(scores, aes(x=marca, y=score, group=marca)) +
  geom_boxplot(aes(fill=marca)) +
  scale_fill_manual(values=cols) +
  geom_jitter(colour="gray40",
              position=position_jitter(width=0.2), alpha=0.3)
#opts(title = "Boxplot - Marcas Sentiment Scores")

# Gerando um histograma com o lattice
install.packages("lattice")
library("lattice")
histogram(data = scores, ~score|marca, main = "Análise de Sentimentos", xlab = "", sub = "Score")


# barplot of average score
meanscore = tapply(scores$score, scores$marca, mean)
df = data.frame(marca=names(meanscore), meanscore=meanscore)
df$marcas <- reorder(df$marca, df$meanscore)

f <- ggplot(df, aes(df$marca, df$meanscore)) 
f + geom_bar(stat= "identity",aes(color=marca,fill=marca))


