install.packages("dplyr")
library(dplyr)

#STEP 1

# 데이터 불러오기
mooc_dataset <- read.csv("big_student_clear_third_version.csv")

# 필요한 데이터 선별
dataset <- mooc_dataset %>% select(userid_DI, institute, course_id, final_cc_cname_DI, LoE_DI)

# 변수 이름 변경
names(dataset)
names(dataset)[1] <- "TransactionID"
names(dataset)[2] <- "Institute"
names(dataset)[3] <- "Course"
names(dataset)[4] <- "Region"
names(dataset)[5] <- "Degree"

# 공백 제거
dataset$Region <- gsub(" ","", dataset$Region)

# 변수 4개 합치기
RawTransactions <- paste(dataset$Institute, dataset$Course, dataset$Region, dataset$Degree, sep="_")

# Transaction_ID 와 연결하기
MOOC_transactions <- paste(dataset$TransactionID, RawTransactions, sep=" ")
MOOC_transactions <- data.frame(MOOC_transactions)

# csv로 저장하기
write.csv(MOOC_transactions, file= "MOOC_User_Course.csv", row.names=FALSE)


# STEP2 

# 데이터 불러오기
library(arules)
library(arulesViz)
library(wordcloud)
data_single <- read.transactions("MOOC_User_Course.csv", format = "single",cols=c(1,2),rm.duplicates=TRUE)

# 데이터 속성 파악
summary(data_single)

inspect(data_single)
data_df <- as(data_single,"data.frame")
str(data_single)
itemInfo(data_single)

# wordcloud 생성
itemName <- itemLabels(data_single)
itemCount <- itemFrequency(data_single)*nrow(data_single)
testing <- as(itemCount,"data.frame")

col <- brewer.pal(8, "Set1")
wordcloud(words=itemName, freq=itemCount, min.freq = 1000, scale=c(1,0.2),col=col,random.order=FALSE)

# itemFrequencyPlot 생성
itemFrequencyPlot(data_single, support = 0.01, cex.names=0.8)


# STEP3

# rule 생성하기
rules <- apriori(data_single, parameter=list(support=0.001, confidence=0.05))

inspect(rules)
inspect(sort(rules, by="lift"))

rules_df <- as(rules,"data.frame")

# 효용성 지표 추가
rules_df <- rules_df %>% mutate(index = support*confidence*lift)

rules_sort_support <- rules_df %>% arrange(desc(support))
rules_sort_confidence <- rules_df %>% arrange(desc(confidence))
rules_sort_lift <- rules_df %>% arrange(desc(lift))
rules_sort_index <- rules_df %>% arrange(desc(index))


# plot graph method로 시각화하기 
plot(rules[1:10], method="graph" , interactive=T)


# 추가 시각화 해보기 (extra question)
# 박종범, 임재인 학우와 같이 진행하였음
plotly_arules(rules, method = "scatterplot", measure = c("support", "confidence"), shading = "lift")

plotly_arules(rules, method = "matrix", measure = c("support", "confidence"), shading = "lift")

plot(rules[1:10], method="graph" , interactive=T)

plot(rules[1:10], method="grouped")

plot(rules, method="matrix", engine="3d")

install.packages("iplots")
plot(rules, method="iplots")

# plot(rules, method="two-key")   plot(rules, method="doubledecker")