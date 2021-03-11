library(dplyr)
install.packages("CHAID", repos="http://R-Forge.R-project.org")
install.packages("partykit")
library(CHAID)
library(partykit)
#file betöltése
bondora_preprocessed <-
  read.csv(
    "C:/Users/Domonkos/Documents/CHAID/bandora/Bondora_dataset/Bondora_preprocessed.csv",
    TRUE
  )

#szűrés ES országokra
bondora_ES <- bondora_preprocessed %>% filter(Country == "ES")

#megnézzük, hogy van-e hiányzó
bondora_ES %>% filter(is.na(Default)) %>% View

#a feladatban javasolt változó pool kiválasztása
bondora_selected <- bondora_ES %>% select(
  Default,
  UseOfLoan,
  Gender,
  County,
  City,
  HomeOwnershipType,
  EmploymentStatus,
  EmploymentDurationCurrentEmployer,
  OccupationArea,
  IncomeTotal,
  MaritalStatus,
  Education,
  ExistingLiabilities,
  DebtToIncome,
  RefinanceLiabilities
  
)
#változókat megnézzük
str(bondora_selected)
#magyarázó változók: 9 faktor változó, 2 numeric, 2 integer

#Mutassa be a lehetséges magyarázó változók és a célváltozó fontosabb statisztikai adatait!
summary(bondora_selected)
summary_df<- data.frame(unclass(summary(bondora_selected)), check.names = FALSE, stringsAsFactors = FALSE)
colnames(summary_df)[8] <- "Emp_dur"

write.csv2(summary_df, "summary.csv")
#default változó factorrá alakítása
bondora_selected <- bondora_selected %>% mutate(Default=factor(Default))


#megnezem, hogy mely nem kategoriavaltozo oszlopoknal van kevesebb, mint 10 kategoria (nincs ilyen az eredményváltozón kívül)
bondora_selected %>%
  select_if(function(col)
    length(unique(col)) <= 5 & !is.factor(col)) %>% head()

#mivel ez egyikre sem igaz -> csak eredetileg is kategóriaváltozókat használunk
#magyarázóváltozók és célváltozó kimentése
CHAID_df <- bondora_selected %>% select(colnames(bondora_selected %>% select_if(is.factor)), Default)


#milyen sok kategoria van egy-egy valtozoban
no_of_unique <- function(input){
  input %>% unique %>% length
  
}
lapply(CHAID_df,no_of_unique)


#kevesbe összefüggő és nem túl sok kategóriával rendelkező változók kiválasztása
CHAID_df <- CHAID_df %>% select(Default,UseOfLoan, Gender, HomeOwnershipType, OccupationArea,EmploymentDurationCurrentEmployer, EmploymentStatus, Education, MaritalStatus)


#modell futtatása
chaid_model <- chaid(Default ~ ., data = CHAID_df)
# 
plot(
  chaid_model,
  main = "Chaid modell eredménye",
  gp = gpar(
    lty = "solid",
    lwd = 3,
    fontsize = 8
  )
)

#mik a legfontosabb változók
sort(varimp(chaid_model), decreasing = TRUE)


#
#kevesbe összefüggő és nem túl sok kategóriával rendelkező változók kiválasztása
CHAID_df <- CHAID_df %>% select(Default,UseOfLoan, Gender, HomeOwnershipType, OccupationArea,EmploymentDurationCurrentEmployer, EmploymentStatus, Education, MaritalStatus)


#kevesbe összefüggő és nem túl sok kategóriával rendelkező változók kiválasztása
CHAID_df <- CHAID_df %>% select(Default, Gender, Education, EmploymentStatus, MaritalStatus)


#modell futtatása a már kiválasztott változókkal
chaid_model <- chaid(Default ~ ., data = CHAID_df)
# 
plot(
  chaid_model,
  main = "Chaid modell eredménye",
  gp = gpar(
    lty = "solid",
    lwd = 3,
    fontsize = 8
  )
)
print(chaid_model)
model_final <- predict(chaid_model)

install.packages("caret")
library(caret)
install.packages("e1071")
confusionMatrix(model_final, CHAID_df$Default)
install.packages("pROC")
pROC::auc(model_final %>% as.numeric(), CHAID_df$Default %>% as.numeric())
