library(dplyr)
install.packages("CHAID")
library(CHAID)
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

#magyarázóváltozók és célváltozó kiválasztása
bondora_selected <- bondora_ES %>% select(
  Default,
  LoanDuration,
  Age,
  HomeOwnershipType,
  EmploymentStatus,
  IncomeTotal,
  DebtToIncome,
  
)

#
proba <- bondora_selected %>% mutate(Home_fact=factor(HomeOwnershipType))

