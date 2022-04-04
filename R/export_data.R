library("xlsx")
library("lubridate")

options(max.print=1000000)

now <- date(as.POSIXlt(now(), format="%Y-/%m"))
last_month <- now - months(2)
month <- format(as.Date(last_month), "%Y-%m")


Narrative_data <- all.form.tbl %>% filter(type == "NARRATIVE" )

print(month)

Narrative_data_Month_2021 <- Narrative_data %>%filter(Month == '2021-10')

Narrative_data_Month_2021 <- Narrative_data_Month_2021 %>% drop_na(response)
write.xlsx(Narrative_data_Month_2021, "C:\\tmp\\OCTUBRE2021.xlsx")
