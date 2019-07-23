
form.table <- jsonlite::fromJSON("data/all-form-table.json")


forms <- unique(form.table$formName)
for(form in forms) {

  responses <- subset(form.table, formName == form & nchar(response) > 15 & type == 'NARRATIVE')

  html <- c(
    paste("<h1>", form, "<h1>"),
    paste("<h2>", responses$question, "</h2>", "<p>", responses$response, "</p>"))

  writeLines(html, sprintf("explore/%s.html", gsub(form, pattern="/", replacement = "_")))

}
