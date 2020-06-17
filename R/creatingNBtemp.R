# tx <- c('---
# title: "R Notebook"
# output: html_notebook
# ---
#
# This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook. When you execute code within the notebook, the results appear beneath the code.
#
# Try executing this chunk by clicking the *Run* button within the chunk or by placing your cursor inside it and pressing *Cmd+Shift+Enter*.
#
# ```{r}
# plot(cars)
# ```
#
# Add a new chunk by clicking the *Insert Chunk* button on the toolbar or by pressing *Cmd+Option+I*.
#
# When you save the notebook, an HTML file containing the code and output will be saved alongside it (click the *Preview* button or press *Cmd+Shift+K* to preview the HTML file).
#
# The preview shows you a rendered HTML copy of the contents of the editor. Consequently, unlike *Knit*, *Preview* does not run any R code chunks. Instead, the output of the chunk when it was last run in the editor is displayed.'
#         )
#
# tx <- paste0(tx,
# "\n\n
# ```{r}
# x <- 1:10
# print(x)
# ```
# ### This is an additional line.
# ## This is 2(second attempt)
# # This is largest.")
#
# cat(tx, file = "trial.rmd")
# rmarkdown::render("trial.rmd", output_format = "html_document", output_file = "trial2.html")
#
#
# # to save and print the explain
# a <- capture.output({explain(mtcars$mpg)})
# for (i in a) {
#   cat(i)
#   cat("\n")
# }
