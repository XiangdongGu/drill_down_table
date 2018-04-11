library(shiny)
library(DT)
library(dplyr)

# Define UI for application that draws a histogram
ui <- fluidPage(
   DT::dataTableOutput("tb")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$tb <- DT::renderDataTable({
     data <- mtcars %>%
       mutate(cyl = paste0("cyl - ", cyl)) %>%
       select(cyl, mpg, wt) %>%
       mutate(skey = paste0("@", cyl))
     
     dsum <- data %>%
       group_by(cyl) %>%
       summarise(mpg = sum(mpg), wt = sum(wt)) %>%
       ungroup() %>%
       mutate(skey = "@")
     data <- bind_rows(data, dsum) %>%
       arrange(cyl, skey) %>%
       mutate(cyl = ifelse(skey == "@", cyl, ""),
              ` ` = ifelse(skey == "@", '<span class="details-control">&oplus;</span>', "")) %>%
       select(skey, ` `, everything())
     
     data %>%
       datatable(
         rownames = FALSE,
         escape = FALSE,
         selection = "single",
         options = list(
           pageLength = 1000,
           dom = "ft",
           ordering = FALSE,
           columnDefs = list(list(visible = FALSE, targets = 0))
         ),
         callback = JS(
           "
           table.column(0).search('^@$', true, false).draw();
           var click = '';
           var oldtd = null;
           table.on('click', 'span.details-control', function() {
           var td = $(this), row = table.row(td.closest('tr'));
           var txt = row.data()[2];
           if (click != txt) {
           table.column(0).search('^@$|^@' + txt + '$', true, false).draw();
           click = txt;
           td.html('&CircleMinus;');
           if (oldtd != null) {
           oldtd.html('&oplus;');
           }
           oldtd = td;
           } else {
           table.column(0).search('^@$', true, false).draw();
           click = '';
           td.html('&oplus;');
           oldtd = null;
           }
           })
           "
         )) 
   }, server = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)

