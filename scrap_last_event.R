
# Carrega Pacotes -----------------------------------------------------------------------------

library(tidyverse)
library(httr)
library(rvest)
library(xml2)

# Carrega script com login e senha ------------------------------------------------------------

source("login.r")

# Login no Tapology ---------------------------------------------------------------------------

# Define a sessão e loga no grupo privado

url <- "https://www.tapology.com/sign_in"
my_session <- session(url) # Create a persistant session
unfilled_forms <- html_form(my_session) # find all forms in the web page
login_form <- unfilled_forms[[2]] # select the form you need to fill
filled_form <- html_form_set(login_form,
  "user[uid]" = username,
  "user[password]" = password
) # fill the form
login_session <- session_submit(my_session, filled_form) # Now you're logged in

# Raspagem ------------------------------------------------------------------------------------

# Define evento/card a ser raspado (pelo número do evento no site)

last_event <- 113674

# Inicia a raspagem das informações do evento

my_session_events <- my_session |> 
  session_jump_to("https://www.tapology.com/groups/583")

for (i in last_event) {
  page <- paste0("https://www.tapology.com/groups/583/events/", last_event)
  url <- jump_to(my_session, page)
  results <- read_html(url) %>% 
    html_nodes("table") %>% 
    .[[2]] %>% 
    html_table()
}

for (i in last_event) {
  page <- paste0("https://www.tapology.com/groups/583/events/", last_event)
  url <- jump_to(my_session, page)
  data <- read_html(url) |> 
    html_nodes("body") |> 
    xml_find_all("/html/body/div[1]/div[1]/div[2]/section/div[1]/div[1]/div[1]") |> 
    html_text()
}

# Limpeza e ajustes finos dos dados -----------------------------------------------------------

# Limpa as colunas e ajusta os nomes

df <- results[, -c(2:3, 5, 7, 9, 11, 13, 15)]

names(df)[1] <- "Rank"
names(df)[2] <- "Member"
names(df)[5] <- "Correct"
names(df)[6] <- "Decision"
names(df)[7] <- "Perfect"
names(df)[8] <- "Semi Perf"

# Cria as colunas com o nome da luta principal, a data do evento, mês e ano

event <- str_remove(data, "Saturday.*") |>   #Se o evento foi em outro dia, ajustar o dia da semana
  trimws()

data <- sub(".*Saturday", "", data) |>
  substring(3) |>
  trimws()

df <- df |>
  mutate(Event = event) |>
  mutate(Date = gsub(",", "", data))

# Edições ocasionais de acordo com o nome do evento para limpeza
# Exemplo:
#|mutate(Event = str_remove_all(Event, "Pay Per View")) %>%
# mutate(Date = str_remove_all(data, "\n\nPBC Fight Night on FS1: Montgomery vs. Jumakhonov\n\n\nFriday, "))  |> 
# mutate(Date = str_remove_all(Date, "DAZN"))

df <- df |> 
  mutate(Month = word(Date, 1)) |>
  mutate(Year = 2022)

# Limpa as pontuações

df$Correct <- gsub("(.*)x.*", "\\1", df$Correct)
df$Decision <- gsub("(.*)x.*", "\\1", df$Decision)
df$Perfect <- gsub("(.*)x.*", "\\1", df$Perfect)
df$`Semi Perf` <- gsub("(.*)x.*", "\\1", df$`Semi Perf`)

df$Correct <- as.double(df$Correct)
df$Perfect <- as.double(df$Perfect)
df$`Semi Perf` <- as.double(df$`Semi Perf`)

last_event <- df |> 
  rename_at("Semi Perf", ~"Semi_Perf")

# Empilha os dados do último evento com a base de eventos anteriores --------------------------

# Carrega a lista com eventos anteriores do tap champ

events <- readRDS("events.rds")

events$Semi_Perf <- as.double(events$Semi_Perf)

# Junta a base do último evento com a base de eventos anteriores e salva em RDS

events_last <- rbind(events, last_event) 

saveRDS(events_last, "events_last.rds") # base final empilhada. Depois renomear como "events.rds"
  