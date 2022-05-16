library(rvest)
library(dplyr)
library(xml2)
library(stringr)
library(RSelenium)

input = "05531130960"
rD <- rsDriver(browser = "chrome", port = 4566L, chromever = "99.0.4844.51")
remDr <- rD[["client"]]
remDr$navigate("https://www.visureinrete.it/01_informazioni_sui_servizi/controllo_Partita_IVA_Imprese.asp")
Sys.sleep(5)

search_box <- remDr$findElement(value = "//input[contains(@class, 'piva_input_a')]")
search_box$sendKeysToElement(list(input, key = "enter"))
#search_box$clickElement()
button <- remDr$findElement(value = "//a[contains(@onclick, 'javascript: CheckAllPiva()')]")
button$clickElement()
reCAPTCHA <- remDr$findElement(value = "//span[contains(@class, 'recaptcha-checkbox goog-inline-block recaptcha-checkbox-unchecked rc-anchor-checkbox')]")
# NON VA

remDr$close()
rd$server$stop()
