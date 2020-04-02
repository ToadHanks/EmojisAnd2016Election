library(openxlsx)
library(tidyverse)
library(utf8splain)

#----------------------------------------------------- emoji dictionary viewer -----------------------------------------------------
#12.1 is the latest release, 13 is in beta channel (Warnings have been suppressed, they were appearing due to a [safe] workaround)
suppressWarnings({
  uncleaned_df_emoji <- readr::read_delim(file = "http://www.unicode.org/Public/emoji/12.1//emoji-data.txt",
                                          delim = ";",
                                          col_names = c("Style", "Description")
  )
})

uncleaned_df_emoji <- uncleaned_df_emoji %>% drop_na() #Drop the rows with NA

#Only save the codepoint that begins with a number 0 or above
df_emoji <- uncleaned_df_emoji[!startsWith(uncleaned_df_emoji$Style, "#"), ]

#Give the codepoint a Unicode format, and then separate the dictionary into three columns
df_emoji %<>%
  dplyr::mutate(Codepoint = paste0("U+000", Style),
                Emoji = emo::ji_extract_all(df_emoji$Description),
                Keyword = stringr::str_extract(Description, "\\).+$") %>% 
                stringr::str_replace_all("\\)", "") %>% 
                stringr::str_trim(side = "left")) %>%
                dplyr::select(-Description) %>%
                dplyr::select(-Style)

View(df_emoji)
openxlsx::write.xlsx(df_emoji, "emoji_dictionary_12p1.xlsx") #alternatively you can write it to file too

#------------------------------------------------------- emoji downloader ------------------------------------------------------

#pass in the emojis you want to download as a character vector
data <- tibble(emojis = c("😐","😥","😴","😉","😛")) %>%
  mutate(rune = map_chr(emojis, ~ utf8splain::runes(.)$rune)) %>%    # convert to runes
  mutate(rune = str_remove(rune, fixed("U+"))) %>%                   # remove leading U+
  mutate(emoji_url = paste0("https://abs.twimg.com/emoji/v2/72x72/", # make url
                            tolower(rune), ".png"))

# download the files
map2(data$emoji_url, paste0(data$rune, ".png"), function(x, y) download.file(x, y, method = "curl"))
