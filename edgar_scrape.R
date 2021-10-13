# This script provides an rvest / tidyverse solution to obtaining 10-k reports from the SEC's EDGAR database.
# Note however there are already (better) tools available doing this, including Micah J Waldstein's R library
# edgarWebR (https://cran.r-project.org/web/packages/edgarWebR/index.html). These tools come with the added
# advantage of easy access to company metadata. Whether using this solution or something like edgarWebR
# you will have to respect the SEC's rules for automated access: (1) no more than 10 requests per second
# and (2) you must provide your user-agent information.

# you'll need to install pacman first if you don't have it already
#install.packages("pacman")
pacman::p_load(tibble, rvest, dplyr, stringr, reader)

# here I'm just building a df from sample URLs (some machine readable, some not)
page_urls <- tribble(~url,
                     "https://www.sec.gov/Archives/edgar/data/1671933/000156459021050550/ttd-ex10_8.htm",
                     "https://www.sec.gov/Archives/edgar/data/1082733/000165495421010530/mercervismspa-9272021fore.htm",
                     "https://www.sec.gov/Archives/edgar/data/925660/000154812321000116/exhflxtlease8172021.htm",
                     "https://www.sec.gov/Archives/edgar/data/1388410/000138841021000022/ex101executiveagreement.htm",
                     "https://www.sec.gov/Archives/edgar/data/16732/000119312521284156/d216246dex10.htm",
                     "https://www.sec.gov/Archives/edgar/data/1091748/000095012321010456/d340796dex101.htm",
                     "https://www.sec.gov/Archives/edgar/data/1057083/000156459021042728/pcti-ex10_215.htm",
                     "https://www.sec.gov/Archives/edgar/data/1598308/000126246321000476/ex10.htm",
                     "https://www.sec.gov/Archives/edgar/data/716643/000071664321000055/rgs2021630ex10c.htm",
                     "https://www.sec.gov/Archives/edgar/data/1616736/000151597121000076/exhibit101.htm")

# to access anything in EDGAR you *have* to change your user-agent or they'll block you from access
ua <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/89.0.4389.90 Safari/537.36"

# create empty list for dfs generated in loop
list_of_dfs <- list()

# write for loop, iterating over urls in page_urls
for (p in page_urls$url){
  
  # create html_session, specifying user_agent information
  page_session <- html_session(p, user_agent(ua))
  
  # read html from session
  page_source <- read_html(page_session) 
  
  # grab full text (if machine readable)
  page_text <- page_source %>% html_text(trim = TRUE)
  
  # grab .jpgs for each page (if .jpg format)
  page_img_urls <- page_source %>% html_nodes("img") %>% html_attr("src") %>% url_absolute(p) %>% paste0(collapse = "; ")
  
  # create df with page url, text, and .jpg urls
  curr_data <- tibble(
    url = p,
    page_text = page_text,
    page_img_url = page_img_urls
  ) %>%
    # note that sometimes there are links to .jpgs, but its not the text, it's just like an image like this 
    # https://www.sec.gov/Archives/edgar/data/1091748/000095012321010456/g340796g0811194346929.jpg;
    # in these cases, we'll call these "both" in the text_or_img column, but only the ones tagged "image"
    # lack the text you're after, and those are the URLs you'll have to target in another script. The
    # other script should download the .jpgs in the page_img_url column and then OCR them
    mutate(page_text_length = str_count(page_text)) %>%
    # note that the above function (str_count) is counting periods, commas, etc. not just words
    mutate(text_or_img = case_when(
      page_text_length > 100 & str_count(page_img_urls) > 2 ~ "both",
      page_text_length < 100 & str_count(page_img_urls) > 2 ~ "image",
      TRUE ~ "text"
    ))

  list_of_dfs[[p]] <- curr_data
  
  Sys.sleep(sample(1:5, 1)) #this is overkill; they allow 10 requests/second
  
}

# merge the results of the for loop
all_results <- bind_rows(list_of_dfs)

# save results as csv; may need to change path depending on working directory
write_csv(all_results, "EDGAR_scrape_results.csv")


