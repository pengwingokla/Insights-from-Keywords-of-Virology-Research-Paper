
library(rvest)
library(ggplot2)
library(stringr)
library(rvest)
library(httr)
library(xml2)
library(jsonlite)
library(dplyr)

#-------------------------------------------------------------------------------
urls <- c(
  "https://virologyj.biomedcentral.com/articles/10.1186/s12985-024-02582-w",
  "https://virologyj.biomedcentral.com/articles/10.1186/s12985-024-02564-y",
  "https://virologyj.biomedcentral.com/articles/10.1186/s12985-024-02531-7",
  "https://virologyj.biomedcentral.com/articles/10.1186/s12985-024-02573-x",
  "https://virologyj.biomedcentral.com/articles/10.1186/s12985-024-02505-9",
  "https://virologyj.biomedcentral.com/articles/10.1186/s12985-024-02568-8",
  "https://virologyj.biomedcentral.com/articles/10.1186/s12985-024-02571-z",
  "https://virologyj.biomedcentral.com/articles/10.1186/s12985-024-02572-y",
  "https://virologyj.biomedcentral.com/articles/10.1186/s12985-024-02551-3",
  "https://virologyj.biomedcentral.com/articles/10.1186/s12985-024-02537-1",
  "https://virologyj.biomedcentral.com/articles/10.1186/s12985-024-02560-2"
)

scrape_keywords <- function(base_url) {
  #' Scrape Keywords
  #'
  #' Extracts keywords from the JSON-LD metadata of a given paper's URL.
  #' The function cleans the keywords by converting them to lowercase and trimming spaces.
  #'
  #' @param base_url A character string containing the URL of the paper.
  #' @return A character vector of cleaned keywords.
  #' @examples
  #' scrape_keywords("https://example.com/paper")
  
  # Locate the script tag with data-test="dataLayer"
  page <- read_html(base_url)
  json_ld_content <- page %>%
    html_node('script[type="application/ld+json"]') %>%
    html_text()
  json_data <- fromJSON(json_ld_content)
  
  # Extract 
  keywords <- json_data$mainEntity$keywords
  
  # Clean the keywords
  keywords <- tolower(keywords)  # Convert to lowercase
  keywords <- trimws(keywords)   # Remove leading and trailing spaces
  
  return(keywords)
}
scrape_auemails <- function(base_url) {
  #' Scrape Authors' Emails
  #'
  #' This function extracts the authors' names and their corresponding email addresses 
  #' from the JSON-LD metadata of a research paper webpage. If an author has no email, 
  #' the function returns `NA` for that author.
  #'
  #' @param base_url A character string containing the URL of the paper.
  #' @return A data frame with two columns: `Name` (author names) and `Email` (email addresses).
  #' @examples
  #' scrape_auemails("https://example.com/paper")
  
  
  # Locate the <script> tag containing JSON-LD data
  page <- read_html(base_url)
  json_ld_content <- page %>%
    html_node('script[type="application/ld+json"]') %>%
    html_text()
  json_data <- fromJSON(json_ld_content)
  #print(json_data)
  # Extract
  authors <- json_data$mainEntity$author$name
  
  author_data <- data.frame(
    Name = character(),
    Email = character(),
    stringsAsFactors = FALSE
  )
  
  for (author in authors) {
    name <- json_data$mainEntity$author$name
    email <- if (!is.null(json_data$mainEntity$author$email)) json_data$mainEntity$author$email else NA
    #print(email)
    author_data <- rbind(author_data, data.frame(Name = name, Email = email, stringsAsFactors = FALSE))
  }
  
  return(unique(author_data))
}

scrape_info <- function(base_url) {
  #' Scrape Paper Information
  #'
  #' This function scrapes metadata and other details about a research paper, including:
  #' - Title
  #' - Authors
  #' - Authors' emails (concatenated into a single string)
  #' - Agent email (if available)
  #' - Publication date
  #' - Abstract
  #' - Keywords
  #'
  #' @param base_url A character string containing the URL of the paper.
  #' @return A data frame with one row containing metadata fields (`PaperURL`, `Title`, 
  #' `Authors`, `Emails`, `AgentEmail`, `PublicationDate`, `Abstract`, `Keywords`).
  #' @examples
  #' scrape_info("https://example.com/paper")
  
  
  # Construct the URL for the specified year
  url <- paste0(base_url)
  
  # Fetch the web page
  page <- read_html(url)
  #write_html(page, "page_debug.html")
  
  
  # Extract the data fields
  titles <- page %>% html_nodes('meta[name="dc.title"]') %>% html_attr("content")
  author <- page %>% html_nodes('meta[name="dc.creator"]') %>% html_attr("content")
  au_email <- scrape_auemails(base_url)
  ag_email <- page %>% html_node('meta[name="dc.rightsAgent"]') %>% html_attr("content")
  pub_date <- page %>% html_node('meta[name="dc.date"]') %>% html_attr("content")
  abstract <- page %>% html_node('meta[name="dc.description"]') %>% html_attr("content")
  keywords <- scrape_keywords(base_url)
  
  # Create a data frame for the paper
  email_str <- if (!is.null(au_email)) {
    # Concatenate all authors' emails into a single string
    paste(na.omit(au_email$Email), collapse = ", ")
  } else {
    NA
  }
  
  df_info <- data.frame(
    PaperURL = base_url,
    Title = titles,
    Authors = paste(author, collapse = ", "),
    Emails = email_str,
    AgentEmail = ag_email,
    PublicationDate = pub_date,
    Abstract = abstract,
    Keywords = paste(keywords, collapse = ", "),
    stringsAsFactors = FALSE
  )
  return(df_info)
}

scrape_section_texts <- function(base_url) {
  
  page <- read_html(base_url)
  
  #Extract text from the Description
  desc_text <- page %>% 
    html_nodes('meta[property="og:description"]') %>% 
    html_attr("content")
  
  # Extract text from the Introduction section
  intro_text <- page %>%
    html_node('div#Sec1-content') %>%
    html_text(trim = TRUE)
  
  # Extract text from the Methods section
  methods_text <- page %>%
    html_node('div#Sec2-content') %>%
    html_text(trim = TRUE)
  
  # Extract text from the Results section
  results_text <- page %>%
    html_node('div#Sec9-content') %>%
    html_text(trim = TRUE)
  
  # Extract text from the Discussion section
  diss_text <- page %>%
    html_node('div#Sec13-content') %>%
    html_text(trim = TRUE)
  
  # Combine the extracted texts into a list
  section_texts <- list(
    Introduction = intro_text,
    Description = desc_text,
    Methods = methods_text,
    Results = results_text,
    Discussion = diss_text
  )
  
  return(section_texts)
}

count_section_word <- function(base_url) {
  #' Count Word Count for Each Section
  #'
  #' This function calculates the word count for each section of a research paper by 
  #' scraping the text content and counting the number of words in each section.
  #'
  #' @param base_url A character string containing the URL of the paper.
  #' @return A data frame with three columns:
  #' - `PaperURL`: The URL of the paper.
  #' - `Section`: The name of the section.
  #' - `WordCount`: The word count for the section.
  #' @examples
  #' count_section_word("https://example.com/paper")
  
  section_texts <- scrape_section_texts(base_url)
  
  # Compute word counts for each section
  count <- sapply(section_texts, function(text) {
    if (!is.null(text) && !is.na(text) && text != "") {
      # Split by whitespace
      length(strsplit(text, "\\s+")[[1]])  
    } else {
      0 
    }
  })
  # Dataframe
  word_count_df <- data.frame(
    PaperURL = rep(base_url, length(count)),
    Section = names(count),
    WordCount = count,
    stringsAsFactors = FALSE
  )
  
  return(word_count_df)
}


scrape_info(urls[10])
section_txt <- scrape_section_texts(urls[10])
print(section_txt)
#$Introduction
#[1] "Diarrhoea is prevalent worldwide"
#$Description
#[1] "Background Infection is"
#$Methods
#[1] "PatientsAccording to the doctor’s..." 
#$Results
#[1] NA
#$Discussion
#[1] NA
word_count  <- count_section_word(urls[10])
print(word_count)
#PaperURL
#Introduction https://virologyj.biomedcentral.com/articles/10.1186/s12985-024-02537-1
#Description  https://virologyj.biomedcentral.com/articles/10.1186/s12985-024-02537-1
#Methods      https://virologyj.biomedcentral.com/articles/10.1186/s12985-024-02537-1
#Results      https://virologyj.biomedcentral.com/articles/10.1186/s12985-024-02537-1
#Discussion   https://virologyj.biomedcentral.com/articles/10.1186/s12985-024-02537-1
#Section WordCount
#Introduction Introduction       353
#Description   Description       271
#Methods           Methods       651
#Results           Results         0
#Discussion     Discussion         0

#-------------------------------------------------------------------------------
# Plot Keyword Occurrences in Recent Articles
#-------------------------------------------------------------------------------
get_all_keyw <-function(urls) {
  #' Get All Unique Keywords from URLs
  #'
  #' Collects all unique keywords across multiple paper URLs by invoking `scrape_keywords` 
  #' for each URL.
  #'
  #' @param urls A character vector of URLs to scrape keywords from.
  #' @return A character vector of all unique keywords found across the URLs.
  #' @examples
  #' get_all_keyw(c("https://example1.com", "https://example2.com"))
  
  all_keyw <- c()
  
  # Loop through URLs and collect keywords
  for (url in urls) {
    cat("Scraping keywords from:", url, "\n")
    keywords <- scrape_keywords(url)
    all_keyw <- unique(c(all_keyw, keywords))
  }
  
  return(all_keyw)
}

combine_urls_texts <- function(urls) {
  #' Combine Texts from All URLs by Section
  #'
  #' Scrapes and combines text content for each section (e.g., Introduction, Methods) 
  #' from multiple paper URLs. The texts are converted to lowercase and concatenated.
  #'
  #' @param urls A character vector of URLs to scrape section texts from.
  #' @return A named list containing combined text for each section.
  #' @examples
  #' combine_urls_texts(c("https://example1.com", "https://example2.com"))
  
  # Initialize an empty list to store texts from all URLs
  all_texts <- list(
    Introduction = "",
    Description = "",
    Methods = "",
    Results = "",
    Discussion = ""
  )
  
  # Loop through each URL and scrape texts
  for (url in urls) {
    section_texts <- scrape_section_texts(url)
    
    # Combine text for each section, convert to lowercase
    all_texts$Introduction <- paste(all_texts$Introduction, tolower(section_texts$Introduction), sep = "\n")
    all_texts$Description <- paste(all_texts$Description, tolower(section_texts$Description), sep = "\n")
    all_texts$Methods <- paste(all_texts$Methods, tolower(section_texts$Methods), sep = "\n")
    all_texts$Results <- paste(all_texts$Results, tolower(section_texts$Results), sep = "\n")
    all_texts$Discussion <- paste(all_texts$Discussion, tolower(section_texts$Discussion), sep = "\n")
  }
  
  return(all_texts)
}

count_keywords <- function(text, keywords) {
  #' Count Keyword Occurrences in Text
  #'
  #' Counts the occurrences of each keyword within a given text.
  #'
  #' @param text A character string containing the combined text from one or more sections.
  #' @param keywords A character vector of keywords to count in the text.
  #' @return A named numeric vector where names are keywords and values are their counts.
  #' @examples
  #' count_keywords("example text example", c("example", "text"))
  
  counts <- sapply(keywords, function(keyword) {
    sum(str_count(text, fixed(keyword)))
  })
  return(counts)
}
  
plot_keyw_freq <- function(keyw_list, combined_text) {
  #' Plot Keyword Frequency
  #'
  #' Creates a bar plot showing the frequency of the most common keywords found in the text.
  #' The plot is limited to the top 20 keywords.
  #'
  #' @param keyw_list A character vector of keywords.
  #' @param combined_text A character string containing combined text from all sections.
  #' @return A ggplot object representing the bar chart of keyword frequencies.
  
  
  # Count occurrences of keywords
  keyword_counts <- count_keywords(combined_text, keyw_list)
  
  # Create a data frame
  keyword_freq <- data.frame(
    Keyword = keyw_list,
    Frequency = keyword_counts
  )
  
  # Sort and select top 20
  keyword_freq <- keyword_freq %>%
    arrange(desc(Frequency)) %>%
    slice_head(n = 20)
  
  # Ensure the `Keyword` column is a factor with levels 
  # in descending order of Frequency
  keyword_freq$Keyword <- factor(keyword_freq$Keyword, 
                                 levels = rev(keyword_freq$Keyword))
  
  # Plot the keyword frequencies
  ggplot(keyword_freq, aes(x = Keyword, y = Frequency)) +
    geom_bar(stat = "identity", fill = "orange") +
    coord_flip() +
    labs(
      title = "Keyword Frequency in Most Recent Articles",
      x = "Keywords",
      y = "Frequency"
    ) +
    theme_minimal()
}

# Plot 
combined_text <- combine_urls_texts(urls)
all_keyw_list  <- c(get_all_keyw(urls))
plot_keyw_freq(all_keyw_list, combined_text)

#-------------------------------------------------------------------------------
# Box Plot for Word Count Distribution by Section
#-------------------------------------------------------------------------------

scrape_info_multiple <- function(urls) {
  # Initialize df
  info_df <- data.frame(
    PaperURL = character(),
    Title = character(),
    Authors = character(),
    Emails = character(),
    AgentEmail = character(),
    PublicationDate = character(),
    Abstract = character(),
    Keywords = character(),
    stringsAsFactors = FALSE
  )
  
  # Iterate through the URLs
  for (url in urls) {
    single_info <- scrape_info(url)
    info_df <- rbind(info_df, single_info)
  }
  
  return(info_df)
}

count_section_words_multiple <- function(urls) {
  # Initialize df
  word_count_df <- data.frame(
    PaperURL = character(),
    Section = character(),
    WordCount = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Iterate through the URLs
  for (url in urls) {
    cat("Counting section words for:", url, "\n")
    single_word_count <- count_section_word(url)
    word_count_df <- rbind(word_count_df, single_word_count)
  }
  
  return(word_count_df)
}

df_info       <- scrape_info_multiple(urls)
df_word_count <- count_section_words_multiple(urls)

# Merge the data frames by `PaperURL`
df_bySection <- merge(df_word_count, df_info, by = "PaperURL", all = TRUE)
df_bySection <- df_bySection[df_bySection$WordCount > 0, ] # filter NA val
write.csv(df_bySection, file = "df_bySection.csv", row.names = FALSE)

ggplot(df_bySection, aes(x = Section, y = WordCount, fill = Section)) +
  geom_boxplot(outlier.colour = "red") +
  stat_summary(fun = mean, geom = "point", shape = 4, color = "black") + # Add mean
  labs(
    title = "Word Count Distribution by Section",
    x = "Section",
    y = "Word Count"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

