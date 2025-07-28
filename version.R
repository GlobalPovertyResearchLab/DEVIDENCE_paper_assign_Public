version <- function(db,
                    task_name,
                    title,
                    ngram = 3,
                    threshold = 0.05,
                    margin = 0.01,
                    override = '0',
                    dev_papers_dir = "Study Assignment/dev_papers") {
    ## Function to compare text similarity between a given
    ## paper and the existing database of papers.
    ##
    ## Inputs:
    ##  > db : dataframe : dataframe formatted same as paper_assign (main)
    ##  > task_name : str : task name of the new study
    ##  > title : str : study title
    ##  > ngram : int : ngram number, used to create ngrams of the text
    ##                  (n-length phrases of the text, each time shifted by
    ##                  one word)
    ##  > threshold : double : threshold for similarity, above which,
    ##                          paper is identified as similar
    ##  > margin : double : tolerance for similarity matching
    ##
    ## Outputs:
    ##  > result : str : "new", "newer", "old"

    dev_papers_path <- drive_get(dev_papers_dir)

    ## Obtain author last name and publication year using task_name
    ## strplit() outputs to lists which needs to be unlisted
    ## Unlist author last name and publication year for easy indexing
    db_copy <- db
    db_copy$author = sapply(strsplit(db_copy$task_name, " "), function(x) x[1])
    db_copy$year = sapply(strsplit(db_copy$task_name, " "), function(x) x[2])

    # (1) Prepare data used in study similarity comparison  ====================

    ## Get author, year, studyID, and pdfID
    author <- strsplit(task_name, " ")[[1]][1]
    year <- substr(task_name, nchar(author) + 2, nchar(author) + 5)
    studyID <- paste0(toupper(substr(author, 0, 3)), year, "_", substr(gsub("[^0-9]", "", digest(title, algo = "md5")), 2, 5))
    filename <- paste0(task_name, ".pdf")
    text <- paste(pdftools::pdf_text(filename), collapse='')
    text <- iconv(text, from = "latin1", to = "UTF-8", sub = "byte")
    text <- gsub("\n", "", text)
    if (nchar(text) > 20000) {text_short <- substr(text, 1, 20000)} else {text_short <- text}
    pdf_id <- paste0(toupper(substr(author, 0, 3)), year, "_PDF", gsub("[^0-9]", "", digest(text, algo = "md5")))
    pdf_name <- paste0(pdf_id, ".pdf")

    # (2) Check if PDF is new ==================================================

    ## Override (force add as new)
    if (override == '1') {
        return(c("New", studyID, pdf_id, text_short, ""))
    }

    ## Identify if paper is new by filename (filename is PDF_ID so must be unique)
    existing_files <- drive_ls(dev_papers_path)
    new <- (length(setdiff(pdf_name, existing_files$name)) != 0)

    ## Toss if PDF already exists
    if (new == F) {
        return(c("Old", studyID, pdf_id, text_short, "(PDF already exists)"))
    }

    # (3) PDF is new, now check if similar =====================================

    token_list <- tokenize_ngrams(text, n = ngram)
    hash_list <- strsplit(toString(hash_string(unlist(token_list))), ", ")
    title_token_list <- tokenize_ngrams(gsub('’', "'", title), n = ngram)
    title_hash_list <- strsplit(toString(hash_string(unlist(title_token_list))), ", ")

    for (idx in 1:nrow(db_copy)) {
        ref_token_list <- tokenize_ngrams(gsub('’', "'", db_copy[idx, "text"]), n = ngram)
        ref_hash_list <- strsplit(toString(hash_string(unlist(ref_token_list))), ", ")
        jac_sim <- jaccard_similarity(ref_hash_list[[1]], hash_list[[1]])
        mean <- c(mean, jac_sim)

        ref_title_token_list <- tokenize_ngrams(db_copy[idx, "title"], n = ngram)
        ref_title_hash_list <- strsplit(toString(hash_string(unlist(ref_title_token_list))), ", ")
        title_jac_sim <- jaccard_similarity(title_hash_list[[1]], ref_title_hash_list[[1]])

        ## If pdf content is similar, file as either newer or old
        if (jac_sim > threshold | (abs(jac_sim - threshold) <= margin) | title_jac_sim >=0.95) {
            studyID <- db_copy$studyID[idx]
            if (year > db_copy$year[idx]) {
                if (year > db_copy$year[idx]) {
                    return(c("Newer", studyID, pdf_id, text_short, paste0("Version of ", db_copy$studyID[idx])))
                }
            }
            else {
                return(c("Old", studyID, pdf_id, text_short, paste0("Version of ", db_copy$studyID[idx])))
            }
        }
    }

    # (4) PDF is unique, add paper =============================================
    return(c("New", studyID, pdf_id, text_short, ""))
}

