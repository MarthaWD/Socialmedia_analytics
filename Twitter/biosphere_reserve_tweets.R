#################################################################################################################
#Reference: Barrie, Christopher and Ho, Justin Chun-ting. (2021).                                               #
#           academictwitteR: an R package to access the Twitter Academic Research Product Track v2 API endpoint.# 
#           Journal of Open Source Software, 6(62), 3272, https://doi.org/10.21105/joss.03272                   #
#################################################################################################################

install.packages("academictwitteR")
devtools::install_github("cjbarrie/academictwitteR", build_vignettes = TRUE) #Alternatively, you can install the development version with
vignette("academictwitteR-intro") #Get started by reading

library(academictwitteR)

bearer_token <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX" # Insert bearer token

file_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(file_dir)

get_tweets <- function(q="",page_n=500,start_time,end_time,token,next_token=""){
  # if(n>500){
  #   warning("n too big. Using 500 instead")
  #   n <- 500
  # }
  # if(n<5){
  #   warning("n too small Using 10 instead")
  #   n <- 500
  # }
  if(missing(start_time)){
    stop("start time must be specified.")
  }
  if(missing(end_time)){
    stop("end time must be specified.")
  }
  if(missing(token)){
    stop("bearer token must be specified.")  
  }
  if(substr(token,1,7)=="Bearer "){
    bearer <- token
  } else{
    bearer <- paste0("Bearer ",token)
  }
  #endpoint
  url <- "https://api.twitter.com/2/tweets/search/all"
  #parameters
  params = list(
    "query" = q,
    "max_results" = page_n,
    "start_time" = start_time,
    "end_time" = end_time, 		
    "tweet.fields" = "attachments,author_id,context_annotations,conversation_id,created_at,entities,geo,id,in_reply_to_user_id,lang,public_metrics,possibly_sensitive,referenced_tweets,source,text,withheld",
    "user.fields" = "created_at,description,entities,id,location,name,pinned_tweet_id,profile_image_url,protected,public_metrics,url,username,verified,withheld",
    "expansions" = "author_id,entities.mentions.username,geo.place_id,in_reply_to_user_id,referenced_tweets.id,referenced_tweets.id.author_id",
    "place.fields" = "contained_within,country,country_code,full_name,geo,id,name,place_type"
  )
  if(next_token!=""){
    params[["next_token"]] <- next_token
  }
  r <- httr::GET(url,httr::add_headers(Authorization = bearer),query=params)
  
  #fix random 503 errors
  count <- 0
  while(httr::status_code(r)==503 & count<4){
    r <- httr::GET(url,httr::add_headers(Authorization = bearer),query=params)
    count <- count+1
    Sys.sleep(count*5)
  }
  if(httr::status_code(r)==429){
    cat("Rate limit reached, sleeping... \n")
    Sys.sleep(900)
    r <- httr::GET(url,httr::add_headers(Authorization = bearer),query=params)
  }
  
  if(httr::status_code(r)!=200){
    stop(paste("something went wrong. Status code:", httr::status_code(r)))
  }
  if(httr::headers(r)$`x-rate-limit-remaining`=="1"){
    warning(paste("x-rate-limit-remaining=1. Resets at",as.POSIXct(as.numeric(httr::headers(r)$`x-rate-limit-reset`), origin="1970-01-01")))
  }
  dat <- jsonlite::fromJSON(httr::content(r, "text"))
  dat
}

fetch_data <- function(built_query, data_path, file, bind_tweets, start_tweets, end_tweets, bearer_token = get_bearer(), n, page_n, verbose){
  nextoken <- ""
  df.all <- data.frame()
  toknum <- 0
  ntweets <- 0
  while (!is.null(nextoken)) {
    df <-
      get_tweets(
        q = built_query,
        page_n = page_n,
        start_time = start_tweets,
        end_time = end_tweets,
        token = bearer_token,
        next_token = nextoken
      )
    if (is.null(data_path)) {
      # if data path is null, generate data.frame object within loop
      df.all <- dplyr::bind_rows(df.all, df$data)
    }
    
    if (!is.null(data_path) & is.null(file) & bind_tweets == F) {
      df_to_json(df, data_path)
    }
    if (!is.null(data_path)) {
      df_to_json(df, data_path)
      df.all <-
        dplyr::bind_rows(df.all, df$data) #and combine new data with old within function
    }
    
    nextoken <-
      df$meta$next_token #this is NULL if there are no pages left
    if(verbose) {
      toknum <- toknum + 1
      ntweets <- ntweets + nrow(df$data)
      cat(
        "query: <",
        built_query,
        ">: ",
        "(tweets captured this page: ",
        nrow(df$data),
        "). Total pages queried: ",
        toknum,
        ". Total tweets ingested: ",
        ntweets, 
        ". \n",
        sep = ""
      )
    }
    Sys.sleep(3.1)
    if (ntweets > n){ # Check n
      df.all <- df.all[1:n,] # remove extra
      cat("Amount of tweets exceeds ", n, ": finishing collection.\n")
      break
    }
    if (is.null(nextoken)) {
      if(verbose) {
        cat("This is the last page for", built_query, ": finishing collection.\n")
      }
      break
    }
  }
  
  if (is.null(data_path) & is.null(file)) {
    return(df.all) # return to data.frame
  }
  if (!is.null(file)) {
    saveRDS(df.all, file = file) # save as RDS
    return(df.all) # return data.frame
  }
  
  if (!is.null(data_path) & bind_tweets==T) {
    return(df.all) # return data.frame
  }
  
  if (!is.null(data_path) &
      is.null(file) & bind_tweets == F) {
    cat("Data stored as JSONs: use bind_tweets_json function to bundle into data.frame")
  }
}

check_bearer <- function(bearer_token){
  if(missing(bearer_token)){
    stop("bearer token must be specified.")
  }
  if(substr(bearer_token,1,7)=="Bearer "){
    bearer <- bearer_token
  } else{
    bearer <- paste0("Bearer ",bearer_token)
  }
  return(bearer)
}

check_data_path <- function(data_path, file, bind_tweets){
  #warning re data storage recommendations if no data path set
  if (is.null(data_path)) {
    warning(
      "Recommended to specify a data path in order to mitigate data loss when ingesting large amounts of data.",
      call. = FALSE,
      immediate. = TRUE
    )
  }
  #warning re data.frame object and necessity of assignment
  if (is.null(data_path) & is.null(file)) {
    warning(
      "Tweets will not be stored as JSONs or as a .rds file and will only be available in local memory if assigned to an object.",
      call. = FALSE,
      immediate. = TRUE
    )
  }
  #stop clause for if user sets bind_tweets to FALSE but sets no data path
  if (is.null(data_path) & bind_tweets == F) {
    stop("Argument (bind_tweets = F) only valid when a data_path is specified.")
  }
  #warning re binding of tweets when a data path and file path have been set but bind_tweets is set to FALSE
  if (!is.null(data_path) & !is.null(file) & bind_tweets == F) {
    warning(
      "Tweets will still be bound in local memory to generate .rds file. Argument (bind_tweets = F) only valid when just a data path has been specified.",
      call. = FALSE,
      immediate. = TRUE
    )
  }
  #warning re data storage and memory limits when setting bind_tweets to TRUE 
  if (!is.null(data_path) & is.null(file) & bind_tweets == T) {
    warning(
      "Tweets will be bound in local memory as well as stored as JSONs.",
      call. = FALSE,
      immediate. = TRUE
    )
  }
}

create_data_dir <- function(data_path){
  #create folders for storage
  ifelse(!dir.exists(file.path(data_path)),
         dir.create(file.path(data_path), showWarnings = FALSE),
         warning(
           "Directory already exists. Existing JSON files may be parsed and returned, choose a new path if this is not intended.",
           call. = FALSE,
           immediate. = TRUE
         ))
}

df_to_json <- function(df, data_path){
  # check input
  # if data path is supplied and file name given, generate data.frame object within loop and JSONs
  jsonlite::write_json(df$data,
                       paste0(data_path, "data_", df$data$id[nrow(df$data)], ".json"))
  jsonlite::write_json(df$includes,
                       paste0(data_path, "users_", df$data$id[nrow(df$data)], ".json"))
}

create_storage_dir <- function(data_path, export_query, built_query, start_tweets, end_tweets){
  if (!is.null(data_path)){
    create_data_dir(data_path)
    if (isTRUE(export_query)){ # Note export_query is called only if data path is supplied
      # Writing query to file (for resuming)
      filecon <- file(paste0(data_path,"query"))
      writeLines(c(built_query,start_tweets,end_tweets), filecon)
      close(filecon)
    }
  }
}


.gen_random_dir <- function() {
  paste0(tempdir(), "/", paste0(sample(letters, 20), collapse = ""))
}

# Build tweet query according to targeted parameters, can then be input to main \code{\link{get_all_tweets}} function as query parameter

build_query <- function(query = NULL,
                        exclude = NULL,
                        is_retweet = NULL, 
                        is_reply = FALSE, 
                        is_quote = FALSE,
                        is_verified = FALSE,
                        place = NULL, 
                        country = NULL, 
                        point_radius = NULL,
                        bbox = NULL,
                        geo_query = FALSE,
                        remove_promoted = FALSE,
                        has_hashtags = FALSE,
                        has_cashtags = FALSE,
                        has_links = FALSE,
                        has_mentions = FALSE,
                        has_media = FALSE,
                        has_images = FALSE,
                        has_videos = FALSE,
                        has_geo = FALSE,
                        lang= NULL,
                        conversation_id = NULL) {
  
  if(isTRUE(length(query) >1)) {
    query <- paste("(",paste(query, collapse = " OR "),")", sep = "")
  }
  
  if(!is.null(exclude)) {
    query <- paste(query, paste("-", exclude, sep = "", collapse = " "))
  }
  
  if (isTRUE(is_retweet) & isTRUE(is_reply)) {
    stop("A tweet cannot be both a retweet and a reply")
  }
  
  if (isTRUE(is_quote) & isTRUE(is_reply)) {
    stop("A tweet cannot be both a quote tweet and a reply")
  }
  
  if (isTRUE(point_radius) & isTRUE(bbox)) {
    stop("Select either point radius or bounding box")
  }
  
  if(!is.null(is_retweet)){
    if(isTRUE(is_retweet)) {
      query <- paste(query, "is:retweet")
    } else if(is_retweet == FALSE) {
      query <- paste(query, "-is:retweet")
    }
  }
  
  if(isTRUE(is_reply)) {
    query <- paste(query, "is:reply")
  }
  
  if(isTRUE(is_quote)) {
    query <- paste(query, "is:quote")
  }
  
  if(isTRUE(is_verified)) {
    query <- paste(query, "is:verified")
  }
  
  if(!is.null(place)) {
    query <- paste(query, paste0("place:", place))
  }
  
  if(!is.null(country)) {
    query <- paste(query, paste0("place_country:", country))
  }
  
  if(isTRUE(geo_query)) {
    if(response <- menu(c("Point radius", "Bounding box"), title="Which geo buffer type type do you want?") ==1) {
      x <- readline("What is longitude? ")  
      y <- readline("What is latitude? ")  
      z <- readline("What is radius? ")
      
      zn<- as.integer(z)
      while(zn>25) {
        cat("Radius must be less than 25 miles")
        z <- readline("What is radius? ")
        zn<- as.integer(z)
      }
      
      z <- paste0(z, "mi")
      
      r <- paste(x,y,z)
      query <- paste(query, paste0("point_radius:","[", r,"]"))
    }
    else if(response <- menu(c("Point radius", "Bounding box"), title="Which geo buffer type type do you want?") ==2) {
      w <- readline("What is west longitude? ")  
      x <- readline("What is south latitude? ")
      y <- readline("What is east longitude? ")
      z <- readline("What is north latitude? ")
      
      z <- paste(w,x,y,z)
      
      query <- paste(query, paste0("bounding_box:","[", z,"]"))
    }
    
  }
  
  if(!is.null(point_radius)) {
    x <- point_radius[1]
    y <- point_radius[2]
    z <- point_radius[3]
    
    zn<- as.numeric(z)
    while(zn>25) {
      cat("Radius must be less than 25 miles")
      z <- readline("Input new radius: ")
      zn<- as.numeric(z)
    }
    
    z <- paste0(z, "mi")
    
    r <- paste(x,y,z)
    query <- paste(query, paste0("point_radius:","[", r,"]"))
  }
  
  if(!is.null(bbox)) {
    w <- bbox[1]
    x <- bbox[2]
    y <- bbox[3]
    z <- bbox[4]
    
    z <- paste(w,x,y,z)
    
    query <- paste(query, paste0("bounding_box:","[", z,"]"))
  }
  
  if(isTRUE(remove_promoted)) {
    query <- paste(query, "-is:nullcast")
  }
  
  if(isTRUE(has_hashtags)) {
    query <- paste(query, "has:hashtags")
  }
  
  if(isTRUE(has_cashtags)) {
    query <- paste(query, "has:cashtags")
  }
  
  if(isTRUE(has_links)) {
    query <- paste(query, "has:links")
  }
  
  if(isTRUE(has_mentions)) {
    query <- paste(query, "has:mentions")
  }
  
  if(isTRUE(has_media)) {
    query <- paste(query, "has:media")
  }
  
  if(isTRUE(has_images)) {
    query <- paste(query, "has:images")
  }
  
  if(isTRUE(has_videos)) {
    query <- paste(query, "has:videos")
  }
  
  if(isTRUE(has_geo)) {
    query <- paste(query, "has:geo")
  }
  
  if(!is.null(lang)) {
    query <- paste(query, paste0("lang:", lang))
  }
  if(!is.null(conversation_id)) {
    query <- paste(query, paste0("conversation_id:", conversation_id))
  }
  
  return(query)
}


ls_files <- function(data_path, pattern) {
  ## parse and bind
  files <-
    list.files(
      path = file.path(data_path),
      pattern = pattern,
      recursive = T,
      include.dirs = T,
      full.names = T
    )
  
  if (length(files) < 1) {
    stop(paste0("There are no files matching the pattern `", pattern, "` in the specified directory."), call. = FALSE)
  }
  return(files)
}

# Bind tweets stored as JSON files function

bind_tweet_jsons <- function(data_path) {
  files <- ls_files(data_path, "^data_")
  pb = utils::txtProgressBar(min = 0,
                             max = length(files),
                             initial = 0)
  
  json.df.all <- data.frame()
  for (i in seq_along(files)) {
    filename = files[[i]]
    json.df <- jsonlite::read_json(filename, simplifyVector = TRUE)
    json.df.all <- dplyr::bind_rows(json.df.all, json.df)
    utils::setTxtProgressBar(pb, i)
  }
  cat("\n")
  return(json.df.all)
}

# Get tweets from full archive search function

get_all_tweets <-
  function(query = NULL,
           start_tweets,
           end_tweets,
           bearer_token = get_bearer(),
           n = 10000,
           file = NULL,
           data_path = NULL,
           export_query = TRUE,
           bind_tweets = TRUE,
           page_n = 500,
           verbose = TRUE,
           ...) {
    
    # Check if path ending with "/"
    if (!is.null(data_path)){
      if(substr(data_path, nchar(data_path), nchar(data_path)) != "/"){
        data_path <- paste0(data_path,"/")
      }
    }
    
    # Check file storage conditions
    check_data_path(data_path, file, bind_tweets)
    
    # Build query
    built_query <- build_query(query, ...)
    
    create_storage_dir(data_path, export_query, built_query, start_tweets, end_tweets)
    
    # Fetch data
    return(fetch_data(built_query = built_query, data_path = data_path, file = file, bind_tweets = bind_tweets, start_tweets = start_tweets, end_tweets = end_tweets, bearer_token = bearer_token, n = n , page_n = page_n, verbose = verbose))
  }


# Get tweets within bounding box function

get_bbox_tweets <-function(query,
                           bbox,
                           start_tweets,
                           end_tweets,
                           bearer_token = get_bearer(),
                           n = 10000,
                           file = NULL,
                           data_path = NULL,
                           bind_tweets = TRUE,
                           verbose = TRUE,
                           ...) {
  #stop clause for if user sets no place
  if (missing(bbox)) {
    stop("bbox coordinates must be specified for get_bbox_tweets() function")
  }
  get_all_tweets(query = query, start_tweets = start_tweets, end_tweets = end_tweets, bearer_token = bearer_token, n = n, file = file, data_path = data_path, bind_tweets = bind_tweets, verbose = verbose, bbox = bbox, ...)
}



#Extract tweets by bounding box

bio_tweets <-
  get_all_tweets(
    "bounding_box:[36.6008556138 12.3995953596 36.796412288 12.5927004326]",
    "2016-01-01T00:00:00Z",
    "2020-12-31T00:00:00Z",
    bearer_token,
    data_path = "data/",
    bind_tweets = TRUE
  )

#Biosphere bounding box (This are divided because width and height of the bounding box must be less than 25mi)

#36.6008556138 12.3995953596 36.796412288 12.5927004326
#36.6008556138 12.197792565 36.796412288 12.3910471206
#36.605250078 12.0065822712 36.8008067522 12.1999762536
#36.5986583817 11.8066330703 36.7942150559 12.0001705505
#36.5986583817 11.6043856756 36.7942150559 11.7980659076
#36.5986583817 11.4062991125 36.7942150559 11.6001168227
#36.7986095201 11.4019914909 36.9941661942 11.5958121651
#36.7986095201 11.6022332648 36.9941661942 11.7959150031
#36.8008067522 11.8087838907 36.9963634263 12.0023198398
#36.8008067522 12.0022838253 36.9963634263 12.1956809173
#36.8008067522 12.2042354101 36.9963634263 12.3974852302
#36.805201384 12.4017413911 37.0007580582 12.5948448617
#37.0029552903 12.4038873232 37.1985119644 12.596989191
#37.0029552903 12.2063830995 37.1985119644 12.3996313406
#37.0007578905 12.0065822712 37.1963145647 12.1999762536
#36.9985606584 11.8001806719 37.1941173326 11.9937227435
#37.0029552903 11.6065379876 37.1985119644 11.8002167132
#37.0029552903 11.4019914909 37.1985119644 11.5958121651
#37.2029065963 11.4062992768 37.3984632704 11.6001169869
#37.2007093642 11.6022332648 37.3962660383 11.7959150031
#37.2007093642 11.804482315 37.3962660383 11.9980213259
#37.2007093642 11.9958359869 37.3962660383 12.1892377415
#37.2051038284 12.197792565 37.4006605025 12.3910471206
#37.2007093642 12.3995953186 37.3962660383 12.5927003917
#37.3984632704 12.4017413707 37.5940199446 12.5948448413
#37.4028577346 12.2063830585 37.5984144088 12.3996312996
#37.4050549667 12.0044330158 37.6006116409 12.1978285532
#37.4006605025 11.8066330703 37.5962171767 12.0001705505
#37.4006605025 11.6065378234 37.5962171767 11.8002165491
#37.4028577346 11.4062991947 37.5984144088 11.6001169048
#37.6006116409 11.3998373681 37.796168315 11.593659524
#37.6006116409 11.6086902831 37.796168315 11.8023675019
#37.602808873 11.8023314609 37.7983655471 11.9958720023
#37.6006116409 12.0001345357 37.796168315 12.1935331821
#37.6006116409 12.2020877852 37.796168315 12.3953391841
#37.6050061051 12.4038873027 37.8005627792 12.5969891706
#37.8005627792 12.4081791751 37.9961194534 12.6012778368
#37.8027601789 12.2063830176 37.9983168531 12.3996312587
#37.8027601789 12.0044328929 37.9983168531 12.1978284303
#37.8027601789 11.8023313378 37.9983168531 11.9958718793
#37.8027601789 11.6000807964 37.9983168531 11.7937640407
#37.8027601789 11.3998374092 37.9983168531 11.593659565


