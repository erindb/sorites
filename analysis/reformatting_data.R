source(paste(project_dir, "/analysis/utils.R", sep=""))

load_give_a_number = function() {
  give_a_number = read.csv(data_dir("/priors/give-a-number.csv")) %>%
    filter(item != "electric.kettle") %>%
    select(-X) %>%
    mutate(object = ifelse(item == "coffee.maker", "coffee maker", char(item))) %>%
    select(-item)
  write.csv(give_a_number, data_dir("/priors/reformatted_give_a_number.csv"));
  return(give_a_number)
}

load_sorites = function() {
  ## this is the script for reformatting sorites data into one single datafile
  sorites_designs = read.csv(data_dir("sorites/sorites_designs.csv"),
                             stringsAsFactors = F)
  lookup_date = function(experiment_id) {
    return((
      sorites_designs %>%
        filter(char(id) == experiment_id)
    )$date[[1]])
  }
  find_sorites_filename = function(experiment_id) {
    directory = data_dir("sorites/")
    filenames = list.files(directory)
    filename = filenames[
      grep(paste("data_exp", experiment_id, sep=""),
           filenames)
      ]
    return(paste(directory, filename, sep = ""))
  }
  read_sorites_data = function(experiment_id) {
    raw_df_all_columns = read.csv(
      find_sorites_filename(experiment_id),
      stringsAsFactors = F
    )

    ## for experiment 10 data format:
    if ("Answer.responses" %in% names(raw_df_all_columns)) {
      raw_df_all_columns$Answer.questions = raw_df_all_columns$Answer.responses
    }
    if (!("Answer.phrasing" %in% names(raw_df_all_columns))) {
      raw_df_all_columns$Answer.phrasing = NA
    }
    if ("Answer.time_in_minutes" %in% names(raw_df_all_columns)) {
      raw_df_all_columns$time_in_minutes = raw_df_all_columns$Answer.time_in_minutes
    } else {
      raw_df_all_columns$time_in_minutes = NA
    }
    if ("Answer.subj_data" %in% names(raw_df_all_columns)) {
      subj_data = do.call(rbind, lapply(
        raw_df_all_columns$Answer.subj_data,
        function(x) {
          x = fromJSON(x)
          x$asses = NULL
          return(x)
        })) %>%
        as.data.frame
      raw_df_all_columns$Answer.comments = unlist(subj_data$comments)
      raw_df_all_columns$Answer.language = unlist(subj_data$language)
      raw_df_all_columns$Answer.education = unlist(subj_data$education)
      raw_df_all_columns$Answer.age = unlist(subj_data$age)
    } else {
      raw_df_all_columns$Answer.education = NA
    }

    ## select only the useful columns
    raw_df = raw_df_all_columns %>%
      rename(comments = Answer.comments,
             language = Answer.language,
             questions = Answer.questions,
             education = Answer.education,
             phrasing = Answer.phrasing,
             age = Answer.age) %>%
      select(workerid, reward, comments,
             age, language, questions) %>%
      mutate(workerid = paste(workerid, experiment_id, sep="_"))

    reformatted_df = do.call(rbind, mapply(function(qn, workerid_for_merge) {
      return(fromJSON(qn) %>% mutate(workerid = workerid_for_merge))
    }, raw_df$questions, raw_df$workerid, SIMPLIFY = F) %>% unname)
    ## some versions collect reaction time, etc.:
    if (!("rt" %in% names(reformatted_df))) { reformatted_df$rt = NA }
    if (!("level" %in% names(reformatted_df))) { reformatted_df$level = NA }
    if (!("sigs" %in% names(reformatted_df))) { reformatted_df$sigs = NA }
    if (!("qNumber" %in% names(reformatted_df))) { reformatted_df$qNumber = NA }
    if ("qType" %in% names(reformatted_df)) { reformatted_df = reformatted_df%>%rename(qtype=qType) }
    if ("dollarAmt" %in% names(reformatted_df)) {
      reformatted_df = reformatted_df %>% rename(dollar_amount=dollarAmt)
    }
    if ("item" %in% names(reformatted_df)) {
      reformatted_df = reformatted_df %>% rename(object=item)
    }
    df = merge(
      raw_df %>% select(-questions),
      reformatted_df,
      by = c("workerid")
    ) %>%
      mutate(id = experiment_id,
             date = lookup_date(experiment_id))
    if (!("phrasing" %in% names(df))) { df$phrasing = NA }
    return(df)
  }
  df = do.call(rbind,
               lapply(c("00", "01", "07a", "07b", "07c", "10", "11"),
                      read_sorites_data)) %>%
    as.data.frame %>% flatten %>% ungroup %>% mutate(
      dollar_amount = as.numeric(unname(sapply(char(dollar_amount), function(money_string) {
        if (substr(money_string, 1, 1) == "$") {
          return(substr(money_string, start = 2, stop = nchar(money_string)))
        } else {
          return(money_string)
        }
      }))),
      qtype = ifelse(qtype == "eps", "inductive", qtype),
      qtype = ifelse(qtype == "val", "concrete", qtype),
      qtype = factor(qtype),
      phrasing = ifelse(id %in% c("00", "01"), "relative", phrasing),
      response = num(response)
    )
  df = df %>% mutate(
    phrasing = ifelse(phrasing == '"relative"', "relative", phrasing),
    phrasing = ifelse(phrasing == "relative_clause", "relative", phrasing),
    phrasing = ifelse(phrasing == '"conditional"', "conditional", phrasing),
    phrasing = factor(as.character(phrasing)),
    id = factor(id)
  )
  return(df)
}

transform_upper_bounds = function(uppers) {
  numeric_uppers = num(uppers[uppers != "infty"])
  highest_ub = max(numeric_uppers)
  next_highest_ub = max(numeric_uppers[numeric_uppers!=highest_ub])
  return(ifelse(uppers=="infty",
                highest_ub + (highest_ub - next_highest_ub),
                uppers) %>% num)
}

process_bins_data = function(filename) {
  raw_exp = read.csv(filename, stringsAsFactors = F)
  exp = raw_exp %>% filter(Answer.cond!="split") %>%
    gather("question", "responses", c(Answer.1, Answer.2, Answer.3, Answer.4))
  if ("Answer.35" %in% names(raw_exp)) {
    exp_split = raw_exp %>% filter(Answer.cond=="split") %>%
      gather("question", "responses", contains("Answer"))
    exp = rbind(exp, exp_split)
  }
  list_data = mapply(
    function(response, question, workerid, #a5, a6, a7, a8, a9,
             condition) {
      # char(a5) %>% fromJSON # we also collected a max price
      rs = response %>% fromJSON %>% as.data.frame %>%
        mutate(workerid = workerid,
               condition = condition[[1]])
      rs = rs %>% mutate(bin = 1:nrow(rs))
      if ("lower" %in% names(rs)) {
        rs = rs %>% mutate(lowers = num(lower)) %>% select(-lower)
      }
      if ("upper" %in% names(rs)) {
        rs = rs %>% mutate(uppers = num(upper)) %>% select(-upper)
      }
      if ("response" %in% names(rs)) {
        rs = rs %>% rename(responses = response)
      }
      rs = rs %>% mutate(uppers = num(uppers))
      return(rs)
    },
    exp$responses, exp$question, exp$workerid,
    # exp$Answer.5, exp$Answer.6, exp$Answer.7, exp$Answer.8, exp$Answer.9,
    exp$Answer.cond,
    SIMPLIFY=F)
  df = do.call(rbind, list_data) %>%
    group_by(item) %>%
    mutate(
      UB = transform_upper_bounds(num(uppers)),
      LB = num(lowers),
      width = UB-LB,
      rating = num(responses)) %>%
    ungroup() %>%
    select(workerid, item, bin, UB, LB, rating, condition) %>%
    as.data.frame
  row.names(df) = 1:nrow(df)
  return(df)
}

load_priors = function() {
  df3 = process_bins_data(data_dir("/priors/data_exp03_2013_12_03_05.csv")) %>% mutate(exp="03")
  df4 = process_bins_data(data_dir("/priors/data_exp03_2013_12_03_05.csv")) %>% mutate(exp="04")
  df5 = process_bins_data(data_dir("/priors/data_exp05_2013_12_04_15.csv")) %>% mutate(exp="05")
  df6 = process_bins_data(data_dir("/priors/data_exp06_2013_12_04.csv")) %>% mutate(exp="06")
  df8 = process_bins_data(data_dir("/priors/data_exp08_2014_01_31_11.csv")) %>% mutate(exp="08")
  df12 = process_bins_data(data_dir("/priors/data_exp12_2015_04_08.csv")) %>% mutate(exp="12")
  prior_bins_data = do.call(rbind, list(df3, df4, df5, df6, df8, df12)) %>% as.data.frame %>%
    mutate(exp = factor(exp),
           condition = factor(ifelse(condition=="\"split\"", "split", "original"))) %>%
    group_by(workerid, item, exp, condition) %>%
    mutate(
      slider_total = sum(rating),
      normed_rating = ifelse(slider_total==0, 0, rating / slider_total)) %>%
    ungroup %>% as.data.frame %>% rename(object = item)
  write_csv(prior_bins_data, data_dir("/priors/reformatted_bins.csv"))
  return (prior_bins_data)
}
