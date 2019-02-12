
get_obj_max = function(obj) {
  give_a_number %>%
    group_by(object) %>%
    summarise(max_price = quantile(price, 0.975)) %>%
    named_vec(object, max_price) %>%
    .[obj] %>%
    return()
}

quantile_errorbars = function(x) {
  return(data.frame(
    y = mean(x),
    ymin = quantile(x, 0.025),
    ymax = quantile(x, 0.975)
  ))
}

get_ecdf = function(d, x) {
  fit = ecdf(d)
  y = fit(x)
  return(y)
}

get_ecdf_with_errorbars = function(df, colname="price", reps=1000) {
  prices = df[[colname]]
  x = df$x
  resamples = replicate(reps, sample(prices, replace = T), simplify=F)
  quantiles = lapply(resamples, function(d) {return(get_ecdf(d, x))}) %>%
    do.call(rbind, .) %>%
    t() %>%
    as.data.frame() %>%
    mutate(x=x) %>%
    gather("run", "val", -x) %>%
    group_by(x) %>%
    summarise(ymin=quantile(val, 0.025),
              ymax=quantile(val, 0.975)) %>%
    as.data.frame()
  df$y = get_ecdf(prices, x)
  df$ymin = quantiles$ymin
  df$ymax = quantiles$ymax
  return(df)
}

plot_priors = function(model_results_file) {
  prior_fit = read.csv(model_results_file,
                                 col.names = c("result_type", "variable",
                                               "IGNORE", "object",
                                               "value", "probability")) %>%
    select(-IGNORE) %>%
    filter(result_type == "price_prior") %>%
    group_by(result_type, variable, object) %>%
    mutate(sample = 1:length(value)) %>%
    ungroup()

  params = prior_fit %>%
    ggplot() +
    aes(x=value, colour=variable, fill=variable) +
    geom_histogram() +
    scale_fill_solarized() +
    scale_colour_solarized() +
    facet_grid(variable~object, scales="free")

  prior_comparison = prior_fit %>%
    filter(result_type == "price_prior") %>%
    spread(variable, value) %>%
    rowwise() %>%
    mutate(price = rlnorm(1, mean=mu, sd = sigma),
           src = "model") %>%
    mutate(object = char(object)) %>%
    ungroup() %>%
    select(price, object, src) %>%
    rbind(give_a_number %>%
            mutate(object = char(object))) %>%
    group_by(object, src) %>%
    filter(price < quantile(price, 0.95)) %>%
    ungroup()

  get_matching_ecdfs = function(df) {
    d = df$price
    obj = df$object[[1]]
    x = seq(1, get_obj_max(obj), length.out=100)
    cdf = get_ecdf(d, x)
    new_df = data.frame(x, cdf)
    return(new_df)
  }

  R_squared = prior_comparison %>%
    group_by(src, object) %>%
    do(get_matching_ecdfs(.)) %>%
    select(x, cdf, src, object) %>%
    spread(src, cdf) %>%
    with(cor(model, data)^2)

  p = prior_comparison %>%
    ggplot() +
    aes(x=price, color=src) +
    stat_ecdf(geom="step") +
    facet_wrap(~object, scales="free",
               ncol=5) +
    ggtitle(paste("Give a Number Data -- R^2 = ", round(R_squared, 3))) +
    scale_colour_solarized()

  last_expt_bins_data = prior_bins %>%
    filter(exp=="12") %>%
    group_by(workerid, object) %>%
    mutate(normed_response = rating/sum(rating)) %>%
    ungroup() %>%
    group_by(object, UB, LB) %>%
    do(mean_cl_boot(.$normed_response)) %>%
    # do(mean_cl_boot(.$rating)) %>%
    ungroup()

  get_obj_max = function(obj) {
    last_expt_bins_data %>%
      group_by(object) %>%
      summarise(max_price = max(LB)) %>%
      named_vec(object, max_price) %>%
      .[obj] %>%
      return()
  }
  draw_curves = function(mu, sigma, obj) {
    last_expt_bins_data %>%
      ungroup() %>%
      mutate(UB = ifelse(is.na(UB), Inf, num(UB))) %>%
      mutate(prob = plnorm(UB, meanlog=mu, sdlog=sigma) - plnorm(LB, meanlog=mu, sdlog=sigma)) %>%
      # mutate(prob = prob / sum(prob)) %>%
      select(object, LB, prob) %>%
      filter(object == obj) %>%
      rename(x = LB) %>%
      return()
  }

  last_expt_bins_fit =  read.csv(model_results_file,
    #"../models/results/results-prior-50000_burn25000_lag10_chain1.csv",
    # "../models/results/results-S1-50_burn25_lag1_chain1_bins_all_listener1normed_rating_ignore_last_bin.csv",
    # "../models/results/results-prior-50000_burn25000_lag10_chain1_normed_rating_ignore_last_bin.csv", # 77minutes
    col.names = c("result_type", "variable",
                  "IGNORE", "object",
                  "value", "probability")) %>%
    select(-IGNORE) %>%
    filter(result_type == "price_prior") %>%
    group_by(result_type, variable, object) %>%
    mutate(sample = 1:length(value)) %>%
    ungroup() %>%
    filter(result_type=="price_prior") %>%
    spread(variable, value) %>%
    group_by(object) %>%
    mutate(sigma = mean(sigma)) %>%
    group_by(object, sigma) %>%
    do(quantile_errorbars(.$mu)) %>%
    ungroup() %>%
    gather("region", "mu", c(y, ymin, ymax)) %>%
    group_by(object, region) %>%
    do(draw_curves(.$mu, .$sigma, char(.$object)))

  nbins = last_expt_bins_data %>%
    group_by(object) %>%
    summarise(N = length(unique(LB))) %>%
    named_vec(object, N)
  last_expt_bins_comparison = last_expt_bins_fit %>%
    select(object, region, x, prob) %>%
    mutate(src="model") %>%
    ungroup() %>%
    rbind(last_expt_bins_data %>%
            rename(x = LB) %>%
            select(-UB) %>%
            gather("region", "prob", c(y, ymin, ymax)) %>%
            mutate(src="data"))
  last_bins_fit_plot = last_expt_bins_comparison %>%
    group_by(object, src, region) %>%
    # mutate(prob = prob/max(prob)) %>%
    filter(x != max(x)) %>%
    ungroup() %>%
    spread(region, prob) %>%
    ggplot() +
    aes(x=x, y=y, ymin=ymin, ymax=ymax, fill=object, linetype=src) +
    geom_ribbon(alpha=1/2) +
    geom_line(aes(colour=object)) +
    facet_wrap(~object, ncol = 5, scales="free") +
    scale_fill_brewer(type="qual", palette = 6) +
    scale_colour_brewer(type="qual", palette = 6) +
    ylab("Normalized slider ratings") +
    xlab("Price range lower bound")

  sum_lower = function(df) {
    df$prob = mapply(function(p, x) {
      sum(df$prob[df$x < x]) %>% return()
    }, df$prob, df$x)
    return(df)
  }

  bins_r_squared = last_expt_bins_comparison %>%
    group_by(object, region, src) %>%
    do(sum_lower(.)) %>%
    filter(region=="y") %>%
    spread(src, prob) %>%
    with(cor(model, data)^2)

  last_bins_ecdf = last_expt_bins_comparison %>%
    group_by(object, region, src) %>%
    do(sum_lower(.)) %>%
    spread(region, prob) %>%
    ggplot(aes(x=x, y=y, ymin=ymin, ymax=ymax,
               fill=src)) +
    geom_line(aes(colour=src)) +
    geom_ribbon(alpha=1/5) +
    facet_wrap(~object, ncol = 5, scales="free") +
    scale_colour_solarized() +
    scale_fill_solarized() +
    ggtitle(paste("Final Bins Data -- R^2 = ", round(bins_r_squared, 3))) +
    geom_hline(yintercept = 1, linetype="dashed", colour="black", alpha=1/5) +
    ylab("Probability") +
    xlab("Price")

  return(list(
    p=p, df=prior_comparison, R_squared=R_squared,
    fit=prior_fit, params=params,
    last_bins_ecdf=last_bins_ecdf, bins_r_squared=bins_r_squared,
    last_bins_fit_plot=last_bins_fit_plot
    ))
}


plot_bins = function(expt_label="07c") {

  rename_list = function(lst, name, new_name) {
    lst[[new_name]] = lst[[name]]
    lst[[name]] = NULL
    return(lst)
  }
  select_expt = function(lst, expt_label) {
    # for each object
    for (name in names(lst)) {
      lst[[name]] = lst[[name]][[expt_label]]
      lst[[name]][["dollar_amount_lookups"]] = NULL
      lst[[name]][["theta_lookups"]] = NULL
      lst[[name]]$upper[[length(lst[[name]]$upper)]] = NA
      lst[[name]]$upper = unlist(lst[[name]]$upper)
    }
    do.call(rbind, lapply(names(lst), function(name) {
      lst[[name]] %>% as.data.frame() %>%
        mutate(object = name) %>% return()
    })) %>% return()
  }
  bins = RJSONIO::fromJSON("../models/results/bins.json") %>%
    rename_list("coffee maker", "coffee_maker") %>%
    select_expt(expt_label) %>%
    gather("variable", "value", -object) %>%
    group_by(object, variable) %>%
    mutate(bin_number = 1:length(value)) %>%
    spread(variable, value)
  theta_bins = bins %>%
    select(object, mid, theta_prob) %>%
    rename(upper_theta = mid) %>%
    mutate(lower_theta = c(0, upper_theta[1:(length(upper_theta)-1)])) %>%
    gather("var", "theta", c(upper_theta, lower_theta)) %>%
    select(-var)
  p = bins %>%
    ggplot() +
    geom_point(aes(x=mid, y=0)) +
    geom_point(aes(x=theta, y=theta_prob), alpha=0.2) +
    geom_vline(aes(xintercept=lower), alpha=0.2) +
    geom_ribbon(data=theta_bins, aes(x=theta, ymin=0, ymax=theta_prob), alpha=0.2) +
    facet_wrap(~object, ncol = 5, scales="free")
  return(p)
}


plot_concrete = function(model_results_file, zscore=F) {

  # load_concrete_model
  concrete_fit = read.csv(model_results_file,
                          col.names = c("result_type", "dollar_amount",
                                        "expt_id", "object",
                                        "value", "probability")) %>%
    filter(result_type == "S1") %>%
    group_by(dollar_amount, expt_id, object) %>%
    do(quantile_errorbars(.$value)) %>%
    ungroup() %>%
    mutate(src = "model")

  if (zscore) {
  concrete_data = df %>%
    mutate(response = num(response)) %>%
    group_by(id, workerid) %>%
    # group_by(id) %>%
    mutate(response = scale(response, center=T, scale=T)) %>%
    ungroup() %>%
    filter(qtype=="concrete") %>%
    rename(expt_id = id) %>%
    group_by(dollar_amount, expt_id, object) %>%
    do(mean_cl_boot(.$response)) %>%
    ungroup() %>%
    mutate(src = "data")
  } else {
    concrete_data = df %>%
      mutate(response = num(response)) %>%
      group_by(id, workerid) %>%
      mutate(response = response/9) %>%
      ungroup() %>%
      filter(qtype=="concrete") %>%
      rename(expt_id = id) %>%
      group_by(dollar_amount, expt_id, object) %>%
      do(mean_cl_boot(.$response)) %>%
      ungroup() %>%
      mutate(src = "data")
  }

  concrete_comparison = concrete_data %>%
    rbind(concrete_fit) %>%
    mutate(dollar_amount = num(dollar_amount))

  plot_responses = concrete_comparison %>%
    # filter(expt_id=="07c") %>%
    ggplot() +
    aes(x=dollar_amount, y=y, ymin=ymin, ymax=ymax, colour=src) +
    geom_pointrange() +
    facet_wrap(~object, scale="free_x", ncol = 5) +
    ylim(0, 1) +
    scale_colour_solarized()

  R_squared = concrete_comparison %>%
    select(-c(ymin, ymax)) %>%
    spread(src, y) %>%
    lm(data~model, data=.) %>%
    summary() %>%
    .$r.squared

  plot_correlation = concrete_comparison %>%
    gather("var", "val", c(y, ymin, ymax)) %>%
    unite("tmp", src, var, sep=".") %>%
    spread(tmp, val) %>%
    ggplot() +
    aes(x=model.y, xmin=model.ymin, xmax=model.ymax,
        y=data.y, ymin=data.ymin, ymax=data.ymax) +
    ylab("Data") +
    xlab("Model") +
    geom_abline(slope = 1, intercept = 0, alpha=0.2) +
    geom_pointrange() +
    geom_errorbarh()

  return(list(plot_responses=plot_responses,
              R_squared=R_squared,
              plot_correlation=plot_correlation,
              concrete_comparison=concrete_comparison,
              df=concrete_comparison))

}


plot_inductive = function(model_results_file, zscore=F) {
  if (zscore) {
    inductive = df %>%
      mutate(response = num(response)) %>%
      group_by(id, workerid) %>%
      # group_by(id) %>%
      mutate(response = scale(response, center=T, scale=T)) %>%
      ungroup() %>%
      filter(qtype=="inductive") %>%
      select(id, object, dollar_amount, response) %>%
      rename(expt_id = id) %>%
      group_by(expt_id, dollar_amount, object) %>%
      do(mean_cl_boot(.$response)) %>%
      ungroup() %>%
      mutate(src = "data")
  } else {
    inductive = df %>%
      mutate(response = num(response)/9) %>%
      filter(qtype=="inductive") %>%
      select(id, object, dollar_amount, response) %>%
      rename(expt_id = id) %>%
      group_by(expt_id, dollar_amount, object) %>%
      do(mean_cl_boot(.$response)) %>%
      ungroup() %>%
      mutate(src = "data")
  }

  epsilons = inductive %>%
    group_by(expt_id, object, dollar_amount) %>%
    summarise()

  l0_comparison = read.csv(model_results_file,
                           col.names = c("result_type", "dollar_amount",
                                         "expt_id", "object",
                                         "value", "probability")) %>%
    filter(result_type == "Inductive") %>%
    group_by(expt_id, object, dollar_amount) %>%
    do(quantile_errorbars(.$value)) %>%
    ungroup() %>%
    mutate(dollar_amount = num(dollar_amount)) %>%
    mutate(src="model") %>%
    rbind(inductive)

  plot_responses= l0_comparison %>%
    ggplot() +
    aes(x=dollar_amount, y=y, ymin=ymin, ymax=ymax, colour=src) +
    geom_pointrange() +
    facet_wrap(~object, scale="free_x", ncol = 5) +
    ylim(0, 1) +
    scale_colour_solarized()

  R_squared = l0_comparison %>%
    select(-c(ymin, ymax)) %>%
    spread(src, y) %>%
    lm(data~model, data=.) %>%
    summary() %>%
    .$r.squared

  plot_correlation = l0_comparison %>%
    gather("var", "val", c(y, ymin, ymax)) %>%
    unite("tmp", src, var, sep=".") %>%
    spread(tmp, val) %>%
    ggplot() +
    aes(x=model.y, xmin=model.ymin, xmax=model.ymax,
        y=data.y, ymin=data.ymin, ymax=data.ymax) +
    ylab("Data") +
    xlab("Model") +
    geom_abline(slope = 1, intercept = 0, alpha=0.2) +
    geom_pointrange() +
    geom_errorbarh()

  return(list(p=plot_responses,
              R_squared=R_squared,
              pcor=plot_correlation,
              df=l0_comparison))
}

plot_sorites_cor = function(concrete_results, inductive_results, all_experiments) {
  sorites_results_wide = concrete_results$df %>%
    mutate(qtype="concrete") %>%
    rbind(inductive_results$df %>% mutate(qtype="inductive")) %>%
    gather("region", "prob", c(y, ymin, ymax)) %>%
    unite("variable", src, region) %>%
    spread(variable, prob)
  concrete_r2 = sorites_results_wide %>%
    filter(expt_id=="11") %>%
    filter(qtype=="concrete") %>%
    with(cor(model_y, data_y)^2) %>%
    round(3)
  inductive_r2 = sorites_results_wide %>%
    filter(expt_id=="11") %>%
    filter(qtype=="inductive") %>%
    with(cor(model_y, data_y)^2) %>%
    round(3)
  if (all_experiments) {
    sorites_results_wide %>%
      ggplot() +
      aes(x=model_y, xmin=model_ymin, xmax=model_ymax,
          y=data_y, ymin=data_ymin, ymax=data_ymax,
          colour=expt_id, shape=object) +
      # geom_abline(intercept=0, slope = 1, alpha=0.5) +
      geom_errorbarh(alpha=1/3) +
      geom_pointrange(alpha=1/3) +
      facet_wrap(~qtype, scales="free") +
      scale_colour_solarized() +
      geom_text(
        data=data.frame(
          qtype=c("concrete", "inductive"),
          r2=c(concrete_r2, inductive_r2),
          model_y=0.3, data_y=1) %>%
          mutate(model_ymin=model_y,
                 model_ymax=model_y,
                 data_ymin=data_y,
                 data_ymax=data_y,
                 object="watch"),
        aes(label=r2, x=0.3, y=1),
        x=0.3,
        y=1,
        colour="black"
      )
  } else {
    sorites_results_wide %>%
      filter(expt_id == "11") %>%
      ggplot() +
      aes(x=model_y, xmin=model_ymin, xmax=model_ymax,
          y=data_y, ymin=data_ymin, ymax=data_ymax,
          colour=object) +
      # geom_abline(intercept=0, slope = 1, alpha=0.5) +
      geom_errorbarh() +
      geom_pointrange() +
      facet_wrap(~qtype, scales="free") +
      scale_colour_solarized() +
      geom_text(
        data=data.frame(
          qtype=c("concrete", "inductive"),
          r2=c(concrete_r2, inductive_r2),
          model_y=0.3, data_y=1) %>%
          mutate(model_ymin=model_y,
                 model_ymax=model_y,
                 data_ymin=data_y,
                 data_ymax=data_y,
                 object="watch"),
        aes(label=r2, x=0.3, y=1),
        x=0.3,
        y=1,
        colour="black"
      )
  }
}

plot_sorites_curves = function(sorites_results) {
  sorites_results %>%
    ggplot() +
    aes(x=dollar_amount, y=y, ymin=ymin, ymax=ymax,
        colour=src, group=paste(qtype, expt_id)) +
    geom_pointrange() +
    facet_wrap(qtype~object, scale="free_x", ncol = 5) +
    scale_colour_solarized()
}

plot_sorites = function(model_results_file, zscore, all_experiments) {
  priors_results = plot_priors(model_results_file)
  concrete_results = plot_concrete(model_results_file, zscore=zscore)
  inductive_results = plot_inductive(model_results_file, zscore=zscore)
  sorites_results = concrete_results$df %>%
    mutate(qtype="concrete") %>%
    rbind(inductive_results$df %>% mutate(qtype="inductive"))
  if (!all_experiments) {
    sorites_results = sorites_results %>%
      filter(expt_id=="11")
  }

  final_sorites_curves = plot_sorites_curves(sorites_results)
  final_prior_ecdf = priors_results$last_bins_ecdf
  final_sorites_cor =plot_sorites_cor(concrete_results, inductive_results, all_experiments)

  return(list(
    final_sorites_curves = final_sorites_curves,
    final_prior_ecdf = final_prior_ecdf,
    final_sorites_cor = final_sorites_cor,
    giveanumber_ecdf = priors_results$p
  ))
}
