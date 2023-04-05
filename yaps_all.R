yaps_all<-function(transmitter_ID, date){ 
  tryCatch({
    
    dat <- applySync(toa=fish_detections %>% # sync model for one download
                       dplyr::filter(date(ts)==date) %>% 
                       droplevels() %>% 
                       setDT %>% 
                       split(.$tag) %>% 
                       pluck(transmitter_ID), 
                     hydros=hydros, 
                     sync_model=sync_model)
    
    hydros_yaps <- data.table::data.table(sync_model$pl$TRUE_H)
    colnames(hydros_yaps) <- c('hx','hy','hz')
    
    rbi_min <- 60
    rbi_max <- 120
    
    toa <- getToaYaps(synced_dat=dat, 
                      hydros=hydros_yaps, 
                      pingType='rbi', 
                      rbi_min=rbi_min, 
                      rbi_max=rbi_max)
    
    nobs <- apply(toa, 1, function(k) sum(!is.na(k)))
    
    ####
    # YAPS
    ##
    
    magicYAPS<-function(x){
      tryCatch({runYaps(
        getInp(hydros_yaps, 
               toa, 
               E_dist="Mixture", 
               n_ss=2, 
               pingType="rbi", 
               sdInits=1, 
               rbi_min=rbi_min, 
               rbi_max=rbi_max, 
               ss_data_what="est", 
               bbox=NULL), 
        silent=F, 
        tmb_smartsearch=TRUE, 
        maxIter=5000)},
        error=function(e){NA})}
    
    YAPS_list<-5 %>% 
      rerun(magicYAPS())
    
    magic<-YAPS_list %>%  # the magic number
      purrr::map(purrr::pluck(4)) %>% # get the AIC cols
      purrr::map(purrr::pluck(1)) %>% # take the number
      bind_cols() %>% # make a df
      t() %>% # oops wrong order
      as_tibble %>% # obv
      dplyr::filter(V1==min(V1)) %>% # get the min val
      as.numeric# make it a number
    
    final_track<-YAPS_list %>% 
      purrr::discard(., ~any(is.na(.x))) %>% 
      purrr::compact() %>% 
      keep(., as_mapper(~.x$obj %>% # keep only the run with the lowest AIC
                          pluck(1) == magic)) %>% 
      pluck(1) %>% 
      pluck(8) %>% 
      as_tibble %>% 
      mutate(dat %>% distinct(tag))},
    error=function(e){NA})
}
