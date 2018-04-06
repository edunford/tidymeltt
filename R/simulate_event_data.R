#' simulate_event_data
#'
#' Simulate multiple event datasets given a specified date and spatial range
#' along with disambiguating meta data and taxonomy dimensions. Data simulates
#' the type of data generating process tidymeltt is designed for.
#'
#' @param N simulated sample size
#' @param known.events number of known to event-event matches
#' @param known.episodes number of known episode-episode matches
#' @param known.episodes.events number of known episode-event matches
#' @param n.datasets number of simulated datasets
#' @param time.distort time distortion around the known values
#' @param space.distort spatial distortion around the known values; spatial
#'   units are in kilometers
#' @param episode.window define the maximum time window from which the simulated episodal entries are drawn.
#' @param min_date minimum (lower bound) on the simulated date range
#' @param max_date maximum (upper bound) on the simulated date range
#' @param lat.bounds latitude for the spatial boundaries of the simulated
#'   location
#' @param lon.bounds longitude for the spatial boundaries of the simulated
#'   location
#' @param taxonomies data.frame of number of taxonomy dimensions (k) and depths
#'   (depth).
#'
#' @return a list continating N number of simulated datasets along with with a taxonomy to disambiguate.
#' @export
#'
#' @examples
#'
#' sim.data = simulate_event_data(N=200,known.events = 4,known.episodes = 0,
#'                      known.episodes.events = 0,
#'                      n.datasets = 4,time.distort = 1,space.distort = 0.05,
#'                      episode.window = 10,
#'                      min_date = as.Date("2001-01-01"),max_date =as.Date("2001-02-01"),
#'                      lat.bounds = c(-1,1),lon.bounds =c(-1,1),
#'                      taxonomies = data.frame(k=c(rep(1,4),rep(2,3),rep(3,4)),
#'                                               depth = c(10,5,3,2, 13,6,3, 15,10,5,2)))
#'
#' tax = sim.data$user.taxonomies
#' D1 = sim.data$user.data %>% filter(dataset=="D1")
#' D2 = sim.data$user.data %>% filter(dataset=="D2")
#' D3 = sim.data$user.data %>% filter(dataset=="D3")
#' D4 = sim.data$user.data %>% filter(dataset=="D4")
#'
#'
#'
#'
simulate_event_data = function(N=100,known.events=4,known.episodes=0,
                               known.episodes.events=0,n.datasets=4,
                               time.distort =1,space.distort=1,
                               episode.window = 5,
                               min_date=as.Date("1990-01-01"),
                               max_date=as.Date("1991-01-22"),
                               lat.bounds=c(-90,90),lon.bounds=c(-180,180),
                               taxonomies=data.frame(k=c(rep(1,4),rep(2,3),rep(3,4)),
                                                     depth = c(10,5,3,2, 13,6,3, 15,10,5,2))){
  UseMethod("simulate_event_data")
}

#' @export
simulate_event_data.default = function(N=100,known.events=4,known.episodes=0,
                    known.episodes.events=0,n.datasets=4,
                    time.distort =1,space.distort=1,
                    episode.window = 5,
                    min_date=as.Date("1990-01-01"),
                    max_date=as.Date("1991-01-22"),
                    lat.bounds=c(-90,90),lon.bounds=c(-180,180),
                    taxonomies=data.frame(k=c(rep(1,4),rep(2,3),rep(3,4)),
                                           depth = c(10,5,3,2, 13,6,3, 15,10,5,2))){

  # Essential Checks -----
  if (known.episodes > 0 | known.episodes.events > 0){
    if (max_date-min_date < episode.window){
      stop("Date range cannot be less than the episode.window when simulating episodes.")
    }
  }
  if (known.events == 0 & known.episodes == 0 & known.episodes.events == 0 ){
    stop("No known events specified!")
  }
  if ( ((known.events+known.episodes+known.episodes.events)*n.datasets) >= N ){
    stop("Insufficient N assigned given the number of known matches and datasets.")
  }

  # Boundary
  boundaries = data.frame(lon=lon.bounds,lat=lat.bounds)

  # Metadata/Taxonomy criteria
  sets = taxonomies[,2]

  # GENERATE taxonomies that are internally consisent.
  K = unique(taxonomies[,1])
  taxonomies_internal = list() # Used in simulation
  out.taxonomies = list() # Output
  for (k in K){
    sub = taxonomies[taxonomies[,1]==k,2]
    M = matrix(1:sub[1])
    if (length(sub) > 1){
      for (i in 2:length(sub)){
        M = cbind(M,round(runif(nrow(M),min = 1,sub[i])))
      }
    }
    taxonomies_internal[[paste0("var_",k)]] = M
    # Create output taxonomies
    M2 = c()
    for(d in 1:n.datasets){
      data.source = paste0("D",d)
      base.categories = paste0(data.source,"_v_",M[,1])
      M2 = rbind(M2,cbind(data.source,base.categories,as.data.frame(M)))
    }
    out.taxonomies[[paste0("var_",k)]] = M2
  }

  # CREATE event-event ----------------------------
  if (known.events > 0){
    # Identifying the "known" points
    lon.locs = runif(known.events,boundaries[1,1],boundaries[2,1])
    lat.locs = runif(known.events,boundaries[1,2],boundaries[2,2])
    known.locs = data.frame(lon=lon.locs,lat=lat.locs)

    # Identifying the temporal window
    window = 1:as.integer(max_date - min_date)
    max.window = max(window)
    known.locs$date = sample(window,known.events,replace=T)
    known.locs$enddate = known.locs$date

    # Range of the temporal distortion on the known observations
    if(time.distort>0){
      time.distort = c((time.distort:1)*-1,0,1:time.distort)
    }

    # Calculating points that are dependent on the known points (dataset 1 is
    # always the known)
    output = NULL
    for (i in 1:known.events){
      dist <- rbind(known.locs[i,1:2],geosphere::destPoint(known.locs[i,1:2],
                                                           b=sample(c(0,90,180,270),n.datasets-1,replace=TRUE),
                                                           d=runif(n.datasets-1,0,space.distort)*1000/sqrt(2)))
      date <- c(known.locs[i,3],known.locs[i,3] + sample(time.distort,n.datasets-1,replace=T))
      date = ifelse(date<=0,1,date) # Make sure there are no 0 or negative date terms (due to time distortion)
      enddate <- date

      # Taxonomy levels....
      crit = c()
      for (k in K){
        v = taxonomies_internal[[k]][taxonomies_internal[[k]][,1]==sample(taxonomies_internal[[k]][,1],1),]
        v2 = rep(v,n.datasets)
        temp.crit = t(matrix(v2,ncol=n.datasets,nrow=ncol(taxonomies_internal[[k]])))
        crit = cbind(crit,temp.crit)
      }
      temp = data.frame(dataset=1:n.datasets,dist,date=date,enddate=enddate,crit,match=i)
      output = rbind(output,temp)
    }
  }


  # CREATE episode-episode ----------------------------
  if (known.episodes > 0){ # Episodes-episodes known matches
    # Generate episodal known Points
    ep.lon.locs = runif(known.episodes,boundaries[1,1],boundaries[2,1])
    ep.lat.locs = runif(known.episodes,boundaries[1,2],boundaries[2,2])
    ep.known.locs = data.frame(lon=ep.lon.locs,lat=ep.lat.locs)
    ep.known.locs$date = sample(window,known.episodes,replace=T)
    # Generate a temporal window: random from 1 to specified window
    ep.known.locs$enddate = round((ep.known.locs$date + runif(known.episodes,1,episode.window)))
    # Episode cannot exceed the specified temporal window
    ep.known.locs$enddate = ifelse(ep.known.locs$enddate > max.window,max.window,ep.known.locs$enddate)

    # Calculating points (episodes) that are dependent on the known points
    # (dataset 1 is always the known)
    ep.output = NULL
    for(i in 1:known.episodes){
      dist <- rbind(ep.known.locs[i,1:2],geosphere::destPoint(ep.known.locs[i,1:2],
                                                              b=sample(c(0,90,180,270),n.datasets-1,replace=TRUE),
                                                              d=runif(n.datasets-1,0,space.distort)*1000/sqrt(2)))
      date <- c(ep.known.locs[i,3],ep.known.locs[i,3] + sample(time.distort,n.datasets-1,replace=T))
      date = ifelse(date<=0,1,date) # Make sure there are no 0 or negative date terms (due to time distortion)
      enddate <- c(ep.known.locs[i,4],ep.known.locs[i,4] + sample(time.distort,n.datasets-1,replace=T))
      enddate = ifelse(enddate<=date,date+1,enddate) # ensure the enddate is at least one day more than the start date.
      enddate = ifelse(enddate > max.window,max.window,enddate) # make sure the event does not exceed the maximum window

      # Taxonomy levels....
      crit = c()
      for (k in K){
        v = taxonomies_internal[[k]][taxonomies_internal[[k]][,1]==sample(taxonomies_internal[[k]][,1],1),]
        v2 = rep(v,n.datasets)
        temp.crit = t(matrix(v2,ncol=n.datasets,nrow=ncol(taxonomies_internal[[k]])))

        crit = cbind(crit,temp.crit)
      }
      temp = data.frame(dataset=1:n.datasets,dist,date=date,enddate=enddate,crit,match=i+30000)
      # For simplicity, I build the ambiguity feature out of the episodal portion.
      ep.output = rbind(ep.output,temp)
    }
  }

  # CREATE episode-event ----------------------------
  if (known.episodes.events > 0){ # Episodes-Event Known Matches
    # Generate episodal known Points
    epev.lon.locs = runif(known.episodes.events,boundaries[1,1],boundaries[2,1])
    epev.lat.locs = runif(known.episodes.events,boundaries[1,2],boundaries[2,2])
    epev.known.locs = data.frame(lon=epev.lon.locs,lat=epev.lat.locs)
    epev.known.locs$date = sample(window,known.episodes.events,replace=T)
    # Generate a temporal window: random from 1 to specified window
    epev.known.locs$enddate = round((epev.known.locs$date + runif(known.episodes.events,1,episode.window)))
    # Episode cannot exceed the specified temporal window
    epev.known.locs$enddate = ifelse(epev.known.locs$enddate > max.window,max.window,epev.known.locs$enddate)


    # Calculating points that are dependent on the known points (dataset 1 is
    # always the known) in the episode.
    epev.output = NULL
    for (i in 1:known.episodes.events){
      dist <- rbind(epev.known.locs[i,1:2],geosphere::destPoint(epev.known.locs[i,1:2],
                                                                b=sample(c(0,90,180,270),n.datasets-1,replace=TRUE),
                                                                d=runif(n.datasets-1,0,space.distort)*1000/sqrt(2)))
      # Range of dates that event can occur
      date.range = range(epev.known.locs[i,3],epev.known.locs[i,4])
      gen.dates = sample(date.range[1]:date.range[2],n.datasets-1,replace=T)
      date <- c(epev.known.locs[i,3],gen.dates)
      date = ifelse(date<=0,1,date) # Make sure there are no 0 or negative date terms (due to time distortion)
      enddate <- c(epev.known.locs[i,4],gen.dates)
      enddate = ifelse(enddate > max.window,max.window,enddate) # make sure the event does not exceed the maximum window

      # Taxonomy levels....
      crit = c()
      for (k in K){
        v = taxonomies_internal[[k]][taxonomies_internal[[k]][,1]==sample(taxonomies_internal[[k]][,1],1),]
        v2 = rep(v,n.datasets)
        temp.crit = t(matrix(v2,ncol=n.datasets,nrow=ncol(taxonomies_internal[[k]])))

        crit = cbind(crit,temp.crit)
      }

      temp = data.frame(dataset=1:n.datasets,dist,date=date,enddate=enddate,crit,match=i+40000)
      # For simplicity, I build the ambiguity feature out of the episodal portion.
      epev.output = rbind(epev.output,temp)
    }
  }


  # random points ---------
  # Calculating random points that are dependent on a randomly selected point within the boundary
  if(known.events > 0){ N = N - (known.events*n.datasets) }  # Adjusting the number of random observations that need to be calculated
  if(known.episodes > 0 ){ N = N - (known.episodes*n.datasets) } # Adjustment for episodes-episodes
  if(known.episodes.events > 0 ){ N = N - (known.episodes.events*n.datasets) } # Adjustment for episodes-events
  # Selecting Random locations from within the boundary
  r.locs = data.frame(lon = runif(N,boundaries[1,1],boundaries[2,1]),
                      lat = runif(N,boundaries[1,2],boundaries[2,2])) # scatter points around the random point
  r.locs$date <- sample(window,N,replace = T) # scatter temporal locations given the window range
  if (known.episodes > 0 | known.episodes.events > 0){
    # Assign Enddates
    draw = as.numeric(sample(row.names(r.locs),size = round(nrow(r.locs)/2),replace = F))
    # Events
    r.locs[draw,"enddate"] <- r.locs[draw,"date"]
    # Episodes
    r.locs[draw*-1,"enddate"] <- round((r.locs[draw*-1,"date"] + runif(length(r.locs[draw*-1,"date"]),1,episode.window)))
    r.locs[draw*-1,"enddate"] = ifelse(r.locs[draw*-1,"enddate"] > max.window,max.window,r.locs[draw*-1,"enddate"])
  } else{
    r.locs$enddate <- r.locs$date
  }

  r.locs$dataset <- sample(1:n.datasets,N,replace = T) # assign a "dataset"

  # Taxonomy levels....
  crit = c()
  for (k in K){
    v = taxonomies_internal[[k]][sample(taxonomies_internal[[k]][,1],N,replace = T),]
    crit = cbind(crit,v)
  }
  r.locs2 = data.frame(r.locs,crit);r.locs2$match=0

  # Bringing random and dependent together
  ev = c()
  ep = c()
  epev = c()
  if (known.events > 0){ev = output}
  if (known.episodes > 0){ep = ep.output}
  if (known.episodes.events > 0){epev = epev.output}

  full.data = rbind(ev,ep,epev,r.locs2)
  ordered.out = NULL
  for(i in 1:n.datasets){
    f = full.data[full.data$dataset==i,]
    f = f[order(f$date),]; f$event <- 1:nrow(f)
    f = cbind(f[,c("dataset","event","date","enddate","lat","lon","match")],f[,c(grep("X",colnames(f)))])
    ordered.out = rbind(ordered.out,f)
  }

  # Identify the Known matches and report them
  ordered.out$id = paste(ordered.out$dataset,ordered.out$event,sep="-")
  true.matches = list()
  detect.known = unique(ordered.out$match[ordered.out$match!=0])
  true.matches = as.data.frame(t(apply(matrix(detect.known),1,function(x)ordered.out[ordered.out$match==x,"id"])),stringsAsFactors = F)

  known.set = rep(NA,length(detect.known))
  known.set[which(detect.known>=30000 & detect.known < 40000)] = "episode-episode"
  known.set[which(detect.known>=40000)] = "episode-event"
  known.set[which(detect.known<30000)] = "event-event"
  true.matches$type = known.set

  # Convert Sim Data to emulate real data -----------
  user.data = ordered.out[,c("dataset","event","date","enddate","lat","lon")]

  # Recover Date Range
  date.range = seq(min_date,max_date,by="day")
  user.data$date = date.range[ordered.out$date]
  user.data$enddate = date.range[ordered.out$enddate]
  # Rename Columns
  colnames(user.data)[5] = "latitude"
  colnames(user.data)[6] = "longitude"
  # assign dataset names a unique character identifier
  user.data$dataset = paste0("D",user.data$dataset)

  s = unique(taxonomies[,1])
  s2 = paste0("var_",s) # variable place holders
  taxs = ordered.out[,grep("X",colnames(ordered.out))]
  datasets = ordered.out[,grep("data",colnames(ordered.out))]
  for(i in s){
    sub = taxonomies[taxonomies[,1]==i,]
    # Subset the taxonomy
    s.taxs = as.matrix(taxs[,1:nrow(sub)])
    var = paste0("v_",s.taxs[,1]) # create a base.category using one taxonomy layer
    user.data[,s2[i]] = var # map it onto the user data
    user.data[,s2[i]] = paste(user.data$dataset,user.data[,s2[i]],sep="_") # make unique to each dataset
    taxs = taxs[,1:nrow(sub)*-1] # clean the used taxonomies
  }

  # Return true.matches and output frame
  out.all = list()
  # Return emulated data
  out.all[["user.data"]] <- user.data
  out.all[["user.taxonomies"]] <- out.taxonomies
  out.all[["true.matches"]] <- true.matches
  return(out.all)
}


