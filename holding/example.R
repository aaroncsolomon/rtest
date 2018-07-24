require(parallel)
numCores <- detectCores() - 1
cpuCluster <- makeCluster(numCores)

parLapply(cpuCluster, combos, function(combo){
  
  require(data.table)
 
  
  lapply(seq(0,24,2), function(currTime){
    op <- rbindlist(lapply(seq(1,5,1), function(cycle.cnt){
      
      pairings <- data.table(expand.grid(stem_samp$sample, leaf_samp$sample, root_samp$sample, apex_samp$sample))
      
      pair.out <- pairings[, runif(nrow(mix.data), 1 - perturb, 1 + perturb) * combo$stem * mix.data[, stem_samp$sample, with = F] + 
                             runif(nrow(mix.data), 1 - perturb, 1 + perturb) * combo$leaf * mix.data[, leaf_samp$sample, with = F] + 
                             runif(nrow(mix.data), 1 - perturb, 1 + perturb) * combo$root * mix.data[, root_samp$sample, with = F] + 
                             runif(nrow(mix.data), 1 - perturb, 1 + perturb) * combo$apex * mix.data[, apex_samp$sample, with = F], by = .I]
      
      setnames(pair.out, paste("sample", 1:nrow(pairings), sep = "_"))
      
      pair.out[, gene := mix.data$gene]          
      
      mp <- paste(getwd(), sig.matrix, sep = "/")
      tp <- pair.out
      
      outputs <- main(marker.path = mp, test.samples = tp, bd.list = boundary.list, start.in = 0, stop.in = stop.time, return.ratios = TRUE, error.fun = error.inter, inf.scale = scaling)
      outputs[, c('run', 'noise', 'input.time') := .(as.numeric(cycle.cnt), as.numeric(perturb), as.numeric(currTime))]
    }))
    
    res <- list(results = op, combo = combo)
    
    saveRDS(res, paste0("training_data_stem_", combo$stem, "_leaf_", combo$leaf, '_root_', combo$root, 
                        "_apex_", combo$apex, "_time_", currTime, '_ratio.RDS'))
    
  })
})

stopCluster(cpuCluster)
