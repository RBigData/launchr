cades = function(script) {
    cades_unique = c(
        ## ppn=32 due to condo policy to concentrate cores on nodes when < 32
        paste0("#PBS -l nodes=", nodes, ":ppn=32"), 
        "#PBS -l qos=std",
        "#PBS -q batch",
        "#PBS -W group_list=cades-ccsd",
        "#PBS -m abe"
    )
    
  script
}
