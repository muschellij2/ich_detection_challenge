cd $structural/ich_detection_challenge/code

if [[ ! -f ../stage_1_data.rds ]];

  if [[ ! -f ../stage_1_test.rds ]];
  then 
  	Rnosave make_test.R -N TEST ;
  fi
  
  
  if [[ ! -f ../stage_1_train.rds ]];
  then 
  	Rnosave read_labels.R -N LABS ;
  fi

fi

if [[ ! -f ../all_headers.rds ]];
then 

  Rnosave get_hdr.R -N HDR -t 1-200 \
  -l mem_free=5G,h_vmem=5G
  
  Rnosave collapse_hdrs.R -N COLL \
    -l mem_free=8G,h_vmem=10G
  
fi

if [[ ! -f ../wide_headers_with_folds.rds ]];
then 
  Rnosave check_head_data.R -N CHECK \
    -l mem_free=5G,h_vmem=7G
fi


Rnosave create_heads.R -N SS  -t 1-200 \
    -l mem_free=8G,h_vmem=10G
    
    
Rnosave registration.R -N REG  -t 1-200 \
    -l mem_free=8G,h_vmem=10G    
    

# Rnosave create_heads.R -N SS2  -t 1-200 \
#     -l mem_free=5G,h_vmem=6G -hold_jid_ad SS
#     
