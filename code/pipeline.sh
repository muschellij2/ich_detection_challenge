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

Rnosave create_hummans.R -N HUMAN -t 1-200 \
    -l mem_free=8G,h_vmem=10G

Rnosave create_heads.R -N SS -t 1-200 \
    -l mem_free=10G,h_vmem=12G
    
# Rnosave create_heads.R -N SS2 -t 1-200 \
#     -l mem_free=10G,h_vmem=12G -hold_jid_ad SS
    
Rnosave registration.R -N REG -t 1-200 \
    -l mem_free=7G,h_vmem=8G -hold_jid_ad SS

Rnosave pitch_segment.R -t 1-200 \
    -N PITCH \
    -l mem_free=20G,h_vmem=21G,h_stack=512M 
    
# Rnosave create_heads.R -N SS2  -t 1-200 \
#     -l mem_free=5G,h_vmem=6G -hold_jid_ad SS
#     
