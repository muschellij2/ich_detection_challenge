cd $structural/ich_detection_challenge/code

stage_number=2
if [[ ! -f ../stage_${stage_number}_data.rds ]];

  if [[ ! -f ../stage_${stage_number}_test.rds ]];
  then 
  	Rnosave make_test.R -N TEST ;
  fi
  
  
  if [[ ! -f ../stage_${stage_number}_train.rds ]];
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

Rnosave create_heads.R -N SS -t 1-200 \
    -l mem_free=12G,h_vmem=14G

Rnosave create_humans.R -N HUMAN -t 1-200 \
    -l mem_free=8G,h_vmem=10G -hold_jid_ad SS  
# Rnosave create_heads.R -N SS2 -t 1-200 \
#     -l mem_free=10G,h_vmem=12G -hold_jid_ad SS
    
Rnosave registration.R -N REG -t 1-200 \
    -l mem_free=8G,h_vmem=10G -hold_jid_ad SS

Rnosave pitch_segment.R -t 1-200 \
    -N PITCH -hold_jid_ad SS \
    -l mem_free=20G,h_vmem=21G,h_stack=512M 
    
Rnosave within_slice_stats.R -N STATS -t 1-200 \
    -l mem_free=8G,h_vmem=9G -hold_jid_ad SS 
    
Rnosave collapse_stats.R -N COLLSTATS \
    -l mem_free=8G,h_vmem=9G -hold_jid STATS
    
Rnosave fit_model.R -N MODEL -t 7-12 \
    -l mem_free=40G,h_vmem=41G -hold_jid COLLSTATS    
    
Rnosave cnn_128_data.R -N DATACNN -t 1 \
    -l mem_free=10G,h_vmem=11G
        
# qrsh -l gpu,mem_free=8G,h_vmem=9G,h_stack=128M 

qsub -cwd -l gpu,mem_free=100G,h_vmem=101G,h_stack=128M \
  -N CNNMOD cnn.sh 
# Rnosave create_heads.R -N SS2  -t 1-200 \
#     -l mem_free=5G,h_vmem=6G -hold_jid_ad SS
#     
