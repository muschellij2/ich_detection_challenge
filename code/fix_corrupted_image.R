cd ~/tmp_copy
dcmdump ID_5005bcb25.dcm +W ./
cp ID_6431af929.dcm ID_6431af929_new.dcm
dcmodify -if "PixelData=ID_5005bcb25.dcm.0.raw" ID_6431af929_new.dcm
  
cp ID_6431af929_new.dcm $structural/ich_detection_challenge/stage_1_train_images/ID_6431af929.dcm
