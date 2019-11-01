rm(list = ls())
library(keras)
library(dplyr)
setwd(here::here())
source("code/file_exists.R")

outcomes = c("any", "epidural", "intraparenchymal", "intraventricular", 
             "subarachnoid", "subdural")


train_outcomes = file.path("predictions", 
                           "training_outcomes.rds")
train = readr::read_rds(train_outcomes)
test_outcomes = file.path("predictions", 
                          "test_outcomes.rds")
test = readr::read_rds(test_outcomes)

# outfile = file.path("predictions", "cnn_128_data.rds")
# mat = readr::read_rds(outfile)

ifold = as.numeric(Sys.getenv("SGE_TASK_ID"))
if (is.na(ifold)) {
  ifold = 3
}
for (ifold in 2:6) {
  ioutcome = outcomes[ifold]
  out_model =   file.path("cnn", paste0("rstudio_model_", ioutcome, ".h5"))
  out_history =   file.path("cnn", paste0("rstudio_history_", ioutcome, ".rds"))
  if (!file.exists(out_model)) {
    train_dir = file.path("cnn", ioutcome, "train") 
    validation_dir = file.path("cnn", ioutcome, "validation") 
    # All images will be rescaled by 1/255
    train_datagen = image_data_generator(
      rotation_range = 40,
      width_shift_range = 0.2,
      height_shift_range = 0.2,
      shear_range = 0.2,
      zoom_range = 0.2,
      horizontal_flip = TRUE,
      fill_mode = "nearest"
    )
    
    validation_datagen <- image_data_generator()
    train_generator <- flow_images_from_directory(
      # This is the target directory
      train_dir,
      # This is the data generator
      train_datagen,
      color_mode = "grayscale",
      # All images will be resized to 150x150
      target_size = c(128, 128),
      batch_size = 20,
      # Since we use binary_crossentropy loss, we need binary labels
      class_mode = "binary"
    )
    
    validation_generator <- flow_images_from_directory(
      validation_dir,
      validation_datagen,
      color_mode = "grayscale",
      target_size = c(128, 128),
      batch_size = 20,
      class_mode = "binary"
    )
    
    
    
    batch <- generator_next(train_generator)
    str(batch)
    
    
    model <- keras_model_sequential() %>% 
      layer_conv_2d(filters = 32, kernel_size = c(3, 3), activation = "relu",
                    input_shape = c(128, 128, 1)) %>% 
      layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
      layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu") %>% 
      layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
      layer_conv_2d(filters = 128, kernel_size = c(3, 3), activation = "relu") %>% 
      layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
      layer_conv_2d(filters = 128, kernel_size = c(3, 3), activation = "relu") %>% 
      layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
      layer_flatten() %>% 
      layer_dense(units = 512, activation = "relu") %>% 
      layer_dense(units = 1, activation = "sigmoid")
    
    
    model %>% compile(
      loss = "binary_crossentropy",
      optimizer = optimizer_rmsprop(lr = 1e-4),
      metrics = c("acc")
    )
    
    
    history <- model %>% fit_generator(
      train_generator,
      steps_per_epoch = 100,
      epochs = 30,
      validation_data = validation_generator,
      validation_steps = 50
    )
    
    model %>% save_model_hdf5(out_model)
    readr::write_rds(history, out_history)
    
    model %>% evaluate_generator(validation_generator, steps = 50)
    model %>% predict_generator(validation_generator, steps = 50)
    
    test_datagen <- image_data_generator()
    test_dir = file.path("cnn", ioutcome, "test") 
    test_generator <- flow_images_from_directory(
      test_dir,
      test_datagen,
      color_mode = "grayscale",
      target_size = c(128, 128),
      batch_size = 20,
      class_mode = "binary"
    )
    
    # model %>% evaluate(x_test, y_test)
    # model %>% predict_classes(x_test)
    
    
  }
}
