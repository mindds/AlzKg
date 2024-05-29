**NOTE:** The README has been updated on 05/28/2024 to reflect that we are no longer appending all files together. Each dataset is processed into PrimeKG format and uploaded to Dropbox after the preprocessing step.


**Guidelines for Adding New Data to AlzKG**

If you're contributing new data from a research paper to AlzKG, please follow these steps:

* **Prepare your data for PrimeKG format** using the **convert_csv_kgraw_format.ipynb** notebook. Follow the example usage provided in the code. This will generate an AlzKG file from your CSV file, with `node_id` initially set to "tbd." Note: Make sure that any new celltypes/states used in the code is entered into **cell_data_dict.csv**.
  
* **Document edge types** by copying and pasting the unique edge types printed by the convert_csv_kgraw_format.ipynb into the **edge_type_data_dict.csv** file
  
* **Do a postprocessing step** using the **postprocess_kg_conversion.ipynb** script.
  
* **Upload your individual AlzKG files** by creating a folder named after your research paper within the provided Dropbox link. Place your AlzKG files in this folder.

**PrimeKG Files List. Location: https://www.dropbox.com/home/CATS-OMICS/AlzKG/PrimeKG**

Original PrimeKG Files:
   1. kg_raw_orig_filtered
   2. nodes_orig_filtered


