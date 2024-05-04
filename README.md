**Guidelines for Adding New Data to AlzKG**

If you're contributing new data from a research paper to AlzKG, please follow these steps:

* **Prepare your data for PrimeKG format** using the **convert_csv_kgraw_format.ipynb** notebook. Follow the example usage provided in the code. This will generate an AlzKG file from your CSV file, with `node_id` initially set to "tbd." Note: Make sure that any new celltypes/states used in the code is entered into **cell_data_dict.csv**.
* **Upload your individual AlzKG files** by creating a folder named after your research paper within the provided Dropbox link. Place your AlzKG files in this folder.
* **Document edge types** by copying and pasting the unique edge types printed by the convert_csv_kgraw_format.ipynb into the **edge_type_data_dict.csv** file.
* **Integrate your data into the AlzKG master dataset** using the **append_to_master_kg.py** script. Once appended, upload the updated file to the Dropbox folder, using the naming convention: **{current_filename}_{research_paper_name}.csv**.
* Update the readme with the details of your paper below.

**PrimeKG Files List. Location: https://www.dropbox.com/home/CATS-OMICS/AlzKG/PrimeKG**
1. Original PrimeKG Files:
   1. kg_raw_orig_filtered
   2. nodes_orig_filtered
2. PrimeKG + data from Mathys et al:
   1. Ex_kg
   2. In_kg
   3. Oli_kg
   4. Opc_kg
   5. Mic_kg
   6. Ast_kg
   7. kgraw_with_mathys
   8. nodes_with_mathys
3. PrimeKG + data from Mathys et al and Lau et al:
   1. kgraw_with_mathys_lau
4. PrimeKG + data from Mathys et al, Lau et al, and Smith et al:
   1. kgraw_with_mathys_lau_smith

**Lau et al Overview**
* data from fig 2C
