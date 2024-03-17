**PrimeKG Files List. Location: https://www.dropbox.com/home/CATS-OMICS/AlzKG/PrimeKG**
1. Original PrimeKG Files:
   1. kgraw_orig_filtered
   2. nodes_orig_filtered
2. PrimeKG + data from Mathys et al:
   1. Ex_kg
   2. In_kg
   3. Oli_kg
   4. Opc_kg
   5. Mic_kg
   6. kgraw_with_mathys
   7. nodes_with_mathys
3. PrimeKG + data from Mathys et al and Lau et al:
   1. kgraw_with_mathys_lau
   2. nodes_with_mathys_lau


**Guidelines for Adding New Data**
If you're contributing new data from a research paper to PrimeKG:
* Prepare your data for PrimeKG format using the **convert_csv_kgraw_format.ipynb** notebook. This script processes data for specific cell types or states and formats it for PrimeKG. It creates smaller, separate files with the node_id initially set to "tbd". Create a folder named after your research paper within the Dropbox link provided above, and upload your files there.
* Append the prepared data to the PrimeKG master files with the **append_to_master_kg.py** script. This script integrates the prepared data files into the main PrimeKG dataset and assigns definitive node_ids. After appending, upload the updated file to the Dropbox folder, adhering to the naming convention: {current_filename}_{research_paper_name}.csv.
* Update the readme with the details of your paper.


Mathys et al Overview
* Celltype nodes added: 6
* Gene nodes added: 845
* Edges added: 
* Sample graph:

Lau et al Overview
* Celltype nodes added: 1
* Gene nodes added: 102
* Edges added:
* data from fig 2C
