import pandas as pd

def general_purpose_kg_tools(file_path, operation_mode, node_types=None):
    """
    Perform various operations on knowledge graphs following the PrimeKG format, such as filtering by node types,
    extracting a nodes file, and printing unique node types.

    Args:
        file_path (str): The path to the CSV file to be processed.
        operation_mode (str): Operation mode;
            - 'retain_node_types': to retain rows where both x_type and y_type match the specified node types,
            - 'delete_node_types': to remove rows where either x_type or y_type match the specified node types,
            - 'print_node_types': to print unique node types in the x_type column,
            - 'extract_nodes_file': to extract specific columns and ensure uniqueness based on x_id, x_type, x_name.
            - node_types (list, optional): List of node types to either retain or remove, based on the operation mode.
              Required for 'retain_node_types' and 'delete_node_types' modes.

    Returns:
        None: Outputs a file or prints data based on the operation mode.
    """
    # Load the dataset
    kg_df = pd.read_csv(file_path)

    if operation_mode == 'print_node_types':
        # Print all unique node types in the x_type column
        unique_node_types = kg_df['x_type'].unique()
        print("Unique node types in x_type column:", unique_node_types)

    elif operation_mode == 'extract_nodes_file':
        # Extract specified columns and remove duplicates based on concatenation of x_id, x_type, x_name
        nodes_df = kg_df[['x_id', 'x_type', 'x_name', 'x_source']].drop_duplicates(subset=['x_id', 'x_type', 'x_name'])
        output_path = file_path.replace('.csv', '_nodes.csv')
        nodes_df.to_csv(output_path, index=False)
        print(f"Nodes data written to {output_path}")

    else:
        if node_types is None:
            raise ValueError("node_types must be provided for 'retain_node_types' or 'delete_node_types' operation modes.")

        if operation_mode == 'retain_node_types':
            # Filter to retain rows with specified node types
            filtered_kg_df = kg_df[(kg_df['x_type'].isin(node_types)) & (kg_df['y_type'].isin(node_types))]
        elif operation_mode == 'delete_node_types':
            # Filter to remove rows with specified node types
            filtered_kg_df = kg_df[~((kg_df['x_type'].isin(node_types)) & (kg_df['y_type'].isin(node_types)))]
        else:
            raise ValueError("Invalid operation mode. Choose 'retain_node_types', 'delete_node_types', 'print_node_types', or 'extract_nodes_file'.")

        if operation_mode in ['retain_node_types', 'delete_node_types']:
            # Write the filtered DataFrame to a new CSV file
            output_path = file_path.replace('.csv', '_filtered.csv')
            filtered_kg_df.to_csv(output_path, index=False)
            print(f"Filtered data written to {output_path}")

# Example usage
file_path = '/content/drive/My Drive/primekg_files/kg_raw_orig.csv'  # Adjust the path as needed
operation_mode = 'print_node_types'  # Can be 'retain_node_types', 'delete_node_types', or 'print_node_types'
node_types = ['gene/protein', 'pathway', 'disease']  # Specify node types for 'retain_node_types' or 'delete_node_types' modes

general_purpose_kg_tools(file_path, operation_mode, node_types)

     
