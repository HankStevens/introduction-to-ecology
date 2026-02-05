#!/usr/bin/env python3
# Shebang line - allows the script to be run directly on Unix-like systems

"""Count all PNG files in a directory and its subdirectories."""
# Docstring describing the purpose of this script

import sys
# Import sys module for command-line arguments and exit functionality

from pathlib import Path
# Import Path class for easier file/directory path manipulation

from collections import defaultdict
# Import defaultdict for easily counting files per directory


def count_pngs(folder_path='.'):
    """Count PNG files recursively in a folder hierarchy."""
    # Function that traverses all subdirectories and counts .png files
    
    folder = Path(folder_path)
    # Convert the string path to a Path object for easier manipulation
    
    if not folder.is_dir():
        # Check if the path is actually a directory
        print(f"Error: '{folder_path}' is not a directory")
        sys.exit(1)
        # Exit the script with error code 1 (non-zero = error)
    
    png_files = list(folder.rglob('*.png'))
    # rglob('*.png') recursively finds all .png files in folder and ALL subdirectories
    # Unlike glob(), rglob() searches the entire directory tree
    # Convert to list so we can iterate multiple times and get length
    
    if not png_files:
        # Check if the list is empty (no PNG files found)
        print(f"No PNG files found in {folder_path} or its subdirectories")
        return
        # Exit the function early if no files to process
    
    # Count files per directory for a detailed breakdown
    files_by_dir = defaultdict(list)
    # defaultdict(list) creates a dictionary where missing keys auto-create empty lists
    # This will map each directory to a list of PNG files it contains
    
    for file in png_files:
        # Loop through each PNG file found
        
        parent_dir = file.parent
        # file.parent gives the directory containing this file
        
        files_by_dir[parent_dir].append(file.name)
        # Add the filename to the list for its parent directory
    
    # Print results organized by directory
    print(f"PNG files in '{folder_path}' and subdirectories:\n")
    # Header for the output
    
    for directory in sorted(files_by_dir.keys()):
        # Loop through each directory that contains PNG files
        # sorted() arranges directories alphabetically
        
        files = files_by_dir[directory]
        # Get the list of PNG files in this directory
        
        relative_dir = directory.relative_to(folder)
        # Convert absolute path to relative path from the starting folder
        # Makes output cleaner and easier to read
        
        dir_display = str(relative_dir) if str(relative_dir) != '.' else '(root)'
        # If relative path is '.', display as '(root)' for clarity
        # Otherwise, use the relative path string
        
        print(f"  {dir_display}: {len(files)} file(s)")
        # Print the directory name and count of PNG files
        # len(files) gives the number of files in this directory
    
    print()
    # Print a blank line for spacing
    
    print("-" * 40)
    # Print a separator line (40 dashes)
    
    print(f"Total: {len(png_files):,} PNG files in {len(files_by_dir)} directories")
    # Print the grand total of PNG files and number of directories containing them
    # :, formats numbers with commas (e.g., 1,234)


def main():
    """Entry point for the script."""
    # Main function that handles command-line arguments
    
    folder = sys.argv[1] if len(sys.argv) > 1 else '.'
    # Check if a command-line argument was provided
    # sys.argv[0] is the script name, sys.argv[1] is the first argument
    # If no argument provided, default to current directory '.'
    
    count_pngs(folder)
    # Call the counting function with the folder path


if __name__ == '__main__':
    # This block only runs if the script is executed directly
    # (not when imported as a module)
    
    main()
    # Call the main function to start the script

