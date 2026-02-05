#!/usr/bin/env python3
# Shebang line - allows the script to be run directly on Unix-like systems

"""Count equations in a Quarto markdown (.qmd) file."""
# Docstring describing the purpose of this script

import sys
# Import sys module for command-line arguments and exit functionality

import re
# Import re module for regular expression pattern matching

from pathlib import Path
# Import Path class for easier file/directory path manipulation


def count_equations(file_path):
    """
    Count different types of equations in a .qmd file.
    
    Detects:
    - Display equations: $$...$$ (can be single or multi-line)
    - Inline equations: $...$ (single dollar signs)
    - LaTeX environments: \begin{equation}, \begin{align}, etc.
    """
    
    with open(file_path, 'r', encoding='utf-8') as f:
        # Open the file in read mode with UTF-8 encoding
        content = f.read()
        # Read the entire file contents into a string
    
    # --- Count display equations ($$...$$) ---
    
    display_pattern = r'\$\$[\s\S]*?\$\$'
    # Pattern explanation:
    # \$\$ matches literal $$
    # [\s\S]*? matches any character (including newlines) non-greedily
    # \$\$ matches closing $$
    
    display_equations = re.findall(display_pattern, content)
    # findall returns a list of all matches
    
    display_count = len(display_equations)
    # Count how many display equations were found
    
    # --- Remove display equations before counting inline ---
    # This prevents $$ from being counted as two inline $ equations
    
    content_no_display = re.sub(display_pattern, '', content)
    # Replace all display equations with empty string
    
    # --- Count inline equations ($...$) ---
    
    inline_pattern = r'(?<!\$)\$(?!\$)([^\$\n]+?)(?<!\$)\$(?!\$)'
    # Pattern explanation:
    # (?<!\$) negative lookbehind - not preceded by $
    # \$ matches literal $
    # (?!\$) negative lookahead - not followed by $
    # ([^\$\n]+?) matches content (no $ or newlines) non-greedily
    # This avoids matching $$ as inline math
    
    inline_equations = re.findall(inline_pattern, content_no_display)
    # findall returns all matches
    
    inline_count = len(inline_equations)
    # Count how many inline equations were found
    
    # --- Count LaTeX environments ---
    
    # List of common LaTeX math environments
    latex_environments = [
        'equation',      # Single numbered equation
        'equation*',     # Single unnumbered equation
        'align',         # Multiple aligned equations, numbered
        'align*',        # Multiple aligned equations, unnumbered
        'gather',        # Multiple centered equations, numbered
        'gather*',       # Multiple centered equations, unnumbered
        'multline',      # Single equation spanning multiple lines
        'multline*',     # Same, unnumbered
        'split',         # Sub-environment for splitting equations
        'aligned',       # Aligned equations (within other math)
        'cases',         # Piecewise functions
        'eqnarray',      # Older alignment (discouraged but still used)
        'eqnarray*',     # Same, unnumbered
    ]
    
    env_counts = {}
    # Dictionary to store count for each environment type
    
    for env in latex_environments:
        # Loop through each environment name
        
        # Escape the * character for regex (it's a special character)
        env_escaped = env.replace('*', r'\*')
        
        pattern = rf'\\begin\{{{env_escaped}\}}'
        # Pattern matches \begin{environment_name}
        # rf'' is a raw f-string (combines raw string with formatting)
        # \{{ and \}} produce literal { and } in the regex
        
        matches = re.findall(pattern, content)
        # Find all occurrences of this environment
        
        if matches:
            # Only add to dict if count > 0
            env_counts[env] = len(matches)
    
    latex_count = sum(env_counts.values())
    # Total count of all LaTeX environments
    
    # --- Return results as a dictionary ---
    
    return {
        'display': display_count,       # $$ ... $$ equations
        'inline': inline_count,         # $ ... $ equations
        'latex_environments': env_counts,  # Detailed breakdown by environment
        'latex_total': latex_count,     # Total LaTeX environments
    }


def main():
    """Main function to process a .qmd file and display equation counts."""
    
    if len(sys.argv) < 2:
        # Check if user provided a filename
        print("Usage: python count_equations.py <filename.qmd>")
        print("       python count_equations.py *.qmd  (multiple files)")
        sys.exit(1)
        # Exit with error if no file provided
    
    files = sys.argv[1:]
    # Get all file arguments (allows processing multiple files)
    
    grand_total = 0
    # Track total across all files
    
    for file_path in files:
        # Process each file
        
        path = Path(file_path)
        # Convert to Path object
        
        if not path.exists():
            # Check if file exists
            print(f"Error: File '{file_path}' not found")
            continue
            # Skip to next file
        
        if not path.suffix == '.qmd':
            # Warn if file doesn't have .qmd extension
            print(f"Warning: '{file_path}' is not a .qmd file")
        
        counts = count_equations(file_path)
        # Get equation counts for this file
        
        # Calculate total for this file
        file_total = counts['display'] + counts['inline'] + counts['latex_total']
        grand_total += file_total
        
        # Print results for this file
        print(f"\n{'='*50}")
        print(f"File: {path.name}")
        print(f"{'='*50}")
        
        print(f"  Display equations ($$...$$):  {counts['display']}")
        print(f"  Inline equations ($...$):     {counts['inline']}")
        
        if counts['latex_environments']:
            # Only print LaTeX section if any were found
            print(f"  LaTeX environments:")
            for env, count in sorted(counts['latex_environments'].items()):
                # Print each environment type and its count
                print(f"    \\begin{{{env}}}: {count}")
        else:
            print(f"  LaTeX environments:           0")
        
        print(f"  ---")
        print(f"  TOTAL equations:              {file_total}")
    
    if len(files) > 1:
        # If multiple files, print grand total
        print(f"\n{'='*50}")
        print(f"GRAND TOTAL across {len(files)} files: {grand_total} equations")
        print(f"{'='*50}")


if __name__ == '__main__':
    # This block only runs if the script is executed directly
    main()