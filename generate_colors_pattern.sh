#!/bin/bash

# Location of the colorsPattern.css file
CSS_FILE="./public/colorsPattern.css"
OUTPUT_FILE="./public/colorsPattern.css"

# Check if the CSS file exists
if [[ ! -f "$CSS_FILE" ]]; then
    echo "Error: $CSS_FILE does not exist."
    exit 1
fi

# Define an array to hold the color variables
color_variables=(
    "primary-text"
    "primary-text-h"
    "secondary-text"
    "link-c"
    "bg-primary-button"
    "primary-button-b"
    "bg-primary-button-h"
    "txt-primary-button"
    "bg-secondary-button"
    "bg-secondary-button-h"
    "secondary-button-b"
    "txt-secondary-button"
    "txt-input"
    "txt-input-b"
    "bg-input"
    "input-f"
    "ph-input"
)

# Start generating the CSS content
css_content="@layer colorConfig {\n    :root {\n"

# Prompt for each color
for var in "${color_variables[@]}"; do
    # Define the default color as a variable
    default_color="var(--DEFAULT-$var)"

    # Prompt the user for a color
    read -p "Enter a color for $var (default: $default_color): " user_color

    # Use the user input or the default color if input is empty
    color_to_use="${user_color:-$default_color}"

    # Append the color definition to the CSS content
    css_content+="        --$var: $color_to_use;\n"
done

# Finish the CSS content
css_content+="    }\n}"

# Write the generated CSS to the output file
echo -e "$css_content" > "$OUTPUT_FILE"

echo "Generated color configuration in $OUTPUT_FILE."
