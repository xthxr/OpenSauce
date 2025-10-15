<?php
// Function to perform bubble sort on an array
function bubble_Sort($my_array )
{
    // Loop until no swaps are made
    do
    {
        $swapped = false; // Flag to track if any elements were swapped
        // Iterate over the array
        for( $i = 0, $c = count( $my_array ) - 1; $i < $c; $i++ )
        {
            // Compare adjacent elements
            if( $my_array[$i] > $my_array[$i + 1] )
            {
                // Swap elements if they are in the wrong order
                list( $my_array[$i + 1], $my_array[$i] ) =
                        array( $my_array[$i], $my_array[$i + 1] );
                $swapped = true; // Set swapped flag to true
            }
        }
    }
    // Continue loop until no swaps are made
    while( $swapped );
    
    return $my_array; // Return the sorted array
}