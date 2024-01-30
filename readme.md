Task - Shortest Path in Grid

There are three files in this repository.

1-> grid_Path_without.erl - This file contains the code to find the shortest path from source point to destination path in a grid without obstacles.

2-> grid_path_with.erl - This file contains the code to find the shortest path from source point to destination path in a grid with obstacles.

3-> grid_path_mnesia.erl - This file contains the code to find the shortest path from source point to destination path in a grid with obstacles but here it stores the grid in the mnesia database in the form of a table, with two columns that are, a list containing the coordinates of row and column ([row_index, col_index]) and other is the value stored in the corresponding box.


In the the 2 and 3 file, The obstacles are represented with value 1 in the grid and the rest are with value 0.