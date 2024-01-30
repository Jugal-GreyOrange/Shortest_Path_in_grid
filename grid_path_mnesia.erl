-module(grid_path_mnesia).
-export([main/5]).
-record(grid, {pos, value}). % 'pos' is a list containing the row and column coordinates, 
                             % 'value' is the value stored in the grid at that corresponding coordinate.

% Initializing Mnesia
init_mnesia() ->
    mnesia:start(),
    mnesia:create_schema([node()]),
    mnesia:create_table(grid, [{attributes, record_info(fields, grid)}]).

% Inserting Data into mnesia_db
insert_grid_into_mnesia(A) ->
    Rows = array:size(A),
    Cols = array:size(array:get(1, A)),
    insert_loop(A, Rows, Cols, 0, 0).

insert_loop(A, Rows, _, Row, _) when Row >= Rows ->
    A;
insert_loop(A, Rows, Cols, Row, Col) when Col >= Cols ->
    insert_loop(A, Rows, Cols, Row + 1, 0);
insert_loop(A, Rows, Cols, Row, Col) ->
    Value = get(Row, Col, A),
    Pos = [Row,Col],
    PointRecord = #grid{pos=Pos, value = Value},
    mnesia:transaction(fun() ->
        mnesia:write(PointRecord)
        % io:format("Inserted: ~p~n", [PointRecord]) 
    end),
    insert_loop(A, Rows, Cols, Row, Col + 1).

% Function to Retreive value from database
get_value_from_mnesia(Row, Col) ->
    % io:format("Row : ~p, Col : ~p ~n", [Row, Col]),
    Record = mnesia:dirty_read(grid,[Row,Col]),
    [#grid{pos = [Row,Col],value=Value}] = Record,
    Value.

% Function to create new grid using array.
new(Rows, Cols) ->
    A = array:new(Rows),
    array:map(fun(_X, _T) -> array:new(Cols) end, A).

% Function to get value using index in grid.
get(RowI, ColI, A) ->
    Row = array:get(RowI, A),
    array:get(ColI, Row).

% Function to set value using index in grid.
set(RowI, ColI, Ele, A) ->
    Row = array:get(RowI, A),
    Row2 = array:set(ColI, Ele, Row),
    array:set(RowI, Row2, A).

% Main Function which will be called from shell, in which the user have to pass
% No of rows, No of cols, A list containing the tuples of coordinates, Source Coordinate, Dest Coordinate
main(NoRows, NoCols, Obst, Src, Dest) ->
    init_mnesia(),
    A = new(NoRows, NoCols),
    UpdatedA = set_values(A, Obst),
    insert_grid_into_mnesia(UpdatedA),
    find_shortest_path(NoRows, NoCols, Src, Dest).

% Function to set value at 1 at the coordinates of obstacles.
set_values(A, Obst) ->
    Rows = array:size(A),
    Cols = array:size(array:get(1, A)), % Assuming all rows have the same number of columns
    set_values_loop(A, Rows, Cols, Obst, 0, 0).

set_values_loop(A, Rows, _, _, Row, _) when Row >= Rows ->
    A;
set_values_loop(A, Rows, Cols, Obst, Row, Col) when Col >= Cols ->
    set_values_loop(A, Rows, Cols, Obst, Row + 1, 0);
set_values_loop(A, Rows, Cols, Obst, Row, Col) ->
    Value = case lists:member({Row + 1, Col + 1}, Obst) of
        true  -> 1;
        false -> 0
    end,
    UpdatedA = set(Row, Col, Value, A),
    set_values_loop(UpdatedA, Rows, Cols, Obst, Row, Col + 1).

%Function to find shortest path
find_shortest_path(NoRows, NoCols, Src, Dest) ->
    Queue = queue:new(),
    Visited = [],
    {RowI, ColI} = Src,
    UpdatedQueue = queue:in({RowI, ColI, [Src]}, Queue),
    % io:format(queue:is_empty(UpdatedQueue)),
    while_loop(NoRows, NoCols, Dest, UpdatedQueue, Visited).

%BFS loop for exploring the paths
while_loop(NoRows, NoCols, Dest, Queue, Visited) ->
    % io:format(queue:is_empty(Queue)),
    case queue:is_empty(Queue) of
        true ->
            not_found;
        false ->
            {{value, Element}, UpdatedQueue} = queue:out(Queue),
            {RowI, ColI, Path} = Element,
            
            case {RowI, ColI} of
                Dest ->
                    Path;
                _ ->
                    UpdatedVisited = [{RowI, ColI} | Visited],

                    Neighbors = get_valid_neighbors({RowI, ColI}, NoRows, NoCols, UpdatedVisited),

                    UpdatedQueueWithNeighbors = enqueue_neighbors(Neighbors, Path, UpdatedQueue),

                    while_loop(NoRows, NoCols, Dest, UpdatedQueueWithNeighbors, UpdatedVisited)
            end
    end.

% To push all the valid neighbors in the queue.
enqueue_neighbors(Neighbors, Path, Queue) ->
    lists:foldl(fun({Row, Col}, AccQueue) ->
                        % Update the path with the new neighbor
                        NewPath = Path ++ [{Row, Col}],
                        % Enqueue the new element
                        UpdatedQueue = queue:in({Row, Col, NewPath}, AccQueue),
                        UpdatedQueue
                end, Queue, Neighbors).
    

% To find the valid neighbors
get_valid_neighbors({RowI, ColI}, NoRows, NoCols, Visited) ->
    % io:format("called~n"),
    Neighbors = [{RowI+1, ColI}, {RowI-1, ColI}, {RowI, ColI+1}, {RowI, ColI-1}],
    ValidNeighbors = lists:filter(fun({Row, Col}) ->
        (Row >= 1 andalso Row =< NoRows) andalso
        (Col >= 1 andalso Col =< NoCols) andalso
        not lists:member({Row, Col}, Visited) andalso
        get_value_from_mnesia(Row-1, Col-1) =:= 0
    end, Neighbors),
    ValidNeighbors.

    